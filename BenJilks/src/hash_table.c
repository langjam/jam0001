#include "hash_table.h"
#include <stdio.h>
#include <stdlib.h>
#include <memory.h>
#include <assert.h>
#include <math.h>

#define REHASH_BELOW_LEFT   10

static void clear_table(HashTableEntry *table, int size)
{
    for (int i = 0; i < size; i++)
    {
        table[i].key = NAN;
        table[i].value = NULL;
    }
}

static HashTable hash_table_new_with_size(int size)
{
    HashTable ht;
    ht.size = size;
    ht.used = 0;
    ht.extended_table = NULL;
    clear_table(ht.table, HASH_TABLE_INLINED_SIZE);
    
    if (size > HASH_TABLE_INLINED_SIZE)
    {
        int extended_size = size - HASH_TABLE_INLINED_SIZE;
        ht.extended_table = malloc(sizeof(HashTableEntry) * extended_size);
        clear_table(ht.extended_table, extended_size);
    }
    return ht;
}

HashTable hash_table_new()
{
    return hash_table_new_with_size(HASH_TABLE_INLINED_SIZE);
}

void hash_table_free(HashTable *ht)
{
    if (ht->extended_table)
        free(ht->extended_table);
}

static void rehash(HashTable *ht)
{
    HashTable new_ht = hash_table_new_with_size(ht->size * 2);
    for (HashTableItorator itor = hash_table_itorator(ht); itor.value; hash_table_next(&itor))
        hash_table_set(&new_ht, itor.key, itor.value);

    hash_table_free(ht);
    memcpy(ht, &new_ht, sizeof(HashTable));
}

static int hash_key(double key, int size)
{
    unsigned long long lkey = *(unsigned long long*)&key;
    return lkey % size;
}

static HashTableEntry *entry_at_index(HashTable *ht, int index)
{
    if (index < HASH_TABLE_INLINED_SIZE)
        return &ht->table[index];
    else
        return &ht->extended_table[index - HASH_TABLE_INLINED_SIZE];
}

static HashTableEntry *entry_for_key(HashTable *ht, double key)
{
    int hash = hash_key(key, ht->size);
    for (int i = 0; i < ht->size; i++)
    {
        HashTableEntry *entry = entry_at_index(ht, hash);
        if (isnan(entry->key) || entry->key == key)
            return entry;

        hash = (hash + 1) % ht->size;
    }

    // Not found and no empty entries left
    return NULL;
}

int should_rehash(HashTable *ht)
{
    return (ht->size - ht->used) <= REHASH_BELOW_LEFT;
}

int hash_table_add(HashTable *ht, double key, void *value)
{
    if (should_rehash(ht))
        rehash(ht);

    HashTableEntry *entry = entry_for_key(ht, key);
    assert(entry);

    if (!isnan(entry->key))
        return 0;

    entry->key = key;
    entry->value = value;
    return 1;
}

void hash_table_set(HashTable *ht, double key, void *value)
{   
    if (should_rehash(ht))
        rehash(ht);

    HashTableEntry *entry = entry_for_key(ht, key);
    assert(entry);

    entry->key = key;
    entry->value = value;
}

void hash_table_remove(HashTable *ht, double key)
{
    HashTableEntry *entry = entry_for_key(ht, key);
    if (!entry)
        return;
    if (isnan(entry->key))
        return;

    entry->key = NAN;
    entry->value = NULL;
}

void *hash_table_get(HashTable *ht, double key)
{
    HashTableEntry *entry = entry_for_key(ht, key);
    if (!entry)
        return NULL;
    return entry->value;
}

HashTableItorator hash_table_itorator(HashTable *ht)
{
    HashTableItorator itor;
    itor.table = ht;
    itor.index = -1;
    itor.value = NULL;
    hash_table_next(&itor);
    return itor;
}

void hash_table_next(HashTableItorator *itor)
{
    HashTable *ht = itor->table;
    for (;;)
    {
        itor->index += 1;

        if (itor->index >= ht->size)
        {
            itor->key = NAN;
            itor->value = NULL;
            break;
        }

        HashTableEntry *entry = entry_at_index(ht, itor->index);
        if (!isnan(entry->key))
        {
            itor->key = entry->key;
            itor->value = entry->value;
            break;
        }
    }
}


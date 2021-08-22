#ifndef HASH_TABLES_H
#define HASH_TABLES_H

#define HASH_TABLE_INLINED_SIZE 32

typedef struct _HashTableEntry
{
    double key;
    void *value;
} HashTableEntry;

typedef struct _HashTable
{
    HashTableEntry table[HASH_TABLE_INLINED_SIZE];
    HashTableEntry *extended_table;
    int size;
    int used;
} HashTable;

typedef struct _HashTableItorator
{
    HashTable *table;
    int index;

    double key;
    void *value;
} HashTableItorator;

HashTable hash_table_new();
void hash_table_free(HashTable*);

int hash_table_add(HashTable*, double key, void *value);
void hash_table_set(HashTable*, double key, void *value);
void hash_table_remove(HashTable*, double key);
void *hash_table_get(HashTable*, double key);

HashTableItorator hash_table_itorator(HashTable*);
void hash_table_next(HashTableItorator*);

#endif // HASH_TABLES_H


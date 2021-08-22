#include "preprocess.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

typedef enum _State
{
    STATE_HEADER,
    STATE_HEADER_NAME,
    STATE_HEADER_VALUE,
    STATE_HEADER_SEPERATOR,

    STATE_DESCRIPTION_START_LINE,
    STATE_DESCRIPTION,
    STATE_DESCRIPTION_SEPERATOR,

    STATE_COMMENT,
    STATE_SLASH,
    STATE_CODE,
} State;

int preprocess(FILE *file, char **out_source, size_t *out_size)
{
    fseek(file, 0L, SEEK_END);
    int file_len = ftell(file);
    rewind(file);

    char *buffer = malloc(file_len + 1);
    size_t buffer_pointer = 0;
    State state = STATE_HEADER;

    bool has_name = false;
    bool has_date = false;
    bool has_mood = false;

    // FIXME: Buffer overflow
    char header_name_buffer[80];
    int header_name_pointer = 0;
    for (int i = 0; i < file_len; i++)
    {
        // TODO: Buffer here?
        char c;
        fread(&c, 1, 1, file);

        switch (state)
        {
            case STATE_HEADER:
                if (c == '-')
                {
                    state = STATE_HEADER_SEPERATOR;
                }
                else
                {
                    state = STATE_HEADER_NAME;
                    header_name_buffer[header_name_pointer++] = c;
                }
                break;
            case STATE_HEADER_NAME:
                if (c == ':')
                {
                    header_name_buffer[header_name_pointer] = '\0';
                    header_name_pointer = 0;

                    if (!strcmp(header_name_buffer, "Name"))
                        has_name = true;
                    else if (!strcmp(header_name_buffer, "Date"))
                        has_date = true;
                    else if (!strcmp(header_name_buffer, "Mood"))
                        has_mood = true;
                    state = STATE_HEADER_VALUE;
                }
                else
                {
                    header_name_buffer[header_name_pointer++] = c;
                }
                break;
            case STATE_HEADER_VALUE:
                if (c == '\n')
                    state = STATE_HEADER;
                break;
            case STATE_HEADER_SEPERATOR:
                if (c == '\n')
                    state = STATE_DESCRIPTION_START_LINE;
                break;

            case STATE_DESCRIPTION_START_LINE:
                if (c == '-')
                    state = STATE_DESCRIPTION_SEPERATOR;
                else
                    state = STATE_DESCRIPTION;
                break;
            case STATE_DESCRIPTION:
                if (c == '\n')
                    state = STATE_DESCRIPTION_START_LINE;
                break;
            case STATE_DESCRIPTION_SEPERATOR:
                if (c == '\n')
                    state = STATE_COMMENT;
                break;

            case STATE_COMMENT:
                if (c == '/')
                    state = STATE_SLASH;
                break;
            case STATE_SLASH:
                if (c == '/')
                    state = STATE_CODE;
                else
                    state = STATE_COMMENT;
                break;
            case STATE_CODE:
                if (c == '\n')
                    state = STATE_COMMENT;
                buffer[buffer_pointer++] = c;
                break;
        }
    }

    if (!has_name || !has_date || !has_mood)
    {
        printf("Error: incomplete header\n");
        return 1;
    }

    buffer[buffer_pointer] = '\0';
    *out_source = buffer;
    *out_size = buffer_pointer;
    return 0;
}


/*
 * Simple file I/O
 */

// Reads text from given path
// Warning: You must free the returned char * after use
char* sfio_read_text(const char *path);
void sfio_write_text(const char *path, const char *text);

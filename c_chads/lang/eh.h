#include "lexer.h"
#include "../aid/strbld/builder.h"

#define EH_MESSAGE(...) sbprintf(eh_messagesb(), __VA_ARGS__)

void eh_at_line(usize line, const string src);
void eh_error(usize line, usize col, const string src);
void eh_point(usize col);
void eh_init();
void eh_deinit();
sb_t* eh_messagesb(); 
void eh_set_file(const string file);
void eh_at_token(struct Token *tok, const string src);



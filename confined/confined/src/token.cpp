#include <token.h>

#include <___token.h>

#ifdef USE_WCHAR
#define TOKEN_MACRO_GENERATE_STR(__name) L###__name,
#else
#define TOKEN_MACRO_GENERATE_STR(__name) #__name,
#endif

static const str_t token_strings_[] =
{
    TOKEN_MACRO(TOKEN_MACRO_GENERATE_STR)
};

str_t Token::to_string() const
{
    return token_strings_[_type];
}

#define TOKEN_MACRO(X)             \
    X(TOKEN_ADD)              \
    X(TOKEN_INCREMENT)       \
    X(TOKEN_SUBTRACT)         \
    X(TOKEN_DECREMENT)       \
    X(TOKEN_MULTIPLY)         \
    X(TOKEN_DIVIDE)           \
    X(TOKEN_EQUALS)           \
    X(TOKEN_EXCLAIM_MARK)     \
    X(TOKEN_AT_SIGN)          \
    X(TOKEN_HASHTAG)          \
    X(TOKEN_DOLLAR)          \
    X(TOKEN_PERCENT)          \
    X(TOKEN_CARET)            \
    X(TOKEN_AMPERSAND)        \
    X(TOKEN_L_PAREN)           \
    X(TOKEN_R_PAREN)            \
    X(TOKEN_L_SQUARE_BRACKET)      \
    X(TOKEN_R_SQUARE_BRACKET)     \
    X(TOKEN_L_CURLY_BRACKET)     \
    X(TOKEN_R_CURLY_BRACKET)     \
    X(TOKEN_L_ANGLE_BRACKET)     \
    X(TOKEN_R_ANGLE_BRACKET)     \
    X(TOKEN_IDENTIFIER)    \
    X(TOKEN_STRING_LITERIAL)    \
    X(TOKEN_DECIMAL)    \
    X(TOKEN_HEXADECIMAL)    \
    X(TOKEN_BINARY)    \
    X(TOKEN_WHITESPACE) \
    X(TOKEN_NEW_LINE)    \
    X(TOKEN_TAB)    \
    X(TOKEN_EOF)

#define TOKEN_MACRO_GENERATE_ENUM(__name) __name,

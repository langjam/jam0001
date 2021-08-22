#include "parser.h"
#include "error.h"
#include <list>
#include <utility>
#include <functional>

class Parser
{
private:
    const LexerInfo& _info;
    u32 _index = 0;
    TokenInfo _eof_tok = { .token = Token::TOKEN_EOF, .line_number = 0, .column_number = 0 };

    const TokenInfo& get(u32 i)
    {
        if(i < 0 || i >= _info.token_info.size()) return _eof_tok;

        return _info.token_info[i];
    }

public:
    Parser(const LexerInfo& info)
    :
    _info(info)
    {
    }

    const TokenInfo& current()
    {
        return get(_index);
    }

    const TokenInfo& peek(u32 how_many = 1)
    {
        return get(_index + how_many);
    }

    const TokenInfo& advance(u32 how_many = 1)
    {
        _index += how_many;

        return get(_index);
    }

    void skip(u32 type)
    {
        while(current().token == type)
        {
            advance();
        }
    }

    void skip(u32 type, std::function<void()> callback)
    {
        while(current().token == type)
        {
            callback();

            advance();
        }
    }

    void consume_whitespace()
    {
        skip(Token::TOKEN_WHITESPACE);
        skip(Token::TOKEN_NEW_LINE);
        skip(Token::TOKEN_TAB);
    }

    ParserASTNode* parse_number(ParserInfo& info)
    {
        ParserASTNode* node = nullptr;

        if(current().token.is_number())
        {
            node = new ParserASTNumberNode(current());

            advance();
        }

        consume_whitespace();

        return node;
    }

    ParserASTNode* parse_identifier(ParserInfo& info)
    {
        ParserASTNode* node = nullptr;

        if(current().token.is_identifier())
        {
            node = new ParserASTIdentifierNode(current());

            consume_whitespace();

            if(peek().token.is_identifier())
            {
                node = new ParserASTUnaryOpNode(node, new ParserASTIdentifierNode(advance()));
            }
            else if(peek().token.is_number())
            {
                node = new ParserASTUnaryOpNode(node, new ParserASTNumberNode(advance()));
            }
            else if(peek().token.is_binary_op())
            {
                //node = new ParserASTUnaryOpNode(node, new ParserASTBinaryOpNode(advance()));
            }
            else
            {
                advance();
            }
        }

        consume_whitespace();

        return node;
    }

    ParserASTNode* parse_string_literial(ParserInfo& info)
    {
        ParserASTNode* node = nullptr;

        if(current().token.is_string_literial())
        {
            node = new ParserASTStringLiterial(current());

            advance();
        }

        consume_whitespace();

        return node;
    }

    ParserASTNode* parse_compound_statment(ParserInfo& info)
    {
        ParserASTCompoundStatementNode* compound_node = new ParserASTCompoundStatementNode;
        u32 current_line_num = current().line_number, current_column_num = current().column_number; 

        if(current().token == Token::TOKEN_L_CURLY_BRACKET)
        {
            ParserASTNode* node = nullptr;
            bool closing_found = false;

            advance();
            consume_whitespace();
    
            for(u32 i = current_line_num; !current().is_eof(); i++)
            {
                while(current().line_number == i)
                {
                    PSOUT() << current().token.to_string() << std::endl;

                    consume_whitespace();
                    
                    node = parse_syntax(info);

                    consume_whitespace();
    
                    if(current().token == Token::TOKEN_R_CURLY_BRACKET)
                    {
                        advance();
                        closing_found = true;
                        
                        break;
                    }
                }

                compound_node->get_nodes().push_back(node);

                if(closing_found) break;
            }

            if(closing_found == false)
            {
                Error error = { .level = ERROR_LEVEL_FAIL, .error_message = S("unterminated compound statement"), .source_file = info.filename, .line_number = current_line_num, .column_number = current_column_num };

                lang_print_error(error);
            }
        }

        return compound_node;
    }

    ParserASTNode* parse_expression(ParserInfo& info)
    {
        std::list<std::pair<u32, str_t>> supported_operators = 
        { 
            { Token::TOKEN_DIVIDE, S("divided_by") },
            { Token::TOKEN_MULTIPLY, S("multiplied_by") },
            { Token::TOKEN_SUBTRACT, S("minus") },
            { Token::TOKEN_ADD, S("plus") },
            { Token::TOKEN_EQUALS, S("equals") },
        };

        ParserASTNode* lhs = nullptr;
        u32 current_line_num = current().line_number;

        if(current().token.is_number())
        {
            lhs = parse_number(info);
        }
        else if(current().token.is_identifier())
        {
            lhs = parse_identifier(info);
        }
        else if(current().token.is_string_literial())
        {
            lhs = parse_string_literial(info);
        }
        else if(current().token == Token::TOKEN_L_CURLY_BRACKET)
        {
            lhs = parse_compound_statment(info);
        }

        while(!current().is_eof())
        {
            bool is_supported = false;
            TokenInfo op;

            for(auto& so : supported_operators)
            {
                if(current().token == so.first)
                {
                    is_supported = true;
                    op = current();
                }
                else if(current().token.is_identifier() && current().token.get_data() == so.second)
                {
                    is_supported = true;
                    op = current();
                    op.token = so.first;
                }
            }

            if(is_supported)
            {
                advance();
                consume_whitespace();

                ParserASTNode* rhs = parse_expression(info);

                lhs = new ParserASTBinaryOpNode(lhs, op, rhs);
            }

            if(current().line_number != current_line_num) break;

            advance();
        }

        return lhs;
    }

    ParserASTNode* parse_syntax(ParserInfo& info)
    {
        ParserASTNode* node = nullptr;

        if(current().token.is_number() || current().token.is_identifier())
        {
            node = parse_expression(info);
        }
        else if(current().token == Token::TOKEN_L_CURLY_BRACKET)
        {
            node = parse_compound_statment(info);
        }
        else
        {
            Error error;
            error.source_file = info.filename;
            error.line_number = current().line_number;
            error.column_number = current().column_number;
            error.level = ERROR_LEVEL_FAIL;
            error.error_message = S("invalid syntax");

            lang_print_error(error);
        }

        return node;
    }

    ParserInfo parse(const str_t& filename)
    {
        ParserInfo parser_info;
        parser_info.filename = filename;

        for(u32 i = 1; !current().is_eof(); i++)
        {
            ParserASTNode* node = nullptr;
            while(current().line_number == i)
            {
                consume_whitespace();

                node = parse_syntax(parser_info);
            }

            if(node != nullptr) parser_info.nodes.push_back(node);
        }

        return parser_info;
    }
};

ParserInfo lang_parse(const LexerInfo& info, const str_t& filename)
{
    Parser parser(info);

    return parser.parse(filename);
}

void LexerInfo::dump()
{
    for(auto& tok : token_info)
    {
        PSOUT() << tok.line_number << ":" << tok.column_number << " " << tok.token.to_string();

        if(!tok.token.get_data().empty())
        {
            PSOUT() << " " << tok.token.get_data();

            #ifdef USE_WCHAR
            PSOUT() << " | unicode bytes: ";

            for(char_t c : tok.token.get_data())
            {
                PSOUT() << std::hex << (u32)c << " " << std::dec;
            }
            #endif
        }

        PSOUT() << "\n";
    }

    PSOUT() << "\n";

    PSOUT().flush();
}

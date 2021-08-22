#pragma once

#include "token.h"
#include <vector>
#include <list>

struct TokenInfo
{
    Token token;
    u32 line_number = 0;
    u32 column_number = 0;

    bool is_eof() const
    {
        return token == Token::TOKEN_EOF;
    }
};

struct LexerInfo
{
    std::vector<TokenInfo> token_info;

    void dump();
};

enum ASTNodeType
{
    AST_NODE_NONE,
    AST_NODE_BINARY_OPERATOR,
    AST_NODE_UNARY_OPERATOR,
    AST_NODE_NUMBER,
    AST_NODE_IDENTIFIER,
    AST_NODE_STRING_LITERIAL,
    AST_NODE_COMPOUND_STATEMENT
};

class ParserASTNode
{
private:
    ASTNodeType _type;

public:
    ParserASTNode(ASTNodeType type = AST_NODE_NONE)
    :
    _type(type)
    {
    }

    ASTNodeType get_node_type() const { return _type; }

    virtual str_t to_string() const
    {
        return str_t();
    }

    virtual ~ParserASTNode()
    {
    }
};

class ParserASTNumberNode : public ParserASTNode
{
private:
    TokenInfo _token_info;

public:
    ParserASTNumberNode(const TokenInfo& token)
    :
    ParserASTNode(AST_NODE_NUMBER),
    _token_info(token)
    {
    }

    const TokenInfo& get_token_info() const { return _token_info; }

    str_t to_string() const
    {
        return _token_info.token.to_string() + S(":") + _token_info.token.get_data();
    }
};

class ParserASTIdentifierNode : public ParserASTNode
{
private:
    TokenInfo _token_info;

public:
    ParserASTIdentifierNode(const TokenInfo& token)
    :
    ParserASTNode(AST_NODE_IDENTIFIER),
    _token_info(token)
    {
    }

    const TokenInfo& get_token_info() const { return _token_info; }

    str_t to_string() const
    {
        return _token_info.token.to_string() + S(":") + _token_info.token.get_data();
    }
};

class ParserASTStringLiterial : public ParserASTNode
{
private:
    TokenInfo _token_info;

public:
    ParserASTStringLiterial(const TokenInfo& token)
    :
    ParserASTNode(AST_NODE_IDENTIFIER),
    _token_info(token)
    {
    }

    str_t to_string() const
    {
        return _token_info.token.to_string() + S(":\"") + _token_info.token.get_data() + S("\"");
    }
};

class ParserASTBinaryOpNode : public ParserASTNode
{
private:
    ParserASTNode* _lhs;
    TokenInfo _op;
    ParserASTNode* _rhs;

public:
    ParserASTBinaryOpNode(ParserASTNode* lhs, TokenInfo op, ParserASTNode* rhs)
    :
    ParserASTNode(AST_NODE_BINARY_OPERATOR),
    _lhs(lhs),
    _op(op),
    _rhs(rhs)
    {
    }

    ParserASTNode* get_lhs() const { return _lhs; }
    const TokenInfo& get_operator() const { return _op; }
    ParserASTNode* get_rhs() const { return _rhs; }

    str_t to_string() const
    {
        return str_t(S("(")) + (_lhs == nullptr ? S("<null>") : _lhs->to_string()) + S(" ") + _op.token.to_string() + S(" ") + (_rhs == nullptr ? S("<null>") : _rhs->to_string()) + S(")");
    }
};

class ParserASTUnaryOpNode : public ParserASTNode
{
private:
    ParserASTNode* _lhs;
    ParserASTNode* _rhs;

public:
    ParserASTUnaryOpNode(ParserASTNode* lhs, ParserASTNode* rhs)
    :
    ParserASTNode(AST_NODE_UNARY_OPERATOR),
    _lhs(lhs),
    _rhs(rhs)
    {
    }

    ParserASTNode* get_lhs() const { return _lhs; }
    ParserASTNode* get_rhs() const { return _rhs; }

    str_t to_string() const
    {
        return (_lhs == nullptr ? S("<null>") : _lhs->to_string()) + S(" ") + (_rhs == nullptr ? S("<null>") : _rhs->to_string());
    }
};

class ParserASTCompoundStatementNode : public ParserASTNode
{
private:
    std::list<ParserASTNode*> _nodes;

public:
    ParserASTCompoundStatementNode()
    :
    ParserASTNode(AST_NODE_COMPOUND_STATEMENT)
    {
    }

    std::list<ParserASTNode*>& get_nodes()
    {
        return _nodes;
    }

    str_t to_string() const
    {
        str_t res;

        res.append(S("COMPOUND_STATEMENT(\n"));
        for(auto& node : _nodes)
        {
            if(node == nullptr) continue;

            if(node->get_node_type() == AST_NODE_COMPOUND_STATEMENT)
            {
                res.append(S("    ") + node->to_string() + S("\n"));
            }
            else
            {
                res.append(S("  ") + node->to_string() + S("\n"));
            }
        }
        res.append(S(")"));

        return res;
    }
};

struct ParserInfo
{
    str_t filename;
    std::list<ParserASTNode*> nodes;
};

LexerInfo lang_lex(const str_t& str);

ParserInfo lang_parse(const LexerInfo& info, const str_t& filename);

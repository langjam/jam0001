#include "runtime.h"
#include "error.h"

#include <utility>
#include <sstream>

enum VariableType
{
    VARIABLE_UNKNOWN,
    VARIABLE_INTERGER,
    VARIABLE_STRING,
    VARIABLE_FUNCTION
};

struct VariableData
{
    VariableType type;
    str_t data;
    ParserASTNode* node = nullptr;
};

struct Variable
{
    str_t name;
    VariableData data;
};

struct RuntimeState
{
    std::vector<Variable> vars = {};
    str_t filename;

    void dump_vars()
    {
        PSOUT() << "Variables: \n";

        for(auto& var : vars)
        {
            PSOUT() << var.name << " = " << var.data.data << "\n";
        }

        PSOUT().flush();
    }

    bool find_var(str_t name, Variable& pair)
    {
        bool found = false;

        for(auto& var : vars)
        {
            if(var.name == name)
            {
                pair = var;
                found = true;
            }
        }

        return found;
    }
};

static void lang_runtime_print_error(RuntimeState& state, u32 line, u32 column, ErrorLevel level, const str_t& s)
{
    Error error = { .level = level, .error_message = s, .source_file = state.filename, .line_number = line, .column_number = column };

    lang_print_error(error);
}

static u64 lang_runtime_convert_to_int(ParserASTNumberNode* node)
{
    SSTREAM ss;
    u64 i = 0;

    ss << node->get_token_info().token.get_data();
    ss >> i;

    return i;
}

static u64 lang_runtime_convert_to_int(str_t num)
{
    SSTREAM ss;
    u64 i = 0;

    ss << num;
    ss >> i;

    return i;
}

static str_t lang_runtime_int_to_string(u64 i)
{
    SSTREAM ss;
    str_t s;

    ss << i;
    ss >> s;

    return s;
}

static ParserASTNode* lang_runtime_run(RuntimeState& state, ParserASTNode* root_node);

static VariableData lang_runtime_evaluate(RuntimeState& state, VariableData lhs, Token op, VariableData rhs)
{
    VariableData res;

    if(lhs.type == VARIABLE_INTERGER && rhs.type == VARIABLE_INTERGER)
    {
        u64 num1 = lang_runtime_convert_to_int(lhs.data), num2 = lang_runtime_convert_to_int(rhs.data);

        res.type = VARIABLE_INTERGER;

        if(op == Token::TOKEN_ADD)
        {
            res.data = lang_runtime_int_to_string(num1 + num2);
        }
        else if(op == Token::TOKEN_SUBTRACT)
        {
            res.data = lang_runtime_int_to_string(num1 - num2);
        }
        else if(op == Token::TOKEN_MULTIPLY)
        {
            res.data = lang_runtime_int_to_string(num1 * num2);
        }
        else if(op == Token::TOKEN_DIVIDE)
        {
            res.data = lang_runtime_int_to_string(num1 / num2);
        }
    }

    return res;
}

static Variable lang_runtime_compute_identifier(RuntimeState& state, ParserASTNode* node)
{
    Variable result;

    if(node->get_node_type() == AST_NODE_IDENTIFIER)
    {
        auto ident_node = (ParserASTIdentifierNode*)node;
        auto var_name = ident_node->get_token_info().token.get_data();
        Variable data;

        if(state.find_var(var_name, data))
        {
            result = data;
        }
        else
        {
            lang_runtime_print_error(state, ident_node->get_token_info().line_number, ident_node->get_token_info().column_number, ERROR_LEVEL_FAIL, S("undefined variable '") + var_name + S("'"));

            result.data.data = S("unknown");
            result.data.type = VARIABLE_UNKNOWN;
        }
    }

    return result;
}

static VariableData lang_runtime_execute_function(RuntimeState& state, ParserASTCompoundStatementNode* compound_node);

static VariableData lang_runtime_compute_op(RuntimeState& state, ParserASTBinaryOpNode* bnode)
{
    Token op = bnode->get_operator().token;
    VariableData res{};

    if(bnode->get_rhs()->get_node_type() == AST_NODE_NUMBER || bnode->get_rhs()->get_node_type() == AST_NODE_IDENTIFIER || bnode->get_rhs()->get_node_type() == AST_NODE_BINARY_OPERATOR)
    {
        VariableData lhs_data, rhs_data;

        if(bnode->get_lhs()->get_node_type() == AST_NODE_NUMBER)
        {
            auto num_node = (ParserASTNumberNode*)bnode->get_lhs();

            lhs_data.data = num_node->get_token_info().token.get_data();
            lhs_data.type = VARIABLE_INTERGER;
        }
        else if(bnode->get_lhs()->get_node_type() == AST_NODE_BINARY_OPERATOR)
        {
            auto op_node = (ParserASTBinaryOpNode*)bnode->get_lhs();

            lhs_data = lang_runtime_compute_op(state, op_node);
        }
        else if(bnode->get_lhs()->get_node_type() == AST_NODE_IDENTIFIER)
        {
            auto id_node = (ParserASTIdentifierNode*)bnode->get_lhs();

            auto data = lang_runtime_compute_identifier(state, id_node);
            if(data.data.type == VARIABLE_FUNCTION)
            {
                lhs_data = lang_runtime_execute_function(state, (ParserASTCompoundStatementNode*)data.data.node);
            }
            else
            {
                lhs_data = data.data;
            }
        }

        if(bnode->get_rhs()->get_node_type() == AST_NODE_NUMBER)
        {
            auto num_node = (ParserASTNumberNode*)bnode->get_rhs();

            rhs_data.data = num_node->get_token_info().token.get_data();
            rhs_data.type = VARIABLE_INTERGER;
        }
        else if(bnode->get_rhs()->get_node_type() == AST_NODE_BINARY_OPERATOR)
        {
            auto op_node = (ParserASTBinaryOpNode*)bnode->get_rhs();

            rhs_data = lang_runtime_compute_op(state, op_node);
        }
        else if(bnode->get_rhs()->get_node_type() == AST_NODE_IDENTIFIER)
        {
            auto id_node = (ParserASTIdentifierNode*)bnode->get_rhs();

            rhs_data = lang_runtime_compute_identifier(state, id_node).data;
        }

        res = lang_runtime_evaluate(state, lhs_data, op, rhs_data);
    }

    return res;
}

static VariableData lang_runtime_execute_function(RuntimeState& state, ParserASTCompoundStatementNode* compound_node)
{
    VariableData return_value;

    for(std::list<ParserASTNode*>::iterator node = compound_node->get_nodes().begin(); node != compound_node->get_nodes().end(); node++)
    {
        if(*node == nullptr) continue;

        if((*node)->get_node_type() == AST_NODE_UNARY_OPERATOR)
        {
            auto op_node = (ParserASTUnaryOpNode*)*node;

            if(op_node->get_lhs()->get_node_type() == AST_NODE_IDENTIFIER)
            {
                auto id_node = (ParserASTIdentifierNode*)op_node->get_lhs();

                if(id_node->get_token_info().token.get_data() == S("ret"))
                {
                    if(op_node->get_rhs()->get_node_type() == AST_NODE_NUMBER)
                    {
                        auto num_node = (ParserASTNumberNode*)op_node->get_rhs();

                        return_value = { VARIABLE_INTERGER, num_node->get_token_info().token.get_data() };
                    }
                    else if(op_node->get_rhs()->get_node_type() == AST_NODE_IDENTIFIER)
                    {
                        return_value = lang_runtime_compute_identifier(state, id_node).data;
                    }
                    else if(op_node->get_rhs()->get_node_type() == AST_NODE_BINARY_OPERATOR)
                    {
                        auto bin_node = (ParserASTBinaryOpNode*)op_node->get_rhs();

                        return_value = lang_runtime_compute_op(state, bin_node);
                    }
                }

                break;
            }
        }

        lang_runtime_run(state, *node);
    }

    return return_value;
}

static ParserASTNode* lang_runtime_run(RuntimeState& state, ParserASTNode* root_node)
{
    ParserASTNode* node = root_node;

    if(root_node->get_node_type() == AST_NODE_BINARY_OPERATOR)
    {
        auto bnode = (ParserASTBinaryOpNode*)root_node;
        Token op = bnode->get_operator().token;

        if(bnode->get_lhs()->get_node_type() == AST_NODE_IDENTIFIER && op == Token::TOKEN_EQUALS)
        {
            auto lhs = (ParserASTIdentifierNode*)bnode->get_lhs();
            VariableData var_data;

            if(bnode->get_rhs()->get_node_type() == AST_NODE_BINARY_OPERATOR)
            {
                var_data = lang_runtime_compute_op(state, (ParserASTBinaryOpNode*)bnode->get_rhs());
            }
            else
            {
                auto rhs = lang_runtime_run(state, bnode->get_rhs());

                if(rhs->get_node_type() == AST_NODE_NUMBER)
                {
                    auto num_node = (ParserASTNumberNode*)rhs;

                    var_data = { VARIABLE_INTERGER, num_node->get_token_info().token.get_data() };
                }
                else if(rhs->get_node_type() == AST_NODE_IDENTIFIER)
                {
                    auto id_node = (ParserASTIdentifierNode*)rhs;

                    var_data = lang_runtime_compute_identifier(state, id_node).data;
                }
                else if(rhs->get_node_type() == AST_NODE_COMPOUND_STATEMENT)
                {
                    auto compound_node = (ParserASTCompoundStatementNode*)rhs;

                    var_data.data = S("<function>");
                    var_data.type = VARIABLE_FUNCTION;
                    var_data.node = compound_node;
                }
            }

            state.vars.push_back(Variable{lhs->get_token_info().token.get_data(), var_data});

            node = (ParserASTNode*)lhs;
        }
    }
    else if(root_node->get_node_type() == AST_NODE_NUMBER)
    {
        node = root_node;
    }
    else if(root_node->get_node_type() == AST_NODE_IDENTIFIER)
    {
        node = root_node;
    }

    return node;
}

void lang_start_runtime(ParserInfo& parser_info)
{
    if(parser_info.nodes.empty()) return;

    RuntimeState state;
    state.filename = parser_info.filename;

    //state.vars.push_back(std::pair<str_t, str_t>(lhs->get_token_info().token.get_data(), str_t()));
    for(auto& node : parser_info.nodes)
    {
        lang_runtime_run(state, node);
    }

    Variable f;
    if(!state.find_var(S("func"), f))
    {
        PSOUT() << "Could not find!" << std::endl;
    }

    state.dump_vars();
}

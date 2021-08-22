#include "interpreter.h"
#include "varmap.h"

#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

static inline void runtime_error(const char* const error_message) {
    printf("RuntimeError: %s.\n", error_message);
    exit(1);
}

static struct Value *expr_eval(struct Interpreter* const interp, struct Expr* const expr) {
    struct Value *lhs, *rhs, value;
    switch (expr->type) {
    case ExprTypeValue:
        return expr->as.value;
    case ExprTypeKey:
        return map_get(&interp->block.vars, expr->as.key);
    case ExprTypeAdd:
        lhs = expr_eval(interp, expr->as.binary.lhs);
        rhs = expr_eval(interp, expr->as.binary.rhs);
        if (lhs->type == rhs->type == ValueTypeNumber) {
            value.type = ValueTypeNumber;
            value.as.number = lhs->as.number + rhs->as.number;
        } else if (lhs->type == rhs->type == ValueTypeString) {
            size_t len_lhs, len_rhs;
            // TODO: optimise this!!!
            value.type = ValueTypeString;
            value.as.string = malloc(((len_lhs=strlen(lhs->as.string))+(len_rhs=strlen(rhs->as.string))+1)*sizeof(char));
            if (!value.as.string)
                runtime_error("Allocation failure when adding up two strings");
            strcpy(value.as.string, lhs->as.string);
            strcpy(value.as.string+len_lhs, rhs->as.string);
            value.as.string[len_lhs+len_rhs] = '\0';
        } else if (lhs->type != rhs->type) {
            runtime_error("Mismatch types");
        } else {
            runtime_error("Invalid operation (+) on types");
        }
        free(rhs);
        *lhs = value;
        return lhs;
    case ExprTypeSub:
        lhs = expr_eval(interp, expr->as.binary.lhs);
        rhs = expr_eval(interp, expr->as.binary.rhs);
        if (lhs->type == rhs->type == ValueTypeNumber) {
            value.type = ValueTypeNumber;
            value.as.number = lhs->as.number + rhs->as.number;
        } else if (lhs->type != rhs->type) {
            runtime_error("Mismatch types");
        } else {
            runtime_error("Invalid operation (-) on types");
        }
        free(rhs);
        *lhs = value;
        return lhs;
    case ExprTypeMul:
        // TODO: add number * string
        lhs = expr_eval(interp, expr->as.binary.lhs);
        rhs = expr_eval(interp, expr->as.binary.rhs);
        if (lhs->type == rhs->type == ValueTypeNumber) {
            value.type = ValueTypeNumber;
            value.as.number = lhs->as.number * rhs->as.number;
        } else if (lhs->type != rhs->type) {
            runtime_error("Mismatch types");
        } else {
            runtime_error("Invalid operation (*) on types");
        }
        free(rhs);
        *lhs = value;
        return lhs;
    case ExprTypeDiv:
        lhs = expr_eval(interp, expr->as.binary.lhs);
        rhs = expr_eval(interp, expr->as.binary.rhs);
        if (lhs->type == rhs->type == ValueTypeNumber) {
            value.type = ValueTypeNumber;
            if (rhs->as.number == 0) {
                runtime_error("Haha! no. you are not dividing by zero. Have a great day!");
            }
            value.as.number = lhs->as.number / rhs->as.number;
        } else if (lhs->type != rhs->type) {
            runtime_error("Mismatch types");
        } else {
            runtime_error("Invalid operation (/) on types");
        }
        free(rhs);
        *lhs = value;
        return lhs;
    case ExprTypeEquals:
        lhs = expr_eval(interp, expr->as.binary.lhs);
        rhs = expr_eval(interp, expr->as.binary.rhs);
        if (lhs->type == rhs->type == ValueTypeNumber) {
            value.type = ValueTypeNumber;
            value.as.number = lhs->as.number == rhs->as.number;
        } else if (lhs->type != rhs->type) {
            runtime_error("Mismatch types");
        } else {
            runtime_error("Invalid operation (=) on types");
        }
        free(rhs);
        *lhs = value;
        return lhs;
    case ExprTypeSmallerThen:
        lhs = expr_eval(interp, expr->as.binary.lhs);
        rhs = expr_eval(interp, expr->as.binary.rhs);
        if (lhs->type == rhs->type == ValueTypeNumber) {
            value.type = ValueTypeNumber;
            value.as.number = lhs->as.number < rhs->as.number;
        } else if (lhs->type != rhs->type) {
            runtime_error("Mismatch types");
        } else {
            runtime_error("Invalid operation (<) on types");
        }
        free(rhs);
        *lhs = value;
        return lhs;
    case ExprTypeBiggerThen:
        lhs = expr_eval(interp, expr->as.binary.lhs);
        rhs = expr_eval(interp, expr->as.binary.rhs);
        if (lhs->type == rhs->type == ValueTypeNumber) {
            value.type = ValueTypeNumber;
            value.as.number = lhs->as.number > rhs->as.number;
        } else if (lhs->type != rhs->type) {
            runtime_error("Mismatch types");
        } else {
            runtime_error("Invalid operation (>) on types");
        }
        free(rhs);
        *lhs = value;
        return lhs;
    default:
        assert(0);
    }
}

static void value_log(struct Value *value) {
    switch (value->type) {
    case ValueTypeNumber:
        printf("%d", value->as.number);
        break;
    case ValueTypeString:
        printf("%s", value->as.string);
        break;
    case ValueTypeVoid:
        printf("(void)");
        break;
    case ValueTypeRoutine:
        printf("routine(");
        if (value->as.routine.amount_parameters > 0) {
            printf(":%s", value->as.routine.parameters[0]);
            for (size_t i = 1; i < value->as.routine.amount_parameters; ++i)
                printf(", :%s", value->as.routine.parameters[i]);
        }
        printf(")");
        break;
    default:
        assert(0);
    }
    printf("\n");
}

void interpret(struct Node **instructions) {
    struct Interpreter interp = {
        .instructions = instructions,
        .block = {
            .depth = 0,
            .vars = NULL
        }
    };
    struct Node *node;
    map_construct(&interp.block.vars);
    for (interp.idx = 0; (node = interp.instructions[interp.idx]); ++interp.idx) {
        switch (node->type) {
        case NodeTypeLog: {
            struct Value *value = expr_eval(&interp, node->value.log_value);
            value_log(value);
            break;
        }
        case NodeTypeSet: {
            map_set(&interp.block.vars, node->value.set.key, expr_eval(&interp, node->value.set.expr));
            break;
        }
        default:
            // FIXME: again, wtf??
            break;
        }
    }
}

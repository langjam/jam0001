#include "interpreter.h"
#include "varmap.h"

#include <string.h>
#include <assert.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

static inline void runtime_error(const char* const error_message) {
    printf("RuntimeError: %s.\n", error_message);
    exit(1);
}

static struct NumberExpr number_add(struct NumberExpr a, struct NumberExpr b) {
    struct NumberExpr res;
    if (a.is_float && b.is_float) {
        res.is_float = true;
        res.as._float = a.as._float + b.as._float;
    } else if (a.is_float && !b.is_float) {
        res.is_float = true;
        res.as._float = a.as._float + (double)b.as._int;
    } else if (!a.is_float && b.is_float) {
        res.is_float = true;
        res.as._float = (double)a.as._int + b.as._float;
    } else {
        res.is_float = false;
        res.as._int = a.as._int + b.as._int;
    }
    return res;
}

static struct NumberExpr number_sub(struct NumberExpr a, struct NumberExpr b) {
    struct NumberExpr res;
    if (a.is_float && b.is_float) {
        res.is_float = true;
        res.as._float = a.as._float - b.as._float;
    } else if (a.is_float && !b.is_float) {
        res.is_float = true;
        res.as._float = a.as._float - (double)b.as._int;
    } else if (!a.is_float && b.is_float) {
        res.is_float = true;
        res.as._float = (double)a.as._int - b.as._float;
    } else {
        res.is_float = false;
        res.as._int = a.as._int - b.as._int;
    }
    return res;
}

static struct NumberExpr number_mul(struct NumberExpr a, struct NumberExpr b) {
    struct NumberExpr res;
    if (a.is_float && b.is_float) {
        res.is_float = true;
        res.as._float = a.as._float * b.as._float;
    } else if (a.is_float && !b.is_float) {
        res.is_float = true;
        res.as._float = a.as._float * (double)b.as._int;
    } else if (!a.is_float && b.is_float) {
        res.is_float = true;
        res.as._float = (double)a.as._int * b.as._float;
    } else {
        res.is_float = false;
        res.as._int = a.as._int * b.as._int;
    }
    return res;
}

static struct NumberExpr number_div(struct NumberExpr a, struct NumberExpr b) {
    struct NumberExpr res;
    if (a.is_float && b.is_float) {
        res.is_float = true;
        if (b.as._float == 0.f)
            runtime_error("Division by zero");
        res.as._float = a.as._float / b.as._float;
    } else if (a.is_float && !b.is_float) {
        res.is_float = true;
        if (b.as._int == 0)
            runtime_error("Division by zero");
        res.as._float = a.as._float / (double)b.as._int;
    } else if (!a.is_float && b.is_float) {
        res.is_float = true;
        if (b.as._float == 0.f)
            runtime_error("Division by zero");
        res.as._float = (double)a.as._int / b.as._float;
    } else {
        div_t division;
        if (b.as._int == 0)
            runtime_error("Division by zero");
        division = div(a.as._int, b.as._int);
        if (division.rem == 0) {
            res.is_float = false;
            res.as._int = division.quot;
        } else {
            res.is_float = true;
            res.as._float = a.as._int / b.as._int;
        }
    }
    return res;
}

static struct NumberExpr number_eq(struct NumberExpr a, struct NumberExpr b) {
    struct NumberExpr res;
    res.is_float = false;
    if (a.is_float && b.is_float) {
        res.as._float = a.as._float == b.as._float;
    } else if (a.is_float && !b.is_float) {
        res.as._float = a.as._float == (double)b.as._int;
    } else if (!a.is_float && b.is_float) {
        res.as._float = (double)a.as._int == b.as._float;
    } else {
        res.as._int = a.as._int == b.as._int;
    }
    return res;
}

static struct NumberExpr number_smaller(struct NumberExpr a, struct NumberExpr b) {
    struct NumberExpr res;
    res.is_float = false;
    if (a.is_float && b.is_float) {
        res.as._float = a.as._float < b.as._float;
    } else if (a.is_float && !b.is_float) {
        res.as._float = a.as._float < (double)b.as._int;
    } else if (!a.is_float && b.is_float) {
        res.as._float = (double)a.as._int < b.as._float;
    } else {
        res.as._int = a.as._int < b.as._int;
    }
    return res;
}

static struct NumberExpr number_bigger(struct NumberExpr a, struct NumberExpr b) {
    struct NumberExpr res;
    res.is_float = false;
    if (a.is_float && b.is_float) {
        res.as._float = a.as._float > b.as._float;
    } else if (a.is_float && !b.is_float) {
        res.as._float = a.as._float > (double)b.as._int;
    } else if (!a.is_float && b.is_float) {
        res.as._float = (double)a.as._int > b.as._float;
    } else {
        res.as._int = a.as._int > b.as._int;
    }
    return res;
}

static struct Value *expr_eval(struct Interpreter* const interp, struct Expr* const expr) {
    struct Value *lhs, *rhs, *value;
    switch (expr->type) {
    case ExprTypeValue:
        return expr->as.value;
    case ExprTypeKey:
        return map_get(&interp->block.vars, expr->as.key);
    case ExprTypeAdd:
        value = malloc(sizeof(struct Value));
        lhs = expr_eval(interp, expr->as.binary.lhs);
        rhs = expr_eval(interp, expr->as.binary.rhs);
        if (lhs->type == rhs->type == ValueTypeNumber) {
            value->type = ValueTypeNumber;
            value->as.number = number_add(lhs->as.number, rhs->as.number);
        } else {
            runtime_error("Invalid operation (+) on types");
        }
        return value;
    case ExprTypeSub:
        value = malloc(sizeof(struct Value));
        lhs = expr_eval(interp, expr->as.binary.lhs);
        rhs = expr_eval(interp, expr->as.binary.rhs);
        if (lhs->type == rhs->type == ValueTypeNumber) {
            value->type = ValueTypeNumber;
            value->as.number = number_sub(lhs->as.number, rhs->as.number);
        } else {
            runtime_error("Invalid operation (-) on types");
        }
        return value;
    case ExprTypeMul:
        value = malloc(sizeof(struct Value));
        lhs = expr_eval(interp, expr->as.binary.lhs);
        rhs = expr_eval(interp, expr->as.binary.rhs);
        if (lhs->type == rhs->type == ValueTypeNumber) {
            value->type = ValueTypeNumber;
            value->as.number = number_mul(lhs->as.number, rhs->as.number);
        } else {
            runtime_error("Invalid operation (*) on types");
        }
        return value;
    case ExprTypeDiv:
        value = malloc(sizeof(struct Value));
        lhs = expr_eval(interp, expr->as.binary.lhs);
        rhs = expr_eval(interp, expr->as.binary.rhs);
        if (lhs->type == rhs->type == ValueTypeNumber) {
            value->type = ValueTypeNumber;
            if (rhs->as.number.as._float == 0.f) {
                runtime_error("Haha! no. you are not dividing by zero. Have a great day!");
            }
            value->as.number = number_div(lhs->as.number, rhs->as.number);
        } else if (lhs->type != rhs->type) {
            runtime_error("Mismatch types");
        } else {
            runtime_error("Invalid operation (/) on types");
        }
        return value;
    case ExprTypeEquals:
        value = malloc(sizeof(struct Value));
        lhs = expr_eval(interp, expr->as.binary.lhs);
        rhs = expr_eval(interp, expr->as.binary.rhs);
        if (lhs->type == rhs->type == ValueTypeNumber) {
            value->type = ValueTypeNumber;
            value->as.number = number_eq(lhs->as.number, rhs->as.number);
        } else {
            runtime_error("Invalid operation (=) on types");
        }
        return value;
    case ExprTypeSmallerThen:
        value = malloc(sizeof(struct Value));
        lhs = expr_eval(interp, expr->as.binary.lhs);
        rhs = expr_eval(interp, expr->as.binary.rhs);
        if (lhs->type == rhs->type == ValueTypeNumber) {
            value->type = ValueTypeNumber;
            value->as.number = number_smaller(lhs->as.number, rhs->as.number);
        } else {
            runtime_error("Invalid operation (<) on types");
        }
        return value;
    case ExprTypeBiggerThen:
        value = malloc(sizeof(struct Value));
        lhs = expr_eval(interp, expr->as.binary.lhs);
        rhs = expr_eval(interp, expr->as.binary.rhs);
        if (lhs->type == rhs->type == ValueTypeNumber) {
            value->type = ValueTypeNumber;
            value->as.number = number_bigger(lhs->as.number, rhs->as.number);
        } else {
            runtime_error("Invalid operation (>) on types");
        }
        return value;
    default:
        assert(0);
    }
}

static void value_log(struct Value *value) {
    switch (value->type) {
    case ValueTypeNumber:
        if (value->as.number.is_float) {
            printf("%f", value->as.number.as._float);
        } else {
            printf("%d", value->as.number.as._int);
        }
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

/* is the value booleanly true? like Python's bool */
static bool value_as_bool(struct Value* const value) {
    switch (value->type) {
    case ValueTypeVoid:
        return false;
    case ValueTypeString:
        return value->as.string[0] != '\0';
    case ValueTypeNumber:
        if (value->as.number.is_float)
            return value->as.number.as._float != 0.0f;
        else
            return value->as.number.as._int != 0;
    case ValueTypeRoutine:
        return true;
    default:
        assert(0);
    }
}

static void interpreter_interpret_node(struct Interpreter* const interp, struct Node* const node) {
    switch (node->type) {
    case NodeTypeLog: {
        struct Value *value = expr_eval(interp, node->value.log_value);
        value_log(value);
        break;
    }
    case NodeTypeSet: {
        map_set(&interp->block.vars, node->value.set.key, expr_eval(interp, node->value.set.expr));
        break;
    }
    case NodeTypeIf: {
        struct Value *val;
        if (value_as_bool((val = expr_eval(interp, node->value.if_stat.condition)))) {
            for (size_t i = 0; node->value.if_stat.block[i]; ++i) {
                interpreter_interpret_node(interp, node->value.if_stat.block[i]);
            }
        } else {
            if (node->value.if_stat.else_block) {
                for (size_t i = 0; node->value.if_stat.else_block[i]; ++i) {
                    interpreter_interpret_node(interp, node->value.if_stat.else_block[i]);
                }
            }
        }
        free(val);
        break;
    }
    case NodeTypeLoop:
        while (value_as_bool(expr_eval(interp, node->value.if_stat.condition))) {
            for (size_t i = 0; node->value.if_stat.block[i]; ++i) {
                interpreter_interpret_node(interp, node->value.if_stat.block[i]);
            }
        }
        break;
    default:
        // FIXME: again, wtf??
        break;
    }
}

void interpret(struct Node **instructions) {
    struct Interpreter interp = {
        .instructions = instructions,
        .block = {
            .depth = 0,
            .vars = NULL
        }
    };
    struct Node *node = NULL;
    map_construct(&interp.block.vars);
    for (interp.idx = 0; (node = interp.instructions[interp.idx]); ++interp.idx) {
        interpreter_interpret_node(&interp, node);
    }
}

from comparator_constants import EQUAL, GREATER_THAN, GT_OR_EQ, LESS_THAN, LT_OR_EQ


def apply_binop(op, left_operand, right_operand):
    if op == "+":
        op_result = left_operand.evaluate() + right_operand.evaluate()
    elif op == "-":
        op_result = left_operand.evaluate() - right_operand.evaluate()
    elif op == "*":
        op_result = left_operand.evaluate() * right_operand.evaluate()
    elif op == "/":
        # Equivalent to python's integer division!
        op_result = left_operand.evaluate() // right_operand.evaluate()
    else:
        # potential_operator_value must be '%'
        op_result = left_operand.evaluate() % right_operand.evaluate()
    return op_result


def apply_comparison(op, left_operand, right_operand):
    if op == EQUAL:
        op_result = left_operand.evaluate() == right_operand.evaluate()
    if op == LESS_THAN:
        op_result = left_operand.evaluate() < right_operand.evaluate()
    if op == GREATER_THAN:
        op_result = left_operand.evaluate() > right_operand.evaluate()
    if op == LT_OR_EQ:
        op_result = left_operand.evaluate() <= right_operand.evaluate()
    if op == GT_OR_EQ:
        op_result = left_operand.evaluate() >= right_operand.evaluate()
    return op_result

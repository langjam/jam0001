from comparator_constants import EQUAL, GREATER_THAN, GT_OR_EQ, LESS_THAN, LT_OR_EQ


def apply_binop(op, left_operand, right_operand):
    if op == "+":
        op_result = int(left_operand) + int(right_operand)
    elif op == "-":
        op_result = int(left_operand) - int(right_operand)
    elif op == "*":
        op_result = int(left_operand) * int(right_operand)
    elif op == "/":
        # Equivalent to python's integer division!
        op_result = int(left_operand) // int(right_operand)
    else:
        # potential_operator_value must be '%'
        op_result = int(left_operand) % int(right_operand)
    return op_result


def apply_comparison(op, left_operand, right_operand):
    if op == EQUAL:
        op_result = left_operand == right_operand
    if op == LESS_THAN:
        op_result = left_operand < right_operand
    if op == GREATER_THAN:
        op_result = left_operand > right_operand
    if op == LT_OR_EQ:
        op_result = left_operand <= right_operand
    if op == GT_OR_EQ:
        op_result = left_operand >= right_operand
    return op_result

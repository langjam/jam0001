package ast.expression;

import ast.comment.ExpressionComment;

public interface ExpressionVisitor<R> {
    R visit(LiteralExpression expression);
    R visit(VariableExpression expression);
    R visit(ExpressionComment expression);
    R visit(CallExpression expression);
    R visit(BinaryExpression expression);
    R visit(ComparisonExpression expression);
    R visit(LogicalExpression expression);
    R visit(UnaryExpression expression);
    R visit(GroupExpression expression);
    R visit(AsyncCallExpression expression);
    R visit(ChangeExpression expression);
    R visit(GetExpression expression);
    R visit(SetExpression expression);
    R visit(SuperExpression expression);
    R visit(ThisExpression expression);
}

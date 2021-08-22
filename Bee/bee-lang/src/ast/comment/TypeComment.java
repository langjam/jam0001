package ast.comment;

import ast.expression.VariableExpression;
import token.Token;

import java.util.List;

public class TypeComment extends Comment {

    private final Token name;
    private final VariableExpression superClass;
    private final List<FunctionComment> method;

    public TypeComment(Token name, VariableExpression superClass, List<FunctionComment> method) {
        this.name = name;
        this.superClass = superClass;
        this.method = method;
    }

    public Token getName() {
        return name;
    }

    public VariableExpression getSuperClass() {
        return superClass;
    }

    public List<FunctionComment> getMethod() {
        return method;
    }

    @Override
    public <R> R accept(CommentVisitor<R> visitor) {
        return visitor.visit(this);
    }
}

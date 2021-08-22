package semantic;

import ast.comment.*;
import ast.expression.*;
import runtime.BeeInterpreter;
import runtime.BeeLanguage;
import token.Token;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

public class BeeResolver implements
        ExpressionVisitor<Void>,
        CommentVisitor<Void> {

    private final BeeLanguage beeLanguage;
    private final BeeInterpreter interpreter;
    private final Stack<Map<String, Boolean>> scopes = new Stack<>();

    private enum FunctionType {
        NONE,
        FUNCTION,
        METHOD,
        INITIALIZER,
    }

    private enum ClassType {
        NONE,
        CLASS,
        SUBCLASS,
    }

    private enum FlowType {
        NONE,
        IF,
        LOOP,
    }

    private ClassType currentClass = ClassType.NONE;
    private FunctionType currentFunction = FunctionType.NONE;
    private FlowType currentFlow = FlowType.NONE;

    public BeeResolver(BeeLanguage beeLanguage, BeeInterpreter interpreter) {
        this.beeLanguage = beeLanguage;
        this.interpreter = interpreter;

        Map<String, Boolean> globalScope = new HashMap<>();
        scopes.push(globalScope);
    }

    public void resolve(CommentsUnit comments) {
        comments.accept(this);
    }

    @Override
    public Void visit(CommentsUnit comments) {
        for(Comment comment : comments.getComments()) {
            resolve(comment);
        }
        return null;
    }

    @Override
    public Void visit(EmptyComment comment) {
        return null;
    }

    @Override
    public Void visit(AlertComment comment) {
        return null;
    }

    @Override
    public Void visit(NoteComment comment) {
        return null;
    }

    @Override
    public Void visit(BlockComment comment) {
        beginScope();
        resolve(comment.getComments());
        endScope();
        return null;
    }

    @Override
    public Void visit(FunctionComment comment) {
        declare(comment.getName());
        define(comment.getName());
        resolveFunction(comment, FunctionType.FUNCTION);
        return null;
    }

    @Override
    public Void visit(IfComment comment) {
        FlowType enclosingType = currentFlow;
        currentFlow = FlowType.IF;
        resolve(comment.getCondition());
        resolve(comment.getBlockComment());
        currentFlow = enclosingType;
        return null;
    }

    @Override
    public Void visit(LetComment comment) {
        declare(comment.getVariableName());
        resolve(comment.getValue());
        define(comment.getVariableName());
        return null;
    }

    @Override
    public Void visit(LoopComment comment) {
        FlowType enclosingType = currentFlow;
        currentFlow = FlowType.LOOP;
        resolve(comment.getCondition());
        resolve(comment.getBlockComment());
        currentFlow = enclosingType;
        return null;
    }

    @Override
    public Void visit(ReturnComment comment) {
        if ((currentFunction == FunctionType.NONE) || (currentFunction == FunctionType.INITIALIZER)) {
            throw beeLanguage.createBeeError("Can't use return outside function", comment.getKeyword());
        }
        if(comment.getValue() != null) resolve(comment.getValue());
        return null;
    }

    @Override
    public Void visit(ControlFlowComment comment) {
        if(currentFlow == FlowType.NONE) {
            throw beeLanguage.createBeeError("ControlFlow comments be inside if or loop comment", comment.getKeyword());
        }
        return null;
    }

    @Override
    public Void visit(AssertComment comment) {
        resolve(comment.getExpression());
        return null;
    }

    @Override
    public Void visit(TypeComment comment) {
        ClassType enclosingClass = currentClass;
        currentClass = ClassType.CLASS;

        declare(comment.getName());

        VariableExpression superClass = comment.getSuperClass();

        if(superClass != null && comment.getName().getLiteral().equals(superClass.getVariable().getLiteral())) {
            throw beeLanguage.createBeeError("Type can't extend itself", comment.getName());
        }

        if(superClass != null) {
            currentClass = ClassType.SUBCLASS;
            resolve(comment.getSuperClass());
        }

        if(superClass != null) {
            beginScope();
            scopes.peek().put("super", true);
        }

        beginScope();
        scopes.peek().put("this", true);

        for(FunctionComment method : comment.getMethod()) {
            FunctionType declaration = FunctionType.METHOD;
            if(method.getName().getLiteral().equals("init")) {
                declaration = FunctionType.INITIALIZER;
            }
            resolveFunction(method, declaration);
        }

        define(comment.getName());
        endScope();
        if(superClass != null) endScope();
        currentClass = enclosingClass;
        return null;
    }

    @Override
    public Void visit(LiteralExpression expression) {
        return null;
    }

    @Override
    public Void visit(VariableExpression expression) {
        resolveLocal(expression, expression.getVariable());
        return null;
    }

    @Override
    public Void visit(ExpressionComment expression) {
        resolve(expression.getExpression());
        return null;
    }

    @Override
    public Void visit(CallExpression expression) {
        resolve(expression.getCallee());
        for(Expression argument : expression.getArguments()) {
            resolve(argument);
        }
        return null;
    }

    @Override
    public Void visit(AsyncCallExpression expression) {
        resolve(expression.getCallee());
        for(Expression argument : expression.getArguments()) {
            resolve(argument);
        }
        return null;
    }

    @Override
    public Void visit(ChangeExpression expression) {
        resolve(expression.getValue());
        resolveLocal(expression, expression.getVariableName());
        return null;
    }

    @Override
    public Void visit(GetExpression expression) {
        resolve(expression.getCalle());
        return null;
    }

    @Override
    public Void visit(SetExpression expression) {
        resolve(expression.getValue());
        resolve(expression.getObject());
        return null;
    }

    @Override
    public Void visit(SuperExpression expression) {
        Token superKeyword = expression.getSuperKeyword();
        if(currentClass == ClassType.NONE) {
            throw beeLanguage.createBeeError("Cannot use 'super' outside of a type.", superKeyword);
        }
        else if(currentClass != ClassType.SUBCLASS) {
            throw beeLanguage.createBeeError("Cannot use 'super' in a type with no supertype.", superKeyword);
        }
        resolveLocal(expression, expression.getSuperKeyword());
        return null;
    }

    @Override
    public Void visit(ThisExpression expression) {
        Token thisKeyword = expression.getThisKeyword();
        if(currentClass == ClassType.NONE) {
            throw beeLanguage.createBeeError("Cannot use 'this' outside of a type", thisKeyword);
        }
        resolveLocal(expression, thisKeyword);
        return null;
    }

    @Override
    public Void visit(BinaryExpression expression) {
        resolve(expression.getLeft());
        resolve(expression.getRight());
        return null;
    }

    @Override
    public Void visit(ComparisonExpression expression) {
        resolve(expression.getLeft());
        resolve(expression.getRight());
        return null;
    }

    @Override
    public Void visit(LogicalExpression expression) {
        resolve(expression.getLeft());
        resolve(expression.getRight());
        return null;
    }

    @Override
    public Void visit(UnaryExpression expression) {
        resolve(expression.getRight());
        return null;
    }

    @Override
    public Void visit(GroupExpression expression) {
        resolve(expression.getExpression());
        return null;
    }

    private void beginScope() {
        scopes.push(new HashMap<>());
    }

    private void endScope() {
        scopes.pop();
    }

    private void resolve(List<Comment> comments) {
        for(Comment comment : comments) comment.accept(this);
    }

    private void resolve(Comment comment) {
        comment.accept(this);
    }

    private void resolve(Expression expression) {
        expression.accept(this);
    }

    private void resolveLocal(Expression expression, Token name) {
        int lastIndex = scopes.size() - 1;
        for (int i = lastIndex; i >= 0; i--) {
            if (scopes.get(i).containsKey(name.getLiteral())) {
                interpreter.resolve(expression, lastIndex - i);
                return;
            }
        }
    }

    private void resolveFunction(FunctionComment function, FunctionType type) {
        FunctionType enclosingFunction = currentFunction;
        currentFunction = type;

        beginScope();
        for(Token parameter : function.getParameters()) {
            declare(parameter);
            define(parameter);
        }
        resolve(function.getBlockComment().getComments());
        endScope();

        currentFunction = enclosingFunction;
    }

    private void declare(Token name) {
        if (scopes.isEmpty()) return;

        Map<String, Boolean> scope = scopes.peek();
        if (scope.containsKey(name.getLiteral())) {
            throw beeLanguage.createBeeError("Variable with this name already declared in this scope.", name);
        }

        if(scopes.size() > 1) {
            if (scopes.get(scopes.size() - 2).containsKey(name.getLiteral())) {
                throw beeLanguage.createBeeError("Variable with this name already declared in this upper scope.", name);
            }
        }

        scope.put(name.getLiteral(), false);
    }

    private void define(Token name) {
        if (scopes.isEmpty()) return;
        scopes.peek().put(name.getLiteral(), true);
    }
}

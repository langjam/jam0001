package runtime;

import ast.comment.*;
import ast.expression.*;
import runtime.exceptions.ControlFlowException;
import runtime.exceptions.ReturnException;
import runtime.modules.BeeModule;
import runtime.modules.Modules;
import token.Token;
import token.TokenType;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.*;

public class BeeInterpreter implements
        CommentVisitor<Void>,
        ExpressionVisitor<Object> {

    private final Environment globalEnvironment = new Environment();
    private Environment environment = globalEnvironment;
    private final Map<Expression, Integer> locals = new HashMap<>();

    private final BeeLanguage beeLanguage;
    private final ExecutorService executorService;
    private static final String ALERT_STRING_FORMAT = "%s alert on %s\nMessage: %s.\n";

    public BeeInterpreter(BeeLanguage beeLanguage) {
        this.beeLanguage = beeLanguage;
        this.executorService = Executors.newFixedThreadPool(beeLanguage.getAvailableCoresNumber());

        this.globalEnvironment.define("BEE_LANG_VERSION", beeLanguage.getBeeLangVersion());
        this.globalEnvironment.define("argc", beeLanguage.getArgc());

        // Inject Modules
        List<BeeModule> modules = Modules.getStdModules();
        for (BeeModule module : modules) module.injectIntoEnvironment(globalEnvironment);
    }

    public void interpret(CommentsUnit commentsUnit) {
        commentsUnit.accept(this);
        executorService.shutdownNow();
    }

    @Override
    public Void visit(CommentsUnit commentsUnit) {
        List<Comment> comments = commentsUnit.getComments();
        for (Comment comment : comments) comment.accept(this);
        return null;
    }

    @Override
    public Void visit(EmptyComment comment) {
        return null;
    }

    @Override
    public Void visit(AlertComment alertComment) {
        if(beeLanguage.shouldShowAlerts()) {
            String alertType = alertComment.getAlertType().name();
            String fileName = alertComment.getAlertToken().getSourceLocation().getFileName();
            String alertMessage = String.format(ALERT_STRING_FORMAT, alertType, fileName, alertComment.getAlertMessage());
            System.out.println(alertMessage);
        }
        return null;
    }

    @Override
    public Void visit(NoteComment comment) {
        if(beeLanguage.shouldShowNotes()) {
            System.out.println(comment.getNote().getLiteral());
        }
        return null;
    }

    @Override
    public Void visit(BlockComment blockComment) {
        execute(blockComment.getComments(), new Environment(environment));
        return null;
    }

    @Override
    public Void visit(FunctionComment comment) {
        BeeFunction function = new BeeFunction(comment, environment);
        environment.define(comment.getName().getLiteral(), function);
        return null;
    }

    @Override
    public Void visit(IfComment comment) {
        if(isTruth(evaluate(comment.getCondition()))) {
            try {
                execute(comment.getBlockComment());
            } catch (ControlFlowException e) {
                return null;
            }
        }
        return null;
    }

    @Override
    public Void visit(LetComment comment) {
        environment.define(comment.getVariableName().getLiteral(), evaluate(comment.getValue()));
        return null;
    }

    @Override
    public Void visit(LoopComment loopComment) {
        Object condition = evaluate(loopComment.getCondition());
        if(condition instanceof Boolean) {
            while (isTruth(evaluate(loopComment.getCondition()))) {
                try {
                    execute(loopComment.getBlockComment());
                }
                catch (ControlFlowException e) {
                    return null;
                }
            }
        } else if(condition instanceof Double) {
            int counter = 0;
            while (counter++ < (double) evaluate(loopComment.getCondition())) {
                try {
                    execute(loopComment.getBlockComment());
                }
                catch (ControlFlowException e) {
                    return null;
                }
            }
        }
        return null;
    }

    @Override
    public Void visit(ReturnComment comment) {
        Object value = null;
        if(comment.getValue() != null) {
            value = comment.getValue().accept(this);
        }
        throw new ReturnException(value);
    }

    @Override
    public Void visit(ControlFlowComment comment) {
        throw new ControlFlowException(comment.getControlFlowType());
    }

    @Override
    public Void visit(AssertComment comment) {
        if(!isTruth(evaluate(comment.getExpression()))) {
            throw beeLanguage.createBeeError("Assert condition fail", comment.getAssertKeyword());
        }
        return null;
    }

    @Override
    public Void visit(TypeComment comment) {
        Object superType = null;
        if(comment.getSuperClass() != null) {
            superType = evaluate(comment.getSuperClass());
            if (!(superType instanceof BeeType)) {
                throw beeLanguage.createBeeError("Super type must be a Type", comment.getName());
            }
        }

        environment.define(comment.getName().getLiteral(), null);

        if(comment.getSuperClass() != null) {
            environment = new Environment(environment);
            environment.define("super", superType);
        }

        Map<String, BeeFunction> methods = new HashMap<>();
        for(FunctionComment method : comment.getMethod()) {
            boolean isInitMethod = method.getName().getLiteral().endsWith("init");
            BeeFunction beeMethod = new BeeFunction(method, environment, isInitMethod);
            methods.put(method.getName().getLiteral(), beeMethod);
        }

        BeeType beeType = new BeeType(comment.getName().getLiteral(), (BeeType) superType, environment, methods);

        if(superType != null) {
            environment = environment.getEnclosing();
        }

        environment.assign(comment.getName(), beeType);

        return null;
    }

    @Override
    public Object visit(LiteralExpression expression) {
        return expression.getValue();
    }

    @Override
    public Object visit(VariableExpression expression) {
        return lookUpVariable(expression.getVariable(), expression);
    }

    @Override
    public Void visit(ExpressionComment comment) {
        comment.getExpression().accept(this);
        return null;
    }

    @Override
    public Object visit(CallExpression call) {
        Object callee = evaluate(call.getCallee());

        List<Object> arguments = new ArrayList<>();
        for(Expression argument : call.getArguments()) {
            arguments.add(argument.accept(this));
        }

        if(!(callee instanceof BeeCallable)) {
            throw beeLanguage.createBeeError("Can only call functions", call.getCallToken());
        }

        BeeCallable function = (BeeCallable) callee;

        if (arguments.size() != function.arity()) {
            throw beeLanguage.createBeeError("Expected " +
                    function.arity() + " arguments but got " +
                    arguments.size() + ".", call.getCallToken());
        }

        BeeInterpreter interpreter = this;
        CallType callType = call.getCallType();
        if (callType == CallType.CALL) return function.call(this, arguments);
        if (callType == CallType.RUN) return executorService.submit(() -> function.call(interpreter, arguments));
        throw beeLanguage.createBeeError("UpSupported call type", call.getCallToken());
    }

    @Override
    @SuppressWarnings(value = "unchecked")
    public Object visit(AsyncCallExpression call) {
        Object callee = call.getCallee().accept(this);
        if (callee instanceof Future) {
            try {
                return ((Future<Object>) callee).get();
            } catch (InterruptedException | ExecutionException e) {
                throw beeLanguage.createBeeError(e.getMessage(), call.getCallToken());
            }
        }
        throw beeLanguage.createBeeError("Wait must be used with Future Type", call.getCallToken());
    }

    @Override
    public Object visit(ChangeExpression expression) {
        Token name = expression.getVariableName();
        Object value = expression.getValue().accept(this);

        Integer distance = locals.get(expression);
        if (distance != null) {
            environment.assignAt(distance, name, value);
        } else {
            globalEnvironment.assign(name, value);
        }
        return value;
    }

    @Override
    public Object visit(GetExpression expression) {
        Object object = evaluate(expression.getCalle());
        if(object instanceof BeeInstance) {
            return ((BeeInstance) object).get(expression.getName());
        }
        throw beeLanguage.createBeeError("Only instances have properties", expression.getName());
    }

    @Override
    public Object visit(SetExpression expression) {
        Object object = evaluate(expression.getObject());

        if (!(object instanceof BeeInstance)) {
            throw beeLanguage.createBeeError("Only instances have fields.", expression.getName());
        }

        Object value = evaluate(expression.getValue());
        ((BeeInstance) object).set(expression.getName(), value);
        return value;
    }

    @Override
    public Object visit(SuperExpression expression) {
        BeeType superType = (BeeType) environment.getAt(2, "super");
        BeeInstance object = (BeeInstance) environment.getAt(1, "this");
        BeeFunction method = superType.findMethod(expression.getMethod().getLiteral());

        if(method == null) {
            throw beeLanguage.createBeeError("Undefined property", expression.getMethod());
        }

        return method.bind(object);
    }

    @Override
    public Object visit(ThisExpression expression) {
        return lookUpVariable(expression.getThisKeyword(), expression);
    }

    @Override
    public Object visit(BinaryExpression expression) {
        Object left = expression.getLeft().accept(this);
        Object right = expression.getRight().accept(this);
        switch (expression.getOperator().getTokenType()) {
            case PLUS: {
                if(left instanceof Double && right instanceof Double) {
                    return (Double) left + (Double) right;
                }
                if(left instanceof String || right instanceof String) {
                    return left.toString() + right.toString();
                }
                if(left instanceof Character && right instanceof Character) {
                    return left + "" + right;
                }
                throw beeLanguage.createBeeError("UnSupported types for plus operator.", expression.getOperator());
            }
            case MINUS: {
                if(left instanceof Double && right instanceof Double) {
                    return (Double) left - (Double) right;
                }
                throw beeLanguage.createBeeError("UnSupported types for minus operator.", expression.getOperator());
            }
            case STAR: {
                if(left instanceof Double && right instanceof Double) {
                    return (Double) left * (Double) right;
                }
                throw beeLanguage.createBeeError("UnSupported types for star operator.", expression.getOperator());
            }
            case SLASH: {
                if(left instanceof Double && right instanceof Double) {
                    Double rightDouble = (Double) right;
                    if(rightDouble == 0.0) {
                        throw beeLanguage.createBeeError("Can't divide number by zero.", expression.getOperator());
                    }
                    return (Double) left / (Double) right;
                }
                throw beeLanguage.createBeeError("UnSupported types for slash operator.", expression.getOperator());
            }
            case EQUAL: return left.equals(right);
            case BANG_EQUAL: return !left.equals(right);
            default:
                throw beeLanguage.createBeeError("UnSupported Binary Operator.", expression.getOperator());
        }
    }

    @Override
    public Object visit(ComparisonExpression expression) {
        Object left = expression.getLeft().accept(this);
        Object right = expression.getRight().accept(this);
        switch (expression.getOperator().getTokenType()) {
            case GREATER: {
                if(left instanceof Double && right instanceof Double) {
                    return (Double) left > (Double) right;
                }
                throw beeLanguage.createBeeError("UnSupported types for Greater than operator.", expression.getOperator());
            }
            case GREATER_EQUAL: {
                if(left instanceof Double && right instanceof Double) {
                    return (Double) left >= (Double) right;
                }
                throw beeLanguage.createBeeError("UnSupported types for greater than and equal operator.", expression.getOperator());
            }
            case LESS: {
                if(left instanceof Double && right instanceof Double) {
                    return (Double) left < (Double) right;
                }
                throw beeLanguage.createBeeError("UnSupported types for Less than operator.", expression.getOperator());
            }
            case LESS_EQUAL: {
                if(left instanceof Double && right instanceof Double) {
                    return (Double) left <= (Double) right;
                }
                throw beeLanguage.createBeeError("UnSupported types for less than or equal operator.", expression.getOperator());
            }
            default:
                throw beeLanguage.createBeeError("UnSupported Comparison Operator.", expression.getOperator());
        }
    }

    @Override
    public Object visit(LogicalExpression expression) {
        Object left = expression.getLeft().accept(this);
        Object right = expression.getRight().accept(this);

        switch (expression.getOperator().getTokenType()) {
            case AND: {
                if(left instanceof Double && right instanceof Double) {
                    double doubleLeft = (double) left;
                    double doubleRight = (double) right;
                    return Double.longBitsToDouble(Double.doubleToRawLongBits(doubleLeft) & Double.doubleToRawLongBits(doubleRight));
                }
                if(left instanceof Boolean && right instanceof Boolean) {
                    return (boolean) left & (boolean) right;
                }
                else throw beeLanguage.createBeeError("Unsupported type for logical expression", expression.getOperator());

            }
            case OR: {
                if(left instanceof Double && right instanceof Double) {
                    double doubleLeft = (double) left;
                    double doubleRight = (double) right;
                    return Double.longBitsToDouble(Double.doubleToRawLongBits(doubleLeft) | Double.doubleToRawLongBits(doubleRight));
                }
                if(left instanceof Boolean && right instanceof Boolean) {
                    return (boolean) left | (boolean) right;
                }
                else throw beeLanguage.createBeeError("Unsupported type for logical expression", expression.getOperator());
            }
            case XOR: {
                if(left instanceof Double && right instanceof Double) {
                    double doubleLeft = (double) left;
                    double doubleRight = (double) right;
                    return Double.longBitsToDouble(Double.doubleToRawLongBits(doubleLeft) ^ Double.doubleToRawLongBits(doubleRight));
                }
                if(left instanceof Boolean && right instanceof Boolean) {
                    return (boolean) left ^ (boolean) right;
                }
                else throw beeLanguage.createBeeError("Unsupported type for logical expression", expression.getOperator());
            }
            default: {
                throw beeLanguage.createBeeError("UnSupported types for Logical operator.", expression.getOperator());
            }
        }
    }

    @Override
    public Object visit(UnaryExpression expression) {
        Object right = expression.getRight().accept(this);
        switch (expression.getOperator().getTokenType()) {
            case BANG: {
                if(right instanceof Boolean) {
                    boolean value = (boolean) right;
                    return !value;
                }
                throw beeLanguage.createBeeError("Invalid type for unary bang operator", expression.getOperator());
            }
            case MINUS: {
                if(right instanceof Double) {
                    return -(double) right;
                }
                throw beeLanguage.createBeeError("Invalid type for unary minus operator", expression.getOperator());
            }
        }
        return null;
    }

    @Override
    public Object visit(GroupExpression expression) {
        return expression.getExpression().accept(this);
    }

    private void execute(Comment comment) {
        comment.accept(this);
    }

    public void execute(List<Comment> comments, Environment localEnvironment) {
        Environment previous = this.environment;
        try {
            this.environment = localEnvironment;
            for (Comment comment : comments) {
                execute(comment);
            }
        } finally {
            this.environment = previous;
        }
    }

    private Object evaluate(Expression expression) {
        return expression.accept(this);
    }

    private boolean isTruth(Object object) {
        if (object == null) return false;
        if (object instanceof Boolean) return (boolean) object;
        return true;
    }

    private Object lookUpVariable(Token name, Expression expr) {
        if(name.getTokenType() == TokenType.THIS){
            return environment.getAt(1, "this");
        }

        Integer distance = locals.get(expr);
        if (distance != null) return environment.getAt(distance, name.getLiteral());
        else return globalEnvironment.get(name);
    }

    public void resolve(Expression expr, int depth) {
        locals.put(expr, depth);
    }
}

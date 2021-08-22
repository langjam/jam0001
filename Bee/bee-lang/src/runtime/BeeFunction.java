package runtime;

import ast.comment.FunctionComment;
import runtime.exceptions.ReturnException;
import token.Token;

import java.util.List;

public class BeeFunction implements BeeCallable {

    private final FunctionComment declaration;
    private final Environment closure;
    private final boolean isInitializerFunction;

    public BeeFunction(FunctionComment declaration, Environment closure) {
        this.declaration = declaration;
        this.closure = closure;
        this.isInitializerFunction = false;
    }

    public BeeFunction(FunctionComment declaration, Environment closure, boolean isInit) {
        this.declaration = declaration;
        this.closure = closure;
        this.isInitializerFunction = isInit;
    }

    public BeeFunction bind(BeeInstance instance) {
        Environment environment = new Environment(closure);
        environment.define("this", instance);
        return new BeeFunction(declaration, environment);
    }

    @Override
    public int arity() {
        return declaration.getParameters().size();
    }

    @Override
    public Object call(BeeInterpreter beeInterpreter, List<Object> arguments) {
        Environment environment = new Environment(closure);
        List<Token> parameters = declaration.getParameters();
        int size = parameters.size();
        for(int i = 0 ; i < size ; i++) {
            environment.define(parameters.get(i).getLiteral(), arguments.get(i));
        }

        try {
            beeInterpreter.execute(declaration.getBlockComment().getComments(), environment);
        } catch (ReturnException returnValue) {
            if (isInitializerFunction) return closure.getAt(0, "this");
            return returnValue.getReturnValue();
        }

        if (isInitializerFunction) return closure.getAt(0, "this");
        return null;
    }
}

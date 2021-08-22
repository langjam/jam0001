package runtime;

import java.util.List;
import java.util.Map;

public class BeeType implements BeeCallable {

    private final String name;
    private final BeeType superClass;
    private final Environment environment;
    private final Map<String, BeeFunction> method;

    public BeeType(String name, BeeType superClass, Environment environment, Map<String, BeeFunction> method) {
        this.name = name;
        this.superClass = superClass;
        this.environment = environment;
        this.method = method;
    }

    public String getName() {
        return name;
    }

    public BeeType getSuperClass() {
        return superClass;
    }

    public Environment getEnvironment() {
        return environment;
    }

    public Map<String, BeeFunction> getMethod() {
        return method;
    }

    public BeeFunction findMethod(String name) {
        if(method.containsKey(name)) return method.get(name);
        if(superClass != null) return superClass.findMethod(name);
        return null;
    }

    @Override
    public int arity() {
        BeeFunction initializer = findMethod("init");
        if(initializer == null) return 0;
        return initializer.arity();
    }

    @Override
    public Object call(BeeInterpreter beeInterpreter, List<Object> arguments) {
        BeeInstance instance = new BeeInstance(this);
        BeeFunction initializer = findMethod("init");
        if(initializer != null) initializer.bind(instance).call(beeInterpreter, arguments);
        return instance;
    }
}

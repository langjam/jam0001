package runtime;

import token.Token;

import java.util.HashMap;
import java.util.Map;

public class Environment {

    private final Environment enclosing;
    private final Map<String, Object> valuesMap = new HashMap<>();

    public Environment() {
        this.enclosing = null;
    }

    public Environment(Environment enclosing) {
        this.enclosing = enclosing;
    }

    public void define(String name, Object value) {
        valuesMap.put(name, value);
    }

    public Object get(Token name) {
        if(valuesMap.containsKey(name.getLiteral())) return valuesMap.get(name.getLiteral());
        if(enclosing != null) return enclosing.get(name);
        throw new RuntimeException("Undefined variable " + name.getLiteral());
    }

    public Object getAt(int distance, String name) {
        return ancestor(distance).valuesMap.get(name);
    }

    public void assign(Token name, Object value) {
        if (valuesMap.containsKey(name.getLiteral())) {
            valuesMap.put(name.getLiteral(), value);
            return;
        }
        if (enclosing != null) {
            enclosing.assign(name, value);
            return;
        }
        throw new RuntimeException("Undefined variable '" + name.getLiteral() + "'.");
    }

    void assignAt(int distance, Token name, Object value) {
        ancestor(distance).valuesMap.put(name.getLiteral(), value);
    }

    Environment ancestor(int distance) {
        Environment environment = this;
        for (int i = 0; i < distance; i++) {
            environment = environment.enclosing;
        }
        return environment;
    }

    public Environment getEnclosing() {
        return enclosing;
    }
}

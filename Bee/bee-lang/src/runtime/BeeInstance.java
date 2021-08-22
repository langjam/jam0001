package runtime;

import token.Token;

import java.util.HashMap;
import java.util.Map;

public class BeeInstance {

    private final BeeType beeType;
    private final Map<String, Object> fields = new HashMap<>();

    public BeeInstance(BeeType beeType) {
        this.beeType = beeType;
    }

    public void set(Token name, Object value){
        fields.put(name.getLiteral(),value);
    }

    public Object get(Token name){
        if (fields.containsKey(name.getLiteral())) {
            return fields.get(name.getLiteral());
        }
        BeeFunction method = beeType.findMethod(name.getLiteral());
        if (method != null) return method.bind(this);
        throw new RuntimeException("Undefined property '" + name.getLiteral() + "'.");
    }

    @Override
    public String toString() {
        return beeType.getName() + " instance";
    }
}

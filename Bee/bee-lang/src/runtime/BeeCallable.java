package runtime;

import java.util.List;

public interface BeeCallable {
    int arity();
    Object call(BeeInterpreter beeInterpreter, List<Object> arguments);
}

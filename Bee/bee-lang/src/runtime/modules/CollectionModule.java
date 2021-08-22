package runtime.modules;

import runtime.BeeCallable;
import runtime.BeeInterpreter;
import runtime.Environment;

import java.util.*;

public class CollectionModule implements BeeModule {

    private final BeeCallable createArray = new BeeCallable() {
        @Override
        public int arity() {
            return 0;
        }

        @Override
        public Object call(BeeInterpreter beeInterpreter, List<Object> arguments) {
            return new ArrayList<>();
        }
    };

    private final BeeCallable createList = new BeeCallable() {
        @Override
        public int arity() {
            return 0;
        }

        @Override
        public Object call(BeeInterpreter beeInterpreter, List<Object> arguments) {
            return new LinkedList<>();
        }
    };

    private final BeeCallable createStack = new BeeCallable() {
        @Override
        public int arity() {
            return 0;
        }

        @Override
        public Object call(BeeInterpreter beeInterpreter, List<Object> arguments) {
            return new Stack<>();
        }
    };

    private final BeeCallable collectionAdd = new BeeCallable() {
        @Override
        public int arity() {
            return 2;
        }

        @Override
        public Object call(BeeInterpreter beeInterpreter, List<Object> arguments) {
            Object collection = arguments.get(0);
            if(collection instanceof Collection) {
                Collection<Object> dataStructure = (Collection) collection;
                return dataStructure.add(arguments.get(1));
            }
            return 0;
        }
    };

    private final BeeCallable collectionGet = new BeeCallable() {
        @Override
        public int arity() {
            return 2;
        }

        @Override
        public Object call(BeeInterpreter beeInterpreter, List<Object> arguments) {
            Object collection = arguments.get(0);
            if(collection instanceof List) {
                int index = ((Double) arguments.get(1)).intValue();
                return ((List<Object>) collection).get(index);
            }
            return 0;
        }
    };

    private final BeeCallable collectionPush = new BeeCallable() {
        @Override
        public int arity() {
            return 2;
        }

        @Override
        public Object call(BeeInterpreter beeInterpreter, List<Object> arguments) {
            Object collection = arguments.get(0);
            if(collection instanceof Stack) {
                Object data = arguments.get(1);
                return ((Stack<Object>) collection).push(data);
            }
            return null;
        }
    };

    private final BeeCallable collectionPop = new BeeCallable() {
        @Override
        public int arity() {
            return 1;
        }

        @Override
        public Object call(BeeInterpreter beeInterpreter, List<Object> arguments) {
            Object collection = arguments.get(0);
            if(collection instanceof Stack) {
                return ((Stack<Object>) collection).pop();
            }
            return null;
        }
    };

    private final BeeCallable collectionPeek = new BeeCallable() {
        @Override
        public int arity() {
            return 1;
        }

        @Override
        public Object call(BeeInterpreter beeInterpreter, List<Object> arguments) {
            Object collection = arguments.get(0);
            if(collection instanceof Stack) {
                return ((Stack<Object>) collection).peek();
            }
            return null;
        }
    };

    private final BeeCallable collectionSize = new BeeCallable() {
        @Override
        public int arity() {
            return 1;
        }

        @Override
        public Object call(BeeInterpreter beeInterpreter, List<Object> arguments) {
            Object collection = arguments.get(0);
            if(collection instanceof Collection) {
                return ((Collection) collection).size();
            }
            return 0;
        }
    };

    @Override
    public void injectIntoEnvironment(Environment environment) {
        environment.define("createArray", createArray);
        environment.define("createList", createList);
        environment.define("createStack", createStack);

        environment.define("collectionAdd", collectionAdd);
        environment.define("collectionGet", collectionGet);

        environment.define("collectionPush", collectionPush);
        environment.define("collectionPop", collectionPop);
        environment.define("collectionPeek", collectionPeek);

        environment.define("collectionSize", collectionSize);
    }
}

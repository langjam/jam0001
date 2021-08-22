package runtime.modules;

import runtime.BeeCallable;
import runtime.BeeInterpreter;
import runtime.Environment;

import java.util.List;
import java.util.Scanner;

public class IoModule implements BeeModule {

    private final Scanner scanner = new Scanner(System.in);

    private final BeeCallable print = new BeeCallable() {
        @Override
        public int arity() {
            return 1;
        }

        @Override
        public Object call(BeeInterpreter beeInterpreter, List<Object> arguments) {
            String value = String.valueOf(arguments.get(0));
            System.out.print(value);
            return value.length();
        }
    };

    private final BeeCallable println = new BeeCallable() {
        @Override
        public int arity() {
            return 1;
        }

        @Override
        public Object call(BeeInterpreter beeInterpreter, List<Object> arguments) {
            String value = String.valueOf(arguments.get(0));
            System.out.println(value);
            return value.length();
        }
    };

    private final BeeCallable scanString = new BeeCallable() {
        @Override
        public int arity() {
            return 0;
        }

        @Override
        public Object call(BeeInterpreter beeInterpreter, List<Object> arguments) {
            return scanner.next();
        }
    };

    private final BeeCallable scanLine = new BeeCallable() {
        @Override
        public int arity() {
            return 0;
        }

        @Override
        public Object call(BeeInterpreter beeInterpreter, List<Object> arguments) {
            return scanner.nextLine();
        }
    };

    private final BeeCallable scanNumber = new BeeCallable() {
        @Override
        public int arity() {
            return 0;
        }

        @Override
        public Object call(BeeInterpreter beeInterpreter, List<Object> arguments) {
            return scanner.nextDouble();
        }
    };

    @Override
    public void injectIntoEnvironment(Environment environment) {
        environment.define("print", print);
        environment.define("println", println);
        environment.define("scanString", scanString);
        environment.define("scanLine", scanLine);
        environment.define("scanNumber", scanNumber);
    }
}

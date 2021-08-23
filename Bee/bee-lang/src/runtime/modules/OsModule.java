package runtime.modules;

import runtime.BeeCallable;
import runtime.BeeInterpreter;
import runtime.Environment;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.List;

public class OsModule implements BeeModule {

    private final BeeCallable threadName = new BeeCallable() {
        @Override
        public int arity() {
            return 0;
        }

        @Override
        public Object call(BeeInterpreter beeInterpreter, List<Object> arguments) {
            return Thread.currentThread().getName();
        }
    };

    private final BeeCallable coreNumber = new BeeCallable() {
        @Override
        public int arity() {
            return 0;
        }

        @Override
        public Object call(BeeInterpreter beeInterpreter, List<Object> arguments) {
            return Runtime.getRuntime().availableProcessors() + 0.0;
        }
    };

    private final BeeCallable osName = new BeeCallable() {
        @Override
        public int arity() {
            return 0;
        }

        @Override
        public Object call(BeeInterpreter beeInterpreter, List<Object> arguments) {
            return System.getProperty("os.name");
        }
    };

    private final BeeCallable userName = new BeeCallable() {
        @Override
        public int arity() {
            return 0;
        }

        @Override
        public Object call(BeeInterpreter beeInterpreter, List<Object> arguments) {
            return System.getProperty("user.name");
        }
    };

    private final BeeCallable sleep = new BeeCallable() {
        @Override
        public int arity() {
            return 1;
        }

        @Override
        public Object call(BeeInterpreter beeInterpreter, List<Object> arguments) {
            try {
                double time = (double) arguments.get(0);
                Thread.sleep((long) time);
            } catch (InterruptedException e) {
                throw new RuntimeException(e.getMessage());
            }
            return null;
        }
    };

    private final BeeCallable system = new BeeCallable() {
        @Override
        public int arity() {
            return 1;
        }

        @Override
        public Object call(BeeInterpreter beeInterpreter, List<Object> arguments) {
            try {
                Process process = Runtime.getRuntime().exec(String.valueOf(arguments.get(0)));
                BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
                String line;
                StringBuilder builder = new StringBuilder();
                while ((line = reader.readLine()) != null) {
                    builder.append(line);
                }
                return builder.toString();
            } catch (IOException e) {
                return null;
            }
        }
    };

    private final BeeCallable getEnvVariable = new BeeCallable() {
        @Override
        public int arity() {
            return 1;
        }

        @Override
        public Object call(BeeInterpreter beeInterpreter, List<Object> arguments) {
            return System.getenv(String.valueOf(arguments.get(0)));
        }
    };

    @Override
    public void injectIntoEnvironment(Environment environment) {
        environment.define("threadName", threadName);
        environment.define("coreNumber", coreNumber);
        environment.define("osName", osName);
        environment.define("userName", userName);
        environment.define("sleep", sleep);
        environment.define("system", system);
        environment.define("getEnvVariable", getEnvVariable);
    }
}

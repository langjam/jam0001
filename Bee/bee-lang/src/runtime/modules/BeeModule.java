package runtime.modules;

import runtime.Environment;

@FunctionalInterface
public interface BeeModule {
    void injectIntoEnvironment(Environment environment);
}

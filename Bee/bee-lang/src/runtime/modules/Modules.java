package runtime.modules;

import java.util.ArrayList;
import java.util.List;

public class Modules {

    private static final List<BeeModule> stdModules = new ArrayList<>();
    static {
        stdModules.add(new IoModule());
        stdModules.add(new OsModule());
        stdModules.add(new CollectionModule());
    }

    public static List<BeeModule> getStdModules() {
        return stdModules;
    }
}

import runtime.BeeLanguage;
import utils.FileUtils;

import java.io.File;
import java.util.Arrays;

public class Main {

    public static void main(String[] args) {
        int argc = args.length;
        if(argc < 1) {
            System.out.println("Invalid number of arguments");
            System.out.println("java -jar bee-lang.jar <source-file> <options>");
            System.exit(1);
        }

        String filePath = args[0];
        if(!FileUtils.isBeeSourceFile(filePath)) {
            System.out.println("Sorry, Bee source file must end with .bb extension");
            printAvailableOptions();
            System.exit(1);
        }

        File sourceFile = new File(filePath);
        if(!sourceFile.exists()) {
            System.out.println("Your Bee source is not exists");
            System.exit(1);
        }

        String sourceDirectory = sourceFile.getParent() + File.separator;
        BeeLanguage beeLanguage = new BeeLanguage(filePath, sourceDirectory);

        beeLanguage.setArgc(argc);
        beeLanguage.setArgv(Arrays.asList(args));

        for (int i = 1; i < argc; i++) {
            String optionName = args[i];
            switch (optionName) {
                case "-A":
                case "-a": {
                    beeLanguage.setShowAlerts(true);
                    break;
                }
                case "-N":
                case "-n": {
                    beeLanguage.setShowNotes(true);
                    break;
                }
                case "-C":
                case "-c": {
                    if (argc == (i + 1)) {
                        System.out.println("Invalid number of arguments");
                        System.out.println("Please check the options");
                        printAvailableOptions();
                        System.exit(1);
                    }
                    int coreCount = Integer.parseInt(args[++i]);
                    if(coreCount < 1) {
                        System.out.println("The number of cores must be greater than or equal 1");
                        System.exit(1);
                    }
                    beeLanguage.setAvailableCoresNumber(coreCount);
                    break;
                }
                case "-v":
                case "-V": {
                    System.out.println("Language version is : " + beeLanguage.getBeeLangVersion());
                    break;
                }
                case "-h":
                case "-H": {
                    printAvailableOptions();
                    break;
                }
            }
        }

        beeLanguage.execute();
    }

    private static void printAvailableOptions() {
        System.out.println("java -jar bee-lang.jar <source-file> <options>");
        System.out.println("Options list:");
        System.out.println("-a                       Enable the alert comments feature");
        System.out.println("-n                       Enable the note comments feature");
        System.out.println("-c <Integer>             Set the number of cores to use them");
        System.out.println("-v                       Print the language version");
        System.out.println("-h                       Print the options");
    }
}
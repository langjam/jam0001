package utils;

public class FileUtils {

    private static final String BEE_LANG_FILE_EXTENSION = "bee";

    public static String extensionName(String fileName) {
        String extension = "";
        int i = fileName.lastIndexOf('.');
        if (i > 0) {
            extension = fileName.substring(i + 1);
        }
        return extension;
    }

    public static boolean isBeeSourceFile(String fileName) {
        return extensionName(fileName).equals(BEE_LANG_FILE_EXTENSION);
    }
}

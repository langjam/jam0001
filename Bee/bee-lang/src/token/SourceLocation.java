package token;

public class SourceLocation {

    private final String fileName;
    private final int lineNumber;
    private final int columnStart;
    private final int columnEnd;

    public SourceLocation(String fileName, int lineNumber, int columnStart, int columnEnd) {
        this.fileName = fileName;
        this.lineNumber = lineNumber;
        this.columnStart = columnStart;
        this.columnEnd = columnEnd;
    }

    public String getFileName() {
        return fileName;
    }

    public int getLineNumber() {
        return lineNumber;
    }

    public int getColumnStart() {
        return columnStart;
    }

    public int getColumnEnd() {
        return columnEnd;
    }
}

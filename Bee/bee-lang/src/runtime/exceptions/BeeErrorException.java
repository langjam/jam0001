package runtime.exceptions;

import token.SourceLocation;

public class BeeErrorException extends RuntimeException {

    private final String message;
    private final SourceLocation location;

    public BeeErrorException(String message, SourceLocation location) {
        super(null, null, false, false);
        this.message = message;
        this.location = location;
    }

    @Override
    public String getMessage() {
        StringBuilder builder = new StringBuilder();
        builder.append("\nOnFile ").append(location.getFileName()).append(".\n");
        builder.append("At Line ").append(location.getLineNumber()).append(" Column ");
        builder.append("( ");
        builder.append(location.getColumnStart());
        if(location.getColumnStart() != location.getColumnEnd()) {
            builder.append(" - ");
            builder.append(location.getColumnEnd());
        }
        builder.append(" )");
        builder.append("\n");
        builder.append("Message : ").append(message);
        builder.append(".\n");
        return builder.toString();
    }
}

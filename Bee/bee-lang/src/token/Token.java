package token;

public class Token {

    private final TokenType tokenType;
    private final SourceLocation sourceLocation;
    private final String literal;
    private final Object value;

    public Token(TokenType tokenType, SourceLocation sourceLocation, String literal, Object value) {
        this.tokenType = tokenType;
        this.value = value;
        this.sourceLocation = sourceLocation;
        this.literal = literal;
    }

    public TokenType getTokenType() {
        return tokenType;
    }

    public SourceLocation getSourceLocation() {
        return sourceLocation;
    }

    public String getLiteral() {
        return literal;
    }

    public Object getValue() {
        return value;
    }
}

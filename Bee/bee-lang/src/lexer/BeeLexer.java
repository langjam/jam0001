package lexer;

import runtime.BeeLanguage;
import token.BeeKeywords;
import token.SourceLocation;
import token.Token;
import token.TokenType;
import utils.FileReader;

public class BeeLexer {

    BeeLanguage language;
    private final String sourceFile;
    private final String sourceContent;
    private final int sourceLength;
    private int sourcePosition;
    private int lineNumber;
    private int columnStart;
    private int columnCurrent;

    public BeeLexer(BeeLanguage language, FileReader fileReader) {
        this.language = language;
        this.sourceFile = fileReader.getFilePath();
        this.sourceContent = fileReader.getFileContent();
        this.sourceLength = sourceContent.length();
        this.sourcePosition = 0;
        this.lineNumber = 1;
        this.columnStart = 0;
        this.columnCurrent = 0;
    }

    public Token scanToken() {
        skipWhitespace();

        columnStart = columnCurrent;

        char c = advance();

        columnStart++;
        columnCurrent = columnStart;

        switch (c) {
            case '#': return makeToken(TokenType.HASH);
            case '@': return makeToken(TokenType.AT);
            case ',': return makeToken(TokenType.COMMA);
            case '.': return makeToken(TokenType.DOT);
            case '(': return makeToken(TokenType.LEFT_PAREN);
            case ')': return makeToken(TokenType.RIGHT_PAREN);

            case '+': return makeToken(TokenType.PLUS);
            case '*': return makeToken(match('/') ? TokenType.STAR_SLASH : TokenType.STAR);
            case '-': {
                if(match('>')) return makeToken(TokenType.ARROW);
                if(match('-')) {
                    advance();

                    while (!isSourceEnd() && peek() != '\n') {
                        advance();
                        columnCurrent++;
                    }

                    int identifierLength = columnCurrent - columnStart;
                    String identifier = sourceContent.substring(sourcePosition - identifierLength - 1, sourcePosition);

                    return makeToken(TokenType.MINIS_MINUS, identifier);
                }
                return makeToken(TokenType.MINUS);
            }

            case '/': {
                if(match('/')) return makeToken(TokenType.SLASH_SLASH);
                else if(match('*')) return makeToken(TokenType.SLASH_STAR);
                else return makeToken(TokenType.SLASH);
            }

            case '=': return makeToken(TokenType.EQUAL);
            case '!': return makeToken(match('=') ? TokenType.BANG_EQUAL : TokenType.BANG);
            case '>': return makeToken(match('=') ? TokenType.GREATER_EQUAL : TokenType.GREATER);
            case '<': return makeToken(match('=') ? TokenType.LESS_EQUAL : TokenType.LESS);

            case '\'': return scanCharacter();
            case '"': return scanString();
            case '0': case '1': case '2': case '3': case '4':
            case '5': case '6': case '7': case '8': case '9':
                return scanNumber();
            case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
            case 'G': case 'H': case 'I': case 'J': case 'K': case 'L':
            case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R':
            case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
            case 'Y': case 'Z':
            case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
            case 'g': case 'h': case 'i': case 'j': case 'k': case 'l':
            case 'm': case 'n': case 'o': case 'p': case 'q': case 'r':
            case 's': case 't': case 'u': case 'v': case 'w': case 'x':
            case 'y': case 'z':
            case '_':
                return scanIdentifier();
            case '\0': return makeToken(TokenType.END_OF_FILE);
            default: {
                throw language.createBeeError("Unexpected Character", makeSourceLocation());
            }
        }
    }

    private Token scanIdentifier() {
        while (isAlpha(peek()) || isDigit(peek())) {
            columnCurrent++;
            advance();
        }

        int identifierLength = columnCurrent - columnStart;
        String identifier = sourceContent.substring(sourcePosition - identifierLength - 1, sourcePosition);
        TokenType tokenType = BeeKeywords.keywordTokenType(identifier);
        if(tokenType == TokenType.IDENTIFIER) return makeToken(tokenType, identifier);
        return makeToken(tokenType);
    }

    private Token scanNumber() {
        while (isDigit(peek())) {
            advance();
            columnCurrent++;
        }

        if(peek() == '.' && isDigit(peekNext())) {
            advance();
            columnCurrent++;
            while (isDigit(peek())) {
                advance();
                columnCurrent++;
            }
        }

        int numberLength = columnCurrent - columnStart;
        String numberLiteral = sourceContent.substring(sourcePosition - numberLength - 1, sourcePosition);
        return makeToken(TokenType.NUMBER, numberLiteral, Double.parseDouble(numberLiteral));
    }

    private Token scanString() {
        while (!isSourceEnd() && peek() != '"') {
            if(peek() == '\n') {
                lineNumber++;
                columnCurrent = 0;
            }
            advance();
            columnCurrent++;
        }

        if(isSourceEnd()) {
            throw language.createBeeError("Unterminated String", makeSourceLocation());
        }

        advance();
        columnCurrent++;
        int wordLength = columnCurrent - columnStart;
        String stringLiteral = sourceContent.substring(sourcePosition - wordLength, sourcePosition - 1);
        return makeToken(TokenType.STRING, stringLiteral, stringLiteral);
    }

    private Token scanCharacter() {
        char c = peek();

        if(isSourceEnd() || peekNext() != '\'') {
            throw language.createBeeError("Unterminated Character", makeSourceLocation());
        }

        sourcePosition = sourcePosition + 2;

        return makeToken(TokenType.CHARACTER, String.valueOf(c), c);
    }

    private void skipWhitespace() {
        while (true) {
            char c = peek();
            switch (c) {
                case ' ':
                case '\r':
                case '\t': {
                    advance();
                    columnCurrent++;
                    break;
                }
                case '\n': {
                    lineNumber++;
                    columnCurrent = 0;
                    advance();
                    break;
                }
                case ';': {
                    while (!isSourceEnd() && peek() != '\n') {
                        advance();
                    }
                    break;
                }
                default:
                    return;
            }
        }
    }

    private Token makeToken(TokenType tokenType) {
        return makeToken(tokenType, null);
    }

    private Token makeToken(TokenType tokenType, String literal) {
        return makeToken(tokenType, literal, null);
    }

    private Token makeToken(TokenType tokenType, String literal, Object value) {
        SourceLocation location = makeSourceLocation();
        return new Token(tokenType, location, literal, value);
    }

    private SourceLocation makeSourceLocation() {
        return new SourceLocation(sourceFile, lineNumber, columnStart, columnCurrent);
    }

    private boolean match(char expected) {
        if(isSourceEnd()) return false;
        if(peek() != expected) return false;
        sourcePosition++;
        return true;
    }

    private boolean isDigit(char c) {
        return c >= '0' && c <= '9';
    }

    private boolean isAlpha(char c) {
        return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
    }

    private char advance() {
        char currentChar = peek();
        sourcePosition++;
        return currentChar;
    }

    private char peek() {
        if(isSourceEnd()) return '\0';
        return sourceContent.charAt(sourcePosition);
    }

    private char peekNext() {
        if(isSourceEnd()) return '\0';
        return sourceContent.charAt(sourcePosition + 1);
    }

    private boolean isSourceEnd() {
        return sourcePosition >= sourceLength;
    }
}

package token;

import java.util.HashMap;
import java.util.Map;

public class BeeKeywords {

    private static final Map<String, TokenType> keywords = new HashMap<>();

    static {
        keywords.put("load", TokenType.LOAD);
        keywords.put("todo", TokenType.TODO);
        keywords.put("fix", TokenType.FIX);
        keywords.put("warn", TokenType.WARN);

        keywords.put("type", TokenType.TYPE);
        keywords.put("extend", TokenType.EXTEND);
        keywords.put("this", TokenType.THIS);
        keywords.put("super", TokenType.SUPER);
        keywords.put("set", TokenType.SET);
        keywords.put("get", TokenType.GET);

        keywords.put("let", TokenType.LET);
        keywords.put("change", TokenType.CHANGE);
        keywords.put("be", TokenType.BE);
        keywords.put("if", TokenType.IF);
        keywords.put("loop", TokenType.LOOP);
        keywords.put("function", TokenType.FUN);
        keywords.put("take", TokenType.TAKE);
        keywords.put("return", TokenType.RETURN);
        keywords.put("call", TokenType.CALL);
        keywords.put("with", TokenType.WITH);
        keywords.put("break", TokenType.BREAK);
        keywords.put("continue", TokenType.CONTINUE);
        keywords.put("assert", TokenType.ASSERT);
        keywords.put("that", TokenType.THAT);

        keywords.put("run", TokenType.RUN);
        keywords.put("wait", TokenType.WAIT);

        keywords.put("and", TokenType.AND);
        keywords.put("or", TokenType.OR);
        keywords.put("xor", TokenType.XOR);

        keywords.put("true", TokenType.TRUE);
        keywords.put("false", TokenType.FALSE);
        keywords.put("null", TokenType.NULL);
    }

    public static boolean isLanguageKeyword(String keyword) {
        return keywords.containsKey(keyword);
    }

    public static TokenType keywordTokenType(String keyword) {
        return keywords.getOrDefault(keyword, TokenType.IDENTIFIER);
    }
}

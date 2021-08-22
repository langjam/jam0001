package parser;

import ast.comment.*;
import ast.expression.*;
import lexer.BeeLexer;
import runtime.BeeLanguage;
import token.Token;
import token.TokenType;
import utils.FileReader;
import utils.FileUtils;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class BeeParser {

    private final BeeLanguage language;
    private final BeeLexer lexer;
    private Token prevToken;
    private Token currentToken;
    private Token nextToken;

    public BeeParser(BeeLanguage language, BeeLexer lexer) {
        this.language = language;
        this.lexer = lexer;
        pointNextToken();
        pointNextToken();
    }

    public CommentsUnit parseCommentsUnit() {
        List<Comment> comments = new ArrayList<>();
        while (isSourceAvailable()) {
            comments.add(parseDeclarationComment());
        }
        return new CommentsUnit(comments);
    }

    private Comment parseDeclarationComment() {
        switch (currentToken.getTokenType()) {
            case HASH: {
                pointNextToken();
                if (matchOneOf(TokenType.LOAD)) return parseLoadComment();
                if (matchOneOf(TokenType.TODO, TokenType.FIX, TokenType.WARN)) return parseAlertComment();
                if (matchOneOf(TokenType.LET)) return parseLetComment();
                if (matchOneOf(TokenType.IF)) return parseIfComment();
                if (matchOneOf(TokenType.LOOP)) return parseLoopComment();
                if (matchOneOf(TokenType.FUN)) return parseFunctionComment();
                if (matchOneOf(TokenType.TYPE)) return parseTypeComment();
                if (matchOneOf(TokenType.RETURN)) return parseReturnComment();
                if (matchOneOf(TokenType.ASSERT)) return parseAssertComment();
                if (matchOneOf(TokenType.BREAK, TokenType.CONTINUE)) return parseControlFlowComment();
                throw language.createBeeError("unsupported hash comment ", currentToken);
            }
            case SLASH_STAR: return parseBlockComment();
            case MINIS_MINUS: return parseNoteComment();
            default: return parseStatementComment();
        }
    }

    private Comment parseLoadComment() {
        Token loadToken = prevToken;
        String file = currentToken.getLiteral();
        pointNextToken();
        if(language.isFileParsed(file)) return EmptyComment.getInstance();
        String filePath = language.getSourceDirectory() + file;

        if(!FileUtils.isBeeSourceFile(file)) {
            throw language.createBeeError("Bee Source File must end with .bee extension", loadToken);
        }

        File sourceFile = new File(filePath);

        if(!sourceFile.exists()) {
            throw language.createBeeError("Your source file is not exists", loadToken);
        }

        if(sourceFile.isFile()) {
            language.addNewFile(file);
            FileReader fileReader = new FileReader(filePath);
            BeeLexer beeLexer = new BeeLexer(language, fileReader);
            BeeParser beeParser = new BeeParser(language, beeLexer);
            return beeParser.parseCommentsUnit();
        }

        List<Comment> unitList = new ArrayList<>();
        for(File source : Objects.requireNonNull(sourceFile.listFiles())) {
            String beeFilePath = source.getPath();
            if(!FileUtils.isBeeSourceFile(beeFilePath)) continue;
            language.addNewFile(beeFilePath);
            FileReader fileReader = new FileReader(beeFilePath);
            BeeLexer beeLexer = new BeeLexer(language, fileReader);
            BeeParser beeParser = new BeeParser(language, beeLexer);
            unitList.add(beeParser.parseCommentsUnit());
        }

        return new CommentsUnit(unitList);
    }

    private AlertComment parseAlertComment() {
        Token token = prevToken;
        AlertType alertType = AlertType.valueOf(token.getTokenType().name());
        String message = currentToken.getLiteral();
        pointNextToken();
        return new AlertComment(token, alertType, message);
    }

    private Comment parseLetComment() {
        Token variableName = currentToken;
        pointNextToken();
        pointNextToken();   // be
        if(prevToken.getTokenType() != TokenType.BE) {
            throw language.createBeeError("Must add be after variable name", variableName);
        }
        Expression expression = parseExpression();
        return new LetComment(variableName, expression);
    }

    private Comment parseIfComment() {
        Token ifKeyword = prevToken;
        Expression condition = parseExpression();
        BlockComment blockComment = parseBlockComment();
        return new IfComment(ifKeyword, condition, blockComment);
    }

    private Comment parseLoopComment() {
        Token loopKeyword = prevToken;
        Expression condition = parseExpression();
        BlockComment blockComment = parseBlockComment();
        return new LoopComment(loopKeyword, condition, blockComment);
    }

    private FunctionComment parseFunctionComment() {
        Token functionName = currentToken;
        pointNextToken();
        List<Token> parameters = new ArrayList<>();
        if(matchOneOf(TokenType.TAKE)) {
            parameters.add(currentToken);
            pointNextToken();
            while (checkType(TokenType.COMMA)) {
                pointNextToken();
                parameters.add(currentToken);
                pointNextToken();
            }
        }
        BlockComment blockComment = parseBlockComment();
        return new FunctionComment(functionName, parameters, blockComment);
    }

    private Comment parseTypeComment() {
        Token typeName = currentToken;
        pointNextToken();

        VariableExpression parentType = null;

        if(matchOneOf(TokenType.EXTEND)) {
            parentType = new VariableExpression(currentToken);
            pointNextToken();
        }

        if(!matchOneOf(TokenType.SLASH_STAR)) {
            throw language.createBeeError("Type declaration Start end with */", typeName);
        }

        List<FunctionComment> methods = new ArrayList<>();

        while (isSourceAvailable() && currentToken.getTokenType() != TokenType.STAR_SLASH) {
            if(matchOneOf(TokenType.HASH) && matchOneOf(TokenType.FUN)) {
                methods.add(parseFunctionComment());
            } else {
                throw language.createBeeError("Unsupported comment inside type", currentToken);
            }
        }

        if(!matchOneOf(TokenType.STAR_SLASH)) {
            throw language.createBeeError("Type declaration must end with */", typeName);
        }

        return new TypeComment(typeName, parentType, methods);
    }

    private Comment parseReturnComment() {
        Token keyword = prevToken;
        Expression expression = null;
        if(matchOneOf(TokenType.WITH)) expression = parseExpression();
        return new ReturnComment(keyword, expression);
    }

    private Comment parseAssertComment() {
        Token keyword = prevToken;
        matchOneOf(TokenType.THAT);
        Expression expression = parseExpression();
        return new AssertComment(keyword, expression);
    }

    private Comment parseControlFlowComment() {
        Token keyword = prevToken;
        ControlFlowType controlFlowType = ControlFlowType.BREAK;
        if(currentToken.getTokenType() == TokenType.CONTINUE) controlFlowType = ControlFlowType.CONTINUE;
        return new ControlFlowComment(keyword, controlFlowType);
    }

    private BlockComment parseBlockComment() {
        pointNextToken();
        List<Comment> comments = new ArrayList<>();
        while (isSourceAvailable() && !checkType(TokenType.STAR_SLASH)) {
            comments.add(parseDeclarationComment());
        }

        if (checkType(TokenType.STAR_SLASH)) pointNextToken();
        else throw language.createBeeError("Block Comments must end with */ " + currentToken.getLiteral(), currentToken);

        return new BlockComment(comments);
    }

    private NoteComment parseNoteComment() {
        pointNextToken();
        return new NoteComment(prevToken);
    }

    private Comment parseStatementComment() {
        return parseExpressionStatement();
    }

    private Comment parseExpressionStatement() {
        return new ExpressionComment(parseExpression());
    }

    private Expression parseExpression() {
        return parseOrExpression();
    }

    private Expression parseOrExpression() {
        Expression expression = parseXorExpression();

        while (matchOneOf(TokenType.OR)) {
            Token operator = prevToken;
            Expression right = parseXorExpression();
            expression = new LogicalExpression(expression, operator, right);
        }

        return expression;
    }

    private Expression parseXorExpression() {
        Expression expression = parseAndExpression();

        while (matchOneOf(TokenType.XOR)) {
            Token operator = prevToken;
            Expression right = parseAndExpression();
            expression = new LogicalExpression(expression, operator, right);
        }

        return expression;
    }

    private Expression parseAndExpression() {
        Expression expression = parseEqualityExpression();

        while (matchOneOf(TokenType.AND)) {
            Token operator = prevToken;
            Expression right = parseEqualityExpression();
            expression = new LogicalExpression(expression, operator, right);
        }

        return expression;
    }

    private Expression parseEqualityExpression() {
        Expression expression = parseComparisonExpression();
        while (matchOneOf(TokenType.EQUAL, TokenType.BANG_EQUAL)) {
            Token operator = prevToken;
            Expression right = parseComparisonExpression();
            expression = new BinaryExpression(expression, operator, right);
        }
        return expression;
    }

    private Expression parseComparisonExpression() {
        Expression expression = parseAdditionExpression();

        while (matchOneOf(TokenType.GREATER,
                TokenType.GREATER_EQUAL,
                TokenType.LESS,
                TokenType.LESS_EQUAL)) {
            Token operator = prevToken;
            Expression right = parseAdditionExpression();
            expression = new ComparisonExpression(expression, operator, right);
        }

        return expression;
    }

    private Expression parseAdditionExpression() {
        Expression expression = parseMultiplicationExpression();
        while (matchOneOf(TokenType.PLUS, TokenType.MINUS)) {
            Token operator = prevToken;
            Expression right = parseMultiplicationExpression();
            expression = new BinaryExpression(expression, operator, right);
        }
        return expression;
    }

    private Expression parseMultiplicationExpression() {
        Expression expression = parseUnaryExpression();
        while (matchOneOf(TokenType.STAR, TokenType.SLASH)) {
            Token operator = prevToken;
            Expression right = parseUnaryExpression();
            expression = new BinaryExpression(expression, operator, right);
        }
        return expression;
    }

    private Expression parseUnaryExpression() {
        if(matchOneOf(TokenType.BANG, TokenType.MINUS)) {
            Token operator = prevToken;
            Expression right = parseUnaryExpression();
            return new UnaryExpression(right, operator);
        }
        return parsePrimaryExpression();
    }

    private Expression parsePrimaryExpression() {
        if (matchOneOf(TokenType.FALSE)) return new LiteralExpression(false);
        if (matchOneOf(TokenType.TRUE)) return new LiteralExpression(true);
        if (matchOneOf(TokenType.NULL)) return new LiteralExpression(null);
        if (matchOneOf(TokenType.NUMBER, TokenType.STRING, TokenType.CHARACTER)) return new LiteralExpression(prevToken.getValue());
        if (matchOneOf(TokenType.THIS)) return new ThisExpression(prevToken);
        if (matchOneOf(TokenType.IDENTIFIER)) return new VariableExpression(prevToken);
        if (matchOneOf(TokenType.AT)) return parseFunctionCallExpressions();
        if (matchOneOf(TokenType.LEFT_PAREN)) return parseGroupExpression();
        if (matchOneOf(TokenType.SUPER)) return parserSuperExpression();
        throw language.createBeeError("Invalid Expression type ", currentToken);
    }

    private Expression parseFunctionCallExpressions() {
        if (matchOneOf(TokenType.CALL, TokenType.RUN)) return parseCallExpression();
        if (matchOneOf(TokenType.WAIT)) return parseAsyncCallExpression();
        if (matchOneOf(TokenType.CHANGE)) return parseChangeExpression();
        if (matchOneOf(TokenType.SET)) return parseSetExpression();
        throw language.createBeeError("Invalid call type", currentToken);
    }

    private Expression parseCallExpression() {
        Token callToken = prevToken;
        String callType = callToken.getTokenType().name();
        CallType asyncCallType = CallType.valueOf(callType);

        Expression callee = parseExpression();

        if(matchOneOf(TokenType.DOT)) {
            Token name = currentToken;
            pointNextToken();
            return new GetExpression(name, callee);
        }

        if(matchOneOf(TokenType.ARROW)) {
            Token name = currentToken;
            pointNextToken();
            callee = new GetExpression(name, callee);
        }

        List<Expression> arguments = new ArrayList<>();
        if(matchOneOf(TokenType.WITH)) {
            arguments.add(parseExpression());
            while (matchOneOf(TokenType.COMMA)) {
                arguments.add(parseExpression());
            }
        }
        return new CallExpression(callToken, callee, arguments, asyncCallType);
    }

    private Expression parseAsyncCallExpression() {
        Token callToken = prevToken;
        Expression callee = parseExpression();
        List<Expression> arguments = new ArrayList<>();
        if(matchOneOf(TokenType.WITH)) {
            arguments.add(parseExpression());
            while (matchOneOf(TokenType.COMMA)) {
                arguments.add(parseExpression());
            }
        }
        return new AsyncCallExpression(callToken, callee, arguments);
    }

    private Expression parseGroupExpression() {
        Expression expression = parseExpression();
        if(!matchOneOf(TokenType.RIGHT_PAREN)) {
            throw language.createBeeError("Group expression must end with )", prevToken);
        }
        return new GroupExpression(expression);
    }

    private Expression parserSuperExpression() {
        Token superKeyword = prevToken;
        if(matchOneOf(TokenType.ARROW)) {
            Token methodName = currentToken;
            return new SuperExpression(superKeyword, methodName);
        }
        throw language.createBeeError("Expect . after super keyword", superKeyword);
    }

    private Expression parseSetExpression() {
        Token setKeyword = prevToken;
        Expression expression = parseExpression();
        if(expression instanceof GetExpression) {
            if(matchOneOf(TokenType.BE)) {
                GetExpression getExpression = (GetExpression) expression;
                Expression value = parseExpression();
                return new SetExpression(getExpression.getCalle(), getExpression.getName(), value);
            }
            else {
                throw language.createBeeError("Set expression be keyword", setKeyword);
            }
        }
        throw language.createBeeError("Set expression expect expression", setKeyword);
    }

    private Expression parseChangeExpression() {
        Token variableName = currentToken;
        pointNextToken();
        pointNextToken();
        if(prevToken.getTokenType() != TokenType.BE) {
            throw language.createBeeError("Must add be after variable name", variableName);
        }
        Expression expression = parseExpression();
        return new ChangeExpression(variableName, expression);
    }

    private boolean matchOneOf(TokenType...types) {
        for(TokenType type : types) {
            if(checkType(type)) {
                pointNextToken();
                return true;
            }
        }
        return false;
    }

    private boolean checkType(TokenType type) {
        if(!isSourceAvailable()) return false;
        return currentToken.getTokenType() == type;
    }

    private void pointNextToken() {
        prevToken = currentToken;
        currentToken = nextToken;
        nextToken = lexer.scanToken();
    }

    private boolean isSourceAvailable() {
        return currentToken.getTokenType() != TokenType.END_OF_FILE;
    }
}

package ast.comment;

import token.Token;

public class AlertComment extends Comment {

    private final Token alertToken;
    private final AlertType alertType;
    private final String alertMessage;

    public AlertComment(Token alertToken, AlertType alertType, String alertMessage) {
        this.alertToken = alertToken;
        this.alertType = alertType;
        this.alertMessage = alertMessage;
    }

    public Token getAlertToken() {
        return alertToken;
    }

    public AlertType getAlertType() {
        return alertType;
    }

    public String getAlertMessage() {
        return alertMessage;
    }

    @Override
    public <R> R accept(CommentVisitor<R> visitor) {
        return visitor.visit(this);
    }
}

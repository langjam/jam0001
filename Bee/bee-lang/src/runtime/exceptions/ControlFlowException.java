package runtime.exceptions;

import ast.comment.ControlFlowType;

public class ControlFlowException extends RuntimeException {

    private final ControlFlowType controlFlowType;

    public ControlFlowException(ControlFlowType controlFlowType) {
        super(null, null, false, false);
        this.controlFlowType = controlFlowType;
    }

    public ControlFlowType getControlFlowType() {
        return controlFlowType;
    }
}

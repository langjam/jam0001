package runtime.exceptions;

public class ReturnException extends RuntimeException {

    private final Object returnValue;

    public ReturnException(Object returnValue) {
        super(null, null, false, false);
        this.returnValue = returnValue;
    }

    public Object getReturnValue() {
        return returnValue;
    }
}

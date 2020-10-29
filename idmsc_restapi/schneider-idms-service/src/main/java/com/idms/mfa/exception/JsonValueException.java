package com.idms.mfa.exception;

import com.idms.model.mfa.JsonValue;

public class JsonValueException extends JsonException {

    /** Serializable class a version number. */
    static final long serialVersionUID = 1L;

    /** The JSON value for which the exception was thrown. */
    private final JsonValue value;

    /**
     * Constructs a new exception with the specified JSON value and {@code null}
     * as its detail message.
     *
     * @param value
     *            The JSON value.
     */
    public JsonValueException(JsonValue value) {
        this.value = value;
    }

    /**
     * Constructs a new exception with the specified JSON value and detail
     * message.
     *
     * @param value
     *            The JSON value.
     * @param message
     *            The message.
     */
    public JsonValueException(JsonValue value, String message) {
        super(message);
        this.value = value;
    }

    /**
     * Constructs a new exception with the specified JSON value and cause.
     *
     * @param value
     *            The JSON value.
     * @param cause
     *            The cause.
     */
    public JsonValueException(JsonValue value, Throwable cause) {
        super(cause);
        this.value = value;
    }

    /**
     * Constructs a new exception with the specified JSON value, detail message
     * and cause.
     *
     * @param value
     *            The JSON value.
     * @param message
     *            The message.
     * @param cause
     *            The cause.
     */
    public JsonValueException(JsonValue value, String message, Throwable cause) {
        super(message, cause);
        this.value = value;
    }

    /**
     * Returns the detail message string of this exception.
     *
     * @return The detail message string of this exception.
     */
    @Override
    public String getMessage() {
        StringBuilder sb = new StringBuilder();
        String message = super.getMessage();
        if (value != null) {
            sb.append(value.getPointer().toString());
        }
        if (value != null && message != null) {
            sb.append(": ");
        }
        if (message != null) {
            sb.append(message);
        }
        return sb.toString();
    }

    /**
     * Returns the JSON value for which the exception was thrown.
     *
     * @return The JSON value for which the exception was thrown.
     */
    public JsonValue getJsonValue() {
        return value;
    }
}


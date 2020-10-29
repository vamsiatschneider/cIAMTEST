package com.idms.mfa.exception;

public class JsonException extends RuntimeException {

   /** Serializable class a version number. */
   static final long serialVersionUID = 1L;

   /**
    * Constructs a new exception with {@code null} as its detail message.
    */
   public JsonException() {
   }

   /**
    * Constructs a new exception with the specified detail message.
    *
    * @param message
    *            The message.
    */
   public JsonException(String message) {
       super(message);
   }

   /**
    * Constructs a new exception with the specified cause.
    *
    * @param cause
    *            The cause.
    */
   public JsonException(Throwable cause) {
       super(cause);
   }

   /**
    * Constructs a new exception with the specified detail message and cause.
    *
    * @param message
    *            The message.
    * @param cause
    *            The cause.
    */
   public JsonException(String message, Throwable cause) {
       super(message, cause);
   }
}

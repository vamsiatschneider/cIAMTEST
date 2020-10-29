package com.idms.model.mfa;

public final class Reject {

    /**
     * Throws a {@code NullPointerException} if the <tt>object</tt> parameter is
     * null, returns the object otherwise.
     *
     * @param <T>
     *            The type of object to test.
     * @param object
     *            the object to test
     * @return the object
     * @throws NullPointerException
     *             if {@code object} is null
     */
    public static <T> T checkNotNull(final T object) {
        return checkNotNull(object, null);
    }

    /**
     * Throws a {@code NullPointerException} if the <tt>object</tt> parameter is
     * null, returns the object otherwise.
     *
     * @param <T>
     *            The type of object to test.
     * @param object
     *            the object to test
     * @param message
     *            a custom exception message to use
     * @return the object
     * @throws NullPointerException
     *             if {@code object} is null
     */
    public static <T> T checkNotNull(final T object, final String message) {
        if (object == null) {
            throw new NullPointerException(message);
        }
        return object;
    }

    /**
     * Throws a {@code NullPointerException} if the {@code str} parameter is {@code null}, throws
     * {@code IllegalArgumentException} if empty or only contains whitespace, and returns the string otherwise.
     *
     * @param str
     *            the string to check
     * @return the string
     * @throws NullPointerException
     *            if {@code str} is {@code null}
     * @throws IllegalArgumentException
     *            if {@code str} is empty or only contains whitespace
     */
    public static String checkNotBlank(final String str) {
        return checkNotBlank(str, null);
    }

    /**
     * Throws a {@code NullPointerException} if the {@code str} parameter is {@code null}, throws
     * {@code IllegalArgumentException} if empty or only contains whitespace, and returns the string otherwise.
     *
     * @param str
     *            the string to check
     * @param message
     *            a custom exception message to use
     * @return the string
     * @throws NullPointerException
     *            if {@code str} is {@code null}
     * @throws IllegalArgumentException
     *            if {@code str} is empty or only contains whitespace
     */
    public static String checkNotBlank(final String str, final String message) {
        checkNotNull(str, message);
        ifTrue(Strings.isBlank(str), message);
        return str;
    }

    /**
     * Alias for {@link #checkNotBlank(String)} to be used in fluent {@code Reject.ifBlank}
     * syntax. Throws a {@code NullPointerException} if the {@code str} parameter is {@code null}, throws
     * {@code IllegalArgumentException} if empty or only contains whitespace, and returns the string otherwise.
     *
     * @param str
     *            the string to check
     * @throws NullPointerException
     *            if {@code str} is {@code null}
     * @throws IllegalArgumentException
     *            if {@code str} is empty or only contains whitespace
     */
    public static void ifBlank(final String str) {
        checkNotBlank(str, null);
    }

    /**
     * Alias for {@link #checkNotBlank(String, String)} to be used in fluent {@code Reject.ifBlank}
     * syntax. Throws a {@code NullPointerException} if the {@code str} parameter is {@code null}, throws
     * {@code IllegalArgumentException} if empty or only contains whitespace, and returns the string otherwise.
     *
     * @param str
     *            the string to check
     * @param message
     *            a custom exception message to use
     * @throws NullPointerException
     *            if {@code str} is {@code null}
     * @throws IllegalArgumentException
     *            if {@code str} is empty or only contains whitespace
     */
    public static void ifBlank(final String str, final String message) {
        checkNotBlank(str, message);
    }

    /**
     * Throws an {@code IllegalArgumentException} if the <tt>condition</tt>
     * parameter is false.
     *
     * @param condition
     *            the condition to test
     * @throws IllegalArgumentException
     *             if {@code condition} is false
     */
    public static void ifFalse(final boolean condition) {
        ifFalse(condition, "Expected condition was true, found false");
    }

    /**
     * Throws an {@code IllegalArgumentException} with a custom {@code message}
     * if the <tt>condition</tt> parameter is false.
     *
     * @param condition
     *            the condition to test
     * @param message
     *            a custom exception message to use
     * @throws IllegalArgumentException
     *             if {@code condition} is false
     */
    public static void ifFalse(final boolean condition, final String message) {
        if (!condition) {
            throw new IllegalArgumentException(message);
        }
    }

    /**
     * Alias for {@code checkNotNull} to be used in fluent {@code Reject.ifNull}
     * syntax. Throws a {@code NullPointerException} if the <tt>object</tt>
     * parameter is null.
     *
     * @param object
     *            the object to test
     * @throws NullPointerException
     *             if {@code object} is null
     */
    public static void ifNull(final Object object) {
        ifNull(object, null);
    }

    /**
     * Throws a {@code NullPointerException} if any of the provided arguments
     * are {@code null}.
     *
     * @param <T>
     *            The type of object to test.
     * @param objects
     *            The objects to test.
     * @throws NullPointerException
     *             If any of the provided arguments are {@code null}.
     */
    @SafeVarargs
    public static <T> void ifNull(final T... objects) {
        /*
         * This method is generic in order to play better with varargs.
         * Otherwise invoking this method with an array of Strings will be
         * flagged with a warning because of the potential ambiguity. See
         * org.forgerock.util.RejectTest.ifNullVarArgsStrings().
         */
        for (final Object o : objects) {
            if (o == null) {
                throw new NullPointerException();
            }
        }
    }

    /**
     * Alias for {@code checkNotNull} to be used in fluent {@code Reject.ifNull}
     * syntax. Throws a {@code NullPointerException} if the <tt>object</tt>
     * parameter is null.
     *
     * @param object
     *            the object to test
     * @param message
     *            a custom exception message to use
     * @throws NullPointerException
     *             if {@code object} is null
     */
    public static void ifNull(final Object object, final String message) {
        checkNotNull(object, message);
    }

    /**
     * Throws an {@code IllegalArgumentException} if the <tt>condition</tt>
     * parameter is true.
     *
     * @param condition
     *            the condition to test
     * @throws IllegalArgumentException
     *             if {@code condition} is true
     */
    public static void ifTrue(final boolean condition) {
        ifTrue(condition, "Expected condition was false, found true");
    }

    /**
     * Throws an {@code IllegalArgumentException} with a custom {@code message}
     * if the <tt>condition</tt> parameter is true.
     *
     * @param condition
     *            the condition to test
     * @param message
     *            a custom exception message to use
     * @throws IllegalArgumentException
     *             if {@code condition} is true
     */
    public static void ifTrue(final boolean condition, final String message) {
        if (condition) {
            throw new IllegalArgumentException(message);
        }
    }

    /**
     * Throws an {@code IllegalStateException} with a custom {@code message}
     * if the <tt>condition</tt> parameter is true.
     *
     * @param condition
     *            the condition to test
     * @param message
     *            a custom exception message to use
     * @throws IllegalStateException
     *             if {@code condition} is true
     */
    public static void rejectStateIfTrue(final boolean condition, final String message) {
        if (condition) {
            throw new IllegalStateException(message);
        }
    }

    // Prevent instantiation
    private Reject() {
        // nothing to do
    }

}
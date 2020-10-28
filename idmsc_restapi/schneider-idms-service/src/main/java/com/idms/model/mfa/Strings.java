package com.idms.model.mfa;

import java.util.Arrays;
import java.util.Iterator;

public final class Strings {
    /**
     * Returns a string whose content is the string representation of the
     * provided objects concatenated together using the provided separator.
     *
     * @param separator
     *            The separator string.
     * @param values
     *            The objects to be joined.
     * @return A string whose content is the string representation of the
     *         provided objects concatenated together using the provided
     *         separator.
     * @throws NullPointerException
     *             If {@code values} or {@code separator} were {@code null}.
     */
    public static String joinAsString(final String separator, final Object... values) {
        return joinAsString(separator, Arrays.asList(values));
    }

    /**
     * Returns a string whose content is the string representation of the
     * objects contained in the provided iterable concatenated together using
     * the provided separator.
     *
     * @param separator
     *            The separator string.
     * @param values
     *            The iterable whose elements are to be joined.
     * @return A string whose content is the string representation of the
     *         objects contained in the provided iterable concatenated
     *         together using the provided separator.
     * @throws NullPointerException
     *             If {@code separator} or {@code values} were {@code null}.
     */
    public static String joinAsString(final String separator, final Iterable<?> values) {
        Reject.ifNull(separator);
        Reject.ifNull(values);

        final StringBuilder builder = new StringBuilder();
        joinAsString(builder, separator, values);
        return builder.toString();
    }

    /**
     * Appends into the provided {@link StringBuilder} the string representation
     * of the provided objects concatenated together using the provided separator.
     *
     * @param builder
     *            The String builder where to append.
     * @param separator
     *            The separator string.
     * @param values
     *            The objects to be joined.
     * @throws NullPointerException
     *             If {@code builder}, {@code separator} or {@code values} were {@code null}.
     */
    public static void joinAsString(final StringBuilder builder, final String separator, final Object... values) {
        joinAsString(builder, separator, Arrays.asList(values));
    }

    /**
     * Appends into the provided {@link StringBuilder} the string representation
     * of the objects contained in the provided iterable concatenated together
     * using the provided separator.
     *
     * @param builder
     *            The String builder where to append.
     * @param separator
     *            The separator string.
     * @param values
     *            The iterable whose elements are to be joined.
     * @throws NullPointerException
     *             If {@code builder}, {@code separator} or {@code values} were {@code null}.
     */
    public static void joinAsString(final StringBuilder builder, final String separator, final Iterable<?> values) {
        Reject.ifNull(builder);
        Reject.ifNull(separator);
        Reject.ifNull(values);

        final Iterator<?> iterator = values.iterator();
        if (iterator.hasNext()) {
            builder.append(iterator.next());

            while (iterator.hasNext()) {
                builder.append(separator);
                builder.append(iterator.next());
            }
        }
    }

    /**
     * Returns the string value as an enum constant of the specified enum
     * type. The string value and enum constants are compared, ignoring case
     * considerations. If the string value is {@code null}, this method returns
     * {@code null}.
     *
     * @param <T>
     *            the enum type sub-class.
     * @param value
     *            the string value
     * @param type
     *            the enum type to match constants with the value.
     * @return the enum constant represented by the string value.
     * @throws IllegalArgumentException
     *             if {@code type} does not represent an enum type,
     *             of if {@code value} does not match one of the enum constants
     * @throws NullPointerException
     *             if {@code type} is {@code null}.
     */
    public static <T extends Enum<T>> T asEnum(final String value, final Class<T> type) {
        if (value == null) {
            return null;
        }
        final T[] constants = type.getEnumConstants();
        if (constants == null) {
            throw new IllegalArgumentException("Type is not an enum class");
        }
        for (final T constant : constants) {
            if (value.equalsIgnoreCase(constant.toString())) {
                return constant;
            }
        }
        final StringBuilder sb = new StringBuilder("Expecting String containing one of: ");
        sb.append(joinAsString(" ", (Object[]) constants));
        throw new IllegalArgumentException(sb.toString());
    }

    /**
     * Check to see if the provided String is {@code null} or empty.
     * @param value The value to check.
     * @return {@code true} if the value is either {@code null} or is empty.
     */
    public static boolean isNullOrEmpty(String value) {
        return value == null || value.isEmpty();
    }

    /**
     * Check to see if a character sequence is null or blank.
     *
     * @param charSeq Sequence to test (String is also a CharSequence)
     * @return true if the char sequence is null or blank.
     */
    public static boolean isBlank(CharSequence charSeq) {
        if (charSeq == null) {
            return true;
        }
        final int length = charSeq.length();
        for (int i = 0; i < length; i++) {
            if (!Character.isWhitespace(charSeq.charAt(i))) {
                return false;
            }
        }
        return true;
    }

    private Strings() {

    }
}


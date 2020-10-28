package com.idms.model.mfa;

@FunctionalInterface
public interface Function<VIN, VOUT, E extends Exception> {
    /**
     * Applies this function to the input parameter {@code value} and returns
     * the result.
     *
     * @param value
     *            The input parameter.
     * @return The result of applying this function to {@code value}.
     * @throws E
     *             If this function cannot be applied to {@code value}.
     */
    VOUT apply(VIN value) throws E;
}

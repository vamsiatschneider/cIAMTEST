package com.idms.model.mfa;

@FunctionalInterface
public interface JsonPatchValueTransformer {
    /**
     * Return the value to be used for a given patch operation.
     *
     * @param target the patch target document.  Target is unused by default, made available
     *               for use by custom transforms.
     * @param op the patch operation.
     * @return The value from target pointed to by op, transformed as required by the implementation.
     * @throws JsonValueException when the value cannot be located in the target.
     */
    Object getTransformedValue(JsonValue target, JsonValue op);
}
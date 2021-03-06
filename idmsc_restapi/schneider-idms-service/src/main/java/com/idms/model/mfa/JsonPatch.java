package com.idms.model.mfa;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.idms.mfa.exception.JsonException;
import com.idms.mfa.exception.JsonValueException;

public final class JsonPatch {

    /**
     * Internet media type for the JSON Patch format.
     */
    public static final String MEDIA_TYPE = "application/json-patch";

    /**
     * Path to the "op" attribute of a patch entry. Required.
     */
    public static final JsonPointer OP_PTR = new JsonPointer("/op");

    /**
     * Path to the "path" attribute of a patch entry. Required.
     */
    public static final JsonPointer PATH_PTR = new JsonPointer("/path");

    /**
     * Path to the "from" attribute of a patch entry. Required only for "move" and "copy"
     * operations. Ignored for all others.
     */
    public static final JsonPointer FROM_PTR = new JsonPointer("/from");

    /**
     * Path to the "value" attribute of a patch entry. Required for "add", "replace" and
     * "test" operations; Ignored for all others.
     *
     * This is public to allow for alternate implementations of {@link JsonPatchValueTransformer}.
     */
    public static final JsonPointer VALUE_PTR = new JsonPointer("/value");

    /**
     * Default transform for patch values; Conforms to RFC6902.
     */
    private static final JsonPatchValueTransformer DEFAULT_TRANSFORM = (target, op) -> {
        if (op.get(JsonPatch.VALUE_PTR) != null) {
            return op.get(JsonPatch.VALUE_PTR).getObject();
        }
        throw new JsonValueException(op, "expecting a value member");
    };

    /**
     * Compares two JSON values, and produces a JSON Patch value, which contains the
     * operations necessary to modify the {@code original} value to arrive at the
     * {@code target} value.
     *
     * @param original the original value.
     * @param target the intended target value.
     * @return the resulting JSON Patch value.
     * @throws NullPointerException if either of {@code original} or {@code target} are {@code null}.
     */
    public static JsonValue diff(JsonValue original, JsonValue target) {
        final List<Object> result = new ArrayList<>();
        if (differentTypes(original, target)) { // different types cause a replace
            result.add(op("replace", original.getPointer(), target));
        } else if (original.isMap()) {
            for (String key : original.keys()) {
                if (target.isDefined(key)) { // target also has the property
                    JsonValue diff = diff(original.get(key), target.get(key)); // recursively compare properties
                    if (diff.size() > 0) {
                        result.addAll(diff.asList()); // add diff results
                    }
                } else { // property is missing in target
                    result.add(op("remove", original.getPointer().child(key), null));
                }
            }
            for (String key : target.keys()) {
                if (!original.isDefined(key)) { // property is in target, not in original
                    result.add(op("add", original.getPointer().child(key), target.get(key)));
                }
            }
        } else if (original.isList()) {
            boolean replace = false;
            if (original.size() != target.size()) {
                replace = true;
            } else {
                Iterator<JsonValue> i1 = original.iterator();
                Iterator<JsonValue> i2 = target.iterator();
                while (i1.hasNext() && i2.hasNext()) {
                    if (diff(i1.next(), i2.next()).size() > 0) { // recursively compare elements
                        replace = true;
                        break;
                    }
                }
            }
            if (replace) { // replace list entirely
                result.add(op("replace", original.getPointer(), target));
            }
        } else if (!original.isNull() && !original.getObject().equals(target.getObject())) { // simple value comparison
            result.add(op("replace", original.getPointer(), target));
        }
        return new JsonValue(result);
    }

    /**
     * Compares two JSON values, and returns whether the two objects are identical.  Fails fast in that a
     * {@code false} is returned as soon as a difference is detected.
     *
     * @param value a value.
     * @param other another value.
     * @return whether the two inputs are equal.
     * @throws NullPointerException if either of {@code value} or {@code other} are {@code null}.
     * @throws IllegalArgumentException if the {@link JsonValue} contains non-JSON primitive values.
     */
    public static boolean isEqual(JsonValue value, JsonValue other) {
        Reject.ifFalse(isJsonPrimitive(value) && isJsonPrimitive(other),
                "JsonPatch#isEqual only supports recognizable JSON primitives");
        if (differentTypes(value, other)) {
            return false;
        }
        if (value.size() != other.size()) {
            return false;
        }
        if (value.isMap()) {
            // only need test that other has same keys with same values as value as they are the same size at this point
            for (String key : value.keys()) {
                if (!other.isDefined(key) // other is missing the property
                            || !isEqual(value.get(key), other.get(key))) { // recursively compare properties
                    return false;
                }
            }
        } else if (value.isList()) {
            Iterator<JsonValue> i1 = value.iterator();
            Iterator<JsonValue> i2 = other.iterator();
            while (i1.hasNext() && i2.hasNext()) {
                if (!isEqual(i1.next(), i2.next())) { // recursively compare elements
                    return false;
                }
            }
        } else if (!value.isNull() && !value.getObject().equals(other.getObject())) { // simple value comparison
            return false;
        }
        return true;
    }

    private static boolean isJsonPrimitive(JsonValue value) {
        return value.isNull() || value.isBoolean() || value.isMap() || value.isList() || value.isNumber()
                || value.isString();
    }

    /**
     * Returns {@code true} if the type contained by {@code v1} is different than the type
     * contained by {@code v2}.
     * <p>
     * Note: If an unexpected (non-JSON) type is encountered, this method returns
     * {@code true}, triggering a change in the resulting patch.
     */
    private static boolean differentTypes(JsonValue v1, JsonValue v2) {
        return !(v1.isNull() && v2.isNull())
                && !(v1.isMap() && v2.isMap())
                && !(v1.isList() && v2.isList())
                && !(v1.isString() && v2.isString())
                && !(v1.isNumber() && v2.isNumber())
                && !(v1.isBoolean() && v2.isBoolean());
    }

    private static Map<String, Object> op(String op, JsonPointer pointer, JsonValue value) {
        Map<String, Object> result = new HashMap<>();
        result.put(OP_PTR.leaf(), op);
        result.put(PATH_PTR.leaf(), pointer.toString());
        if (value != null) {
            result.put(VALUE_PTR.leaf(), value.copy().getObject());
        }
        return result;
    }

    /**
     * Applies a set of modifications in a JSON patch value to an original value, resulting
     * in the intended target value. In the event of a failure, this method does not revert
     * any modifications applied up to the point of failure.
     *
     * @param original the original value on which to apply the modifications.
     * @param patch the JSON Patch value, specifying the modifications to apply to the original value.
     * @throws JsonValueException if application of the patch failed.
     */
    public static void patch(JsonValue original, JsonValue patch) {
        patch(original, patch, DEFAULT_TRANSFORM);
    }

    /**
     * Applies a set of modifications in a JSON patch value to an original value, resulting
     * in the intended target value. In the event of a failure, this method does not revert
     * any modifications applied up to the point of failure.
     *
     * @param original the original value on which to apply the modifications.
     * @param patch the JSON Patch value, specifying the modifications to apply to the original value.
     * @param transform a custom transform used to determine the target value.
     * @throws JsonValueException if application of the patch failed.
     */
    public static void patch(JsonValue original, JsonValue patch, JsonPatchValueTransformer transform) {
        for (JsonValue operation : patch.required().expect(List.class)) {
            if (!operation.isDefined("op")) {
                throw new JsonValueException(operation, "op not specified");
            }
            PatchOperation op = PatchOperation.valueOf(operation.get(OP_PTR));
            if (op == null) {
                throw new JsonValueException(operation, "invalid op specified");
            }
            op.execute(original, operation, transform);
        }
    }

    private enum PatchOperation {
        ADD {
            // http://tools.ietf.org/html/rfc6902#section-4.1
            @Override
            void execute(JsonValue original, JsonValue operation, JsonPatchValueTransformer transform) {
                JsonPointer modifyPath = operation.get(PATH_PTR).expect(String.class).as(pointer());
                JsonValue parent = parentValue(modifyPath, original);
                if (parent == null) {
                    // patch specifies a new root object
                    if (original.getObject() != null) {
                        throw new JsonValueException(operation, "root value already exists");
                    }
                    original.setObject(transform.getTransformedValue(original, operation));
                } else {
                    try {
                        if (parent.isList()) {
                            try {
                                // if the path points to an array index then we should insert the value
                                Integer index = Integer.valueOf(modifyPath.leaf());
                                parent.add(index, transform.getTransformedValue(original, operation));
                            } catch (Exception e) {
                                // leaf is not an array index, replace value
                                parent.add(modifyPath.leaf(), transform.getTransformedValue(original, operation));
                            }
                        } else if (original.get(modifyPath) != null && original.get(modifyPath).isList()) {
                            // modifyPath does not indicate an index, use the whole object
                            JsonValue target = original.get(modifyPath);
                            target.asList().add(transform.getTransformedValue(original, operation));
                        } else {
                            // this will replace the value even if present
                            parent.add(modifyPath.leaf(), transform.getTransformedValue(original, operation));
                        }
                    } catch (JsonException je) {
                        throw new JsonValueException(operation, je);
                    }
                }
            }
        },
        REMOVE {
            //http://tools.ietf.org/html/rfc6902#section-4.2
            @Override
            void execute(JsonValue original, JsonValue operation, JsonPatchValueTransformer transform) {
                JsonPointer modifyPath = operation.get(PATH_PTR).expect(String.class).as(pointer());
                JsonValue parent = parentValue(modifyPath, original);
                String leaf = modifyPath.leaf();
                if (parent == null) {
                    // patch specifies root object
                    original.setObject(null);
                } else {
                    if (!parent.isDefined(leaf)) {
                        throw new JsonValueException(operation, "value to remove not found");
                    }
                    try {
                        parent.remove(leaf);
                    } catch (JsonException je) {
                        throw new JsonValueException(operation, je);
                    }
                }
            }
        },
        REPLACE {
            //http://tools.ietf.org/html/rfc6902#section-4.3
            @Override
            void execute(JsonValue original, JsonValue operation, JsonPatchValueTransformer transform) {
                JsonPointer modifyPath = operation.get(PATH_PTR).expect(String.class).as(pointer());
                JsonValue parent = parentValue(modifyPath, original);
                if (parent != null) {
                    // replacing a child
                    String leaf = modifyPath.leaf();
                    if (!parent.isDefined(leaf)) {
                        throw new JsonValueException(operation, "value to replace not found");
                    }
                    parent.put(leaf, transform.getTransformedValue(original, operation));
                } else {
                    // replacing the root value itself
                    original.setObject(transform.getTransformedValue(original, operation));
                }
            }
        },
        MOVE {
            // http://tools.ietf.org/html/rfc6902#section-4.4
            @Override
            void execute(JsonValue original, JsonValue operation, JsonPatchValueTransformer transform) {
                JsonPointer sourcePath = operation.get(FROM_PTR).expect(String.class).as(pointer());
                JsonPointer destPath = operation.get(PATH_PTR).expect(String.class).as(pointer());
                JsonValue sourceParent = parentValue(sourcePath, original);
                if (sourceParent == null) {
                    throw new JsonValueException(operation, "cannot move root object");
                }
                JsonValue object = sourceParent.get(sourcePath.leaf());
                JsonValue destParent = parentValue(destPath, original);
                if (destParent == null) {
                    // replacing root object with moved object
                    original.setObject(object);
                } else {
                    sourceParent.remove(sourcePath.leaf());
                    destParent.put(destPath.leaf(), object);
                }
            }
        },
        COPY {
            // http://tools.ietf.org/html/rfc6902#section-4.5
            @Override
            void execute(JsonValue original, JsonValue operation, JsonPatchValueTransformer transform) {
                JsonPointer sourcePath = operation.get(FROM_PTR).expect(String.class).as(pointer());
                JsonPointer destPath = operation.get(PATH_PTR).expect(String.class).as(pointer());
                JsonValue sourceParent = parentValue(sourcePath, original);
                JsonValue object = sourceParent.get(sourcePath.leaf());
                JsonValue destParent = parentValue(destPath, original);
                if (destParent == null) {
                    // replacing root object with copied object
                    original.setObject(object);
                } else {
                    destParent.put(destPath.leaf(), object);
                }
            }
        },
        TEST {
            // http://tools.ietf.org/html/rfc6902#section-4.6
            @Override
            void execute(JsonValue original, JsonValue operation, JsonPatchValueTransformer transform) {
                JsonPointer testPath = operation.get(PATH_PTR).expect(String.class).as(pointer());
                JsonValue testTarget = parentValue(testPath, original).get(testPath.leaf());
                JsonValue testValue = new JsonValue(transform.getTransformedValue(original, operation));

                if (diff(testTarget, testValue).asList().size() > 0) {
                    throw new JsonValueException(operation, "test failed");
                }
            }
        };

        void execute(JsonValue original, JsonValue operation, JsonPatchValueTransformer transform) {
            throw new JsonValueException(original, "unsupported operation");
        }

        static PatchOperation valueOf(JsonValue op) {
            return valueOf(op.expect(String.class).asString().toUpperCase());
        }
    }

    /**
     * Returns the parent value of the value identified by the JSON pointer.
     *
     * @param pointer the pointer to the value whose parent value is to be returned.
     * @param target the JSON value against which to resolve the JSON pointer.
     * @return the parent value of the value identified by the JSON pointer.
     * @throws JsonException if the parent value could not be found.
     */
    private static JsonValue parentValue(JsonPointer pointer, JsonValue target) {
        JsonValue result = null;
        JsonPointer parent = pointer.parent();
        if (parent != null) {
            result = target.get(parent);
            if (result == null) {
                throw new JsonException("parent value not found");
            }
        }
        return result;
    }

    public static Function<JsonValue, JsonPointer, JsonValueException> pointer() {
        return value -> {
            try {
                return value.isNull() ? null : new JsonPointer(value.asString());
            } catch (final JsonValueException jve) {
                throw jve;
            } catch (final JsonException je) {
                throw new JsonValueException(value, je);
            }
        };
    }
    // prevent construction
    private JsonPatch() {
    }
}
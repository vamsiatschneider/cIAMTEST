package com.idms.model.mfa;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.idms.mfa.exception.JsonException;

/**
 * Builder factory class for a fluent way of creating JsonValue objects.
 *
 */
public class JsonValueBuilder {
    private static final ObjectMapper mapper = new ObjectMapper();

    /**
     * Creates a Builder object for creating JsonValue objects.
     *
     * @return A Builder class for creating JsonValues.
     */
    public static JsonObject jsonValue() {
        return new JsonObject();
    }

    /**
     * Converts a String into a JsonValue.
     *
     * @param json The json String.
     * @return A JsonValue object.
     * @throws IOException If there is a problem parsing the json String.
     */
    public static JsonValue toJsonValue(String json) throws JsonException {
        try {
            return new JsonValue(mapper.readValue(json, Map.class));
        } catch (IOException e) {
            throw new JsonException("Failed to parse json", e);
        }
    }

    /**
     * Converts the passed json string into a {@link JsonValue} represented as a list.
     *
     * @param json
     *         the json string
     *
     * @return a JsonValue instance represented as a list
     *
     * @throws JsonException
     *         should an error occur whilst parsing the json
     */
    public static JsonValue toJsonArray(final String json) throws JsonException {
        try {
            return new JsonValue(mapper.readValue(json, List.class));
        } catch (IOException e) {
            throw new JsonException("Failed to parse json", e);
        }
    }

    /**
     * Get singleton ObjectMapper instance for serialising to/from JSON.
     *
     * @return the shared ObjectMapper instance.
     * @see <a href="http://wiki.fasterxml.com/JacksonBestPracticesPerformance">Jackson Best Practices: Performance</a>
     */
    public static ObjectMapper getObjectMapper() {
        return mapper;
    }
}
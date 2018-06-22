package com.idms.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class TransliteratorErrorResponse {

	@JsonProperty("key")
	private String key;

	@JsonProperty("code")
	private String code;

	@JsonProperty("message")
	private String message;

	@JsonInclude(Include.NON_NULL)
	public String getKey() {
		return key;
	}

	@JsonInclude(Include.NON_NULL)
	public void setKey(String key) {
		this.key = key;
	}

	@JsonInclude(Include.NON_NULL)
	public String getCode() {
		return code;
	}

	@JsonInclude(Include.NON_NULL)
	public void setCode(String code) {
		this.code = code;
	}

	@JsonInclude(Include.NON_NULL)
	public String getMessage() {
		return message;
	}

	@JsonInclude(Include.NON_NULL)
	public void setMessage(String message) {
		this.message = message;
	}

}

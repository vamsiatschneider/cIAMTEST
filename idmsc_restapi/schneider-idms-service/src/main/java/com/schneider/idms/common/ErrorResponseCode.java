/**
 * 
 */
package com.schneider.idms.common;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author SESA508936
 *
 */
public class ErrorResponseCode {

	@JsonProperty("Code")
	private String code;
	
	@JsonProperty("Message")
	private String message;
	
	public String getCode() {
		return code;
	}
	public void setCode(String code) {
		this.code = code;
	}
	public String getMessage() {
		return message;
	}
	public void setMessage(String message) {
		this.message = message;
	}
	
}

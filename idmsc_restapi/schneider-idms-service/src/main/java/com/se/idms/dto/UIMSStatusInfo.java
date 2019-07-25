package com.se.idms.dto;

import com.fasterxml.jackson.annotation.JsonProperty;

public class UIMSStatusInfo {
	
	@JsonProperty("statusCode")
    private String statusCode;
    
    /**
     * Message of response.
     */
	@JsonProperty("message")
    private String message;

	public String getStatusCode() {
		return statusCode;
	}

	public void setStatusCode(String statusCode) {
		this.statusCode = statusCode;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	@Override
	public String toString() {
		return "UIMSStatusInfo [statusCode=" + statusCode + ", message=" + message + "]";
	}

}

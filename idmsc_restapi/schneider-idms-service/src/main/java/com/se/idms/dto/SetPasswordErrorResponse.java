package com.se.idms.dto;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The SetPasswordRequest class.
 * 
 * @author Pujarani Panda
 *
 */
public class SetPasswordErrorResponse {
	/**
     * status of response.
     */
	@JsonProperty("Status")
    private String status;
    
    /**
     * Message of response.
     */
	@JsonProperty("Message")
    private String message;
	
	@JsonProperty("Id")
	private String id;

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	
}

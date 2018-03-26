package com.se.idms.dto;

/**
 * The Class UserExistsResponse.
 * @author Aravindh Kumar
 *
 */
public class UserExistsResponse {

	private String message;
	
	public UserExistsResponse(){
		
	}

	public UserExistsResponse(String message) {
		this.setMessage(message);
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

}

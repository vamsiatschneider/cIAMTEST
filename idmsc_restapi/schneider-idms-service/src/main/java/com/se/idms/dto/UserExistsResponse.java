package com.se.idms.dto;

/**
 * The Class UserExistsResponse.
 * @author Aravindh Kumar
 *
 */
public class UserExistsResponse {

	private String message=null;
	private String userInfo=null;
	
	public String getMessage() {
		return message;
	}
	public void setMessage(String message) {
		this.message = message;
	}
	public String getUserInfo() {
		return userInfo;
	}
	public void setUserInfo(String userInfo) {
		this.userInfo = userInfo;
	}
	
	/*public UserExistsResponse(){
		
	}

	public UserExistsResponse(String message) {
		this.setMessage(message);
	}*/

}

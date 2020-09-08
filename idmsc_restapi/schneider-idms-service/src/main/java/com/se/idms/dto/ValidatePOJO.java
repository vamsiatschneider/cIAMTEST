package com.se.idms.dto;

import com.fasterxml.jackson.annotation.JsonProperty;

public class ValidatePOJO {
	
	@JsonProperty
	String NECaptchaValidate;
	
	@JsonProperty
	String username;

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public String getNECaptchaValidate() {
		return NECaptchaValidate;
	}

	public void setNECaptchaValidate(String nECaptchaValidate) {
		NECaptchaValidate = nECaptchaValidate;
	}
	
	@Override
	public String toString() {
		return "ValidatePOJO [NECaptchaValidate=" + NECaptchaValidate + ", username=" + username + "]";
	}

}

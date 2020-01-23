package com.idms.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class VerifyEmailPinRequest {

	@JsonInclude(Include.NON_NULL)
	private String email;
	
	@JsonInclude(Include.NON_NULL)
	private String otp;

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public String getOtp() {
		return otp;
	}

	public void setOtp(String otp) {
		this.otp = otp;
	}

}

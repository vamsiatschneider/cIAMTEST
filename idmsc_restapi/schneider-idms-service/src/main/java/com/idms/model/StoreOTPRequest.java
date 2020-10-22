/**
 * 
 */
package com.idms.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author SESA508936(Santosh Kumar)
 *
 */
public class StoreOTPRequest {
	
	@JsonProperty
	private String  emailOrMobile;
	
	@JsonProperty
	private String  otpValue;

	public String getEmailOrMobile() {
		return emailOrMobile;
	}

	public void setEmailOrMobile(String emailOrMobile) {
		this.emailOrMobile = emailOrMobile;
	}

	public String getOtpValue() {
		return otpValue;
	}

	public void setOtpValue(String otpValue) {
		this.otpValue = otpValue;
	}
	
}

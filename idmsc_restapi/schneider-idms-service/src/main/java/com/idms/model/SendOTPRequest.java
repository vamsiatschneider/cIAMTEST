/**
 * 
 */
package com.idms.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

/**
 * @author SESA508936
 *
 */
public class SendOTPRequest {
	
	@JsonInclude(Include.NON_NULL)
	private String  mobile;
	
	@JsonProperty
	private String  reqType; //OTP2FA, DUALREGMOBILE
	
	@JsonProperty
	private String  otpValue;

	public String getMobile() {
		return mobile;
	}

	public void setMobile(String mobile) {
		this.mobile = mobile;
	}
	
	public String getReqType() {
		return reqType;
	}

	public void setReqType(String reqType) {
		this.reqType = reqType;
	}

	public String getOtpValue() {
		return otpValue;
	}

	public void setOtpValue(String otpValue) {
		this.otpValue = otpValue;
	}
}

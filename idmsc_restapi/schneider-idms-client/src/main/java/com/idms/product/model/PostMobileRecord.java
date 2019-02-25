/**
 * 
 */
package com.idms.product.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

/**
 * @author SESA508936
 *
 */
public class PostMobileRecord {
	
	@JsonProperty
	private String _id;

	@JsonProperty
	private String tokenStatus;

	@JsonProperty
	private String mobileNumber;
	
	@JsonProperty
	private String otpToken;
	
	@JsonProperty
	private String tokenExpirationTstamp;

	public String get_id() {
		return _id;
	}

	public void set_id(String _id) {
		this._id = _id;
	}

	public String getTokenStatus() {
		return tokenStatus;
	}

	public void setTokenStatus(String tokenStatus) {
		this.tokenStatus = tokenStatus;
	}

	public String getMobileNumber() {
		return mobileNumber;
	}

	public void setMobileNumber(String mobileNumber) {
		this.mobileNumber = mobileNumber;
	}

	public String getOtpToken() {
		return otpToken;
	}

	public void setOtpToken(String otpToken) {
		this.otpToken = otpToken;
	}

	public String getTokenExpirationTstamp() {
		return tokenExpirationTstamp;
	}

	public void setTokenExpirationTstamp(String tokenExpirationTstamp) {
		this.tokenExpirationTstamp = tokenExpirationTstamp;
	}
	
}

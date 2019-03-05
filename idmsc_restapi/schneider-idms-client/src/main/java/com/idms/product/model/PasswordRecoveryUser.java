/**
 * 
 */
package com.idms.product.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author SESA508936
 *
 */
public class PasswordRecoveryUser {
	@JsonProperty
	private String email;

	@JsonProperty
	private String mobile;
	
	@JsonProperty
	private String profileLastUpdateSource;

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public String getMobile() {
		return mobile;
	}

	public void setMobile(String mobile) {
		this.mobile = mobile;
	}

	public String getProfileLastUpdateSource() {
		return profileLastUpdateSource;
	}

	public void setProfileLastUpdateSource(String profileLastUpdateSource) {
		this.profileLastUpdateSource = profileLastUpdateSource;
	}
	
}

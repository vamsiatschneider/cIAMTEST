package com.idms.product.model;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class OpenAmUserRecord {

	@JsonInclude(Include.NON_EMPTY)
	private String email;
	
	@JsonInclude(Include.NON_EMPTY)
	private String mobile;
	
	@JsonInclude(Include.NON_EMPTY)
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
	
	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}
}

package com.idms.model.digital;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.idms.model.digital.Authentication;
import com.idms.model.digital.GoDigitalUser;

public class GoDigitalUserRequest {

	@JsonProperty
	private Authentication authentication;

	@JsonProperty
	private GoDigitalUser userDetails;

	public Authentication getAuthentication() {
		return authentication;
	}

	public void setAuthentication(Authentication authentication) {
		this.authentication = authentication;
	}

	public GoDigitalUser getUserDetails() {
		return userDetails;
	}

	public void setUserDetails(GoDigitalUser userDetails) {
		this.userDetails = userDetails;
	}

	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}

}

package com.idms.model;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.idms.model.digital.GoDigitalUserRequest;

public class UserRegistrationInfoRequest {

	@JsonProperty
	private GoDigitalUserRequest userRegistrationInfoRequest;

	public GoDigitalUserRequest getUserRegistrationInfoRequest() {
		return userRegistrationInfoRequest;
	}

	public void setUserRegistrationInfoRequest(GoDigitalUserRequest userRegistrationInfoRequest) {
		this.userRegistrationInfoRequest = userRegistrationInfoRequest;
	}

	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}

}

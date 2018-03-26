package com.idms.model;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonProperty;

public class CreateUserRequest {

	@JsonProperty
	private IFWUser UserRecord;

	@JsonProperty
	private String Password;

	public IFWUser getUserRecord() {
		return UserRecord;
	}

	public void setUserRecord(IFWUser userRecord) {
		UserRecord = userRecord;
	}

	public String getPassword() {
		return Password;
	}

	public void setPassword(String password) {
		Password = password;
	}

	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}
}

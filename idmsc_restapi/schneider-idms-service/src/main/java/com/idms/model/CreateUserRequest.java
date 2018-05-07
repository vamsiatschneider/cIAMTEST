package com.idms.model;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonProperty;

public class CreateUserRequest {

	@JsonProperty
	private IFWUser UserRecord;

	@JsonProperty
	private String Password;

	@JsonProperty
	private String UIFlag;

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

	public String getUIFlag() {
		return UIFlag;
	}

	public void setUIFlag(String uIFlag) {
		UIFlag = uIFlag;
	}

	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}
}

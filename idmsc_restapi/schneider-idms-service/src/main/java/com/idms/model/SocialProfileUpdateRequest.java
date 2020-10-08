package com.idms.model;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonProperty;

public class SocialProfileUpdateRequest {

	@JsonProperty
	private IFWUser UserRecord;
	
	@JsonProperty
	private String UIFlag;

	@JsonProperty("UserRecord")
	public IFWUser getUserRecord() {
		return UserRecord;
	}

	@JsonProperty("UserRecord")
	public void setUserRecord(IFWUser userRecord) {
		UserRecord = userRecord;
	}

	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}

	public String getUIFlag() {
		return UIFlag;
	}

	public void setUIFlag(String uIFlag) {
		UIFlag = uIFlag;
	}
}
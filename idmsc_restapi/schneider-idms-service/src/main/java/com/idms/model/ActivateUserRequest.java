package com.idms.model;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonProperty;

public class ActivateUserRequest {

	@JsonProperty
	private ActivateUser UserRecord;

	public ActivateUser getUserRecord() {
		return UserRecord;
	}

	public void setUserRecord(ActivateUser userRecord) {
		UserRecord = userRecord;
	}

	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}
}

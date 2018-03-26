package com.idms.model;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonProperty;

public class UpdateUserRequest {

	@JsonProperty
	private IFWUser UserRecord;

	public IFWUser getUserRecord() {
		return UserRecord;
	}

	public void setUserRecord(IFWUser userRecord) {
		UserRecord = userRecord;
	}
	
	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}
}

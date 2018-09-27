package com.idms.model;

import java.util.List;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonProperty;

public class UpdateUserRequest {

	@JsonProperty
	private IFWUser UserRecord;
	
	@JsonProperty
	private List<RegistrationAttributes> attributes;

	public IFWUser getUserRecord() {
		return UserRecord;
	}

	public void setUserRecord(IFWUser userRecord) {
		UserRecord = userRecord;
	}
	
	public List<RegistrationAttributes> getAttributes() {
		return attributes;
	}

	public void setAttributes(List<RegistrationAttributes> attributes) {
		this.attributes = attributes;
	}

	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}
}

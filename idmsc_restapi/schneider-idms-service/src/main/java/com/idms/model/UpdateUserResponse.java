package com.idms.model;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonProperty;

public class UpdateUserResponse {

	private String status;

	private String message;

	private Object IDMSUserRecord;

	@JsonProperty("Status")
	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	@JsonProperty("Message")
	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	@JsonProperty("IDMSUserRecord")
	public Object getIDMSUserRecord() {
		return IDMSUserRecord;
	}

	public void setIDMSUserRecord(Object iDMSUserRecord) {
		IDMSUserRecord = iDMSUserRecord;
	}

	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}
}

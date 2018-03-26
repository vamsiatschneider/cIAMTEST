package com.idms.model;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonProperty;

public class CreateUserResponse {

	private String status;

	private String message;

	//private IFWUser IDMSUserRecord;
	
	private IDMSUserResponse IDMSUserRecord;
	
	@JsonProperty("IDMSUserRecord")
	public IDMSUserResponse getIDMSUserRecord() {
		return IDMSUserRecord;
	}

	public void setIDMSUserRecord(IDMSUserResponse iDMSUserRecord) {
		IDMSUserRecord = iDMSUserRecord;
	}

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
	
	/*@JsonProperty("IDMSUserRecord")
	public IFWUser getIDMSUserRecord() {
		return IDMSUserRecord;
	}

	public void setIDMSUserRecord(IFWUser iDMSUserRecord) {
		IDMSUserRecord = iDMSUserRecord;
	}*/

	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}
}

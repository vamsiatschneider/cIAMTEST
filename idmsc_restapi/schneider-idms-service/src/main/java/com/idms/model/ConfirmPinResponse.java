package com.idms.model;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonProperty;

public class ConfirmPinResponse {

	private String Status;

	private String Message;

	private String Id;

	private String Federation_Id;

	@JsonProperty("Status")
	public String getStatus() {
		return Status;
	}

	public void setStatus(String status) {
		Status = status;
	}

	@JsonProperty("Message")
	public String getMessage() {
		return Message;
	}

	public void setMessage(String message) {
		Message = message;
	}

	@JsonProperty("Id")
	public String getId() {
		return Id;
	}

	public void setId(String id) {
		Id = id;
	}

	@JsonProperty("Federation_Id")
	public String getFederation_Id() {
		return Federation_Id;
	}

	public void setFederation_Id(String federation_Id) {
		Federation_Id = federation_Id;
	}

	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}
}

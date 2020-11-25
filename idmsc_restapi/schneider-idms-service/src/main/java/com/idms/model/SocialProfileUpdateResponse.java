package com.idms.model;

import org.apache.commons.lang3.builder.ToStringBuilder;

public class SocialProfileUpdateResponse {

	private String status;

	private String message;

	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}
}
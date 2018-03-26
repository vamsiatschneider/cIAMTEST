package com.se.idms.dto;

import com.fasterxml.jackson.annotation.JsonProperty;

public class GetUserHomeByOauthResponse {

	@JsonProperty("user_id")
	private String userId;
	
	@JsonProperty("custom_attributes")
	private IFWCustomAttributesForHome customAttributes;

	public String getUserId() {
		return userId;
	}

	public void setUserId(String userId) {
		this.userId = userId;
	}

	public IFWCustomAttributesForHome getCustomAttributes() {
		return customAttributes;
	}

	public void setCustomAttributes(IFWCustomAttributesForHome customAttributes) {
		this.customAttributes = customAttributes;
	}

}

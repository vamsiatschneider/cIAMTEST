package com.se.idms.dto;

import com.fasterxml.jackson.annotation.JsonProperty;

public class GetUserWorkByOauthResponse {

	@JsonProperty("user_id")
	private String userId;

	@JsonProperty("custom_attributes")
	private IFWCustomAttributesForWork customAttributes;

	public String getUserId() {
		return userId;
	}

	public void setUserId(String userId) {
		this.userId = userId;
	}

	public IFWCustomAttributesForWork getCustomAttributes() {
		return customAttributes;
	}

	public void setCustomAttributes(IFWCustomAttributesForWork customAttributes) {
		this.customAttributes = customAttributes;
	}

}

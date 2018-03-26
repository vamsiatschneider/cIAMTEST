package com.idms.model;

import com.fasterxml.jackson.annotation.JsonProperty;

public class SendInvitationRequest {
	
	@JsonProperty
	private String email;
	
	@JsonProperty
	private String invitationId;
	
	@JsonProperty
	private String redirectUrl;

	@JsonProperty("email")
	public String getEmail() {
		return email;
	}

	@JsonProperty("email")
	public void setEmail(String email) {
		this.email = email;
	}

	@JsonProperty("InvitationId")
	public String getInvitationId() {
		return invitationId;
	}

	@JsonProperty("InvitationId")
	public void setInvitationId(String invitationId) {
		this.invitationId = invitationId;
	}

	@JsonProperty("RedirectUrl")
	public String getRedirectUrl() {
		return redirectUrl;
	}

	@JsonProperty("RedirectUrl")
	public void setRedirectUrl(String redirectUrl) {
		this.redirectUrl = redirectUrl;
	}

	

}

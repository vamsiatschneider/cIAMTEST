package com.schneider.idms.model;

import com.fasterxml.jackson.annotation.JsonProperty;

public class IdmsUserConfirmRequest {

	@JsonProperty
	private String federatedId;

	@JsonProperty
	private String pin;

	@JsonProperty
	private String operation;

	@JsonProperty
	private String profileLastUpdateSource;

	@JsonProperty
	private String uimsFederatedId;

	public String getFederatedId() {
		return federatedId;
	}

	public void setFederatedId(String federatedId) {
		this.federatedId = federatedId;
	}

	public String getPin() {
		return pin;
	}

	public void setPin(String pin) {
		this.pin = pin;
	}

	public String getOperation() {
		return operation;
	}

	public void setOperation(String operation) {
		this.operation = operation;
	}

	public String getProfileLastUpdateSource() {
		return profileLastUpdateSource;
	}

	public void setProfileLastUpdateSource(String profileLastUpdateSource) {
		this.profileLastUpdateSource = profileLastUpdateSource;
	}

	public String getUimsFederatedId() {
		return uimsFederatedId;
	}

	public void setUimsFederatedId(String uimsFederatedId) {
		this.uimsFederatedId = uimsFederatedId;
	}

}

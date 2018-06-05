package com.idms.model;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonProperty;

public class ResendPinRequest {

	@JsonProperty
	private String idmsUserId;

	@JsonProperty
	private String IDMS_Federated_ID__c;

	@JsonProperty
	private String Operation;

	@JsonProperty
	private String FederationIdentifier;

	public String getIdmsUserId() {
		return idmsUserId;
	}

	public void setIdmsUserId(String idmsUserId) {
		this.idmsUserId = idmsUserId;
	}

	public String getIDMS_Federated_ID__c() {
		return IDMS_Federated_ID__c;
	}

	public void setIDMS_Federated_ID__c(String iDMS_Federated_ID__c) {
		IDMS_Federated_ID__c = iDMS_Federated_ID__c;
	}

	public String getOperation() {
		return Operation;
	}

	public void setOperation(String operation) {
		Operation = operation;
	}

	@JsonProperty("FederationIdentifier")
	public String getFederationIdentifier() {
		return FederationIdentifier;
	}

	@JsonProperty("FederationIdentifier")
	public void setFederationIdentifier(String federationIdentifier) {
		FederationIdentifier = federationIdentifier;
	}

	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}
}

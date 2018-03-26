package com.idms.model;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonProperty;

public class ActivateUser {

	@JsonProperty
	private String Id;

	@JsonProperty
	private String IDMS_Federated_ID__c;

	@JsonProperty
	private String IDMS_Registration_Source__c;

	public String getId() {
		return Id;
	}

	public void setId(String id) {
		Id = id;
	}

	public String getIDMS_Federated_ID__c() {
		return IDMS_Federated_ID__c;
	}

	public void setIDMS_Federated_ID__c(String iDMS_Federated_ID__c) {
		IDMS_Federated_ID__c = iDMS_Federated_ID__c;
	}

	public String getIDMS_Registration_Source__c() {
		return IDMS_Registration_Source__c;
	}

	public void setIDMS_Registration_Source__c(String iDMS_Registration_Source__c) {
		IDMS_Registration_Source__c = iDMS_Registration_Source__c;
	}

	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}
}

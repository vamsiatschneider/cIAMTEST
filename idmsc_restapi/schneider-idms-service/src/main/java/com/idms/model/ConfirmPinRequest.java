package com.idms.model;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class ConfirmPinRequest {

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String id;
	
	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String  IDMS_Federated_ID__c;
	
	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String  PinCode;
	
	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String  Operation;
	
	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String  Password;
	
	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String IDMS_Profile_update_source;
	
	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String IDMS_Email_opt_in__c;
	
	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String UIFlag;
	
	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String tncFlag;
	
	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String FederationIdentifier;

	@JsonProperty("id")
	public String getId() {
		return id;
	}

	@JsonProperty("id")
	public void setId(String id) {
		this.id = id;
	}

	@JsonProperty("IDMS_Federated_ID__c")
	public String getIDMS_Federated_ID__c() {
		return IDMS_Federated_ID__c;
	}

	@JsonProperty("IDMS_Federated_ID__c")
	public void setIDMS_Federated_ID__c(String iDMS_Federated_ID__c) {
		IDMS_Federated_ID__c = iDMS_Federated_ID__c;
	}

	@JsonProperty("PinCode")
	public String getPinCode() {
		return PinCode;
	}

	@JsonProperty("PinCode")
	public void setPinCode(String pinCode) {
		PinCode = pinCode;
	}

	@JsonProperty("Operation")
	public String getOperation() {
		return Operation;
	}

	@JsonProperty("Operation")
	public void setOperation(String operation) {
		Operation = operation;
	}

	@JsonProperty("Password")
	public String getPassword() {
		return Password;
	}

	@JsonProperty("Password")
	public void setPassword(String password) {
		Password = password;
	}
	
	@JsonProperty("IDMS_Profile_update_source")
	public String getIDMS_Profile_update_source() {
		return IDMS_Profile_update_source;
	}

	@JsonProperty("IDMS_Profile_update_source")
	public void setIDMS_Profile_update_source(String iDMS_Profile_update_source) {
		IDMS_Profile_update_source = iDMS_Profile_update_source;
	}

	@JsonProperty("IDMS_Email_opt_in__c")
	public String getIDMS_Email_opt_in__c() {
		return IDMS_Email_opt_in__c;
	}

	@JsonProperty("IDMS_Email_opt_in__c")
	public void setIDMS_Email_opt_in__c(String iDMS_Email_opt_in__c) {
		IDMS_Email_opt_in__c = iDMS_Email_opt_in__c;
	}

	@JsonProperty("tncFlag")
	public String getTncFlag() {
		return tncFlag;
	}

	@JsonProperty("tncFlag")
	public void setTncFlag(String tncFlag) {
		this.tncFlag = tncFlag;
	}

	
	
	@JsonProperty("UIFlag")
	public String getUIFlag() {
		return UIFlag;
	}

	@JsonProperty("UIFlag")
	public void setUIFlag(String uIFlag) {
		UIFlag = uIFlag;
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

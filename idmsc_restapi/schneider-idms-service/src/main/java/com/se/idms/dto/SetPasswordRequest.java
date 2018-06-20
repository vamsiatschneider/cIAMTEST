package com.se.idms.dto;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The SetPasswordRequest class.
 * 
 * @author Pujarani Panda
 *
 */
public class SetPasswordRequest {

	@JsonProperty
	private String Id;
	
	@JsonProperty
	private String FederationIdentifier;
	
	@JsonProperty
	private String IDMS_Federated_ID__c;
	
	@JsonProperty
	private  String NewPwd;
	
	@JsonProperty
	private String IDMS_Profile_update_source;
	
	@JsonProperty
	private  String Token;
	
	@JsonProperty
	private String UIFlag;

	@JsonProperty("Id")
	public String getId() {
		return Id;
	}

	@JsonProperty("Id")
	public void setId(String id) {
		Id = id;
	}

	@JsonProperty("IDMS_Federated_ID__c")
	public String getIDMS_Federated_ID__c() {
		return IDMS_Federated_ID__c;
	}

	@JsonProperty("IDMS_Federated_ID__c")
	public void setIDMS_Federated_ID__c(String iDMS_Federated_ID__c) {
		IDMS_Federated_ID__c = iDMS_Federated_ID__c;
	}

	@JsonProperty("NewPwd")
	public String getNewPwd() {
		return NewPwd;
	}

	@JsonProperty("NewPwd")
	public void setNewPwd(String newPwd) {
		NewPwd = newPwd;
	}

	@JsonProperty("IDMS_Profile_update_source")
	public String getIDMS_Profile_update_source() {
		return IDMS_Profile_update_source;
	}

	@JsonProperty("IDMS_Profile_update_source")
	public void setIDMS_Profile_update_source(String iDMS_Profile_update_source) {
		IDMS_Profile_update_source = iDMS_Profile_update_source;
	}

	@JsonProperty("Token")
	public String getToken() {
		return Token;
	}

	@JsonProperty("Token")
	public void setToken(String token) {
		Token = token;
	}

	@JsonProperty("FederationIdentifier")
	public String getFederationIdentifier() {
		return FederationIdentifier;
	}

	@JsonProperty("FederationIdentifier")
	public void setFederationIdentifier(String federationIdentifier) {
		FederationIdentifier = federationIdentifier;
	}

	@JsonProperty("UIFlag")
	public String getUIFlag() {
		return UIFlag;
	}

	@JsonProperty("UIFlag")
	public void setUIFlag(String uIFlag) {
		UIFlag = uIFlag;
	}
	
	
}

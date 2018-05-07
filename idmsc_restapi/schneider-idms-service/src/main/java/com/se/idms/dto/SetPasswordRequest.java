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

	public String getNewPwd() {
		return NewPwd;
	}

	public void setNewPwd(String newPwd) {
		NewPwd = newPwd;
	}

	public String getIDMS_Profile_update_source() {
		return IDMS_Profile_update_source;
	}

	public void setIDMS_Profile_update_source(String iDMS_Profile_update_source) {
		IDMS_Profile_update_source = iDMS_Profile_update_source;
	}

	public String getToken() {
		return Token;
	}

	public void setToken(String token) {
		Token = token;
	}

	public String getFederationIdentifier() {
		return FederationIdentifier;
	}

	public void setFederationIdentifier(String federationIdentifier) {
		FederationIdentifier = federationIdentifier;
	}

	public String getUIFlag() {
		return UIFlag;
	}

	public void setUIFlag(String uIFlag) {
		UIFlag = uIFlag;
	}
	
	
}

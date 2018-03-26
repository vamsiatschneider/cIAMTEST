package com.se.idms.dto;

import javax.inject.Inject;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The AILResponse class.
 * @author Pujarani Panda
 *
 */

public class AILResponse {
	
	@JsonProperty("Status")
	private String status;
	
	@JsonProperty("Message")
	private String message;
	
	@JsonProperty("IDMSUserAIL")
	@Inject
	private IDMSUserAIL  idmsUserAil;
	
	public AILResponse(){
		
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

	public IDMSUserAIL getIdmsUserAil() {
		return idmsUserAil;
	}

	public void setIdmsUserAil(IDMSUserAIL idmsUserAil) {
		this.idmsUserAil = idmsUserAil;
	}

	public AILResponse(IDMSUserAIL idmsUserAil){
		this.idmsUserAil=idmsUserAil;
	}

	/*private String IDMSAil__c;
	

	private String IDMSAclType__c;
	
	private String Operation;
	

	public String getOperation() {
		return Operation;
	}

	public void setOperation(String operation) {
		Operation = operation;
	}

	private String IDMSAIL_Applications__c;
	

	private String IDMSAIL_Programs__c;
	

	private String IDMSAIL_Features__c;

	public String getIDMSAil__c() {
		return IDMSAil__c;
	}

	public void setIDMSAil__c(String iDMSAil__c) {
		IDMSAil__c = iDMSAil__c;
	}
	

	public String getIDMSAclType__c() {
		return IDMSAclType__c;
	}

	public void setIDMSAclType__c(String iDMSAclType__c) {
		IDMSAclType__c = iDMSAclType__c;
	}
*/
	

}

/**
 * 
 */
package com.idms.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author SESA508936
 *
 */
public class GetUserRecordResponse {
	
	private String Id;
	
	private String FirstName;

	private String LastName;

	private String Email;

	private String MobilePhone;
	
	private String IDMSAil__c;
	
	private String IDMSAIL_Applications__c;
	
	private String IDMSAIL_Features__c;
	
	private String IDMSAIL_Programs__c;

	@JsonProperty("Id")
	public String getId() {
		return Id;
	}

	@JsonProperty("Id")
	public void setId(String id) {
		Id = id;
	}

	@JsonProperty("FirstName")
	public String getFirstName() {
		return FirstName;
	}

	@JsonProperty("FirstName")
	public void setFirstName(String firstName) {
		FirstName = firstName;
	}

	@JsonProperty("LastName")
	public String getLastName() {
		return LastName;
	}

	@JsonProperty("LastName")
	public void setLastName(String lastName) {
		LastName = lastName;
	}

	@JsonProperty("Email")
	public String getEmail() {
		return Email;
	}

	@JsonProperty("Email")
	public void setEmail(String email) {
		Email = email;
	}

	@JsonProperty("MobilePhone")
	public String getMobilePhone() {
		return MobilePhone;
	}

	@JsonProperty("MobilePhone")
	public void setMobilePhone(String mobilePhone) {
		MobilePhone = mobilePhone;
	}

	@JsonProperty("IDMSAil__c")
	public String getIDMSAil__c() {
		return IDMSAil__c;
	}

	@JsonProperty("IDMSAil__c")
	public void setIDMSAil__c(String iDMSAil__c) {
		IDMSAil__c = iDMSAil__c;
	}

	@JsonProperty("IDMSAIL_Applications__c")
	public String getIDMSAIL_Applications__c() {
		return IDMSAIL_Applications__c;
	}

	@JsonProperty("IDMSAIL_Applications__c")
	public void setIDMSAIL_Applications__c(String iDMSAIL_Applications__c) {
		IDMSAIL_Applications__c = iDMSAIL_Applications__c;
	}

	@JsonProperty("IDMSAIL_Features__c")
	public String getIDMSAIL_Features__c() {
		return IDMSAIL_Features__c;
	}

	@JsonProperty("IDMSAIL_Features__c")
	public void setIDMSAIL_Features__c(String iDMSAIL_Features__c) {
		IDMSAIL_Features__c = iDMSAIL_Features__c;
	}

	@JsonProperty("IDMSAIL_Programs__c")
	public String getIDMSAIL_Programs__c() {
		return IDMSAIL_Programs__c;
	}

	@JsonProperty("IDMSAIL_Programs__c")
	public void setIDMSAIL_Programs__c(String iDMSAIL_Programs__c) {
		IDMSAIL_Programs__c = iDMSAIL_Programs__c;
	}

}

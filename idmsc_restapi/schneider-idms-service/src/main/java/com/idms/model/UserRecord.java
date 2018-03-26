package com.idms.model;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The UserRecord class.
 * @author Aravindh Kumar
 *
 */
public class UserRecord {

	@JsonProperty("Email")
	private String Email;
	
	@JsonProperty("MobilePhone")
	private String MobilePhone;
	
	@JsonProperty("IDMS_Profile_update_source__c")
	private String IDMS_Profile_update_source__c;
	
	public String getEmail() {
		return Email;
	}

	public void setEmail(String email) {
		Email = email;
	}

	public String getMobilePhone() {
		return MobilePhone;
	}

	public void setMobilePhone(String mobilePhone) {
		MobilePhone = mobilePhone;
	}

	public String getIDMS_Profile_update_source__c() {
		return IDMS_Profile_update_source__c;
	}

	public void setIDMS_Profile_update_source__c(String iDMS_Profile_update_source__c) {
		IDMS_Profile_update_source__c = iDMS_Profile_update_source__c;
	}
	
	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}
}

package com.se.idms.dto;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.idms.product.model.Attributes;

/**
 * The IDMSUserRecord class.
 * 
 * @author Aravindh Kumar
 *
 */
public class IDMSUserRecord {

	@JsonInclude
	@JsonProperty("attributes")
	private Attributes attributes;

	@JsonProperty("Id")
	private String id;
	
	@JsonProperty("Name")
	private String name;
	
	@JsonProperty("IDMS_PreferredLanguage__c")
	private String idmsPreferredLanguage;
	
	@JsonProperty("IDMS_Profile_update_source__c")
	private String idmsProfileUpdateSource;
	
	@JsonProperty("IDMS_Registration_Source__c")
	private String idmsRegistrationSource;
	
	@JsonProperty("Username")
	private String username;
	
	@JsonProperty("IDMSIdentityType__c")
	private String idmsIdentityType;
	
	@JsonProperty("Email")
	private String email;
	
	@JsonProperty("MobilePhone")
	private String mobilePhone;

	public Attributes getAttributes() {
		return attributes;
	}

	public void setAttributes(Attributes attributes) {
		this.attributes = attributes;
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getIdmsPreferredLanguage() {
		return idmsPreferredLanguage;
	}

	public void setIdmsPreferredLanguage(String idmsPreferredLanguage) {
		this.idmsPreferredLanguage = idmsPreferredLanguage;
	}

	public String getIdmsProfileUpdateSource() {
		return idmsProfileUpdateSource;
	}

	public void setIdmsProfileUpdateSource(String idmsProfileUpdateSource) {
		this.idmsProfileUpdateSource = idmsProfileUpdateSource;
	}

	public String getIdmsRegistrationSource() {
		return idmsRegistrationSource;
	}

	public void setIdmsRegistrationSource(String idmsRegistrationSource) {
		this.idmsRegistrationSource = idmsRegistrationSource;
	}

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public String getIdmsIdentityType() {
		return idmsIdentityType;
	}

	public void setIdmsIdentityType(String idmsIdentityType) {
		this.idmsIdentityType = idmsIdentityType;
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public String getMobilePhone() {
		return mobilePhone;
	}

	public void setMobilePhone(String mobilePhone) {
		this.mobilePhone = mobilePhone;
	}
	
	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}

}

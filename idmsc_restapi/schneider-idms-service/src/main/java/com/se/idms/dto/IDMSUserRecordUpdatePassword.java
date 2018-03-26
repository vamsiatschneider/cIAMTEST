package com.se.idms.dto;

import javax.xml.bind.annotation.XmlRootElement;

import org.springframework.stereotype.Component;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonRootName;
import com.idms.product.model.Attributes;

/**
 * The IDMSUserRecordUpdatePassword class.
 * 
 * @author Aravindh Kumar
 *
 */
@Component
@XmlRootElement
@JsonRootName("IDMSUserRecord")
public class IDMSUserRecordUpdatePassword {

	@JsonInclude
	@JsonProperty("attributes")
	private Attributes attributes;

	@JsonProperty("Id")
	private String id;
	
	@JsonProperty("IDMS_Federated_ID__c")
	private String IDMS_Federated_ID__c;
	
	@JsonProperty("Username")
	private String userName;

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

	public String getIDMS_Federated_ID__c() {
		return IDMS_Federated_ID__c;
	}

	public void setIDMS_Federated_ID__c(String iDMS_Federated_ID__c) {
		IDMS_Federated_ID__c = iDMS_Federated_ID__c;
	}

	public String getUserName() {
		return userName;
	}

	public void setUserName(String userName) {
		this.userName = userName;
	}
	
}

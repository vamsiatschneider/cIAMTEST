package com.se.idms.dto;

import javax.inject.Inject;
import javax.xml.bind.annotation.XmlRootElement;
import org.springframework.stereotype.Component;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.idms.product.model.Attributes;

/**
 * The IDMSUserAIL class.
 * 
 * @author Pujarani Panda
 *
 */
@Component
@XmlRootElement
public class IDMSUserAIL {

	@JsonInclude
	@JsonProperty("attributes")
	private Attributes attributes;

	@JsonProperty("Id")
	private String id;
	
	@JsonProperty("IDMSUser__r")
	@Inject
	private IDMSUser__r  idmsUser_r;
	
	@JsonProperty("IDMSAcl__c")
	private  String idmsacl__c;

	@JsonProperty("IDMSAclType__c")
	private String idmsaclType__c;

	@JsonProperty("IDMSIsRevokedOperation__c")
	private boolean idmsisRevokedOperation__c;

	@JsonProperty("IDMSOperation__c")
	private String idmsoperation__c;
	
	@JsonProperty("IDMS_Profile_update_source__c")
	private String idms_Profile_update_source__c;

	@JsonProperty("IDMSUser__c")
	private  String idmsuser__c;

	
	public IDMSUserAIL(){
		
	}
	
	public IDMSUserAIL(IDMSUser__r idmsUser__r){
		this.idmsUser_r = idmsUser__r;
	}
	
	

	public IDMSUser__r getIdmsUser_r() {
		return idmsUser_r;
	}

	public void setIdmsUser_r(IDMSUser__r idmsUser_r) {
		this.idmsUser_r = idmsUser_r;
	}

	public String getIdmsacl__c() {
		return idmsacl__c;
	}

	public void setIdmsacl__c(String idmsacl__c) {
		this.idmsacl__c = idmsacl__c;
	}
	public String getIdmsaclType__c() {
		return idmsaclType__c;
	}

	public void setIdmsaclType__c(String idmsaclType__c) {
		this.idmsaclType__c = idmsaclType__c;
	}

	public boolean isIdmsisRevokedOperation__c() {
		return idmsisRevokedOperation__c;
	}

	public void setIdmsisRevokedOperation__c(boolean idmsisRevokedOperation__c) {
		this.idmsisRevokedOperation__c = idmsisRevokedOperation__c;
	}

	public String getIdmsoperation__c() {
		return idmsoperation__c;
	}

	public void setIdmsoperation__c(String idmsoperation__c) {
		this.idmsoperation__c = idmsoperation__c;
	}

	public String getIdms_Profile_update_source__c() {
		return idms_Profile_update_source__c;
	}

	public void setIdms_Profile_update_source__c(String idms_Profile_update_source__c) {
		this.idms_Profile_update_source__c = idms_Profile_update_source__c;
	}

	public String getIdmsuser__c() {
		return idmsuser__c;
	}

	public void setIdmsuser__c(String idmsuser__c) {
		this.idmsuser__c = idmsuser__c;
	}

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


}

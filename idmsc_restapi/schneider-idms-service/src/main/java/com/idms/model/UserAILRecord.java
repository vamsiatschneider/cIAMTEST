package com.idms.model;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The AIL Record
 * 
 * @author Pujarani Panda
 *
 */
public class UserAILRecord {

	@JsonProperty
	private String IDMSUser__c;

	@JsonProperty
	private String IDMSAclType__c;

	@JsonProperty
	private String IDMSAcl__c;

	@JsonProperty
	private String IDMSOperation__c;

	@JsonProperty
	private String IDMS_Profile_update_source__c;

	@JsonProperty
	private String IDMS_Federated_ID__c;

	public String getIDMSUser__c() {
		return IDMSUser__c;
	}

	public void setIDMSUser__c(String iDMSUser__c) {
		IDMSUser__c = iDMSUser__c;
	}

	public String getIDMSAclType__c() {
		return IDMSAclType__c;
	}

	public void setIDMSAclType__c(String iDMSAclType__c) {
		IDMSAclType__c = iDMSAclType__c;
	}

	public String getIDMSAcl__c() {
		return IDMSAcl__c;
	}

	public void setIDMSAcl__c(String iDMSAcl__c) {
		IDMSAcl__c = iDMSAcl__c;
	}

	public String getIDMSOperation__c() {
		return IDMSOperation__c;
	}

	public void setIDMSOperation__c(String iDMSOperation__c) {
		IDMSOperation__c = iDMSOperation__c;
	}

	public String getIDMS_Profile_update_source__c() {
		return IDMS_Profile_update_source__c;
	}

	public void setIDMS_Profile_update_source__c(String iDMS_Profile_update_source__c) {
		IDMS_Profile_update_source__c = iDMS_Profile_update_source__c;
	}

	public String getIDMS_Federated_ID__c() {
		return IDMS_Federated_ID__c;
	}

	public void setIDMS_Federated_ID__c(String iDMS_Federated_ID__c) {
		IDMS_Federated_ID__c = iDMS_Federated_ID__c;
	}

	// 1277: Updating proper response
	public String getAILvalue(String ailType, String ailValue) {
		String[] acl = ailValue.split(",");
		String ail = "";
		for (int i = 0; i < acl.length; i++) {
			if (ail.isEmpty()) {
				ail = "(" + ailType + ";" + acl[i] + ")";
			} else {
				ail = ail + ",(" + ailType + ";" + acl[i] + ")";
			}
		}
		return ail;
	}
	
	
	
	public String revokeAIL(String[] ailParts, String revokeVal) {
		String ail = "";
		for (String pair : ailParts) {
			if (!pair.equalsIgnoreCase(revokeVal)) {
				ail = ail + pair + ",";
			}
		}
		return ail;
	}
	public String revoke(String IDMSAil__c, String revokeVal) {	
		String[] revokeAIL=revokeVal.split(",");
		for (String revokepair : revokeAIL) {
		IDMSAil__c = revokeAIL(IDMSAil__c.split(","),revokepair);
		}
		return IDMSAil__c;
	}
}

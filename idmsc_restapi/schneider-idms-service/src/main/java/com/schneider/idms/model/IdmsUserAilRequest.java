/**
 * 
 */
package com.schneider.idms.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author SESA508936
 * For Direct Call API
 */
public class IdmsUserAilRequest {

	@JsonProperty
	private  String aclType;
	
	@JsonProperty
	private String acl;
	
	@JsonProperty
	private String operation;
	
	@JsonProperty
	private String federatedId;

	@JsonProperty
	private String profileLastUpdateSource;

	public String getAclType() {
		return aclType;
	}

	public void setAclType(String aclType) {
		this.aclType = aclType;
	}

	public String getAcl() {
		return acl;
	}

	public void setAcl(String acl) {
		this.acl = acl;
	}

	public String getOperation() {
		return operation;
	}

	public void setOperation(String operation) {
		this.operation = operation;
	}

	public String getProfileLastUpdateSource() {
		return profileLastUpdateSource;
	}

	public void setProfileLastUpdateSource(String profileLastUpdateSource) {
		this.profileLastUpdateSource = profileLastUpdateSource;
	}

	public String getFederatedId() {
		return federatedId;
	}

	public void setFederatedId(String federatedId) {
		this.federatedId = federatedId;
	}
	
}

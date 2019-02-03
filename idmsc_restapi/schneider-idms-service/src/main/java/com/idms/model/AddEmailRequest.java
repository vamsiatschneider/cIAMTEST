/**
 * 
 */
package com.idms.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

/**
 * @author SESA508936
 *
 */
public class AddEmailRequest {
	
	@JsonInclude(Include.NON_NULL)
	private String  email;
	
	@JsonInclude(Include.NON_NULL)
	private String  fedId;
	
	@JsonInclude(Include.NON_NULL)
	private String  profileUpdateSource;
	
	@JsonInclude(Include.NON_NULL)
	private String  operationType;
	
	@JsonInclude(Include.NON_NULL)
	private String  accesstoken;

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public String getFedId() {
		return fedId;
	}

	public void setFedId(String fedId) {
		this.fedId = fedId;
	}

	public String getProfileUpdateSource() {
		return profileUpdateSource;
	}

	public void setProfileUpdateSource(String profileUpdateSource) {
		this.profileUpdateSource = profileUpdateSource;
	}

	public String getOperationType() {
		return operationType;
	}

	public void setOperationType(String operationType) {
		this.operationType = operationType;
	}

	public String getAccesstoken() {
		return accesstoken;
	}

	public void setAccesstoken(String accesstoken) {
		this.accesstoken = accesstoken;
	}

}

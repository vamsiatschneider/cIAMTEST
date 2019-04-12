/**
 * 
 */
package com.idms.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author SESA508936
 *
 */
public class UserDetailByApplicationRequest {
	
	@JsonProperty
	private String appHash;

	@JsonProperty
	private String email;

	@JsonProperty
	private String mobile;

	@JsonProperty
	private String idmsFederatedId;

	public String getAppHash() {
		return appHash;
	}

	public void setAppHash(String appHash) {
		this.appHash = appHash;
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public String getMobile() {
		return mobile;
	}

	public void setMobile(String mobile) {
		this.mobile = mobile;
	}

	public String getIdmsFederatedId() {
		return idmsFederatedId;
	}

	public void setIdmsFederatedId(String idmsFederatedId) {
		this.idmsFederatedId = idmsFederatedId;
	}

}

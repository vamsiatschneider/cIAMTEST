package com.idms.model;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonProperty;

public class CheckUserExistsRequestDemo {

	@JsonProperty
	private String email;

	@JsonProperty
	private String mobile;
	
	@JsonProperty
	private String idmsFederatedId;

	@JsonProperty
	private String loginID;

	@JsonProperty
	private String withGlobalUsers;

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

	public String getLoginID() {
		return loginID;
	}

	public void setLoginID(String loginID) {
		this.loginID = loginID;
	}

	public String getWithGlobalUsers() {
		return withGlobalUsers;
	}

	public void setWithGlobalUsers(String withGlobalUsers) {
		this.withGlobalUsers = withGlobalUsers;
	}

	public String getIdmsFederatedId() {
		return idmsFederatedId;
	}

	public void setIdmsFederatedId(String idmsFederatedId) {
		this.idmsFederatedId = idmsFederatedId;
	}

	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}
}

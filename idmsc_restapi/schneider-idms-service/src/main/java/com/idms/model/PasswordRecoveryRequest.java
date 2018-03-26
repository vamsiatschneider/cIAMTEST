package com.idms.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The Password Recovery Request.
 * @author Aravindh Kumar
 *
 */
public class PasswordRecoveryRequest  extends BaseEntity {

	@JsonProperty
	private UserRecord UserRecord;

	@JsonProperty
	private String WithGlobalUsers;
	
	public UserRecord getUserRecord() {
		return UserRecord;
	}

	public void setUserRecord(UserRecord userRecord) {
		this.UserRecord = userRecord;
	}

	public String getWithGlobalUsers() {
		return WithGlobalUsers;
	}

	public void setWithGlobalUsers(String withGlobalUsers) {
		WithGlobalUsers = withGlobalUsers;
	}
	
	
}

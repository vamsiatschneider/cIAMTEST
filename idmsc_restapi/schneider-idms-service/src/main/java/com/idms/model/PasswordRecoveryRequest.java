package com.idms.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

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
	
	@JsonInclude(Include.NON_NULL)
	private String pathValue;
	
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

	public String getPathValue() {
		return pathValue;
	}

	public void setPathValue(String pathValue) {
		this.pathValue = pathValue;
	}
}

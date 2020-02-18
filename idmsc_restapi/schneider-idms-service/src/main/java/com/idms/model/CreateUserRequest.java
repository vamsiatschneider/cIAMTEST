package com.idms.model;

import java.util.List;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class CreateUserRequest {

	@JsonProperty
	private IFWUser UserRecord;

	@JsonProperty
	private String Password;

	@JsonProperty
	private String UIFlag;
	
	@JsonProperty
	private String mobileRegFlag;
	
	@JsonInclude(Include.NON_NULL)
	private String pathValue;

	@JsonProperty
	private List<RegistrationAttributes> attributes;

	@JsonProperty("UserRecord")
	public IFWUser getUserRecord() {
		return UserRecord;
	}

	@JsonProperty("UserRecord")
	public void setUserRecord(IFWUser userRecord) {
		UserRecord = userRecord;
	}

	@JsonProperty("Password")
	public String getPassword() {
		return Password;
	}

	@JsonProperty("Password")
	public void setPassword(String password) {
		Password = password;
	}

	@JsonProperty("UIFlag")
	public String getUIFlag() {
		return UIFlag;
	}

	@JsonProperty("UIFlag")
	public void setUIFlag(String uIFlag) {
		UIFlag = uIFlag;
	}

	public List<RegistrationAttributes> getAttributes() {
		return attributes;
	}

	public void setAttributes(List<RegistrationAttributes> attributes) {
		this.attributes = attributes;
	}

	public String getMobileRegFlag() {
		return mobileRegFlag;
	}

	public void setMobileRegFlag(String mobileRegFlag) {
		this.mobileRegFlag = mobileRegFlag;
	}

	public String getPathValue() {
		return pathValue;
	}

	public void setPathValue(String pathValue) {
		this.pathValue = pathValue;
	}

	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}
}

package com.idms.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class MFARequest {
	
	@JsonInclude(Include.NON_NULL)
	private String is2FAEnabled;
	@JsonProperty
	private String isFirstTimeUser;
	@JsonProperty
	private String UIFlag;
	

	public String getUIFlag() {
		return UIFlag;
	}
	public void setUIFlag(String uIFlag) {
		UIFlag = uIFlag;
	}
	public String getIsFirstTimeUser() {
		return isFirstTimeUser;
	}
	public void setIsFirstTimeUser(String isFirstTimeUser) {
		this.isFirstTimeUser = isFirstTimeUser;
	}
	public String getIs2FAEnabled() {
		return is2FAEnabled;
	}
	public void setIs2FAEnabled(String is2faEnabled) {
		is2FAEnabled = is2faEnabled;
	}

	


}

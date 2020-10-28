package com.idms.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class MFARequest {
	
	@JsonInclude(Include.NON_NULL)
	private String is2FAEnabled;
	private String isFirstTimeUser;
	
	
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

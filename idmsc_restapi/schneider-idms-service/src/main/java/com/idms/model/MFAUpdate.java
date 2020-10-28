package com.idms.model;

public class MFAUpdate {
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

/**
 * 
 */
package com.idms.model;

/**
 * User MFA Device Info 
 * @author Santosh Kumar
 *
 */
public class UserMFADataRequest {

	private String authId;
	private String stageData;
	
	/**
	 * stageName can be any of two
	 * deviceStage or OTPStage
	 */
	private String stageName;
	private String loginUser;
	private String appName;
	
	public String getAuthId() {
		return authId;
	}
	public void setAuthId(String authId) {
		this.authId = authId;
	}
	public String getStageData() {
		return stageData;
	}
	public void setStageData(String stageData) {
		this.stageData = stageData;
	}
	public String getStageName() {
		return stageName;
	}
	public void setStageName(String stageName) {
		this.stageName = stageName;
	}
	public String getLoginUser() {
		return loginUser;
	}
	public void setLoginUser(String loginUser) {
		this.loginUser = loginUser;
	}
	public String getAppName() {
		return appName;
	}
	public void setAppName(String appName) {
		this.appName = appName;
	}

}

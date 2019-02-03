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
public class AddMobileRequest {
	
	@JsonInclude(Include.NON_NULL)
	private String  mobile;
	
	@JsonInclude(Include.NON_NULL)
	private String  fedId;
	
	@JsonInclude(Include.NON_NULL)
	private String  profileUpdateSource;
	
	@JsonInclude(Include.NON_NULL)
	private String  accesstoken;

	public String getMobile() {
		return mobile;
	}

	public void setMobile(String mobile) {
		this.mobile = mobile;
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

	public String getAccesstoken() {
		return accesstoken;
	}

	public void setAccesstoken(String accesstoken) {
		this.accesstoken = accesstoken;
	}
	
}

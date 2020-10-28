/**
 * 
 */
package com.idms.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

/**
 * @author SESA508936 (Santosh Kumar)
 *
 */
public class Send2FAOTPRequest {
	
	@JsonInclude(Include.NON_NULL)
	private String  userid;

	public String getUserid() {
		return userid;
	}

	public void setUserid(String userid) {
		this.userid = userid;
	}
	
}

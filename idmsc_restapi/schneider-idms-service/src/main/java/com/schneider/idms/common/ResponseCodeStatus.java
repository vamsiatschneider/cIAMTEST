/**
 * 
 */
package com.schneider.idms.common;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

/**
 * @author SESA508936
 *
 */
public class ResponseCodeStatus extends ErrorResponseCode {
	
	@JsonInclude(Include.NON_NULL)
	private String status;

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}
	
}

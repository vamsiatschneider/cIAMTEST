/**
 * 
 */
package com.idms.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author santosh kumar (SESA508936)
 *
 */
public class GetUserByApplicationResponse {
	
	private String status;

	private String message;

	private GetUserRecordResponse IDMSUserRecord;

	@JsonProperty("Status")
	public String getStatus() {
		return status;
	}

	@JsonProperty("Status")
	public void setStatus(String status) {
		this.status = status;
	}

	@JsonProperty("Message")
	public String getMessage() {
		return message;
	}

	@JsonProperty("Message")
	public void setMessage(String message) {
		this.message = message;
	}

	@JsonProperty("IDMSUserRecord")
	public GetUserRecordResponse getIDMSUserRecord() {
		return IDMSUserRecord;
	}

	@JsonProperty("IDMSUserRecord")
	public void setIDMSUserRecord(GetUserRecordResponse iDMSUserRecord) {
		IDMSUserRecord = iDMSUserRecord;
	}

}

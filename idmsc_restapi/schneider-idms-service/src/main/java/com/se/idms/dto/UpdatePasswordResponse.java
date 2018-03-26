package com.se.idms.dto;

import javax.inject.Inject;
import javax.xml.bind.annotation.XmlRootElement;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The UpdatePasswordResponse class.
 * @author Pujarani Panda
 *
 */

@XmlRootElement
public class UpdatePasswordResponse {

	@JsonProperty("Status")
	private String status;
	
	@JsonProperty("Message")
	private String message;
	
	@JsonProperty("IDMSUserRecord")
	@Inject
	private IDMSUserRecordUpdatePassword idmsUserRecord;
	

	public UpdatePasswordResponse(){
		
	}
	
	public UpdatePasswordResponse(IDMSUserRecordUpdatePassword idmsUserRecord){
		this.idmsUserRecord = idmsUserRecord;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public IDMSUserRecordUpdatePassword getIdmsUserRecord() {
		return idmsUserRecord;
	}

	public void setIdmsUserRecord(IDMSUserRecordUpdatePassword idmsUserRecord) {
		this.idmsUserRecord = idmsUserRecord;
	}

	
	
}

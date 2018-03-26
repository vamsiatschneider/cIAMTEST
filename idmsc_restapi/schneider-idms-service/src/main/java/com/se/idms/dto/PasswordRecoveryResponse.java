package com.se.idms.dto;

import javax.inject.Inject;
import javax.xml.bind.annotation.XmlRootElement;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The PasswordRecoveryResponse class.
 * @author Aravindh Kumar
 *
 */
@XmlRootElement
public class PasswordRecoveryResponse {

	@JsonProperty("Status")
	private String status;
	
	@JsonProperty("Message")
	private String message;
	
	@JsonProperty("IDMSUserRecord")
	@Inject
	private IDMSUserRecord idmsUserRecord;
	
	public PasswordRecoveryResponse(){
		
	}
	
	public PasswordRecoveryResponse(IDMSUserRecord idmsUserRecord){
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

	public IDMSUserRecord getIdmsUserRecord() {
		return idmsUserRecord;
	}

	public void setIdmsUserRecord(IDMSUserRecord idmsUserRecord) {
		this.idmsUserRecord = idmsUserRecord;
	}

	
	
}

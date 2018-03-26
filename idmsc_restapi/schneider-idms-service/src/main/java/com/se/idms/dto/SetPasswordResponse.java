package com.se.idms.dto;

import javax.inject.Inject;
import javax.xml.bind.annotation.XmlRootElement;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The SetPasswordResponse class.
 * @author Pujarani Panda
 *
 */
@XmlRootElement
public class SetPasswordResponse {
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

	@JsonProperty("Status")
	private String status;
	
	@JsonProperty("Message")
	private String message;
	
	@JsonProperty("IDMSUserRecord")
	@Inject
	private IDMSUserRecord idmsUserRecord;
	
	public SetPasswordResponse(){
		
	}
	public SetPasswordResponse(IDMSUserRecord idmsUserRecord) {
		this.idmsUserRecord=idmsUserRecord;
	}
}

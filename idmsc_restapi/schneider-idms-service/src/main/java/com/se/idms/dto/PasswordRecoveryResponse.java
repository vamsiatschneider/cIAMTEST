package com.se.idms.dto;

import javax.inject.Inject;
import javax.xml.bind.annotation.XmlRootElement;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

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
	@JsonInclude(Include.NON_NULL)
	@Inject
	private IDMSUserRecord idmsUserRecord;
	
	@JsonProperty("CounterResponse")
	@JsonInclude(Include.NON_NULL)
	@Inject
	private CounterResponse counterResponse;
	
	
	public CounterResponse getCounterResponse() {
		return counterResponse;
	}

	public void setCounterResponse(CounterResponse counterResponse) {
		this.counterResponse = counterResponse;
	}

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

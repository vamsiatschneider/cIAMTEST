package com.idms.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonInclude(Include.NON_NULL) 
public class BulkAILResponse {
	
	@JsonProperty
	private String userFedID;

	@JsonProperty
	private List<AILResponse> response;
	
	@JsonProperty
	private String code;
	
	@JsonProperty
	private String message;

	public String getUserFedID() {
		return userFedID;
	}

	public void setUserFedID(String userFedID) {
		this.userFedID = userFedID;
	}

	public List<AILResponse> getResponse() {
		return response;
	}

	public void setResponse(List<AILResponse> response) {
		this.response = response;
	}

	public String getCode() {
		return code;
	}

	public void setCode(String code) {
		this.code = code;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}
	
	
}

package com.idms.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;

@JsonInclude(Include.NON_NULL) 
public class BulkAILResponse {
	
	@JsonProperty
	private List<BulkAILRecord> userAils;

	@JsonProperty
	private String profileLastUpdateSource;
	
	@JsonProperty
	private String code;
	
	@JsonProperty
	private String message;

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

	public String getProfileLastUpdateSource() {
		return profileLastUpdateSource;
	}

	public void setProfileLastUpdateSource(String profileLastUpdateSource) {
		this.profileLastUpdateSource = profileLastUpdateSource;
	}

	public List<BulkAILRecord> getUserAils() {
		return userAils;
	}

	public void setUserAils(List<BulkAILRecord> userAils) {
		this.userAils = userAils;
	}
	
	
}

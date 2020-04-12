package com.idms.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

public class BulkAILRequest {
	
	@JsonProperty
	private List<BulkAILRecord> userAils;
	
	@JsonProperty
	private String profileLastUpdateSource;

	public List<BulkAILRecord> getUserAils() {
		return userAils;
	}

	public void setUserAils(List<BulkAILRecord> userAils) {
		this.userAils = userAils;
	}

	public String getProfileLastUpdateSource() {
		return profileLastUpdateSource;
	}

	public void setProfileLastUpdateSource(String profileLastUpdateSource) {
		this.profileLastUpdateSource = profileLastUpdateSource;
	}

}

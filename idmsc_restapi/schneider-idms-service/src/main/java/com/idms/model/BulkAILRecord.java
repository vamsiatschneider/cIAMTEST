package com.idms.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

public class BulkAILRecord {

	@JsonProperty
	private String userFedID;

	@JsonProperty
	private List<AILRecord> ails;

	public String getUserFedID() {
		return userFedID;
	}

	public void setUserFedID(String userFedID) {
		this.userFedID = userFedID;
	}

	public List<AILRecord> getAils() {
		return ails;
	}

	public void setAils(List<AILRecord> ails) {
		this.ails = ails;
	}

}

package com.idms.service.bulkail.util;

import com.fasterxml.jackson.annotation.JsonProperty;

public class AILMasterRecord {

	@JsonProperty(value = "_id")
	private String id;

	@JsonProperty(value = "_AILType__c")
	private String ailType;

	@JsonProperty(value = "_AILValue__c")
	private String ailValue;

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getAilType() {
		return ailType;
	}

	public void setAilType(String ailType) {
		this.ailType = ailType;
	}

	public String getAilValue() {
		return ailValue;
	}

	public void setAilValue(String ailValue) {
		this.ailValue = ailValue;
	}
}

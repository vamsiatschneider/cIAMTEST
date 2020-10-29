package com.idms.model;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;

public class DeviceProfileRequest {

	@JsonProperty
	private JsonNode devicePrint;
	
	@JsonProperty
	private String uiFlag;

	@JsonProperty("UIFlag")
	public String getUiFlag() {
		return uiFlag;
	}

	@JsonProperty("UIFlag")
	public void setUiFlag(String uiFlag) {
		this.uiFlag = uiFlag;
	}

	public JsonNode getDevicePrint() {
		return devicePrint;
	}

	public void setDevicePrint(JsonNode devicePrint) {
		this.devicePrint = devicePrint;
	}
}

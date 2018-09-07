package com.idms.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * 
 * "attributes" : [
        {
                        "keyName": "brand",
                        "keyValue" : "clipsal"
        },
        {
                        "keyName": "brand1",
                        "keyValue" : "clipsal1"
        }
	]
 */

public class RegistrationAttributes {

	@JsonProperty
	private String keyName;
	@JsonProperty
	private String keyValue;
	
	public String getKeyName() {
		return keyName;
	}
	
	public void setKeyName(String keyName) {
		this.keyName = keyName;
	}
	
	public String getKeyValue() {
		return keyValue;
	}
	
	public void setKeyValue(String keyValue) {
		this.keyValue = keyValue;
	}
	
}

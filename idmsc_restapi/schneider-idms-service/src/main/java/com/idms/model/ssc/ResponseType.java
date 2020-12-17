package com.idms.model.ssc;

import java.util.Arrays;

public enum ResponseType {
	CODE("code"),
	TOKEN("token"),
	ID_TOKEN("id_token"),
	CODE_TOKEN("code token"),
	TOKEN_ID_TOKEN("token id_token"),
	CODE_ID_TOKEN("code id_token"),
	CODE_TOKEN_ID_TOKEN("code token id_token"),
	DEVICE_CODE("device_code"),
	DEVICE_CODE_ID_TOKEN("device_code id_token"),
	INVALID("Invalid");
	
	private String type;
	
	ResponseType(String type) {
		this.type = type;
	}
	
	public String getType() {
		return type;
	}
	
	public static ResponseType getKey(String type) {
		return Arrays.stream(ResponseType.values()).
				filter(e -> e.type.equalsIgnoreCase(type)).
				findFirst().orElse(ResponseType.INVALID);
	}
	
}

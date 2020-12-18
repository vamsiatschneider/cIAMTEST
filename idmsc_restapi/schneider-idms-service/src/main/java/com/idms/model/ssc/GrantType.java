package com.idms.model.ssc;

import java.util.Arrays;

public enum GrantType {

	AUTHORIZATION_CODE("Authorization Code"),
	IMPLICIT("Implicit"),
    ROPC("Resource Owner Password Credentials"),
    CLIENT_CREDENTIALS("Client Credentials"),
    REFRESH_TOKEN("Refresh Token"),
    UMA("UMA"),
    DEVICE_CODE("Device Code"),
    SAML2("SAML2"),
    JWT_BEARER("JWT Bearer"),
    INVALID("Invalid");
	
	private String type;
	
	GrantType(String type) {
		this.type = type;
	}
	
	public String getType() {
		return type;
	}
    
	public static GrantType getKey(String type) {
		return Arrays.stream(GrantType.values()).
				filter(e -> e.type.equalsIgnoreCase(type)).
				findFirst().orElse(GrantType.INVALID);
	}
}

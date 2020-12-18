package com.idms.model.ssc;

import java.util.Arrays;

public enum TokenEndpointAuthMethod {

	CLIENT_SECRET_POST("client_secret_post"),
	CLIENT_SECRET_BASIC("client_secret_basic"),
	PRIVATE_KEY_JWT("private_key_jwt"),
	TLS_CLIENT_AUTH("tls_client_auth"),
	SELF_SIGNED_TLS_CLIENT_AUTH("self_signed_tls_client_auth"),
	NONE("none"),
    INVALID("Invalid");
	
	private String method;
	
	TokenEndpointAuthMethod(String method) {
		this.method = method;
	}
	
	public String getMethod() {
		return method;
	}
	
	public static TokenEndpointAuthMethod getKey(String method) {
		return Arrays.stream(TokenEndpointAuthMethod.values()).
				filter(e -> e.method.equalsIgnoreCase(method)).
				findFirst().orElse(TokenEndpointAuthMethod.INVALID);
	}
	
}

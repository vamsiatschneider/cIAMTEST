package com.idms.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

public class OAuth2ClientRequest {

	@JsonProperty
	private String clientId;
	
	@JsonProperty
	private String clientSecret;
	
	@JsonProperty
	private List<String> scopes;
	
	@JsonProperty
	private List<String> redirectionUris;
	
	@JsonProperty
	private List<String> defaultScopes;
	
	@JsonProperty
	private List<String> responseTypes;
	
	@JsonProperty
	private List<String> grantTypes;
	
	@JsonProperty
	private List<String> tokenEndpointAuthMethod;
	
	@JsonProperty
	private boolean isConsentImplied;
	
	@JsonProperty
	private boolean isOnboardingCall;
	
	public String getClientId() {
		return clientId;
	}

	public void setClientId(String clientId) {
		this.clientId = clientId;
	}

	public String getClientSecret() {
		return clientSecret;
	}

	public void setClientSecret(String clientSecret) {
		this.clientSecret = clientSecret;
	}

	public List<String> getScopes() {
		return scopes;
	}

	public void setScopes(List<String> scopes) {
		this.scopes = scopes;
	}

	public List<String> getRedirectionUris() {
		return redirectionUris;
	}

	public void setRedirectionUris(List<String> redirectionUris) {
		this.redirectionUris = redirectionUris;
	}

	public List<String> getDefaultScopes() {
		return defaultScopes;
	}

	public void setDefaultScopes(List<String> defaultScopes) {
		this.defaultScopes = defaultScopes;
	}

	public List<String> getTokenEndpointAuthMethod() {
		return tokenEndpointAuthMethod;
	}

	public void setTokenEndpointAuthMethod(List<String> tokenEndpointAuthMethod) {
		this.tokenEndpointAuthMethod = tokenEndpointAuthMethod;
	}

	public boolean isConsentImplied() {
		return isConsentImplied;
	}

	public void setConsentImplied(boolean isConsentImplied) {
		this.isConsentImplied = isConsentImplied;
	}

	public List<String> getResponseTypes() {
		return responseTypes;
	}

	public void setResponseTypes(List<String> responseTypes) {
		this.responseTypes = responseTypes;
	}

	public List<String> getGrantTypes() {
		return grantTypes;
	}

	public void setGrantTypes(List<String> grantTypes) {
		this.grantTypes = grantTypes;
	}

	public boolean isOnboardingCall() {
		return isOnboardingCall;
	}

	public void setOnboardingCall(boolean isOnboardingCall) {
		this.isOnboardingCall = isOnboardingCall;
	}
	
}

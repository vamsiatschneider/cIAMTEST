package com.idms.product.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class CoreOAuth2ClientConfig {

	@JsonInclude(Include.NON_NULL)
	private String userpassword;
	
	@JsonInclude(Include.NON_NULL)
	private List<String> scopes;
	
	@JsonInclude(Include.NON_NULL)
	private List<String> redirectionUris;
	
	@JsonInclude(Include.NON_NULL)
	private List<String> defaultScopes;
	
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

	public String getUserpassword() {
		return userpassword;
	}

	public void setUserpassword(String userpassword) {
		this.userpassword = userpassword;
	}
}

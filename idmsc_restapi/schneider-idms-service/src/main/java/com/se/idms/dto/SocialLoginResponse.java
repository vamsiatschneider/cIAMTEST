package com.se.idms.dto;

import com.fasterxml.jackson.annotation.JsonProperty;

public class SocialLoginResponse {

	
	private String state;
	
	private String authId;
	
	private String amlbcookie;
	
	private String proxyUrl;
	
	private String origUrl;
	
	private String ntId;

	public String getState() {
		return state;
	}

	public void setState(String state) {
		this.state = state;
	}

	public String getAuthId() {
		return authId;
	}

	public void setAuthId(String authId) {
		this.authId = authId;
	}

	public String getAmlbcookie() {
		return amlbcookie;
	}

	public void setAmlbcookie(String amlbcookie) {
		this.amlbcookie = amlbcookie;
	}

	@JsonProperty("PROXY_URL")
	public String getProxyUrl() {
		return proxyUrl;
	}

	@JsonProperty("PROXY_URL")
	public void setProxyUrl(String proxyUrl) {
		this.proxyUrl = proxyUrl;
	}

	@JsonProperty("ORIG_URL")
	public String getOrigUrl() {
		return origUrl;
	}

	@JsonProperty("ORIG_URL")
	public void setOrigUrl(String origUrl) {
		this.origUrl = origUrl;
	}

	@JsonProperty("NTID")
	public String getNtId() {
		return ntId;
	}

	@JsonProperty("NTID")
	public void setNtId(String ntId) {
		this.ntId = ntId;
	}
	
	
}

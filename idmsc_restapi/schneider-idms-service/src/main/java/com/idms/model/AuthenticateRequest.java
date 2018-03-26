package com.idms.model;

import java.util.List;

import org.apache.commons.lang3.builder.ToStringBuilder;

public class AuthenticateRequest {

	private String authId;

	private String template;
	
	private String stage;
	
	private String header;
	
	private List<TokenCallBacks> callbacks;

	public String getAuthId() {
		return authId;
	}

	public void setAuthId(String authId) {
		this.authId = authId;
	}

	public String getTemplate() {
		return template;
	}

	public void setTemplate(String template) {
		this.template = template;
	}

	public String getStage() {
		return stage;
	}

	public void setStage(String stage) {
		this.stage = stage;
	}

	public String getHeader() {
		return header;
	}

	public void setHeader(String header) {
		this.header = header;
	}

	public List<TokenCallBacks> getCallbacks() {
		return callbacks;
	}

	public void setCallbacks(List<TokenCallBacks> callbacks) {
		this.callbacks = callbacks;
	}
	
	@Override
    public String toString() {
        return  ToStringBuilder.reflectionToString(this);
    }
	
}

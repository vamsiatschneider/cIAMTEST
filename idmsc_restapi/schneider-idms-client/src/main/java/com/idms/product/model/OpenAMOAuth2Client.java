package com.idms.product.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class OpenAMOAuth2Client {

	@JsonInclude(Include.NON_NULL)
	private CoreOAuth2ClientConfig coreOAuth2ClientConfig;
	
	@JsonInclude(Include.NON_NULL)
	private AdvancedOAuth2ClientConfig advancedOAuth2ClientConfig;

	public CoreOAuth2ClientConfig getCoreOAuth2ClientConfig() {
		return coreOAuth2ClientConfig;
	}

	public void setCoreOAuth2ClientConfig(CoreOAuth2ClientConfig coreOAuth2ClientConfig) {
		this.coreOAuth2ClientConfig = coreOAuth2ClientConfig;
	}

	public AdvancedOAuth2ClientConfig getAdvancedOAuth2ClientConfig() {
		return advancedOAuth2ClientConfig;
	}

	public void setAdvancedOAuth2ClientConfig(AdvancedOAuth2ClientConfig advancedOAuth2ClientConfig) {
		this.advancedOAuth2ClientConfig = advancedOAuth2ClientConfig;
	}
}

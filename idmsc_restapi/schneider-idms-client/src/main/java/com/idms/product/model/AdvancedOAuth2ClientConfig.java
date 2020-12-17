package com.idms.product.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;

public class AdvancedOAuth2ClientConfig {

	@JsonInclude(Include.NON_NULL)
	@JsonProperty("responseTypes")
	private List<String> responseTypes;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("grantTypes")
	private List<String> grantTypes;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("tokenEndpointAuthMethod")
	private List<String> tokenEndpointAuthMethod;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("isConsentImplied")
	private boolean isConsentImplied;

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

	public List<String> getTokenEndpointAuthMethod() {
		return tokenEndpointAuthMethod;
	}

	public void setTokenEndpointAuthMethod(List<String> tokenEndpointAuthMethod) {
		this.tokenEndpointAuthMethod = tokenEndpointAuthMethod;
	}

	@JsonProperty(value="isConsentImplied")
	public boolean isConsentImplied() {
		return isConsentImplied;
	}

	public void setConsentImplied(boolean isConsentImplied) {
		this.isConsentImplied = isConsentImplied;
	}
}

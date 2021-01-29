package com.idms.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class AppOnboardingResponse {

	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private String clientId;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private String clientSecret;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private List<String> redirectionUris;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private List<String> responseTypes;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private List<String> grantTypes;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private List<String> applicationType;

	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private List<String> contacts;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private String clientName;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private String logoUri;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private String clientUri;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private String policyUri;

	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private String tosUri;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private List<String> tokenEndpointAuthMethod;

	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private List<String> scope;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private String frontchannelLogoutUri;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private List<String> postLogoutRedirectUris;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private String initiateLoginUri;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private List<String> seClientContext;

	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private String seClientBackgroundImage;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private String seClientDescription;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private String seClientTabName;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private String seClientFooter;

	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private List<String> seSocialProviders;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private String seSupportUri;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private List<String> seRegistrationLevel;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private String seRegistrationEmailDescription;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private String seProfileUpdateRedirectUri;

	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private List<String> seAilFeature;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private List<String> seAilProgram;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private List<String> seApiEnabled;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private String seEmailAppName;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private List<String> seProfileUpgradeLevel;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private String seProfileUpgradeRedirectUri;

	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private String seServiceAccountUsername;

	@JsonInclude(Include.NON_NULL)
	@JsonProperty
	private String seServiceAccountUserPassword;
	
	public List<String> getRedirectionUris() {
		return redirectionUris;
	}

	public void setRedirectionUris(List<String> redirectionUris) {
		this.redirectionUris = redirectionUris;
	}

	public List<String> getTokenEndpointAuthMethod() {
		return tokenEndpointAuthMethod;
	}

	public void setTokenEndpointAuthMethod(List<String> tokenEndpointAuthMethod) {
		this.tokenEndpointAuthMethod = tokenEndpointAuthMethod;
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

	public String getClientName() {
		return clientName;
	}

	public void setClientName(String clientName) {
		this.clientName = clientName;
	}

	public List<String> getApplicationType() {
		return applicationType;
	}

	public void setApplicationType(List<String> applicationType) {
		this.applicationType = applicationType;
	}

	public List<String> getContacts() {
		return contacts;
	}

	public void setContacts(List<String> contacts) {
		this.contacts = contacts;
	}

	public String getLogoUri() {
		return logoUri;
	}

	public void setLogoUri(String logoUri) {
		this.logoUri = logoUri;
	}

	public String getClientUri() {
		return clientUri;
	}

	public void setClientUri(String clientUri) {
		this.clientUri = clientUri;
	}

	public String getPolicyUri() {
		return policyUri;
	}

	public void setPolicyUri(String policyUri) {
		this.policyUri = policyUri;
	}

	public String getTosUri() {
		return tosUri;
	}

	public void setTosUri(String tosUri) {
		this.tosUri = tosUri;
	}

	public List<String> getScope() {
		return scope;
	}

	public void setScope(List<String> scope) {
		this.scope = scope;
	}

	public String getFrontchannelLogoutUri() {
		return frontchannelLogoutUri;
	}

	public void setFrontchannelLogoutUri(String frontchannelLogoutUri) {
		this.frontchannelLogoutUri = frontchannelLogoutUri;
	}

	public List<String> getPostLogoutRedirectUris() {
		return postLogoutRedirectUris;
	}

	public void setPostLogoutRedirectUris(List<String> postLogoutRedirectUris) {
		this.postLogoutRedirectUris = postLogoutRedirectUris;
	}

	public String getInitiateLoginUri() {
		return initiateLoginUri;
	}

	public void setInitiateLoginUri(String initiateLoginUri) {
		this.initiateLoginUri = initiateLoginUri;
	}

	public List<String> getSeClientContext() {
		return seClientContext;
	}

	public void setSeClientContext(List<String> seClientContext) {
		this.seClientContext = seClientContext;
	}

	public String getSeClientBackgroundImage() {
		return seClientBackgroundImage;
	}

	public void setSeClientBackgroundImage(String seClientBackgroundImage) {
		this.seClientBackgroundImage = seClientBackgroundImage;
	}

	public String getSeClientDescription() {
		return seClientDescription;
	}

	public void setSeClientDescription(String seClientDescription) {
		this.seClientDescription = seClientDescription;
	}

	public String getSeClientTabName() {
		return seClientTabName;
	}

	public void setSeClientTabName(String seClientTabName) {
		this.seClientTabName = seClientTabName;
	}

	public String getSeClientFooter() {
		return seClientFooter;
	}

	public void setSeClientFooter(String seClientFooter) {
		this.seClientFooter = seClientFooter;
	}

	public List<String> getSeSocialProviders() {
		return seSocialProviders;
	}

	public void setSeSocialProviders(List<String> seSocialProviders) {
		this.seSocialProviders = seSocialProviders;
	}

	public String getSeSupportUri() {
		return seSupportUri;
	}

	public void setSeSupportUri(String seSupportUri) {
		this.seSupportUri = seSupportUri;
	}

	public List<String> getSeRegistrationLevel() {
		return seRegistrationLevel;
	}

	public void setSeRegistrationLevel(List<String> seRegistrationLevel) {
		this.seRegistrationLevel = seRegistrationLevel;
	}

	public String getSeRegistrationEmailDescription() {
		return seRegistrationEmailDescription;
	}

	public void setSeRegistrationEmailDescription(String seRegistrationEmailDescription) {
		this.seRegistrationEmailDescription = seRegistrationEmailDescription;
	}

	public String getSeProfileUpdateRedirectUri() {
		return seProfileUpdateRedirectUri;
	}

	public void setSeProfileUpdateRedirectUri(String seProfileUpdateRedirectUri) {
		this.seProfileUpdateRedirectUri = seProfileUpdateRedirectUri;
	}

	public List<String> getSeAilFeature() {
		return seAilFeature;
	}

	public void setSeAilFeature(List<String> seAilFeature) {
		this.seAilFeature = seAilFeature;
	}

	public List<String> getSeAilProgram() {
		return seAilProgram;
	}

	public void setSeAilProgram(List<String> seAilProgram) {
		this.seAilProgram = seAilProgram;
	}

	public List<String> getSeApiEnabled() {
		return seApiEnabled;
	}

	public void setSeApiEnabled(List<String> seApiEnabled) {
		this.seApiEnabled = seApiEnabled;
	}

	public String getSeEmailAppName() {
		return seEmailAppName;
	}

	public void setSeEmailAppName(String seEmailAppName) {
		this.seEmailAppName = seEmailAppName;
	}

	public List<String> getSeProfileUpgradeLevel() {
		return seProfileUpgradeLevel;
	}

	public void setSeProfileUpgradeLevel(List<String> seProfileUpgradeLevel) {
		this.seProfileUpgradeLevel = seProfileUpgradeLevel;
	}

	public String getSeProfileUpgradeRedirectUri() {
		return seProfileUpgradeRedirectUri;
	}

	public void setSeProfileUpgradeRedirectUri(String seProfileUpgradeRedirectUri) {
		this.seProfileUpgradeRedirectUri = seProfileUpgradeRedirectUri;
	}

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

	public String getSeServiceAccountUsername() {
		return seServiceAccountUsername;
	}

	public void setSeServiceAccountUsername(String seServiceAccountUsername) {
		this.seServiceAccountUsername = seServiceAccountUsername;
	}

	public String getSeServiceAccountUserPassword() {
		return seServiceAccountUserPassword;
	}

	public void setSeServiceAccountUserPassword(String seServiceAccountUserPassword) {
		this.seServiceAccountUserPassword = seServiceAccountUserPassword;
	}
}

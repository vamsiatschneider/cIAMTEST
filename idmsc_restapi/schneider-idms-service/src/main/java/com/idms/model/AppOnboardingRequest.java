package com.idms.model;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonProperty;

public class AppOnboardingRequest {

	@JsonProperty
	private List<String> redirectionUris;
	
	@JsonProperty
	private List<String> responseTypes;
	
	@JsonProperty
	private List<String> grantTypes;
	
	@JsonProperty
	private List<String> applicationType;

	@JsonProperty
	private List<String> contacts;
	
	@JsonProperty
	private String clientName;
	
	@JsonProperty
	private String logoUri;
	
	@JsonProperty
	private String clientUri;
	
	@JsonProperty
	private String policyUri;
	
	@JsonProperty
	private String tosUri;
	
	@JsonProperty
	private List<String> tokenEndpointAuthMethod;

	@JsonProperty
	private List<String> scope;
	
	@JsonProperty
	private String frontchannelLogoutUri;
	
	@JsonProperty
	private List<String> postLogoutRedirectUris;
	
	@JsonProperty
	private String initiateLoginUri;
	
	@JsonProperty
	private List<String> seClientContext;

	@JsonProperty
	private String seClientBackgroundImage;
	
	@JsonProperty
	private String seClientDescription;
	
	@JsonProperty
	private String seClientTabName;
	
	@JsonProperty
	private String seClientFooter;

	@JsonProperty
	private List<String> seSocialProviders;
	
	@JsonProperty
	private String seSupportUri;
	
	@JsonProperty
	private List<String> seRegistrationLevel;
	
	@JsonProperty
	private String seRegistrationEmailDescription;
	
	@JsonProperty
	private String seProfileUpdateRedirectUri;

	@JsonProperty
	private List<String> seAilFeature;
	
	@JsonProperty
	private List<String> seAilProgram;
	
	@JsonProperty
	private List<String> seApiEnabled;
	
	@JsonProperty
	private String seEmailAppName;
	
	@JsonProperty
	private List<String> seProfileUpgradeLevel;
	
	@JsonProperty
	private String seProfileUpgradeRedirectUri;

	@JsonProperty
	private boolean isOnboardingCall;
	
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

	public boolean isOnboardingCall() {
		return isOnboardingCall;
	}

	public void setOnboardingCall(boolean isOnboardingCall) {
		this.isOnboardingCall = isOnboardingCall;
	}
}

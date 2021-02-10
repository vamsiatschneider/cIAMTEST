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
	private String clientId;

	@JsonProperty
	private String clientSecret;

	@JsonProperty
	private String seTechnicalUserName;

	@JsonProperty
	private String seTechnicalUserPassword;

	@JsonProperty
	private String seAilApplication;

	@JsonProperty
	private boolean isOnboardingCall;
	
	@JsonProperty("redirect_uris")
	public List<String> getRedirectionUris() {
		return redirectionUris;
	}

	public void setRedirectionUris(List<String> redirectionUris) {
		this.redirectionUris = redirectionUris;
	}

	@JsonProperty("token_endpoint_auth_method")
	public List<String> getTokenEndpointAuthMethod() {
		return tokenEndpointAuthMethod;
	}

	public void setTokenEndpointAuthMethod(List<String> tokenEndpointAuthMethod) {
		this.tokenEndpointAuthMethod = tokenEndpointAuthMethod;
	}

	@JsonProperty("response_types")
	public List<String> getResponseTypes() {
		return responseTypes;
	}

	public void setResponseTypes(List<String> responseTypes) {
		this.responseTypes = responseTypes;
	}

	@JsonProperty("grant_types")
	public List<String> getGrantTypes() {
		return grantTypes;
	}

	public void setGrantTypes(List<String> grantTypes) {
		this.grantTypes = grantTypes;
	}

	@JsonProperty("client_name")
	public String getClientName() {
		return clientName;
	}

	public void setClientName(String clientName) {
		this.clientName = clientName;
	}

	@JsonProperty("application_type")
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

	@JsonProperty("logo_uri")
	public String getLogoUri() {
		return logoUri;
	}

	public void setLogoUri(String logoUri) {
		this.logoUri = logoUri;
	}

	@JsonProperty("client_uri")
	public String getClientUri() {
		return clientUri;
	}

	public void setClientUri(String clientUri) {
		this.clientUri = clientUri;
	}

	@JsonProperty("policy_uri")
	public String getPolicyUri() {
		return policyUri;
	}

	public void setPolicyUri(String policyUri) {
		this.policyUri = policyUri;
	}

	@JsonProperty("tos_uri")
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

	@JsonProperty("frontchannel_logout_uri")
	public String getFrontchannelLogoutUri() {
		return frontchannelLogoutUri;
	}

	public void setFrontchannelLogoutUri(String frontchannelLogoutUri) {
		this.frontchannelLogoutUri = frontchannelLogoutUri;
	}

	@JsonProperty("post_logout_redirect_uris")
	public List<String> getPostLogoutRedirectUris() {
		return postLogoutRedirectUris;
	}

	public void setPostLogoutRedirectUris(List<String> postLogoutRedirectUris) {
		this.postLogoutRedirectUris = postLogoutRedirectUris;
	}

	@JsonProperty("initiate_login_uri")
	public String getInitiateLoginUri() {
		return initiateLoginUri;
	}

	public void setInitiateLoginUri(String initiateLoginUri) {
		this.initiateLoginUri = initiateLoginUri;
	}

	@JsonProperty("se_client_context")
	public List<String> getSeClientContext() {
		return seClientContext;
	}

	public void setSeClientContext(List<String> seClientContext) {
		this.seClientContext = seClientContext;
	}

	@JsonProperty("se_client_background_image")
	public String getSeClientBackgroundImage() {
		return seClientBackgroundImage;
	}

	public void setSeClientBackgroundImage(String seClientBackgroundImage) {
		this.seClientBackgroundImage = seClientBackgroundImage;
	}

	@JsonProperty("se_client_description")
	public String getSeClientDescription() {
		return seClientDescription;
	}

	public void setSeClientDescription(String seClientDescription) {
		this.seClientDescription = seClientDescription;
	}

	@JsonProperty("se_client_tab_name")
	public String getSeClientTabName() {
		return seClientTabName;
	}

	public void setSeClientTabName(String seClientTabName) {
		this.seClientTabName = seClientTabName;
	}

	@JsonProperty("se_client_footer")
	public String getSeClientFooter() {
		return seClientFooter;
	}

	public void setSeClientFooter(String seClientFooter) {
		this.seClientFooter = seClientFooter;
	}

	@JsonProperty("se_social_providers")
	public List<String> getSeSocialProviders() {
		return seSocialProviders;
	}

	public void setSeSocialProviders(List<String> seSocialProviders) {
		this.seSocialProviders = seSocialProviders;
	}

	@JsonProperty("se_support_uri")
	public String getSeSupportUri() {
		return seSupportUri;
	}

	public void setSeSupportUri(String seSupportUri) {
		this.seSupportUri = seSupportUri;
	}

	@JsonProperty("se_registration_level")
	public List<String> getSeRegistrationLevel() {
		return seRegistrationLevel;
	}

	public void setSeRegistrationLevel(List<String> seRegistrationLevel) {
		this.seRegistrationLevel = seRegistrationLevel;
	}

	@JsonProperty("se_registration_email_description")
	public String getSeRegistrationEmailDescription() {
		return seRegistrationEmailDescription;
	}

	public void setSeRegistrationEmailDescription(String seRegistrationEmailDescription) {
		this.seRegistrationEmailDescription = seRegistrationEmailDescription;
	}

	@JsonProperty("se_profile_update_redirect_uri")
	public String getSeProfileUpdateRedirectUri() {
		return seProfileUpdateRedirectUri;
	}

	public void setSeProfileUpdateRedirectUri(String seProfileUpdateRedirectUri) {
		this.seProfileUpdateRedirectUri = seProfileUpdateRedirectUri;
	}

	@JsonProperty("se_ail_feature")
	public List<String> getSeAilFeature() {
		return seAilFeature;
	}

	public void setSeAilFeature(List<String> seAilFeature) {
		this.seAilFeature = seAilFeature;
	}

	@JsonProperty("se_ail_program")
	public List<String> getSeAilProgram() {
		return seAilProgram;
	}

	public void setSeAilProgram(List<String> seAilProgram) {
		this.seAilProgram = seAilProgram;
	}

	@JsonProperty("se_api_enabled")
	public List<String> getSeApiEnabled() {
		return seApiEnabled;
	}

	public void setSeApiEnabled(List<String> seApiEnabled) {
		this.seApiEnabled = seApiEnabled;
	}

	@JsonProperty("se_email_app_name")
	public String getSeEmailAppName() {
		return seEmailAppName;
	}

	public void setSeEmailAppName(String seEmailAppName) {
		this.seEmailAppName = seEmailAppName;
	}

	@JsonProperty("se_profile_upgrate_level")
	public List<String> getSeProfileUpgradeLevel() {
		return seProfileUpgradeLevel;
	}

	public void setSeProfileUpgradeLevel(List<String> seProfileUpgradeLevel) {
		this.seProfileUpgradeLevel = seProfileUpgradeLevel;
	}
	
	@JsonProperty("se_profile_upgrade_redirect_uri")
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

	@JsonProperty("client_id")
	public String getClientId() {
		return clientId;
	}

	public void setClientId(String clientId) {
		this.clientId = clientId;
	}

	@JsonProperty("client_secret")
	public String getClientSecret() {
		return clientSecret;
	}

	public void setClientSecret(String clientSecret) {
		this.clientSecret = clientSecret;
	}

	@JsonProperty("se_technical_user_username")
	public String getSeTechnicalUserName() {
		return seTechnicalUserName;
	}

	public void setSeTechnicalUserName(String seTechnicalUserName) {
		this.seTechnicalUserName = seTechnicalUserName;
	}

	@JsonProperty("se_technical_user_password")
	public String getSeTechnicalUserPassword() {
		return seTechnicalUserPassword;
	}

	public void setSeTechnicalUserPassword(String seTechnicalUserPassword) {
		this.seTechnicalUserPassword = seTechnicalUserPassword;
	}

	@JsonProperty("se_ail_application")
	public String getSeAilApplication() {
		return seAilApplication;
	}

	public void setSeAilApplication(String seAilApplication) {
		this.seAilApplication = seAilApplication;
	}
}

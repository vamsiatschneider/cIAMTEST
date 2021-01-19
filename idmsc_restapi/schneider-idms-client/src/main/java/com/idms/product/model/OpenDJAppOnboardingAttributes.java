package com.idms.product.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class OpenDJAppOnboardingAttributes {

	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_app_source")
	private String appSource;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_appBackgoundImage")
	private String appBackgroundImage;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_appDescriptionEN")
	private String appDescriptionEN;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_appDescriptionZH")
	private String appDescriptionZH;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_appFooterLogo")
	private String appFooter;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_appImage")
	private String appLogo;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_id")
	private String appName;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_appNameEN")
	private String appNameEN;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_appNameZH")
	private String appNameZH;

	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_context")
	private String context;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_isLinkedInEnabled")
	private boolean isLinkedinEnabled;
    
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_isQQEnabled")
	private boolean isQqEnabled;
    
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_isWeChatEnabled")
	private boolean isWeChatEnabled;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_isWeiboEnabled")
	private boolean isWeiboEnabled;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_isAppleEnabled")
	private boolean isAppleEnabled;
    
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_isAILValidation")
	private boolean isAILValidation;

	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_registrationLevel")
	private String registrationLevel;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_urlForLogOff")
	private String urlForLogOff;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_urlForLogoffEC")
	private String urlForLogoffEC;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_urlForLogoffPC")
	private String urlForLogoffPC;
    
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_UrlRedirectAfterReg")
	private String urlRedirectAfterReg;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_UrlRedirectAfterProfileUpdate")
	private String urlRedirectAfterProfileUpdate;
    
	/* Constants */
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_appFooterText1")
	private String appFooterText1;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_appFooterText1ZH")
	private String appFooterText1ZH;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_appFooterText2")
	private String appFooterText2;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_appFooterText2ZH")
	private String appFooterText2ZH;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_appLoginIdentifierType")
	private String appLoginIdentifierType;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_appMobileFooterLogo")
	private String appMobileFooter;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_appMobileLoginCancelRedirectURL")
	private String appMobileLoginCancelRedirectURL;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_appRedirectURL")
	private String appRedirectUrl;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_bfoSupportUrl")
	private String bfoSupportUrl;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_chainedGoto")
	private String chainedGoto;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_chainedSunQueryParam")
	private String chainedSunQueryParam;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_chainedSunQueryParam_G")
	private String chainedSunQueryParam_G;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_idpChaining")
	private String idpChaining;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_ctxHome")
	private String ctxHome;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_ctxWork")
	private String ctxWork;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_emailReminderCount")
	private String emailReminderCount;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_phnBackgroundImage")
	private String phnBackgroundImage;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_enableTestMailDomain")
	private String enableTestMailDomain;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_globalLoginUrl")
	private String globalLoginUrl;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_IDMS_Application_CSS")
	private String idmsApplicationCSS;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_ChangeEmailCancelRedirectURL")
	private String changeEmailCancelRedirectURL;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_ChangePasswordCancelRedirectURL")
	private String changePasswordCancelRedirectURL;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_isChangeEmailLinkEnabled")
	private String isChangeEmailLinkEnabled;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_isCompanyTabEnable")
	private String isCompanyTabEnabled;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_isContextSupportEnabled")
	private String isContextSupportEnabled;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_isForgotPwdLinkEnabled")
	private String isForgotPwdLinkEnabled;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_isMobileBackButtonEnabled")
	private String isMobileBackButtonEnabled;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_isDynamicEmailEnabled")
	private String isDynamicEmailEnabled;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_isResendRegLinkEnabled")
	private String isResendRegLinkEnabled;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_isPingEnabled")
	private boolean isPingEnabled;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_isNeedHelpLinkEnabled")
	private String isNeedHelpLinkEnabled;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_isOTPEnabled")
	private String isOTPEnabled;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_isUsingIdmsProfilePage")
	private String isUsingIdmsProfilePage;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_isUsingIdmsRegPage")
	private String isUsingIdmsRegPage;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_otherIDPEndPoint")
	private String otherIDPEndPoint;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_idpRole")
	private String idpRole;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_isSeamlessLoginEnabled")
	private String isSeamlessLoginEnabled;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_technicalUser")
	private String technicalUser;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_isSEexternalApp")
	private String isSEexternalApp;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_travellerFlag")
	private String travellerFlag;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_returnToLoginURL")
	private String returnToLoginUrl;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_registrationEnabled")
	private String registrationEnabled;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_pingIdentityURL")
	private String pingIdentityURL;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_pwdInReg")
	private String pwdInReg;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_registrationURL")
	private String registrationURL;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_startURL")
	private String startURL;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_UrlRedirectAfterEmailVerification")
	private String urlRedirectAfterEmailVerification;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("UrlRedirectAfterIdentityUpdate")
	private String urlRedirectAfterIdentityUpdate;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_UrlRedirectAfterIdentityUpdate")
	private String urlRedirectBeforeIdentityUpdate;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_urlAfterProfileValidation")
	private String urlAfterProfileValidation;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_urlRedirectAfterPreReg")
	private String urlRedirectAfterPreReg;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_isSocialEnabledInReg")
	private String isSocialEnabledInReg;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_isDiscoveryLoginEnabled")
	private String isDiscoveryLoginEnabled;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_DisableCaptcha")
	private String disableCaptcha;
	
	@JsonInclude(Include.NON_NULL)
	@JsonProperty("_ProfileCancelRedirectURL")
	private String profileCancelRedirectURL;

	public String getAppSource() {
		return appSource;
	}

	public void setAppSource(String appSource) {
		this.appSource = appSource;
	}

	public String getAppBackgroundImage() {
		return appBackgroundImage;
	}

	public void setAppBackgroundImage(String appBackgroundImage) {
		this.appBackgroundImage = appBackgroundImage;
	}

	public String getAppDescriptionEN() {
		return appDescriptionEN;
	}

	public void setAppDescriptionEN(String appDescriptionEN) {
		this.appDescriptionEN = appDescriptionEN;
	}

	public String getAppDescriptionZH() {
		return appDescriptionZH;
	}

	public void setAppDescriptionZH(String appDescriptionZH) {
		this.appDescriptionZH = appDescriptionZH;
	}

	public String getAppFooter() {
		return appFooter;
	}

	public void setAppFooter(String appFooter) {
		this.appFooter = appFooter;
	}

	public String getAppLogo() {
		return appLogo;
	}

	public void setAppLogo(String appLogo) {
		this.appLogo = appLogo;
	}

	public String getAppName() {
		return appName;
	}

	public void setAppName(String appName) {
		this.appName = appName;
	}

	public String getAppNameEN() {
		return appNameEN;
	}

	public void setAppNameEN(String appNameEN) {
		this.appNameEN = appNameEN;
	}

	public String getAppNameZH() {
		return appNameZH;
	}

	public void setAppNameZH(String appNameZH) {
		this.appNameZH = appNameZH;
	}

	public String getContext() {
		return context;
	}

	public void setContext(String context) {
		this.context = context;
	}

	public String getRegistrationLevel() {
		return registrationLevel;
	}

	public void setRegistrationLevel(String registrationLevel) {
		this.registrationLevel = registrationLevel;
	}

	public String getUrlForLogOff() {
		return urlForLogOff;
	}

	public void setUrlForLogOff(String urlForLogOff) {
		this.urlForLogOff = urlForLogOff;
	}

	public String getUrlForLogoffEC() {
		return urlForLogoffEC;
	}

	public void setUrlForLogoffEC(String urlForLogoffEC) {
		this.urlForLogoffEC = urlForLogoffEC;
	}

	public String getUrlForLogoffPC() {
		return urlForLogoffPC;
	}

	public void setUrlForLogoffPC(String urlForLogoffPC) {
		this.urlForLogoffPC = urlForLogoffPC;
	}

	public String getUrlRedirectAfterReg() {
		return urlRedirectAfterReg;
	}

	public void setUrlRedirectAfterReg(String urlRedirectAfterReg) {
		this.urlRedirectAfterReg = urlRedirectAfterReg;
	}

	public String getUrlRedirectAfterProfileUpdate() {
		return urlRedirectAfterProfileUpdate;
	}

	public void setUrlRedirectAfterProfileUpdate(String urlRedirectAfterProfileUpdate) {
		this.urlRedirectAfterProfileUpdate = urlRedirectAfterProfileUpdate;
	}

	public String getAppFooterText1() {
		return appFooterText1;
	}

	public void setAppFooterText1(String appFooterText1) {
		this.appFooterText1 = appFooterText1;
	}

	public String getAppFooterText1ZH() {
		return appFooterText1ZH;
	}

	public void setAppFooterText1ZH(String appFooterText1ZH) {
		this.appFooterText1ZH = appFooterText1ZH;
	}

	public String getAppFooterText2() {
		return appFooterText2;
	}

	public void setAppFooterText2(String appFooterText2) {
		this.appFooterText2 = appFooterText2;
	}

	public String getAppFooterText2ZH() {
		return appFooterText2ZH;
	}

	public void setAppFooterText2ZH(String appFooterText2ZH) {
		this.appFooterText2ZH = appFooterText2ZH;
	}

	public String getAppLoginIdentifierType() {
		return appLoginIdentifierType;
	}

	public void setAppLoginIdentifierType(String appLoginIdentifierType) {
		this.appLoginIdentifierType = appLoginIdentifierType;
	}

	public String getAppMobileFooter() {
		return appMobileFooter;
	}

	public void setAppMobileFooter(String appMobileFooter) {
		this.appMobileFooter = appMobileFooter;
	}

	public String getAppMobileLoginCancelRedirectURL() {
		return appMobileLoginCancelRedirectURL;
	}

	public void setAppMobileLoginCancelRedirectURL(String appMobileLoginCancelRedirectURL) {
		this.appMobileLoginCancelRedirectURL = appMobileLoginCancelRedirectURL;
	}

	public String getAppRedirectUrl() {
		return appRedirectUrl;
	}

	public void setAppRedirectUrl(String appRedirectUrl) {
		this.appRedirectUrl = appRedirectUrl;
	}

	public String getBfoSupportUrl() {
		return bfoSupportUrl;
	}

	public void setBfoSupportUrl(String bfoSupportUrl) {
		this.bfoSupportUrl = bfoSupportUrl;
	}

	public String getChainedGoto() {
		return chainedGoto;
	}

	public void setChainedGoto(String chainedGoto) {
		this.chainedGoto = chainedGoto;
	}

	public String getChainedSunQueryParam() {
		return chainedSunQueryParam;
	}

	public void setChainedSunQueryParam(String chainedSunQueryParam) {
		this.chainedSunQueryParam = chainedSunQueryParam;
	}

	public String getChainedSunQueryParam_G() {
		return chainedSunQueryParam_G;
	}

	public void setChainedSunQueryParam_G(String chainedSunQueryParam_G) {
		this.chainedSunQueryParam_G = chainedSunQueryParam_G;
	}

	public String getIdpChaining() {
		return idpChaining;
	}

	public void setIdpChaining(String idpChaining) {
		this.idpChaining = idpChaining;
	}

	public String getCtxHome() {
		return ctxHome;
	}

	public void setCtxHome(String ctxHome) {
		this.ctxHome = ctxHome;
	}

	public String getCtxWork() {
		return ctxWork;
	}

	public void setCtxWork(String ctxWork) {
		this.ctxWork = ctxWork;
	}

	public String getEmailReminderCount() {
		return emailReminderCount;
	}

	public void setEmailReminderCount(String emailReminderCount) {
		this.emailReminderCount = emailReminderCount;
	}

	public String getPhnBackgroundImage() {
		return phnBackgroundImage;
	}

	public void setPhnBackgroundImage(String phnBackgroundImage) {
		this.phnBackgroundImage = phnBackgroundImage;
	}

	public String getEnableTestMailDomain() {
		return enableTestMailDomain;
	}

	public void setEnableTestMailDomain(String enableTestMailDomain) {
		this.enableTestMailDomain = enableTestMailDomain;
	}

	public String getGlobalLoginUrl() {
		return globalLoginUrl;
	}

	public void setGlobalLoginUrl(String globalLoginUrl) {
		this.globalLoginUrl = globalLoginUrl;
	}

	public String getIdmsApplicationCSS() {
		return idmsApplicationCSS;
	}

	public void setIdmsApplicationCSS(String idmsApplicationCSS) {
		this.idmsApplicationCSS = idmsApplicationCSS;
	}

	public String getChangeEmailCancelRedirectURL() {
		return changeEmailCancelRedirectURL;
	}

	public void setChangeEmailCancelRedirectURL(String changeEmailCancelRedirectURL) {
		this.changeEmailCancelRedirectURL = changeEmailCancelRedirectURL;
	}

	public String getChangePasswordCancelRedirectURL() {
		return changePasswordCancelRedirectURL;
	}

	public void setChangePasswordCancelRedirectURL(String changePasswordCancelRedirectURL) {
		this.changePasswordCancelRedirectURL = changePasswordCancelRedirectURL;
	}

	public String getIsChangeEmailLinkEnabled() {
		return isChangeEmailLinkEnabled;
	}

	public void setIsChangeEmailLinkEnabled(String isChangeEmailLinkEnabled) {
		this.isChangeEmailLinkEnabled = isChangeEmailLinkEnabled;
	}

	public String getIsCompanyTabEnabled() {
		return isCompanyTabEnabled;
	}

	public void setIsCompanyTabEnabled(String isCompanyTabEnabled) {
		this.isCompanyTabEnabled = isCompanyTabEnabled;
	}

	public String getIsContextSupportEnabled() {
		return isContextSupportEnabled;
	}

	public void setIsContextSupportEnabled(String isContextSupportEnabled) {
		this.isContextSupportEnabled = isContextSupportEnabled;
	}

	public String getIsForgotPwdLinkEnabled() {
		return isForgotPwdLinkEnabled;
	}

	public void setIsForgotPwdLinkEnabled(String isForgotPwdLinkEnabled) {
		this.isForgotPwdLinkEnabled = isForgotPwdLinkEnabled;
	}

	public String getIsMobileBackButtonEnabled() {
		return isMobileBackButtonEnabled;
	}

	public void setIsMobileBackButtonEnabled(String isMobileBackButtonEnabled) {
		this.isMobileBackButtonEnabled = isMobileBackButtonEnabled;
	}

	public String getIsDynamicEmailEnabled() {
		return isDynamicEmailEnabled;
	}

	public void setIsDynamicEmailEnabled(String isDynamicEmailEnabled) {
		this.isDynamicEmailEnabled = isDynamicEmailEnabled;
	}

	public String getIsResendRegLinkEnabled() {
		return isResendRegLinkEnabled;
	}

	public void setIsResendRegLinkEnabled(String isResendRegLinkEnabled) {
		this.isResendRegLinkEnabled = isResendRegLinkEnabled;
	}

	public String getIsNeedHelpLinkEnabled() {
		return isNeedHelpLinkEnabled;
	}

	public void setIsNeedHelpLinkEnabled(String isNeedHelpLinkEnabled) {
		this.isNeedHelpLinkEnabled = isNeedHelpLinkEnabled;
	}

	public String getIsOTPEnabled() {
		return isOTPEnabled;
	}

	public void setIsOTPEnabled(String isOTPEnabled) {
		this.isOTPEnabled = isOTPEnabled;
	}

	public String getIsUsingIdmsProfilePage() {
		return isUsingIdmsProfilePage;
	}

	public void setIsUsingIdmsProfilePage(String isUsingIdmsProfilePage) {
		this.isUsingIdmsProfilePage = isUsingIdmsProfilePage;
	}

	public String getIsUsingIdmsRegPage() {
		return isUsingIdmsRegPage;
	}

	public void setIsUsingIdmsRegPage(String isUsingIdmsRegPage) {
		this.isUsingIdmsRegPage = isUsingIdmsRegPage;
	}

	public String getOtherIDPEndPoint() {
		return otherIDPEndPoint;
	}

	public void setOtherIDPEndPoint(String otherIDPEndPoint) {
		this.otherIDPEndPoint = otherIDPEndPoint;
	}

	public String getIdpRole() {
		return idpRole;
	}

	public void setIdpRole(String idpRole) {
		this.idpRole = idpRole;
	}

	public String getIsSeamlessLoginEnabled() {
		return isSeamlessLoginEnabled;
	}

	public void setIsSeamlessLoginEnabled(String isSeamlessLoginEnabled) {
		this.isSeamlessLoginEnabled = isSeamlessLoginEnabled;
	}

	public String getTechnicalUser() {
		return technicalUser;
	}

	public void setTechnicalUser(String technicalUser) {
		this.technicalUser = technicalUser;
	}

	public String getIsSEexternalApp() {
		return isSEexternalApp;
	}

	public void setIsSEexternalApp(String isSEexternalApp) {
		this.isSEexternalApp = isSEexternalApp;
	}

	public String getTravellerFlag() {
		return travellerFlag;
	}

	public void setTravellerFlag(String travellerFlag) {
		this.travellerFlag = travellerFlag;
	}

	public String getReturnToLoginUrl() {
		return returnToLoginUrl;
	}

	public void setReturnToLoginUrl(String returnToLoginUrl) {
		this.returnToLoginUrl = returnToLoginUrl;
	}

	public String getRegistrationEnabled() {
		return registrationEnabled;
	}

	public void setRegistrationEnabled(String registrationEnabled) {
		this.registrationEnabled = registrationEnabled;
	}

	public String getPingIdentityURL() {
		return pingIdentityURL;
	}

	public void setPingIdentityURL(String pingIdentityURL) {
		this.pingIdentityURL = pingIdentityURL;
	}

	public String getPwdInReg() {
		return pwdInReg;
	}

	public void setPwdInReg(String pwdInReg) {
		this.pwdInReg = pwdInReg;
	}

	public String getRegistrationURL() {
		return registrationURL;
	}

	public void setRegistrationURL(String registrationURL) {
		this.registrationURL = registrationURL;
	}

	public String getStartURL() {
		return startURL;
	}

	public void setStartURL(String startURL) {
		this.startURL = startURL;
	}

	public String getUrlRedirectAfterEmailVerification() {
		return urlRedirectAfterEmailVerification;
	}

	public void setUrlRedirectAfterEmailVerification(String urlRedirectAfterEmailVerification) {
		this.urlRedirectAfterEmailVerification = urlRedirectAfterEmailVerification;
	}

	public String getUrlRedirectAfterIdentityUpdate() {
		return urlRedirectAfterIdentityUpdate;
	}

	public void setUrlRedirectAfterIdentityUpdate(String urlRedirectAfterIdentityUpdate) {
		this.urlRedirectAfterIdentityUpdate = urlRedirectAfterIdentityUpdate;
	}

	public String getUrlRedirectBeforeIdentityUpdate() {
		return urlRedirectBeforeIdentityUpdate;
	}

	public void setUrlRedirectBeforeIdentityUpdate(String urlRedirectBeforeIdentityUpdate) {
		this.urlRedirectBeforeIdentityUpdate = urlRedirectBeforeIdentityUpdate;
	}

	public String getUrlAfterProfileValidation() {
		return urlAfterProfileValidation;
	}

	public void setUrlAfterProfileValidation(String urlAfterProfileValidation) {
		this.urlAfterProfileValidation = urlAfterProfileValidation;
	}

	public String getUrlRedirectAfterPreReg() {
		return urlRedirectAfterPreReg;
	}

	public void setUrlRedirectAfterPreReg(String urlRedirectAfterPreReg) {
		this.urlRedirectAfterPreReg = urlRedirectAfterPreReg;
	}

	public String getIsSocialEnabledInReg() {
		return isSocialEnabledInReg;
	}

	public void setIsSocialEnabledInReg(String isSocialEnabledInReg) {
		this.isSocialEnabledInReg = isSocialEnabledInReg;
	}

	public String getIsDiscoveryLoginEnabled() {
		return isDiscoveryLoginEnabled;
	}

	public void setIsDiscoveryLoginEnabled(String isDiscoveryLoginEnabled) {
		this.isDiscoveryLoginEnabled = isDiscoveryLoginEnabled;
	}

	public String getDisableCaptcha() {
		return disableCaptcha;
	}

	public void setDisableCaptcha(String disableCaptcha) {
		this.disableCaptcha = disableCaptcha;
	}

	public String getProfileCancelRedirectURL() {
		return profileCancelRedirectURL;
	}

	public void setProfileCancelRedirectURL(String profileCancelRedirectURL) {
		this.profileCancelRedirectURL = profileCancelRedirectURL;
	}

	@JsonProperty(value="_isLinkedInEnabled")
	public boolean isLinkedinEnabled() {
		return isLinkedinEnabled;
	}

	public void setLinkedinEnabled(boolean isLinkedinEnabled) {
		this.isLinkedinEnabled = isLinkedinEnabled;
	}

	@JsonProperty(value="_isQQEnabled")
	public boolean isQqEnabled() {
		return isQqEnabled;
	}

	public void setQqEnabled(boolean isQqEnabled) {
		this.isQqEnabled = isQqEnabled;
	}

	@JsonProperty(value="_isWeChatEnabled")
	public boolean isWeChatEnabled() {
		return isWeChatEnabled;
	}

	public void setWeChatEnabled(boolean isWeChatEnabled) {
		this.isWeChatEnabled = isWeChatEnabled;
	}

	@JsonProperty(value="_isWeiboEnabled")
	public boolean isWeiboEnabled() {
		return isWeiboEnabled;
	}

	public void setWeiboEnabled(boolean isWeiboEnabled) {
		this.isWeiboEnabled = isWeiboEnabled;
	}

	@JsonProperty(value="_isAppleEnabled")
	public boolean isAppleEnabled() {
		return isAppleEnabled;
	}

	public void setAppleEnabled(boolean isAppleEnabled) {
		this.isAppleEnabled = isAppleEnabled;
	}

	@JsonProperty(value="_isPingEnabled")
	public boolean isPingEnabled() {
		return isPingEnabled;
	}

	public void setPingEnabled(boolean isPingEnabled) {
		this.isPingEnabled = isPingEnabled;
	}

	@JsonProperty(value="_isAILValidation")
	public boolean isAILValidation() {
		return isAILValidation;
	}

	public void setAILValidation(boolean isAILValidation) {
		this.isAILValidation = isAILValidation;
	}

}

package com.idms.service.util;

import static com.se.idms.util.UserConstants.ACCEPT_VERSION_GET_HEADER;
import static com.se.idms.util.UserConstants.ACCEPT_VERSION_HEADER;
import static com.se.idms.util.UserConstants.AUTH_VERSION_HEADER;
import static com.se.idms.util.UserConstants.CONTENT_TYPE_APP_JSON;
import static com.se.idms.util.UserConstants.FR6_5Version;
import static com.se.idms.util.UserConstants.SESSION_LOGOUT_VERSION_HEADER;
import static com.se.idms.util.UserConstants.TOKEN_STATUS_VERSION_HEADER;

import javax.ws.rs.core.Response;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.idms.model.IFWUser;
import com.idms.model.SocialProfileActivationRequest;
import com.idms.model.UserAMProfile;
import com.idms.product.client.OpenAMService;
import com.idms.product.model.OpenAmUser;

public class UserServiceUtil {

	private static final Logger LOGGER = LoggerFactory.getLogger(UserServiceUtil.class);
	// User APIs 
	public static String getUserBasedOnFRVersion(OpenAMService productService, String frVersion, String userId, String token) {
		LOGGER.info("Get User Call for version : " + frVersion);
		if(FR6_5Version.equalsIgnoreCase(frVersion)) {
			return productService.getUser(ACCEPT_VERSION_GET_HEADER, token, userId);
		} else {
			return productService.getUser(token, userId);
		}
	}
	public static String checkUserExistsBasedOnFRVersion(OpenAMService productService, String frVersion, String token, String query) {
		LOGGER.info("Check User Exists Call for version : " + frVersion);
		if(FR6_5Version.equalsIgnoreCase(frVersion)) {
			return productService.checkUserExistsWithEmailMobile(ACCEPT_VERSION_GET_HEADER, token, query);
		} else {
			return productService.checkUserExistsWithEmailMobile(token, query);
		}
	}
	public static String updateCounterBasedOnFRVersion(OpenAMService productService, String frVersion, String cookie, String userId, String reqJson) {
		LOGGER.info("Update Counter Call for version : " + frVersion);
		if(FR6_5Version.equalsIgnoreCase(frVersion)) {
			return productService.updateCounter(ACCEPT_VERSION_HEADER, cookie, userId, reqJson);
		} else {
			return productService.updateCounter(cookie, userId, reqJson);
		}
	}
	public static Response userRegistrationBasedOnFRVersion(OpenAMService productService, String frVersion, String cookie, String action, String reqJson) {
		LOGGER.info("Create User Call for version : " + frVersion);
		if(FR6_5Version.equalsIgnoreCase(frVersion)) {
			return productService.userRegistration(ACCEPT_VERSION_HEADER, cookie, action, reqJson);
		} else {
			return productService.userRegistration(cookie, action, reqJson);
		}
	}
	public static String updateUserBasedOnFRVersion(OpenAMService productService, String frVersion, String cookie, String userId, String reqJson) {
		LOGGER.info("Update User Call for version : " + frVersion);
		if(FR6_5Version.equalsIgnoreCase(frVersion)) {
			return productService.updateUser(ACCEPT_VERSION_HEADER, cookie, userId, reqJson);
		} else {
			return productService.updateUser(cookie, userId, reqJson);
		}
	}
	public static Response updateUserPasswordBasedOnFRVersion(OpenAMService productService, String frVersion, String cookie, String userId, String reqJson) {
		LOGGER.info("Update Password Call for version : " + frVersion);
		if(FR6_5Version.equalsIgnoreCase(frVersion)) {
			return productService.updateUserForPassword(ACCEPT_VERSION_HEADER, cookie, userId, reqJson);
		} else {
			return productService.updateUserForPassword(cookie, userId, reqJson);
		}
	}
	public static Response deleteUserBasedOnFRVersion(OpenAMService productService, String frVersion, String cookie, String userId) {
		LOGGER.info("Delete User Call for version : " + frVersion);
		if(FR6_5Version.equalsIgnoreCase(frVersion)) {
			return productService.deleteUser(ACCEPT_VERSION_HEADER, cookie, userId);
		} else {
			return productService.deleteUser(cookie, userId);
		}
	}
	// Authenticate APIs
	public static String authenticateUserBasedOnFRVersion(OpenAMService productService, String frVersion, String adminUserName, String adminPassword, String realm) {
		LOGGER.info("Authenticate Call for version : " + frVersion);
		if(FR6_5Version.equalsIgnoreCase(frVersion)) {
			return productService.authenticateUser(AUTH_VERSION_HEADER, adminUserName, adminPassword, realm);
		} else {
			return productService.authenticateUser(adminUserName, adminPassword, realm);
		}
	}
	public static Response otpAuthenticationBasedOnFRVersion(OpenAMService productService, String frVersion, String cookie, String service, String authIndexType, String authIndexValue, String reqJson, String realm) {
		LOGGER.info("OTP Authentication Call for version : " + frVersion);
		if(FR6_5Version.equalsIgnoreCase(frVersion)) {
			return productService.otpAuthentication(AUTH_VERSION_HEADER, realm, authIndexType, authIndexValue);
		} else {
			return productService.otpAuthentication(cookie, service, authIndexType, authIndexValue, reqJson);
		}
	}
	public static String authenticateChinaUserBasedOnFRVersion(OpenAMService productService, String frVersion, String adminUserName, String adminPassword, String realm) {
		LOGGER.info("Authenticate China User Call for version : " + frVersion);
		if(FR6_5Version.equalsIgnoreCase(frVersion)) {
			return productService.authenticateIdmsChinaUser(AUTH_VERSION_HEADER, adminUserName, adminPassword, realm);
		} else {
			return productService.authenticateIdmsChinaUser(adminUserName, adminPassword, realm);
		}
	}
	// Session APIs
	public static String checkTokenStatusBasedOnFRVersion(OpenAMService productService, String frVersion, String cookie, String action, String tokenId) {
		LOGGER.info("Active Token Call for version : " + frVersion);
		if(FR6_5Version.equalsIgnoreCase(frVersion)) {
			return productService.activeToken(TOKEN_STATUS_VERSION_HEADER, cookie, action, tokenId);
		} else {
			return productService.activeToken(cookie, action, tokenId);
		}
	}
	public static String sessionLogoutBasedOnFRVersion(OpenAMService productService, String frVersion, String cookie, String action) {
		LOGGER.info("Session Logout Call for version : " + frVersion);
		if(FR6_5Version.equalsIgnoreCase(frVersion)) {
			return productService.sessionLogout(SESSION_LOGOUT_VERSION_HEADER, CONTENT_TYPE_APP_JSON, cookie, action);
		} else {
			return productService.sessionLogout(cookie, action);
		}
	}
	public static String validateAtrributes(IFWUser userRecord) {
		String errorMessage = null;
		if(StringUtils.isBlank(userRecord.getFirstName())){
			errorMessage = "First Name cannot be null or empty!";
		}
		if(StringUtils.isBlank(userRecord.getLastName())){
			errorMessage = "Last Name cannot be null or empty!";
		}
		if(StringUtils.isBlank(userRecord.getEmail())){
			errorMessage = "Email Id cannot be null or empty!";
		}
		if(StringUtils.isBlank(userRecord.getTncFlag())){
			errorMessage = "T & C flag cannot be null or empty!";
		}
		if(StringUtils.isBlank(userRecord.getIDMS_Federated_ID__c())){
			errorMessage = "Federation ID cannot be null or empty!";
		}
		if(StringUtils.isBlank(userRecord.getIDMS_Email_opt_in__c())){
			errorMessage = "Email Opt In cannot be null or empty!";
		}
		return errorMessage;
	}
	
	public static void populateOpenAMUserAttributes(IFWUser userRecord, OpenAmUser openamUser) {
		openamUser.setMail(userRecord.getEmail());
		openamUser.setGivenName(userRecord.getFirstName());
		openamUser.setSn(userRecord.getLastName());
		openamUser.setTncFlag(userRecord.getTncFlag());
		openamUser.setIDMSisInternal__c("FALSE");
		openamUser.setIsActivated("false");
		openamUser.setEmailOptIn(userRecord.getIDMS_Email_opt_in__c());
		openamUser.setFederationID(userRecord.getIDMS_Federated_ID__c());
		openamUser.setRegisterationSource(userRecord.getIDMS_Registration_Source__c());
		openamUser.setCompanyName(userRecord.getCompanyName());
		openamUser.setCurrency(userRecord.getDefaultCurrencyIsoCode());
		openamUser.setCompanyStreet(userRecord.getCompany_Address1__c());
		openamUser.setCompanyCity(userRecord.getCompany_City__c());
		openamUser.setCompanyState(userRecord.getCompany_State__c());
		openamUser.setCompanyPostalCode(userRecord.getCompany_Postal_Code__c());
		openamUser.setIam1(userRecord.getIDMSClassLevel1__c());
		openamUser.setIam2(userRecord.getIDMSClassLevel2__c());
		openamUser.setIndustrySegment(userRecord.getIDMSMarketSegment__c());
		openamUser.setCompanyCountry(userRecord.getCompany_Country__c());
		openamUser.setC(userRecord.getCountry());
		openamUser.setEmployeeType(userRecord.getIDMS_User_Context__c());
		openamUser.setPreferredlanguage(userRecord.getIDMS_PreferredLanguage__c());
		openamUser.setCn(userRecord.getFirstName() + " " + userRecord.getLastName());
	}

	public static Response updateSocialProfileBasedOnFRVersion(OpenAMService productService, String adminToken, String fedId, String requestJson) {
		LogMessageUtil.logInfoMessage("Update Social Profile Openam Call!");
		return productService.updateSocialProfile(ACCEPT_VERSION_HEADER, adminToken, fedId, requestJson);
	}
	public static String validateActivationRequest(SocialProfileActivationRequest socialProfileRequest) {
		String errorMessage = null;
		if(StringUtils.isBlank(socialProfileRequest.getId())){
			errorMessage = "Id cannot be null or empty!";
		}
		if(StringUtils.isBlank(socialProfileRequest.getIDMS_Federated_ID__c())){
			errorMessage = "Federation ID cannot be null or empty!";
		}
		if(StringUtils.isBlank(socialProfileRequest.getIDMS_Profile_update_source())){
			errorMessage = "Profile Update Source cannot be null or empty!";
		}
		if(StringUtils.isBlank(socialProfileRequest.getOperation())){
			errorMessage = "Operation Type cannot be null or empty!";
		}
		if(StringUtils.isBlank(socialProfileRequest.getPinCode())){
			errorMessage = "Pin cannot be null or empty!";
		}
		if(StringUtils.isBlank(socialProfileRequest.getProvider())){
			errorMessage = "Provider Name cannot be null or empty!";
		}
		return errorMessage;
	}
	public static String buildMergeJson(UserAMProfile appleUser) {

		StringBuilder mergeJson = new StringBuilder("{");
		if(StringUtils.isNotBlank(appleUser.getGivenName()[0])) {
			String gName = "\"givenName\": \"" + appleUser.getGivenName()[0] + "\"";
			mergeJson.append(gName).append(",");
		}
		if(null != appleUser.getPreferredlanguage()) {
			String preferredlanguage = "\"preferredlanguage\": \"" + appleUser.getPreferredlanguage()[0] + "\"";
			mergeJson.append(preferredlanguage).append(",");
		}
		if(null != appleUser.getSn()) {
			String sn = "\"sn\": \"" + appleUser.getSn()[0] + "\"";
			mergeJson.append(sn).append(",");
		}
		if(null != appleUser.getCn()) {
			String cn = "\"cn\": \"" + appleUser.getCn()[0] + "\"";
			mergeJson.append(cn).append(",");
		}
		if(null != appleUser.getMail()) {
			String mail = "\"mail\": \"" + appleUser.getMail()[0] + "\"";
			mergeJson.append(mail).append(",");
		}
		if(null != appleUser.getEmailOptIn()) {
			String emailOptIn = "\"emailOptIn\": \"" + appleUser.getEmailOptIn()[0] + "\"";
			mergeJson.append(emailOptIn).append(",");
		}
		if(null != appleUser.getCompanyName()) {
			String companyName = "\"companyName\": \"" + appleUser.getCompanyName()[0] + "\"";
			mergeJson.append(companyName).append(",");
		}
		if(null != appleUser.getCompanyStreet()) {
			String companyStreet = "\"companyStreet\": \"" + appleUser.getCompanyStreet()[0] + "\"";
			mergeJson.append(companyStreet).append(",");
		}
		if(null != appleUser.getCompanyCity()) {
			String companyCity = "\"companyCity\": \"" + appleUser.getCompanyCity()[0] + "\"";
			mergeJson.append(companyCity).append(",");
		}
		if(null != appleUser.getCompanyState()) {
			String companyState = "\"companyState\": \"" + appleUser.getCompanyState()[0] + "\"";
			mergeJson.append(companyState).append(",");
		}
		if(null != appleUser.getCompanyPostalCode()) {
			String companyPostalCode = "\"companyPostalCode\": \"" + appleUser.getCompanyPostalCode()[0] + "\"";
			mergeJson.append(companyPostalCode).append(",");
		}
		if(null != appleUser.getCompanyCountry()) {
			String companyCountry = "\"companyCountry\": \"" + appleUser.getCompanyCountry()[0] + "\"";
			mergeJson.append(companyCountry).append(",");
		}
		if(null != appleUser.getC()) {
			String country = "\"c\": \"" + appleUser.getC()[0] + "\"";
			mergeJson.append(country).append(",");
		}
		if(null != appleUser.getIam1()) {
			String iam1 = "\"iam1\": \"" + appleUser.getIam1()[0] + "\"";
			mergeJson.append(iam1).append(",");
		}
		if(null != appleUser.getIam2()) {
			String iam2 = "\"iam2\": \"" + appleUser.getIam2()[0] + "\"";
			mergeJson.append(iam2).append(",");
		}
		if(null != appleUser.getIndustrySegment()) {
			String industrySegment = "\"industrySegment\": \"" + appleUser.getIndustrySegment()[0] + "\"";
			mergeJson.append(industrySegment).append(",");
		}
		if(null != appleUser.getTncFlag()) {
			String tncFlag = "\"tncFlag\": \"" + appleUser.getTncFlag()[0] + "\"";
			mergeJson.append(tncFlag).append(",");
		}
		if(null != appleUser.getUpdateSource()) {
			String updateSource = "\"updateSource\": \"" + appleUser.getUpdateSource()[0] + "\"";
			mergeJson.append(updateSource).append(",");
		}
		if(null != appleUser.getEmployeeType()) {
			String employeeType = "\"employeeType\": \"" + appleUser.getEmployeeType()[0] + "\"";
			mergeJson.append(employeeType).append(",");
		}
		if(null != appleUser.getAppleid()) {
			String aId = "\"appleid\": \"" + appleUser.getAppleid()[0] + "\"";
			mergeJson.append(aId);
		}
		mergeJson.append("}");
		LogMessageUtil.logInfoMessage("mergeJson= ", mergeJson.toString());
	    return mergeJson.toString();
	}
}

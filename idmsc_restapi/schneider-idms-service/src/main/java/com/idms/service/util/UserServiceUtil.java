package com.idms.service.util;

import static com.se.idms.util.UserConstants.ACCEPT_VERSION_GET_HEADER;
import static com.se.idms.util.UserConstants.ACCEPT_VERSION_HEADER;
import static com.se.idms.util.UserConstants.AUTH_VERSION_HEADER;
import static com.se.idms.util.UserConstants.CONTENT_TYPE_APP_JSON;
import static com.se.idms.util.UserConstants.FR6_5Version;
import static com.se.idms.util.UserConstants.OAUTH_VERSION_CREATE_HEADER;
import static com.se.idms.util.UserConstants.SESSION_LOGOUT_VERSION_HEADER;
import static com.se.idms.util.UserConstants.TOKEN_STATUS_VERSION_HEADER;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Base64;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static com.se.idms.util.UserConstants.ADDUSER_GROUP_VERSION_HEADER;

import javax.ws.rs.core.Response;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.idms.model.AppOnboardingRequest;
import com.idms.model.IFWUser;
import com.idms.model.OAuth2ClientRequest;
import com.idms.model.SocialProfileActivationRequest;
import com.idms.model.UserAMProfile;
import com.idms.model.ssc.GrantType;
import com.idms.model.ssc.ResponseType;
import com.idms.model.ssc.TokenEndpointAuthMethod;
import com.idms.product.client.OpenAMService;
import com.idms.product.model.AdvancedOAuth2ClientConfig;
import com.idms.product.model.CoreOAuth2ClientConfig;
import com.idms.product.model.OpenAMOAuth2Client;
import com.idms.product.model.OpenAmUser;
import com.idms.product.model.OpenAmUserInput;
import com.idms.product.model.OpenAmUserRequest;
import com.idms.product.model.OpenDJAppOnboardingAttributes;
import com.se.idms.cache.validate.IValidator;
import com.se.idms.dto.ErrorResponse;
import com.se.idms.util.AppOnboardingConstants;
import com.se.idms.util.UserConstants;

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
	public static Response updateMFADetailsBasedOnFRVersion(OpenAMService productService, String adminToken, String fedId, String requestJson) {
		LogMessageUtil.logInfoMessage("Update MFA Details Openam Call!");
		return productService.updateMFADetails(ACCEPT_VERSION_HEADER, adminToken, fedId, requestJson);
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
	
	public static Response updateUserGroupMemberships(OpenAMService productService, String frVersion, String realm, String sessionToken,
			String technicalUser, String action, String requestJson) {
		LOGGER.info("Add User to Group version : " + frVersion);
		return productService.updateUserGroupMemberships(ADDUSER_GROUP_VERSION_HEADER, realm, sessionToken, technicalUser, action, requestJson);
	}
	public static String getSessionsWithUserIdBasedOnFRVersion(OpenAMService productService, String adminToken, String query) {
		LogMessageUtil.logInfoMessage("Get All Sessions based on userId Openam Call!");
		return productService.getSessionsWithUserId(SESSION_LOGOUT_VERSION_HEADER, adminToken, query);
	}
	public static Response invalidateAllSessionsBasedOnFRVersion(OpenAMService productService, String adminToken,String sessionHandles_json, String query) {
		LogMessageUtil.logInfoMessage("logoutByHandle Openam Call!");
		return productService.invalidateAllSessions(SESSION_LOGOUT_VERSION_HEADER, adminToken, sessionHandles_json, query);
		 
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

	public static Response validateClientRequestAttributes(OAuth2ClientRequest createClientRequest) {
		Response response  = null;
		ErrorResponse errorResponse = new ErrorResponse();
		if(StringUtils.isBlank(createClientRequest.getClientId())) {
			errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
			errorResponse.setMessage("Client Id is mandatory!!");
			response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
		}
		if(StringUtils.isBlank(createClientRequest.getClientSecret())) {
			errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
			errorResponse.setMessage("Client Secret is mandatory!!");
			response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
		}
		if( null != createClientRequest.getRedirectionUris() && createClientRequest.getRedirectionUris().size() == 0) {
			errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
			errorResponse.setMessage("Redirection URI is mandatory!!");
			response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
		}
		if( null != createClientRequest.getScopes() && createClientRequest.getScopes().size() == 0) {
			errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
			errorResponse.setMessage("Scope is mandatory!!");
			response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
		}
		if( null == createClientRequest.getGrantTypes() || createClientRequest.getGrantTypes().size() == 0) {
			errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
			errorResponse.setMessage("Grant Type is mandatory!!");
			response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
		} else {
			for(String type: createClientRequest.getGrantTypes()) {
				if(GrantType.INVALID.equals(GrantType.getKey(type))) {
					errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
					errorResponse.setMessage("Invalid grant type: " + type);
					response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
					break;
				}
			}
		}
		if( null == createClientRequest.getResponseTypes() || createClientRequest.getResponseTypes().size() == 0) {
			errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
			errorResponse.setMessage("Response Type is mandatory!!");
			response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
		} else {
			for(String type: createClientRequest.getResponseTypes()) {
				if(ResponseType.INVALID.equals(ResponseType.getKey(type))) {
					errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
					errorResponse.setMessage("Invalid response type: " + type);
					response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
					break;
				}
			}
		}
		if( null == createClientRequest.getTokenEndpointAuthMethod() || createClientRequest.getTokenEndpointAuthMethod().size() == 0) {
			errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
			errorResponse.setMessage("Token Endpoint Authentication Method is mandatory!!");
			response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
		} else {
			for(String type: createClientRequest.getTokenEndpointAuthMethod()) {
				if(TokenEndpointAuthMethod.INVALID.equals(TokenEndpointAuthMethod.getKey(type))) {
					errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
					errorResponse.setMessage("Invalid Token Endpoint Authentication Method: " + type);
					response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
					break;
				}
			}
		}
		if(!createClientRequest.isConsentImplied()) {
			errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
			errorResponse.setMessage("Implied Consent must be set to true");
			response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
		}
		return response;
	}
	public static Response validateTechnicalUserAttributes(AppOnboardingRequest request){
		Response response  = null;
		ErrorResponse errorResponse = new ErrorResponse();
		if (StringUtils.isBlank(request.getSeTechnicalUserName())) {
			errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
			errorResponse.setMessage("Technical User Name is mandatory!!");
			return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
		}
		if (StringUtils.isBlank(request.getSeTechnicalUserPassword())) {
			errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
			errorResponse.setMessage("Technical User Password is mandatory!!");
			return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
		}else {
			if(!checkPasswordPolicy(request.getSeTechnicalUserPassword(), request.getClientName(), "TechnicalUser", "", "")) {
				errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
				errorResponse.setMessage("Technical User Password must match password policy!!");
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
			}
		}
		return response;
    }

	public static Response createOpenamClient(OpenAMService openamService, String realm, String adminToken, String clientId, String requestJson) {
		LogMessageUtil.logInfoMessage("Create OAuth2 Client Openam Call!");
		return openamService.createOauthClient(OAUTH_VERSION_CREATE_HEADER, realm, adminToken, clientId, requestJson);
	}
	public static void populateOpenAMClientAttributes(OAuth2ClientRequest createClientRequest,
			OpenAMOAuth2Client openamOAuth2Client) {
		openamOAuth2Client.setCoreOAuth2ClientConfig(new CoreOAuth2ClientConfig());
		openamOAuth2Client.getCoreOAuth2ClientConfig().setUserpassword(createClientRequest.getClientSecret());
		openamOAuth2Client.getCoreOAuth2ClientConfig().setDefaultScopes(createClientRequest.getDefaultScopes());
		openamOAuth2Client.getCoreOAuth2ClientConfig().setScopes(createClientRequest.getScopes());
		openamOAuth2Client.getCoreOAuth2ClientConfig().setRedirectionUris(createClientRequest.getRedirectionUris());

		openamOAuth2Client.setAdvancedOAuth2ClientConfig(new AdvancedOAuth2ClientConfig());
		openamOAuth2Client.getAdvancedOAuth2ClientConfig().setGrantTypes(createClientRequest.getGrantTypes());
		openamOAuth2Client.getAdvancedOAuth2ClientConfig().setConsentImplied(createClientRequest.isConsentImplied());
		openamOAuth2Client.getAdvancedOAuth2ClientConfig().setResponseTypes(createClientRequest.getResponseTypes());
		openamOAuth2Client.getAdvancedOAuth2ClientConfig().setTokenEndpointAuthMethod(createClientRequest.getTokenEndpointAuthMethod());
	}
	
	public static Response validateOnboardingRequestAttributes(IValidator onboardingFieldsValidator, AppOnboardingRequest appOnboardingRequest) {
		Response response  = null;
		ErrorResponse errorResponse = new ErrorResponse();
		if(StringUtils.isBlank(appOnboardingRequest.getClientName())) {
			errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
			errorResponse.setMessage("Client/Application Name attribute is mandatory!!");
			response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
		}
		if(StringUtils.isBlank(appOnboardingRequest.getSeClientDescription())) {
			errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
			errorResponse.setMessage("Client Description attribute is mandatory!!");
			response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
		}
		if(StringUtils.isBlank(appOnboardingRequest.getSeClientFooter())) {
			errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
			errorResponse.setMessage("Client Footer attribute is mandatory!!");
			response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
		}
		if( null == appOnboardingRequest.getApplicationType() || appOnboardingRequest.getApplicationType().size() == 0 ) {
			errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
			errorResponse.setMessage("Application Type attribute is mandatory!!");
			response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
		}
		if(StringUtils.isBlank(appOnboardingRequest.getSeAilApplication())) {
			errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
			errorResponse.setMessage("AIL Application attribute is mandatory!!");
			response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
		}
		if (null == appOnboardingRequest.getSeClientContext()
				|| appOnboardingRequest.getSeClientContext().size() == 0) {
			errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
			errorResponse.setMessage("SE Client Context attribute is mandatory!!");
			response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
		} else {
			if ((appOnboardingRequest.getSeClientContext().contains("B2B")
					|| appOnboardingRequest.getSeClientContext().contains("b2b"))
					&& null == appOnboardingRequest.getSeRegistrationLevel()
					|| appOnboardingRequest.getSeRegistrationLevel().size() == 0) {
				errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
				errorResponse.setMessage("SE Registration level attribute is mandatory for B2B!!");
				response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
			}
		}
		if(response == null) {
			response = validateOnboardingFieldsWithValidator(onboardingFieldsValidator, appOnboardingRequest, response);
		}
		return response;
//		if(StringUtils.isBlank(appOnboardingRequest.getSeClientBackgroundImage())) {
//			errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
//			errorResponse.setMessage("Client Background Image attribute is mandatory!!");
//			response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
//		}
//		if(StringUtils.isBlank(appOnboardingRequest.getLogoUri())) {
//			errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
//			errorResponse.setMessage("Logo URI attribute is mandatory!!");
//			response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
//		}
//		if(StringUtils.isBlank(appOnboardingRequest.getSeClientTabName())) {
//			errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
//			errorResponse.setMessage("SE Client Tab Name attribute is mandatory!!");
//			response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
//		}
//		if( null == appOnboardingRequest.getSeAilFeature() || appOnboardingRequest.getSeAilFeature().size() == 0 ) {
//			errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
//			errorResponse.setMessage("SE AIL Feature attribute is mandatory!!");
//			response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
//		}
//		if( StringUtils.isBlank(appOnboardingRequest.getFrontchannelLogoutUri())) {
//			errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
//			errorResponse.setMessage("SE Front Channel Logout URI attribute is mandatory!!");
//			response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
//		}
//		if( StringUtils.isBlank(appOnboardingRequest.getSeProfileUpgradeRedirectUri())) {
//			errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
//			errorResponse.setMessage("SE Profile Upgrade Redirect URI attribute is mandatory!!");
//			response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
//		}
//		if( StringUtils.isBlank(appOnboardingRequest.getSeProfileUpdateRedirectUri())) {
//			errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
//			errorResponse.setMessage("SE Profile Update Redirect URI attribute is mandatory!!");
//			response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
//		}
//		if( null == appOnboardingRequest.getContacts() || appOnboardingRequest.getContacts().size() == 0 ) {
//			errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
//			errorResponse.setMessage("Contacts attribute is mandatory!!");
//			response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
//		}
//		if( null == appOnboardingRequest.getSeSocialProviders() || appOnboardingRequest.getSeSocialProviders().size() == 0 ) {
//			errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
//			errorResponse.setMessage("SE Social Providers attribute is mandatory!!");
//			response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
//		}
	}
	
	public static Response validateOnboardingFieldsWithValidator(IValidator onboardingFieldsValidator,
			AppOnboardingRequest appOnboardingRequest, Response response) {
		ErrorResponse errorResponse = new ErrorResponse();
		for( String context : appOnboardingRequest.getSeClientContext()) {
			if (!onboardingFieldsValidator.validate(AppOnboardingConstants.SE_CLIENT_CONTEXT, context)) {
				errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
				errorResponse.setMessage("SE Client Context value is invalid!!");
				response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
			}
			if(response == null && "B2B".equalsIgnoreCase(context)) {
				for( String regLevel : appOnboardingRequest.getSeRegistrationLevel()) {
					if (!onboardingFieldsValidator.validate(AppOnboardingConstants.SE_REGISTRATION_LEVEL, regLevel)) {
						errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
						errorResponse.setMessage("SE Registration Level value is invalid!!");
						response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
					}
				}
			}
		}
		if (null != appOnboardingRequest.getSeSocialProviders()
				|| appOnboardingRequest.getSeSocialProviders().size() != 0) {
			for (String socialProvider : appOnboardingRequest.getSeSocialProviders()) {
				if (!onboardingFieldsValidator.validate(AppOnboardingConstants.SE_SOCIAL_PROVIDERS, socialProvider)) {
					errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
					errorResponse.setMessage("SE Social Provider value is invalid!!");
					response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
				}
			}
		}
		for( String applnType : appOnboardingRequest.getApplicationType()) {
			if (!onboardingFieldsValidator.validate(AppOnboardingConstants.APPLICATION_TYPE, applnType)) {
				errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
				errorResponse.setMessage("Application Type value is invalid!!");
				response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
			}
			if (response == null && "native".equalsIgnoreCase(applnType)) {
				if (null != appOnboardingRequest.getSeApiEnabled() && appOnboardingRequest.getSeApiEnabled().size() > 0) {
					errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
					errorResponse.setMessage("SE API Enabled list should be empty for "+  applnType +"  application type!!");
					response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
				}
			}
			if (response == null && !"native".equalsIgnoreCase(applnType)) {
				if (null == appOnboardingRequest.getSeApiEnabled() || appOnboardingRequest.getSeApiEnabled().size() == 0) {
					errorResponse.setStatus(HttpStatus.BAD_REQUEST.toString());
					errorResponse.setMessage("SE API Enabled list cannot be empty for "+  applnType +" application type!!");
					response = Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
				}
			}
		}
		return response;
	}
	public static void populateOpenDJAppOnboardingAttributes(AppOnboardingRequest appOnboardingRequest,
			OpenDJAppOnboardingAttributes appOnboardingDJAttributes) {

		appOnboardingDJAttributes.setAppSource(appOnboardingRequest.getClientName());
		appOnboardingDJAttributes.setAppName(appOnboardingRequest.getClientName());
		appOnboardingDJAttributes.setAppNameEN(appOnboardingRequest.getSeClientTabName());
		appOnboardingDJAttributes.setAppNameZH(appOnboardingRequest.getSeClientTabName());
		appOnboardingDJAttributes.setAppBackgroundImage(appOnboardingRequest.getSeClientBackgroundImage());
		appOnboardingDJAttributes.setAppDescriptionEN(appOnboardingRequest.getSeClientDescription());
		appOnboardingDJAttributes.setAppDescriptionZH(appOnboardingRequest.getSeClientDescription());
		appOnboardingDJAttributes.setAppFooter(appOnboardingRequest.getSeClientFooter());
		appOnboardingDJAttributes.setAppLogo(appOnboardingRequest.getLogoUri());
//		appOnboardingDJAttributes.setAILValidation(appOnboardingRequest.getSeAilFeature());
		appOnboardingDJAttributes.setContext(appOnboardingRequest.getSeClientContext().get(0));
		appOnboardingDJAttributes.setRegistrationLevel(appOnboardingRequest.getSeRegistrationLevel().get(0));
		appOnboardingDJAttributes.setUrlForLogOff(appOnboardingRequest.getFrontchannelLogoutUri());
		appOnboardingDJAttributes.setUrlForLogoffPC(appOnboardingRequest.getFrontchannelLogoutUri());
		appOnboardingDJAttributes.setUrlRedirectAfterReg(appOnboardingRequest.getSeProfileUpgradeRedirectUri());
		appOnboardingDJAttributes.setUrlRedirectAfterProfileUpdate(appOnboardingRequest.getSeProfileUpdateRedirectUri());
		//ping, qq, weibo, linkedin, wechat, apple
		for( String socialProvider : appOnboardingRequest.getSeSocialProviders()) {
			if ("linkedin".equalsIgnoreCase(socialProvider)) {
				appOnboardingDJAttributes.setLinkedinEnabled(true);
			}
			if ("apple".equalsIgnoreCase(socialProvider)) {
				appOnboardingDJAttributes.setAppleEnabled(true);
			}
			if ("qq".equalsIgnoreCase(socialProvider)) {
				appOnboardingDJAttributes.setQqEnabled(true);
			}
			if ("wechat".equalsIgnoreCase(socialProvider)) {
				appOnboardingDJAttributes.setWeChatEnabled(true);
			}
			if ("weibo".equalsIgnoreCase(socialProvider)) {
				appOnboardingDJAttributes.setWeiboEnabled(true);
			}
			if ("ping".equalsIgnoreCase(socialProvider)) {
				appOnboardingDJAttributes.setPingEnabled(true);
			}
		}
	}

	public static String populateOpenAMInput(AppOnboardingRequest request, String password, ObjectMapper objMapper, String technicalUser) throws JsonProcessingException {
		OpenAmUserRequest userReq = new OpenAmUserRequest();
		OpenAmUserInput openamInput = new OpenAmUserInput();
		OpenAmUser openamUser = new OpenAmUser();
		openamUser.setUsername(technicalUser);
		openamUser.setUserPassword(password);
		openamUser.setGivenName(request.getClientName());
		openamUser.setSn("TechnicalUser");
		openamUser.setIsActivated("true");
		openamUser.setCn(request.getClientName().concat(" ").concat(openamUser.getSn()));
		openamInput.setUser(openamUser);
		userReq.setInput(openamInput);
		String json = objMapper.writeValueAsString(userReq);
		json = json.replace("\"\"", "[]");
		return json;
	}

	public static OAuth2ClientRequest createClientRequest(AppOnboardingRequest appOnboardingRequest) {
		OAuth2ClientRequest createClientRequest = new OAuth2ClientRequest();
		createClientRequest.setClientId(appOnboardingRequest.getClientId());
		createClientRequest.setClientSecret(appOnboardingRequest.getClientSecret());
		createClientRequest.setConsentImplied(true);
		createClientRequest.setOnboardingCall(true);
		createClientRequest.setGrantTypes(appOnboardingRequest.getGrantTypes());
		createClientRequest.setRedirectionUris(appOnboardingRequest.getRedirectionUris());
		createClientRequest.setResponseTypes(appOnboardingRequest.getResponseTypes());
		createClientRequest.setScopes(appOnboardingRequest.getScope());
		createClientRequest.setTokenEndpointAuthMethod(appOnboardingRequest.getTokenEndpointAuthMethod());
		return createClientRequest;
	}
	public static String generateAppHash(AppOnboardingRequest appOnboardingRequest) throws NoSuchAlgorithmException {
		MessageDigest md = MessageDigest.getInstance("SHA-256");
	    md.update(appOnboardingRequest.getClientName().getBytes());
		String hashStr = Base64.getEncoder().encodeToString(md.digest());
		Pattern nonAlphanumeric = Pattern.compile("[^a-zA-Z0-9]");
		Matcher matcher = nonAlphanumeric.matcher(hashStr);
		String strFinalHash = matcher.replaceAll("");
		return strFinalHash;
	}
	private static boolean checkPasswordPolicy(String userPassword, String firstName, String lastName, String email, String mobile) {
		LOGGER.info("Entered checkPasswordPolicy() -> Start");
		LOGGER.info("Parameter firstName -> " + firstName + " , lastName -> " + lastName);
		LOGGER.info("Parameter email -> " + email + " , mobile -> " + mobile);

		boolean isPwdLengthInvalid = userPassword.length() < UserConstants.PASSWORD_LENGTH;
		boolean containsFName = userPassword.contains(firstName);
		boolean containsLName = userPassword.contains(lastName);
		boolean containsEmailOrMobile = ChinaIdmsUtil.passwordCheck(userPassword,email,mobile);

		boolean containsCheck = isPwdLengthInvalid || containsFName || containsLName || containsEmailOrMobile;
		// fail password policy check if any of the contains check fails.
		if(containsCheck) {
			LOGGER.info("Contains check Failed!");
			return false;
		}
		// Match all the regex with given password one by one
		boolean minLengthCheck = Pattern.matches(UserConstants.MIN_LENGTH_REGEX, userPassword);
		boolean wSpaceCheck = !Pattern.matches(UserConstants.WSPACE_REGEX, userPassword);
		boolean upperCaseMatch = Pattern.matches(UserConstants.UPPER_CASE_REGEX, userPassword);
		boolean lowerCaseMatch = Pattern.matches(UserConstants.LOWER_CASE_REGEX, userPassword);
		boolean numberMatch = Pattern.matches(UserConstants.NUMBER_REGEX, userPassword);
		boolean specialCharMatch = Pattern.matches(UserConstants.SPECIAL_CHARS_REGEX, userPassword);

		//Check for all permutations and combinations of given password
		boolean combination1 = upperCaseMatch && lowerCaseMatch && numberMatch;
		boolean combination2 = upperCaseMatch && lowerCaseMatch && specialCharMatch;
		boolean combination3 = lowerCaseMatch && numberMatch && specialCharMatch;
		boolean combination4 = upperCaseMatch && numberMatch && specialCharMatch;

		boolean isPwdPolicyCompliant = minLengthCheck && wSpaceCheck && (combination1 || combination2 || combination3 || combination4);
		LOGGER.info("isPwdPolicyCompliant: " + isPwdPolicyCompliant);
		return isPwdPolicyCompliant;
	}
}

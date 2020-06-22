package com.idms.service.util;

import javax.ws.rs.core.Response;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.idms.product.client.OpenAMService;
import com.idms.service.UserServiceImpl;

import static com.se.idms.util.UserConstants.FR6_5Version;
import static com.se.idms.util.UserConstants.ACCEPT_VERSION_HEADER;
import static com.se.idms.util.UserConstants.ACCEPT_VERSION_GET_HEADER;
import static com.se.idms.util.UserConstants.AUTH_VERSION_HEADER;
import static com.se.idms.util.UserConstants.SESSION_LOGOUT_VERSION_HEADER;
import static com.se.idms.util.UserConstants.TOKEN_STATUS_VERSION_HEADER;
import static com.se.idms.util.UserConstants.CONTENT_TYPE_APP_JSON;

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
	
}

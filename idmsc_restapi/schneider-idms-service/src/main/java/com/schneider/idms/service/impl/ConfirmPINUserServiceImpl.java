package com.schneider.idms.service.impl;

import static com.se.idms.util.UserConstants.AUDIT_API_ADMIN;
import static com.se.idms.util.UserConstants.AUDIT_IMPERSONATING_USER;
import static com.se.idms.util.UserConstants.AUDIT_LOG_CLOSURE;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_API;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_GET_CALL;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_UPDATE_CALL;
import static com.se.idms.util.UserConstants.AUDIT_REQUESTING_USER;
import static com.se.idms.util.UserConstants.AUDIT_TECHNICAL_USER;

import java.io.IOException;
import java.io.InputStream;
import java.util.Date;

import javax.inject.Inject;
import javax.ws.rs.BadRequestException;
import javax.ws.rs.NotAuthorizedException;
import javax.ws.rs.NotFoundException;
import javax.ws.rs.core.Response;

import org.apache.cxf.helpers.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.idms.model.ConfirmPinErrorResponse;
import com.idms.model.ConfirmPinRequest;
import com.idms.model.ConfirmPinResponse;
import com.idms.model.CreateUserRequest;
import com.idms.model.IFWUser;
import com.idms.product.model.Attributes;
import com.idms.service.util.ChinaIdmsUtil;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Option;
import com.schneider.idms.service.IConfirmPinUserService;
import com.schneider.idms.service.ICreateUserService;
import com.se.idms.dto.IDMSUserRecord;
import com.se.idms.dto.IFWCustomAttributesForWork;
import com.se.idms.dto.PasswordRecoveryResponse;
import com.se.idms.util.JsonConstants;
import com.se.idms.util.UserConstants;

@Service("confirmPinService")
public class ConfirmPINUserServiceImpl extends IdmsCommonServiceImpl implements IConfirmPinUserService {

	/**
	 * Logger instance.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(ConfirmPINUserServiceImpl.class);

	private static final Logger EMAIL_CHANGE_LOGGER = LoggerFactory.getLogger("emailChangeLogger");

	@Inject
	private ICreateUserService createUserService;

	@Override
	public Response userPinConfirmation(ConfirmPinRequest confirmPIN) {
		LOGGER.info("Entered userPinConfirmation() -> Start");
		LOGGER.info("Parameter confirmRequest -> " + confirmPIN);

		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		ConfirmPinResponse response = new ConfirmPinResponse();
		ConfirmPinErrorResponse errorResponse = new ConfirmPinErrorResponse();
		DocumentContext productDocCtx = null;
		DocumentContext provProductDocCtx = null;
		String iPlanetDirectoryKey = null;
		String getUserResponse = null;
		String authId = null;
		String emailOrMobile = null;
		String loginIdentifierType = null;
		String firstName = null;
		String lastName = null;
		String PRODUCT_JSON_STRING = null;
		String hotpEmailVerification = null;
		String hotpMobileVerification = null;
		String openamVnew = null;
		Integer vNewCntValue = 0;
		String ifwAccessToken = null;
		boolean validPinStatus = false;
		ObjectMapper objMapper = new ObjectMapper();
		String uniqueIdentifier = null;
		String federationID = null;
		try {

			LOGGER.info("UserServiceImpl:userPinConfirmation -> : Request :  -> "
					+ objMapper.writeValueAsString(confirmPIN));

			if (null == confirmPIN.getPinCode() || confirmPIN.getPinCode().isEmpty()) {
				response.setStatus(errorStatus);
				response.setMessage(UserConstants.MANDATORY_PINCODE);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}

			if ((null == confirmPIN.getId() || confirmPIN.getId().isEmpty())
					&& (null == confirmPIN.getIDMS_Federated_ID__c() || confirmPIN.getIDMS_Federated_ID__c().isEmpty())
					&& (null == confirmPIN.getFederationIdentifier()
							|| confirmPIN.getFederationIdentifier().isEmpty())) {
				response.setStatus(errorStatus);
				response.setMessage(UserConstants.MANDATORY_FEDERATION_ID);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}

			if (null == confirmPIN.getIDMS_Profile_update_source()
					|| confirmPIN.getIDMS_Profile_update_source().isEmpty()) {
				response.setStatus(errorStatus);
				response.setMessage(UserConstants.PROFILE_UPDATE_SOURCE);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}
			if ((null != confirmPIN.getIDMS_Profile_update_source()
					&& !confirmPIN.getIDMS_Profile_update_source().isEmpty())
					&& (!pickListValidator.validate(UserConstants.UPDATE_SOURCE,
							confirmPIN.getIDMS_Profile_update_source()))) {
				response.setMessage(UserConstants.INVALID_VALUE_IDMS + UserConstants.UPDATE_SOURCE);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}

			if ((null != confirmPIN.getOperation())
					&& !(UserConstants.USER_REGISTRATION.equalsIgnoreCase(confirmPIN.getOperation())
							|| UserConstants.SET_USER_PR.equalsIgnoreCase(confirmPIN.getOperation())
							|| UserConstants.UPDATE_USER_RECORD.equalsIgnoreCase(confirmPIN.getOperation()))) {

				response.setStatus(errorStatus);
				response.setMessage(UserConstants.OPERATION_MISMATCH);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			} else if (null != confirmPIN.getOperation()
					&& UserConstants.SET_USER_PR.equalsIgnoreCase(confirmPIN.getOperation())
					&& (null == confirmPIN.getPassword() || confirmPIN.getPassword().isEmpty())) {

				if (null != confirmPIN.getUIFlag() && !confirmPIN.getUIFlag().isEmpty()
						&& UserConstants.TRUE.equalsIgnoreCase(confirmPIN.getUIFlag())) {
					response.setStatus(errorStatus);
					response.setMessage(UserConstants.MANDATORY_PR);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
					return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
				} else {
					response.setStatus(errorStatus);
					response.setMessage(UserConstants.OPERATION_BLCOKED);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
					return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
				}
			} else if (null == confirmPIN.getOperation() || confirmPIN.getOperation().isEmpty()) {

				response.setStatus(errorStatus);
				response.setMessage(UserConstants.OPERATION_MISMATCH);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}

			/**
			 * The below change applying for R5 Release start
			 * 
			 */

			if (((null == confirmPIN.getUIFlag() || !UserConstants.TRUE.equalsIgnoreCase(confirmPIN.getUIFlag()))
					|| UserConstants.UPDATE_USER_RECORD.equalsIgnoreCase(confirmPIN.getOperation()))
					&& (null != confirmPIN.getPassword() && !confirmPIN.getPassword().isEmpty())
					&& (!UserConstants.UIMS.equalsIgnoreCase(confirmPIN.getIDMS_Profile_update_source()))) {

				response.setStatus(errorStatus);
				response.setMessage(UserConstants.OPERATION_BLCOKED);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}

			if (null != confirmPIN.getIDMS_Federated_ID__c() && !confirmPIN.getIDMS_Federated_ID__c().isEmpty()) {

				uniqueIdentifier = confirmPIN.getIDMS_Federated_ID__c();
			} else if (null != confirmPIN.getFederationIdentifier()
					&& !confirmPIN.getFederationIdentifier().isEmpty()) {

				uniqueIdentifier = confirmPIN.getFederationIdentifier();
			} else if (null != confirmPIN.getId() && !confirmPIN.getId().isEmpty()) {

				uniqueIdentifier = confirmPIN.getId();
			}

			/**
			 * call /json/authenticate to iplanetDirectoryPro token for admins
			 */
			iPlanetDirectoryKey = getSSOToken();

			/**
			 * Call GET : /se/users/{userId}
			 */

			String getUserReponseProv = null;
			if (null != iPlanetDirectoryKey) {
				LOGGER.info(AUDIT_REQUESTING_USER + AUDIT_TECHNICAL_USER + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN
						+ AUDIT_OPENAM_API + AUDIT_OPENAM_GET_CALL + uniqueIdentifier + AUDIT_LOG_CLOSURE);
				getUserResponse = productService.getUser(iPlanetDirectoryKey, uniqueIdentifier);

				LOGGER.info("UserServiceImpl:userPinConfirmation -> : productService.getUser: Response :  -> "
						+ getUserResponse);
				productDocCtx = JsonPath.using(conf).parse(getUserResponse);

				// if loginId already updated -> then instead of 500, we need to
				// return 409 already activated

				String loginIdCheck = null != productDocCtx.read(JsonConstants.LOGIN_ID_UPPER_0)
						? getValue(productDocCtx.read(JsonConstants.LOGIN_ID_UPPER_0)) : getDelimeter();

				if (null != loginIdCheck && "userRegistration".equals(confirmPIN.getOperation())) {
					response.setMessage("The user is already activated");
					response.setId(uniqueIdentifier);
					response.setFederation_Id(uniqueIdentifier);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
					return Response.status(Response.Status.CONFLICT).entity(response).build();
				}

				openamVnew = null != productDocCtx.read("$.V_New[0]") ? getValue(productDocCtx.read("$.V_New[0]"))
						: getDelimeter();
				if (null != vNewCntValue && null != openamVnew) {
					vNewCntValue = Integer.parseInt(openamVnew) + 1;
				}
				String version = "{\"V_New\": \"" + vNewCntValue + "\"" + "}";
				// Adding V_New
				productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, uniqueIdentifier,
						version);
				if ("[]".equalsIgnoreCase(productDocCtx.read("$.AuthID[0]"))
						|| "[]".equalsIgnoreCase(productDocCtx.read("$.authId[0]"))) {
					throw new Exception("Pin got expired or invalid!!");
				} else {
					authId = productDocCtx.read("$.authId[0]");
					if (null == authId) {
						authId = productDocCtx.read("$.AuthID[0]");
					}
				}
				emailOrMobile = productDocCtx.read("$.mail[0]");
				loginIdentifierType = UserConstants.EMAIL;
				if (null == emailOrMobile) {
					emailOrMobile = productDocCtx.read("$.mobile[0]");
					loginIdentifierType = UserConstants.MOBILE;

				}

				federationID = productDocCtx.read("$.federationID[0]");

				if (UserConstants.UPDATE_USER_RECORD.equalsIgnoreCase(confirmPIN.getOperation())) {
					getUserReponseProv = productService.getUser(iPlanetDirectoryKey, uniqueIdentifier);
					provProductDocCtx = JsonPath.using(conf).parse(getUserReponseProv);
					emailOrMobile = provProductDocCtx.read("$.newmail[0]");
					loginIdentifierType = UserConstants.EMAIL;
					if (null == emailOrMobile) {
						emailOrMobile = provProductDocCtx.read("$.newmobile[0]");
						loginIdentifierType = UserConstants.MOBILE;

					}
				}

			} else {
				response.setStatus(errorStatus);
				response.setMessage(UserConstants.TOKEN_INVALID);
				response.setId(uniqueIdentifier);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}

			firstName = null != productDocCtx.read("$.givenName")
					? getValue(productDocCtx.read("$.givenName").toString()) : getDelimeter();
			lastName = null != productDocCtx.read("$.sn") ? getValue(productDocCtx.read("$.sn").toString())
					: getDelimeter();

			// amlbcookieValue = UserConstants.AMLB_COOKIE+amlbcookieValue;
			if (((null != confirmPIN.getPassword() && !confirmPIN.getPassword().isEmpty()))
					&& !checkPasswordPolicy(confirmPIN.getPassword(), firstName, lastName)) {
				response.setStatus(errorStatus);
				response.setMessage(UserConstants.PR_POLICY);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}

			LOGGER.info("User Reponse Document  " + productDocCtx.jsonString());

			/**
			 * HOTP Call 4 to Submit HOTP
			 */
			String hotpService = null;
			String userService = null;
			if (UserConstants.USER_REGISTRATION.equalsIgnoreCase(confirmPIN.getOperation())
					&& UserConstants.MOBILE.equals(loginIdentifierType)) {
				hotpService = UserConstants.HOTP_MOBILE_USER_REGISTRATION;
				userService = UserConstants.CREATE_USER_SERVICE;
			} else if (UserConstants.USER_REGISTRATION.equalsIgnoreCase(confirmPIN.getOperation())
					&& UserConstants.EMAIL.equals(loginIdentifierType)) {
				hotpService = UserConstants.HOTP_EMAIL;
				userService = UserConstants.CREATE_USER_SERVICE;
			} else if (UserConstants.SET_USER_PR.equalsIgnoreCase(confirmPIN.getOperation())
					&& UserConstants.EMAIL.equals(loginIdentifierType)) {
				userService = UserConstants.CREATE_USER_SERVICE;
			} else if (UserConstants.SET_USER_PR.equalsIgnoreCase(confirmPIN.getOperation())
					&& UserConstants.MOBILE.equals(loginIdentifierType)) {
				userService = UserConstants.CREATE_USER_SERVICE;
			}
			if (UserConstants.UPDATE_USER_RECORD.equalsIgnoreCase(confirmPIN.getOperation())
					&& UserConstants.MOBILE.equals(loginIdentifierType)) {
				userService = UserConstants.UPDATE_USER_SERVICE;
			} else if (UserConstants.UPDATE_USER_RECORD.equalsIgnoreCase(confirmPIN.getOperation())
					&& UserConstants.EMAIL.equals(loginIdentifierType)) {
				userService = UserConstants.UPDATE_USER_SERVICE;
			}
			try {

				LOGGER.info("productDocCtx.jsonString() - >" + productDocCtx.jsonString());
				LOGGER.info("userService" + userService);
				if (UserConstants.USER_REGISTRATION.equalsIgnoreCase(confirmPIN.getOperation())) {
					validPinStatus = sendEmail.validatePin(confirmPIN.getPinCode(), uniqueIdentifier);
				} else {
					validPinStatus = sendEmail.validatePin(confirmPIN.getPinCode(), uniqueIdentifier);
				}
				if (!validPinStatus) {
					throw new Exception("Pin got expired or invalid!!");
				}
			} catch (NotAuthorizedException e) {
				e.printStackTrace();
				response.setStatus(errorStatus);
				response.setMessage(e.getMessage());
				response.setId(uniqueIdentifier);
				response.setFederation_Id(uniqueIdentifier);

				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
				LOGGER.error("Executing while creating the User :: -> " + e.getMessage());
				return Response.status(Response.Status.UNAUTHORIZED).entity(response).build();
			} catch (Exception e) {
				e.printStackTrace();
				errorResponse.setStatus(errorStatus);
				errorResponse.setMessage(UserConstants.INVALID_PINCODE);
				errorResponse.setId(uniqueIdentifier);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
				LOGGER.error("Exception while confirming the User pin:: -> " + e.getMessage());
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
			}

			LOGGER.info(
					"UserServiceImpl:userPinConfirmation -> : Operation: Requset :  -> " + confirmPIN.getOperation());
			if (UserConstants.USER_REGISTRATION.equalsIgnoreCase(confirmPIN.getOperation())) {

				if (UserConstants.MOBILE.equalsIgnoreCase(loginIdentifierType)) {

					PRODUCT_JSON_STRING = "{" + "\"loginid\": \"" + emailOrMobile + "\",\"mobile\": \"" + emailOrMobile
							+ "\"" + "}";

					if ((null != confirmPIN.getUIFlag() && !confirmPIN.getUIFlag().isEmpty())
							&& (null != confirmPIN.getPassword() && !confirmPIN.getPassword().isEmpty())) {
						PRODUCT_JSON_STRING = "{" + "\"loginid\": \"" + emailOrMobile + "\",\"mobile\": \""
								+ emailOrMobile + "\",\"userPassword\": \"" + confirmPIN.getPassword().trim() + "\""
								+ "}";
					}
				} else if (UserConstants.EMAIL.equalsIgnoreCase(loginIdentifierType)) {
					PRODUCT_JSON_STRING = "{" + "\"loginid\": \"" + emailOrMobile + "\",\"mail\": \"" + emailOrMobile
							+ "\"" + "}";
					if ((null != confirmPIN.getUIFlag() && !confirmPIN.getUIFlag().isEmpty())
							&& (null != confirmPIN.getPassword() && !confirmPIN.getPassword().isEmpty())) {
						PRODUCT_JSON_STRING = "{" + "\"loginid\": \"" + emailOrMobile + "\",\"mail\": \""
								+ emailOrMobile + "\",\"userPassword\": \"" + confirmPIN.getPassword().trim() + "\""
								+ "}";
					}
				}

				if (null != confirmPIN.getIDMS_Email_opt_in__c() && !confirmPIN.getIDMS_Email_opt_in__c().isEmpty()) {
					PRODUCT_JSON_STRING = PRODUCT_JSON_STRING.substring(0, PRODUCT_JSON_STRING.length() - 1)
							.concat(",\"emailOptIn\":\"" + confirmPIN.getIDMS_Email_opt_in__c() + "\"}");
				}
				if (null != confirmPIN.getTncFlag() && !confirmPIN.getTncFlag().isEmpty()) {
					PRODUCT_JSON_STRING = PRODUCT_JSON_STRING.substring(0, PRODUCT_JSON_STRING.length() - 1)
							.concat(",\"tncFlag\":\"" + confirmPIN.getTncFlag() + "\"}");
				}

				/**
				 * For User Activation
				 * 
				 */

				PRODUCT_JSON_STRING = PRODUCT_JSON_STRING.substring(0, PRODUCT_JSON_STRING.length() - 1)
						.concat(",\"isActivated\":\"true\"}");

				if (null != emailOrMobile && !emailOrMobile.isEmpty()) {
					LOGGER.info(AUDIT_REQUESTING_USER + uniqueIdentifier + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN
							+ AUDIT_OPENAM_API + AUDIT_OPENAM_UPDATE_CALL + uniqueIdentifier + AUDIT_LOG_CLOSURE);
					LOGGER.info("UserServiceImpl:userPinConfirmation -> : productService.updateUser: Requset :  -> ",
							PRODUCT_JSON_STRING);
					productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, uniqueIdentifier,
							PRODUCT_JSON_STRING);
				}

				/**
				 * The below code to activate the IDMSSetActivationDate
				 * 
				 */

				if (UserConstants.PRMPORTAL.equalsIgnoreCase(confirmPIN.getIDMS_Profile_update_source())) {

					PRODUCT_JSON_STRING = "{" + "\"federationId\": \"" + uniqueIdentifier + "\",\"appName\": \""
							+ confirmPIN.getIDMS_Profile_update_source() + "\"" + "}";

					String salesForceToken = getSaleforceToken();

					LOGGER.info(
							"Request sending to  salesForceService.populateActivationDate : " + PRODUCT_JSON_STRING);

					Response activationResponse = salesForceService.populateActivationDate(
							UserConstants.ACCEPT_TYPE_APP_JSON, salesForceToken, PRODUCT_JSON_STRING);

					LOGGER.info("populateActivationDate Status :: " + activationResponse.getStatus());
					if (200 != activationResponse.getStatus()) {
						LOGGER.error("Failed to populate the activate date on PRM :: populateActivationDate -> "
								+ IOUtils.toString((InputStream) activationResponse.getEntity()));
					} else {
						LOGGER.info("Successfully populated the activation date on PRM :: populateActivationDate -> "
								+ IOUtils.toString((InputStream) activationResponse.getEntity()));
					}

				}

			}
			if (UserConstants.UPDATE_USER_RECORD.equalsIgnoreCase(confirmPIN.getOperation())) {

				if (UserConstants.MOBILE.equalsIgnoreCase(loginIdentifierType)) {

					PRODUCT_JSON_STRING = "{" + "\"loginid\": \"" + emailOrMobile + "\",\"mobile\": \"" + emailOrMobile
							+ "\",\"hotpMobileVerification\": \"" + hotpMobileVerification + "\"" + "}";
				} else if (UserConstants.EMAIL.equalsIgnoreCase(loginIdentifierType)) {
					PRODUCT_JSON_STRING = "{" + "\"loginid\": \"" + emailOrMobile + "\",\"mail\": \"" + emailOrMobile
							+ "\",\"idmsuid\": \"" + emailOrMobile + "\",\"hotpEmailVerification\": \""
							+ hotpEmailVerification + "\"" + "}";
				}

				if (null != emailOrMobile && !emailOrMobile.isEmpty()) {
					LOGGER.info(AUDIT_REQUESTING_USER + uniqueIdentifier + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN
							+ AUDIT_OPENAM_API + AUDIT_OPENAM_UPDATE_CALL + uniqueIdentifier + AUDIT_LOG_CLOSURE);
					LOGGER.info("UserServiceImpl:userPinConfirmation -> : productService.updateUser: Requset :  -> ",
							PRODUCT_JSON_STRING);
					productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, uniqueIdentifier,
							PRODUCT_JSON_STRING);
				}
				EMAIL_CHANGE_LOGGER.info("{},{},{}", formatter.format(new Date()), uniqueIdentifier, emailOrMobile);
			}
			if ((null != confirmPIN.getUIFlag() && !confirmPIN.getUIFlag().isEmpty())
					&& (UserConstants.SET_USER_PR.equalsIgnoreCase(confirmPIN.getOperation()))) {

				/**
				 * Checking if password want to update
				 */

				if (null != confirmPIN.getPassword() && !confirmPIN.getPassword().isEmpty()) {

					PRODUCT_JSON_STRING = "{" + "\"loginid\": \"" + emailOrMobile + "\",\"userPassword\": \""
							+ confirmPIN.getPassword().trim() + "\"" + "}";
					LOGGER.info(AUDIT_REQUESTING_USER + uniqueIdentifier + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN
							+ AUDIT_OPENAM_API + AUDIT_OPENAM_GET_CALL + uniqueIdentifier + AUDIT_LOG_CLOSURE);
					LOGGER.info("UserServiceImpl:userPinConfirmation -> : productService.updateUser: Requset :  -> ",
							PRODUCT_JSON_STRING);
					productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, uniqueIdentifier,
							PRODUCT_JSON_STRING);
				}

			}

			LOGGER.info("authToken  " + authId);
			// After creating an user and while calling confirm pin api, if
			// ‘password’ comes in the request then call setPassword UIMS api
			// Otherwise if there is no password then call Activate User UIMS
			// api.
			if (null != confirmPIN.getIDMS_Profile_update_source()
					&& !UserConstants.UIMS.equalsIgnoreCase(confirmPIN.getIDMS_Profile_update_source())
					&& (null != confirmPIN.getOperation()
							&& UserConstants.USER_REGISTRATION.equalsIgnoreCase(confirmPIN.getOperation()))) {
				confirmPIN.setId(uniqueIdentifier);
				confirmPIN.setIDMS_Federated_ID__c(federationID);
				uimsUserManagerSoapService.activateUIMSUserConfirmPIN(confirmPIN, vNewCntValue.toString(),
						UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, loginIdentifierType, emailOrMobile);
			} else if (null != confirmPIN.getIDMS_Profile_update_source()
					&& !UserConstants.UIMS.equalsIgnoreCase(confirmPIN.getIDMS_Profile_update_source())
					&& (null != confirmPIN.getOperation()
							&& UserConstants.SET_USER_PR.equalsIgnoreCase(confirmPIN.getOperation()))
					&& (null != confirmPIN.getUIFlag() && !confirmPIN.getUIFlag().isEmpty())) {

				// Calling Async method of setUIMSPassword
				uimsUserManagerSoapService.setUIMSPassword(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
						uniqueIdentifier, federationID, confirmPIN.getPassword(), vNewCntValue.toString(),
						loginIdentifierType, emailOrMobile);
			}
			LOGGER.info("activateUIMSUserConfirmPIN is completed successfully::::");

		} catch (BadRequestException e) {
			e.printStackTrace();
			response.setStatus(errorStatus);
			response.setMessage(e.getMessage());
			response.setId(uniqueIdentifier);
			response.setFederation_Id(uniqueIdentifier);

			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
			LOGGER.error("Executing while creating the User :: -> " + e.getMessage());
			return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
		} catch (NotFoundException e) {
			e.printStackTrace();
			LOGGER.error(e.getMessage());
			// logic for PRM set password, if the user not found, call the
			// Global get user api
			// and retrieve the user details and pass it to create user
			if (null != confirmPIN.getIDMS_Profile_update_source() && (pickListValidator
					.validate(UserConstants.IDMS_BFO_profile, confirmPIN.getIDMS_Profile_update_source()))) {
				ifwAccessToken = ifwService.getIFWToken(UserConstants.CONTENT_TYPE_URL_FROM,
						UserConstants.IFW_GRANT_TYPE, ifwClientId, ifwClientSecret);
				productDocCtx = JsonPath.using(conf).parse(ifwAccessToken);
				String accessToken = productDocCtx.read("$.access_token");

				LOGGER.info("getSalesForceToken : => " + "PASSWORD_GRANT_TYPE : " + UserConstants.PR_GRANT_TYPE
						+ " salesForceClientId: " + salesForceClientId + " salesForceClientSecret :"
						+ salesForceClientSecret + " salesForceUserName: " + salesForceUserName
						+ " salesForcePassword :" + salesForcePassword);
				String bfoAuthorization = salesForceService.getSalesForceToken(UserConstants.CONTENT_TYPE_URL_FROM,
						UserConstants.PR_GRANT_TYPE, salesForceClientId, salesForceClientSecret, salesForceUserName,
						salesForcePassword);
				conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
				productDocCtx = JsonPath.using(conf).parse(bfoAuthorization);
				String bfoAuthorizationToken = productDocCtx.read("$.access_token");

				String authorization = "Bearer " + accessToken;

				Response globalGetUserResponse = ifwService.getUser(authorization, bfoAuthorizationToken,
						UserConstants.ACCEPT_TYPE_APP_JSON, "", "", "", "", confirmPIN.getIDMS_Federated_ID__c());
				productDocCtx = JsonPath.using(conf).parse(globalGetUserResponse);
				String responseAsString = globalGetUserResponse.readEntity(String.class);
				LOGGER.info("globalGetUserResponse : " + responseAsString);
				try {
					IFWCustomAttributesForWork idmsUser = objMapper.readValue(responseAsString,
							IFWCustomAttributesForWork.class);
					/**
					 * Added the below condition if user doesn't send the hashed
					 * token form api
					 */
					if (null == idmsUser.getIdmsHashedToken() || idmsUser.getIdmsHashedToken().isEmpty()) {
						idmsUser.setIdmsHashedToken(ChinaIdmsUtil.generateHashValue(confirmPIN.getPinCode()));
					}

					IFWUser ifwUser = mapper.map(idmsUser, IFWUser.class);
					LOGGER.info("iDMSUser : " + ifwUser);
					// creating the user
					CreateUserRequest createUserRequest = new CreateUserRequest();
					createUserRequest.setUserRecord(ifwUser);
					Response userRegistrationResponse = createUserService.userRegistration("", "", createUserRequest);
					if (200 == userRegistrationResponse.getStatus()) {
						// confirm the user
						ConfirmPinRequest confirmPinRequest = new ConfirmPinRequest();
						confirmPinRequest.setId(uniqueIdentifier);
						confirmPinRequest.setIDMS_Email_opt_in__c(confirmPIN.getIDMS_Email_opt_in__c());
						confirmPinRequest.setIDMS_Federated_ID__c(uniqueIdentifier);
						confirmPinRequest.setIDMS_Profile_update_source(confirmPIN.getIDMS_Profile_update_source());
						confirmPinRequest.setOperation(confirmPIN.getOperation());
						confirmPinRequest.setPassword(confirmPIN.getPassword());
						confirmPinRequest.setPinCode(confirmPIN.getPinCode());
						confirmPinRequest.setTncFlag(confirmPIN.getTncFlag());
						confirmPinRequest.setUIFlag("true");
						userPinConfirmation(confirmPinRequest);
					} else {
						return userRegistrationResponse;
					}

				} catch (IOException e1) {
					LOGGER.error("Executing while creating the User :: -> " + e1.getMessage());
					e1.printStackTrace();
				}

				// PRM success response
				Attributes attributes = new Attributes();
				IDMSUserRecord idmsUserRecord = new IDMSUserRecord();
				idmsUserRecord.setAttributes(attributes);
				idmsUserRecord.setId(uniqueIdentifier);
				idmsUserRecord.setIDMS_Federated_ID__c(uniqueIdentifier);

				PasswordRecoveryResponse passwordRecoveryResponse = new PasswordRecoveryResponse(idmsUserRecord);
				passwordRecoveryResponse.setStatus(successStatus);
				passwordRecoveryResponse.setMessage("PIN validated Successfully");

				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
				return Response.status(Response.Status.OK).entity(passwordRecoveryResponse).build();
			}
			response.setStatus(errorStatus);
			response.setMessage("404 Not Found");
			response.setId(uniqueIdentifier);
			response.setFederation_Id(uniqueIdentifier);

			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
			LOGGER.error("Executing while creating the User :: -> " + e.getMessage());
			return Response.status(Response.Status.NOT_FOUND).entity(response).build();
		}

		catch (Exception e) {
			e.printStackTrace();
			LOGGER.error(e.getMessage());
			response.setStatus(errorStatus);
			response.setMessage(UserConstants.SERVER_ERROR);
			response.setId(uniqueIdentifier);
			response.setFederation_Id(uniqueIdentifier);

			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(response).build();
		}

		Attributes attributes = new Attributes();
		IDMSUserRecord idmsUserRecord = new IDMSUserRecord();
		idmsUserRecord.setAttributes(attributes);
		idmsUserRecord.setId(uniqueIdentifier);
		idmsUserRecord.setIDMS_Federated_ID__c(uniqueIdentifier);

		PasswordRecoveryResponse passwordRecoveryResponse = new PasswordRecoveryResponse(idmsUserRecord);
		passwordRecoveryResponse.setStatus(successStatus);
		passwordRecoveryResponse.setMessage("PIN validated Successfully");

		elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
		return Response.status(Response.Status.OK).entity(passwordRecoveryResponse).build();
	}

}

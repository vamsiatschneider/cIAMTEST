package com.schneider.idms.service.impl;

import static com.se.idms.util.UserConstants.AUDIT_API_ADMIN;
import static com.se.idms.util.UserConstants.AUDIT_IMPERSONATING_USER;
import static com.se.idms.util.UserConstants.AUDIT_LOG_CLOSURE;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_API;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_GET_CALL;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_UPDATE_CALL;
import static com.se.idms.util.UserConstants.AUDIT_REQUESTING_USER;
import static com.se.idms.util.UserConstants.AUDIT_TECHNICAL_USER;

import java.io.InputStream;
import java.util.Date;

import javax.ws.rs.BadRequestException;
import javax.ws.rs.NotAuthorizedException;
import javax.ws.rs.NotFoundException;
import javax.ws.rs.core.Response;

import org.apache.commons.codec.binary.Base64;
import org.apache.cxf.helpers.IOUtils;
import org.json.simple.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.idms.model.ConfirmPinErrorResponse;
import com.idms.model.ResendPinRequest;
import com.idms.product.model.Attributes;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Option;
import com.schneider.idms.common.DirectApiConstants;
import com.schneider.idms.common.ErrorCodeConstants;
import com.schneider.idms.common.ErrorResponseCode;
import com.schneider.idms.model.IdmsUserConfirmRequest;
import com.schneider.idms.service.IConfirmPinUserService;
import com.se.idms.cache.utils.EmailConstants;
import com.se.idms.dto.IDMSUserRecord;
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

	@Override
	public Response userPinConfirmation(String authorization, String accept, String region,
			IdmsUserConfirmRequest confirmPinRequest) {
		LOGGER.info("Entered userPinConfirmation() -> Start");
		LOGGER.info("Parameter confirmRequest -> " + confirmPinRequest);

		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		ErrorResponseCode response = new ErrorResponseCode();
		ConfirmPinErrorResponse errorResponse = new ConfirmPinErrorResponse();
		DocumentContext productDocCtx = null;
		DocumentContext provProductDocCtx = null;
		String iPlanetDirectoryKey = null;
		String getUserResponse = null;
		String authId = null;
		String emailOrMobile = null;
		String loginIdentifierType = null;
		String PRODUCT_JSON_STRING = null;
		String openamVnew = null;
		Integer vNewCntValue = 0;
		boolean validPinStatus = false;
		ObjectMapper objMapper = new ObjectMapper();
		String uniqueIdentifier = null;
		String federationID = null;
		try {

			LOGGER.info("UserServiceImpl:userPinConfirmation -> : Request :  -> "
					+ objMapper.writeValueAsString(confirmPinRequest));

			if (null == confirmPinRequest.getPin() || confirmPinRequest.getPin().isEmpty()) {
				response.setCode(ErrorCodeConstants.BAD_REQUEST);
				response.setMessage(UserConstants.MANDATORY_PINCODE);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}

			if ((null == confirmPinRequest.getFederatedId() || confirmPinRequest.getFederatedId().isEmpty())) {
				response.setCode(ErrorCodeConstants.BAD_REQUEST);
				response.setMessage(UserConstants.MANDATORY_FEDERATION_ID);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}

			if (null == confirmPinRequest.getProfileLastUpdateSource()
					|| confirmPinRequest.getProfileLastUpdateSource().isEmpty()) {
				response.setCode(ErrorCodeConstants.BAD_REQUEST);
				response.setMessage(DirectApiConstants.PROFILELASTUPDATESOURCE);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}
			if ((null != confirmPinRequest.getProfileLastUpdateSource()
					&& !confirmPinRequest.getProfileLastUpdateSource().isEmpty())
					&& (!pickListValidator.validate(UserConstants.UPDATE_SOURCE,
							confirmPinRequest.getProfileLastUpdateSource()))) {
				response.setCode(ErrorCodeConstants.BAD_REQUEST);
				response.setMessage(UserConstants.INVALID_VALUE_IDMS + DirectApiConstants.PROFILELASTUPDATESOURCE);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}

			if ((null != confirmPinRequest.getOperation())
					&& !(UserConstants.USER_REGISTRATION.equalsIgnoreCase(confirmPinRequest.getOperation())
							|| UserConstants.SET_USER_PR.equalsIgnoreCase(confirmPinRequest.getOperation())
							|| UserConstants.UPDATE_USER_RECORD.equalsIgnoreCase(confirmPinRequest.getOperation()))) {

				response.setCode(ErrorCodeConstants.BAD_REQUEST);
				response.setMessage(UserConstants.OPERATION_MISMATCH);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}  else if (null == confirmPinRequest.getOperation() || confirmPinRequest.getOperation().isEmpty()) {

				response.setCode(ErrorCodeConstants.BAD_REQUEST);
				response.setMessage(UserConstants.OPERATION_MISMATCH);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}

			uniqueIdentifier = confirmPinRequest.getFederatedId();
			
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

				if (null != loginIdCheck && "userRegistration".equals(confirmPinRequest.getOperation())) {
					response.setMessage(ErrorCodeConstants.CONFLICT_MESSAGE);
					response.setCode(ErrorCodeConstants.CONFLICT_REQUEST);
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

				if (UserConstants.UPDATE_USER_RECORD.equalsIgnoreCase(confirmPinRequest.getOperation())) {
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
				response.setCode(ErrorCodeConstants.BAD_REQUEST);
				response.setMessage(ErrorCodeConstants.BADREQUEST_MESSAGE);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}

			LOGGER.info("User Reponse Document  " + productDocCtx.jsonString());

			/**
			 * HOTP Call 4 to Submit HOTP
			 */
			String userService = null;
			if (UserConstants.USER_REGISTRATION.equalsIgnoreCase(confirmPinRequest.getOperation())
					&& UserConstants.MOBILE.equals(loginIdentifierType)) {
				userService = UserConstants.CREATE_USER_SERVICE;
			} else if (UserConstants.USER_REGISTRATION.equalsIgnoreCase(confirmPinRequest.getOperation())
					&& UserConstants.EMAIL.equals(loginIdentifierType)) {
				userService = UserConstants.CREATE_USER_SERVICE;
			} 
			if (UserConstants.UPDATE_USER_RECORD.equalsIgnoreCase(confirmPinRequest.getOperation())
					&& UserConstants.MOBILE.equals(loginIdentifierType)) {
				userService = UserConstants.UPDATE_USER_SERVICE;
			} else if (UserConstants.UPDATE_USER_RECORD.equalsIgnoreCase(confirmPinRequest.getOperation())
					&& UserConstants.EMAIL.equals(loginIdentifierType)) {
				userService = UserConstants.UPDATE_USER_SERVICE;
			}
			try {

				LOGGER.info("productDocCtx.jsonString() - >" + productDocCtx.jsonString());
				LOGGER.info("userService" + userService);
				if (UserConstants.USER_REGISTRATION.equalsIgnoreCase(confirmPinRequest.getOperation())) {
					validPinStatus = sendEmail.validatePin(confirmPinRequest.getPin(), uniqueIdentifier);
				} else {
					validPinStatus = sendEmail.validatePin(confirmPinRequest.getPin(), uniqueIdentifier);
				}
				if (!validPinStatus) {
					throw new Exception("Pin got expired or invalid!!");
				}
			} catch (NotAuthorizedException e) {
				e.printStackTrace();
				response.setMessage(ErrorCodeConstants.UNAUTHORIZED_MESSAGE);
				response.setCode(ErrorCodeConstants.UNAUTHORIZED_REQUEST);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
				LOGGER.error("Executing while creating the User :: -> " + e.getMessage());
				return Response.status(Response.Status.UNAUTHORIZED).entity(response).build();
			} catch (Exception e) {
				e.printStackTrace();
				response.setCode(ErrorCodeConstants.BAD_REQUEST);
				errorResponse.setMessage(UserConstants.INVALID_PINCODE);
				errorResponse.setId(uniqueIdentifier);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
				LOGGER.error("Exception while confirming the User pin:: -> " + e.getMessage());
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
			}

			LOGGER.info(
					"UserServiceImpl:userPinConfirmation -> : Operation: Requset :  -> " + confirmPinRequest.getOperation());
			if (UserConstants.USER_REGISTRATION.equalsIgnoreCase(confirmPinRequest.getOperation())) {

				if (UserConstants.MOBILE.equalsIgnoreCase(loginIdentifierType)) {

					PRODUCT_JSON_STRING = "{" + "\"loginid\": \"" + emailOrMobile + "\",\"mobile\": \"" + emailOrMobile
							+ "\"" + "}";
				} else if (UserConstants.EMAIL.equalsIgnoreCase(loginIdentifierType)) {
					PRODUCT_JSON_STRING = "{" + "\"loginid\": \"" + emailOrMobile + "\",\"mail\": \"" + emailOrMobile
							+ "\"" + "}";
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

				if(pickListValidator.validate(UserConstants.IDMS_BFO_profile, confirmPinRequest.getProfileLastUpdateSource())){
					
					PRODUCT_JSON_STRING = "{" + "\"federationId\": \"" + uniqueIdentifier + "\",\"appName\": \"" + confirmPinRequest.getProfileLastUpdateSource() + "\"" + "}";
					
					LOGGER.info("going to call getSaleforceToken()");
					String salesForceToken = getSaleforceToken();
					LOGGER.info("getSaleforceToken() call finsihed");					
					LOGGER.info("Request sending to  salesForceService.populateActivationDate : "+ PRODUCT_JSON_STRING);
					LOGGER.info("going to call populateActivationDate() of SalesForceService");
					Response activationResponse = salesForceService.populateActivationDate(UserConstants.ACCEPT_TYPE_APP_JSON,salesForceToken, PRODUCT_JSON_STRING); 
					LOGGER.info("populateActivationDate() of SalesForceService finished");
					LOGGER.info("populateActivationDate Status :: "+ activationResponse.getStatus());
					if(200 != activationResponse.getStatus()){
						LOGGER.error("Failed to populate the activate date on PRM :: populateActivationDate -> " + IOUtils.toString((InputStream) activationResponse.getEntity()));
					}else{
						LOGGER.info("Successfully populated the activation date on PRM :: populateActivationDate -> " + IOUtils.toString((InputStream) activationResponse.getEntity()));
					}
					
				}

			}
			if (UserConstants.UPDATE_USER_RECORD.equalsIgnoreCase(confirmPinRequest.getOperation())) {

				if (UserConstants.MOBILE.equalsIgnoreCase(loginIdentifierType)) {

					PRODUCT_JSON_STRING = "{" + "\"loginid\": \"" + emailOrMobile + "\",\"mobile\": \"" + emailOrMobile + "\"" + "}";
				} else if (UserConstants.EMAIL.equalsIgnoreCase(loginIdentifierType)) {
					PRODUCT_JSON_STRING = "{" + "\"loginid\": \"" + emailOrMobile + "\",\"mail\": \"" + emailOrMobile
							+ "\",\"idmsuid\": \"" + emailOrMobile +"\"" + "}";
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
			

			LOGGER.info("authToken  " + authId);
			// After creating an user and while calling confirm pin api, if
			// ‘password’ comes in the request then call setPassword UIMS api
			// Otherwise if there is no password then call Activate User UIMS
			// api.
			if (null != confirmPinRequest.getProfileLastUpdateSource()
					&& !UserConstants.UIMS.equalsIgnoreCase(confirmPinRequest.getProfileLastUpdateSource())
					&& (null != confirmPinRequest.getOperation()
							&& UserConstants.USER_REGISTRATION.equalsIgnoreCase(confirmPinRequest.getOperation()))) {
				
				confirmPinRequest.setFederatedId(uniqueIdentifier);
				confirmPinRequest.setUimsFederatedId(federationID);
				directUIMSUserManagerSoapService.activateUIMSUserConfirmPIN(confirmPinRequest, vNewCntValue.toString(),
						UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, loginIdentifierType, emailOrMobile);
			} 
			LOGGER.info("activateUIMSUserConfirmPIN is completed successfully::::");

		} catch (BadRequestException e) {
			e.printStackTrace();
			response.setCode(ErrorCodeConstants.BAD_REQUEST);
			response.setMessage(ErrorCodeConstants.BADREQUEST_MESSAGE);

			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
			LOGGER.error("Executing while creating the User :: -> " + e.getMessage());
			return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
		} catch (NotFoundException e) {
			e.printStackTrace();
			LOGGER.error(e.getMessage());
			response.setCode(ErrorCodeConstants.NOTFOUND_REQUEST);
			response.setMessage(ErrorCodeConstants.NOTFOUND_MESSAGE);

			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
			LOGGER.error("Executing while creating the User :: -> " + e.getMessage());
			return Response.status(Response.Status.NOT_FOUND).entity(response).build();
		}

		catch (Exception e) {
			e.printStackTrace();
			LOGGER.error(e.getMessage());
			response.setCode(ErrorCodeConstants.SERVER_ERROR_REQUEST);
			response.setMessage(ErrorCodeConstants.SERVER_ERROR_MESSAGE);
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

	@SuppressWarnings("unchecked")
	@Override
	public Response resendPIN(String token, ResendPinRequest resendPinRequest) {

		LOGGER.info("Entered resendPIN() -> Start");
		LOGGER.info("Parameter token -> " + token);
		LOGGER.info("Parameter resendPinRequest -> " + resendPinRequest);
		

		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		JSONObject response = new JSONObject();
		DocumentContext productDocCtx = null;
		String userData = null;
		String loginIdentifier = null;
		String tmpPR = null;
		String PRODUCT_JSON_STRING = null;
		String iPlanetDirectoryKey = null;
		String sendEmailOptType = "";
		String resendId= "";
		ObjectMapper objMapper=new ObjectMapper();

		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		response.put(UserConstants.STATUS, UserConstants.STATUS_FAILD);
		try {
			LOGGER.info("UserServiceImpl:resendPIN : Request   -> " + objMapper.writeValueAsString(resendPinRequest));
			iPlanetDirectoryKey = getSSOToken();
			
			if ((null == resendPinRequest.getIdmsUserId() || resendPinRequest.getIdmsUserId().isEmpty())
					&& (null == resendPinRequest.getIDMS_Federated_ID__c()|| resendPinRequest.getIDMS_Federated_ID__c().isEmpty())
					&& (null == resendPinRequest.getFederationIdentifier()|| resendPinRequest.getFederationIdentifier().isEmpty())
					&& (null == resendPinRequest.getFederationId()|| resendPinRequest.getFederationId().isEmpty())) {
				response.put(UserConstants.STATUS, UserConstants.STATUS_FAILD);
				response.put(UserConstants.MESSAGE, UserConstants.RESPONSE_MESSAGE_NULL);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error(UserConstants.RESPONSE_MESSAGE_NULL);
				LOGGER.info("Time taken by UserServiceImpl.resendPIN() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();

			}else{
				
				if(null != resendPinRequest.getIdmsUserId() && !resendPinRequest.getIdmsUserId().isEmpty()){
					resendId = resendPinRequest.getIdmsUserId();
				} else if(null != resendPinRequest.getIDMS_Federated_ID__c() && !resendPinRequest.getIDMS_Federated_ID__c().isEmpty()){
					resendId = resendPinRequest.getIDMS_Federated_ID__c();
				}else if(null != resendPinRequest.getFederationId() && !resendPinRequest.getFederationId().isEmpty()){
					resendId = resendPinRequest.getFederationId();
				}else{
					resendId = resendPinRequest.getFederationIdentifier();
				}
				/*if((null == resendId)|| (resendId.isEmpty()) || "".equalsIgnoreCase(resendId)){
					resendId = resendPinRequest.getIDMS_Federated_ID__c();
				}*/
			}
			// Federation Identifier
			/*if (null == resendPinRequest.getFederationId() || resendPinRequest.getFederationId().isEmpty()) {
				response.put(UserConstants.STATUS, UserConstants.STATUS_FAILD);
				response.put(UserConstants.MESSAGE, UserConstants.MANDATORY_FEDERATION_ID);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.setPassword() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}*/

			if (null != resendId) {
				LOGGER.info(AUDIT_REQUESTING_USER + AUDIT_TECHNICAL_USER + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN
						+ AUDIT_OPENAM_API + AUDIT_OPENAM_GET_CALL + AUDIT_LOG_CLOSURE);
				userData = productService.getUser(iPlanetDirectoryKey, resendId);
				LOGGER.info("user data from Openam: " + userData);

				productDocCtx = JsonPath.using(conf).parse(userData);

				if (UserConstants.USER_REGISTRATION.equalsIgnoreCase(resendPinRequest.getOperation())) {
					loginIdentifier = productDocCtx.read("$.mobile[0]");
				} else if(UserConstants.UPDATE_USER_RECORD.equals(resendPinRequest.getOperation())){
					loginIdentifier = productDocCtx.read("$.newmobile[0]");
				}else{
					loginIdentifier = productDocCtx.read(JsonConstants.LOGIN_ID_LOWER_0);
					if (null == loginIdentifier) {
						loginIdentifier = productDocCtx.read(JsonConstants.LOGIN_ID_UPPER_0);
					}
				}
				if (null != loginIdentifier && validateMobile(loginIdentifier)) {

					tmpPR = productDocCtx.read("$.tmp_password[0]");
					if (null == tmpPR || tmpPR.isEmpty()) {
						tmpPR = generateRamdomPassWord();
					} else {
						tmpPR = new String(Base64.decodeBase64(tmpPR));
					}

					/*String hotpService = null;
					String userService = null;
					if (UserConstants.USER_REGISTRATION.equalsIgnoreCase(resendPinRequest.getOperation())) {
						hotpService = UserConstants.HOTP_MOBILE_USER_REGISTRATION;
						userService = UserConstants.CREATE_USER_SERVICE;
						sendEmailOptType = EmailConstants.USERREGISTRATION_OPT_TYPE;
					} else if (UserConstants.UPDATE_USER_RECORD.equalsIgnoreCase(resendPinRequest.getOperation())) {
						hotpService = UserConstants.HOTP_MOBILE_UPDATE;
						userService = UserConstants.UPDATE_USER_SERVICE;
						sendEmailOptType = EmailConstants.UPDATEUSERRECORD_OPT_TYPE;
					} else {
						hotpService = UserConstants.HOTP_MOBILE_RESET_PR;
						userService = UserConstants.CREATE_USER_SERVICE;
						sendEmailOptType = EmailConstants.SETUSERPWD_OPT_TYPE;
					}*/

					//PRODUCT_JSON_STRING = "{" + "\"userPassword\": \"" + tmpPR + "\"" + "}";

					LOGGER.info("UserServiceImpl:resendPIN : productService.updateUser : Request   -> " + PRODUCT_JSON_STRING);
					/*productService.updateUser(UserConstants.IPLANET_DIRECTORY_PRO + iPlanetDirectoryKey,
							resendPinRequest.getIdmsUserId(), PRODUCT_JSON_STRING);*/

					 //To update authId in openAM extended attribute
					//PRODUCT_JSON_STRING = sendOtp(hotpService, resendPinRequest.getIdmsUserId(), tmpPR, userService);
					
					String regestrationSource=productDocCtx.read("$.registerationSource[0]");
					if ((EmailConstants.UPDATEUSERRECORD_OPT_TYPE.equalsIgnoreCase(sendEmailOptType) && null != productDocCtx.read("$.newmail[0]"))
							|| EmailConstants.USERREGISTRATION_OPT_TYPE.equalsIgnoreCase(sendEmailOptType)
							|| EmailConstants.SETUSERPWD_OPT_TYPE.equalsIgnoreCase(sendEmailOptType)) {
						String otp = sendEmail.generateOtp(resendId);
						LOGGER.info("Successfully OTP generated for "+resendId);
						sendEmail.sendSMSMessage(otp, sendEmailOptType, resendId, regestrationSource);
						
						sendEmail.sendOpenAmMobileEmail(otp, sendEmailOptType, resendId, regestrationSource);
						//sendEmail.sendOpenAmEmail(otp, sendEmailOptType, resendId, regestrationSource);
					} else {
						response.put(UserConstants.STATUS, UserConstants.STATUS_FAILD);
						response.put(UserConstants.MESSAGE, UserConstants.RESEND_UPDATEOPTTYPE_ERROR);
						elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
						LOGGER.error(UserConstants.RESEND_UPDATEOPTTYPE_ERROR);
						LOGGER.info("Time taken by UserServiceImpl.resendPIN() : " + elapsedTime);
						return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(response).build();
					}
				} else {
					response.put(UserConstants.STATUS, UserConstants.STATUS_FAILD);
					response.put(UserConstants.MESSAGE, UserConstants.RESEND_ONLYMOBILE_ERROR_MESSAGE);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.error(UserConstants.RESEND_ONLYMOBILE_ERROR_MESSAGE);
					LOGGER.info("Time taken by UserServiceImpl.resendPIN() : " + elapsedTime);
					return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(response).build();
				}

			} else {
				response.put(UserConstants.STATUS, UserConstants.STATUS_FAILD);
				response.put(UserConstants.MESSAGE, UserConstants.RESPONSE_MESSAGE);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error(UserConstants.RESPONSE_MESSAGE);
				LOGGER.info("Time taken by UserServiceImpl.resendPIN() : " + elapsedTime);
				return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(response).build();
			}
		} catch (NotFoundException e) {
			e.printStackTrace();
			response.put(UserConstants.MESSAGE, UserConstants.ERROR_RESEND_PIN);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.resendPIN() : " + elapsedTime);
			LOGGER.error("Executing while Resending User PIN :: -> " + e.getMessage());
			//productService.sessionLogout(UserConstants.IPLANET_DIRECTORY_PRO+iPlanetDirectoryKey, "logout");
			return Response.status(Response.Status.NOT_FOUND).entity(response).build();
		} catch (BadRequestException e) {
			e.printStackTrace();
			response.put(UserConstants.MESSAGE, UserConstants.ERROR_RESEND_PIN);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.resendPIN() : " + elapsedTime);
			LOGGER.error("Executing while Resending User PIN :: -> " + e.getMessage());
			//productService.sessionLogout(UserConstants.IPLANET_DIRECTORY_PRO+iPlanetDirectoryKey, "logout");
			return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
		} catch (Exception e) {
			e.printStackTrace();
			response.put(UserConstants.MESSAGE, UserConstants.ERROR_RESEND_PIN);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.resendPIN() : " + elapsedTime);
			LOGGER.error("Executing while Resending User PIN :: -> " + e.getMessage());
			//productService.sessionLogout(UserConstants.IPLANET_DIRECTORY_PRO+iPlanetDirectoryKey, "logout");
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(response).build();
		}
		response.put(UserConstants.STATUS, successStatus);
		response.put(UserConstants.MESSAGE, "Pin Code has been sent successfully");
		elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		LOGGER.info("Time taken by UserServiceImpl.resendPIN() : " + elapsedTime);
		//productService.sessionLogout(UserConstants.IPLANET_DIRECTORY_PRO+iPlanetDirectoryKey, "logout");
		return Response.status(Response.Status.OK).entity(response).build();
	
	}
}

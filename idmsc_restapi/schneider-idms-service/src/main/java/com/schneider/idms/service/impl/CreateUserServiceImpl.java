package com.schneider.idms.service.impl;

import static com.se.idms.util.UserConstants.AUDIT_API_ADMIN;
import static com.se.idms.util.UserConstants.AUDIT_IMPERSONATING_USER;
import static com.se.idms.util.UserConstants.AUDIT_LOG_CLOSURE;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_API;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_AUTHENTICATE_CALL;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_USER_EXISTS_CALL;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_USER_REGISTRATION_CALL;
import static com.se.idms.util.UserConstants.AUDIT_REQUESTING_USER;
import static com.se.idms.util.UserConstants.AUDIT_TECHNICAL_USER;

import java.io.InputStream;
import java.net.URLEncoder;
import java.util.Date;

import javax.inject.Inject;
import javax.ws.rs.BadRequestException;
import javax.ws.rs.NotFoundException;
import javax.ws.rs.core.Response;

import org.apache.commons.codec.binary.Base64;
import org.apache.cxf.helpers.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.idms.model.CreateUserRequest;
import com.idms.model.CreateUserResponse;
import com.idms.model.IDMSUserResponse;
import com.idms.product.model.Attributes;
import com.idms.product.model.OpenAmUserRequest;
import com.idms.service.util.AsyncUtil;
import com.idms.service.util.ChinaIdmsUtil;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Option;
import com.schneider.idms.service.ICreateUserService;
import com.se.idms.cache.utils.EmailConstants;
import com.se.idms.dto.ErrorResponse;
import com.se.idms.dto.UserServiceResponse;
import com.se.idms.util.JsonConstants;
import com.se.idms.util.UimsConstants;
import com.se.idms.util.UserConstants;
import com.uims.authenticatedUsermanager.UserV6;
import com.uims.companymanager.CompanyV3;

@Service("createUserService")
public class CreateUserServiceImpl extends IdmsCommonServiceImpl implements ICreateUserService {

	/**
	 * Logger instance.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(CreateUserServiceImpl.class);

	@Inject
	public static UserServiceResponse userResponse;

	@Override
	public Response userRegistration(String clientId, String clientSecret, CreateUserRequest userRequest) {
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		CreateUserResponse sucessRespone;
		ErrorResponse errorResponse = new ErrorResponse();
		DocumentContext productDocCtx = null;
		DocumentContext productDocCtxCheck = null;
		String loginIdentifier = null;
		String identifierType = null;
		ObjectMapper objMapper = null;
		String userName = null;
		String iPlanetDirectoryKey = null;
		String userExists = null;
		boolean uimsAlreadyCreatedFlag = false;
		Response userCreation = null;
		UserServiceResponse userResponse = new UserServiceResponse();
		try {

			objMapper = new ObjectMapper();

			LOGGER.info("Entered userRegistration() -> Start");
			LOGGER.info("Parameter userRequest -> " + objMapper.writeValueAsString(userRequest));
			LOGGER.info("Parameter clientId -> " + clientId + " ,clientSecret -> " + clientSecret);

			// Step 1:
			/**
			 * Check mandatory values and user type (home/work)
			 */

			try {

				if (checkMandatoryFieldsFromRequest(userRequest.getUserRecord(), userResponse, true)) {
					errorResponse.setMessage(userResponse.getMessage());
					errorResponse.setStatus(userResponse.getStatus());
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
					LOGGER.error("Error while processing is " + errorResponse.getMessage());
					return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
				}

				/**
				 * R4 Release changes
				 */

				if (null != userRequest.getUIFlag() && UserConstants.TRUE.equalsIgnoreCase(userRequest.getUIFlag())) {
					if (((null != userRequest.getPassword() && !userRequest.getPassword().isEmpty()))
							&& !checkPasswordPolicy(userRequest.getPassword(),
									userRequest.getUserRecord().getFirstName(),
									userRequest.getUserRecord().getLastName())) {
						errorResponse.setStatus(errorStatus);
						errorResponse.setMessage(UserConstants.PR_POLICY);
						elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
						LOGGER.info("Time taken by UserServiceImpl.userRegistration() : " + elapsedTime);
						LOGGER.error("Error while processing is " + errorResponse.getMessage());
						return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
					}
				} else if (((null == userRequest.getUIFlag()
						|| !UserConstants.TRUE.equalsIgnoreCase(userRequest.getUIFlag()))
						&& (!UserConstants.UIMS
								.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Registration_Source__c())))
						&& (null != userRequest.getPassword() && !userRequest.getPassword().isEmpty())) {
					errorResponse.setStatus(errorStatus);
					errorResponse.setMessage(UserConstants.PASSWORD_WITH_USER_REG_BLCOKED);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by UserServiceImpl.userRegistration() : " + elapsedTime);
					LOGGER.error("Error while processing is " + errorResponse.getMessage());
					return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
				}
			} catch (Exception e) {
				e.printStackTrace();
				LOGGER.error("UserServiceImpl:userRegistration ->" + e.getMessage());
				errorResponse.setMessage(UserConstants.ATTRIBUTE_NOT_AVAILABELE);
				errorResponse.setStatus(userResponse.getStatus());
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
				LOGGER.error("Error while processing is " + errorResponse.getMessage());
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
			}

			LOGGER.info("CheckMandatoryFieldsFromRequest Completed ");

			if (null != userRequest.getUserRecord().getIDMS_Registration_Source__c() && UserConstants.UIMS
					.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Registration_Source__c())) {

				if (null == clientId || null == clientSecret) {
					userResponse.setStatus(errorStatus);
					userResponse.setMessage(UserConstants.UIMS_CLIENTID_SECRET);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by UserServiceImpl.updateUser() : " + elapsedTime);
					LOGGER.error("Error while processing is " + userResponse.getMessage());
					return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
				}

				if ((null != clientId && !clientId.equalsIgnoreCase(uimsClientId))
						|| (null != clientSecret && !clientSecret.equalsIgnoreCase(uimsClientSecret))) {
					userResponse.setStatus(errorStatus);
					userResponse.setMessage(UserConstants.INVALID_UIMS_CREDENTIALS);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by UserServiceImpl.updateUser() : " + elapsedTime);
					LOGGER.error("Error while processing is " + userResponse.getMessage());
					return Response.status(Response.Status.UNAUTHORIZED).entity(userResponse).build();
				}
			}
			/**
			 * Checking primary contact is coming from source map the property
			 * or if it is not coming from UIMS set it false otherwise true
			 * 
			 */

			if (null == userRequest.getUserRecord().getIDMSPrimaryContact__c()
					|| userRequest.getUserRecord().getIDMSPrimaryContact__c().isEmpty()) {

				if (UserConstants.UIMS.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Registration_Source__c())) {
					userRequest.getUserRecord().setIDMSPrimaryContact__c(UserConstants.FALSE);
				} else {
					userRequest.getUserRecord().setIDMSPrimaryContact__c(UserConstants.TRUE);
				}

			}

			if (null != userRequest.getUserRecord().getAdminCompanyFederatedId()
					&& !userRequest.getUserRecord().getAdminCompanyFederatedId().isEmpty()) {
				userRequest.getUserRecord().setIDMSPrimaryContact__c(UserConstants.FALSE);
			}

			// Step 2:

			OpenAmUserRequest openAmReq = mapper.map(userRequest, OpenAmUserRequest.class);

			/**
			 * call /json/authenticate to iplanetDirectoryPro token for admin
			 */
			LOGGER.info(AUDIT_REQUESTING_USER + AUDIT_TECHNICAL_USER + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN
					+ AUDIT_OPENAM_API + AUDIT_OPENAM_AUTHENTICATE_CALL + AUDIT_LOG_CLOSURE);
			iPlanetDirectoryKey = getSSOToken();

			LOGGER.info("Admin Token Generated SuccessFully {} ");

			/**
			 * check email and mobile phone for login identifier
			 */
			if ((null != userRequest.getUserRecord().getEmail() && !userRequest.getUserRecord().getEmail().isEmpty())
					&& (null != userRequest.getUserRecord().getMobilePhone()
							&& !userRequest.getUserRecord().getMobilePhone().isEmpty())) {

				openAmReq.getInput().getUser().setIdmsuid(userRequest.getUserRecord().getEmail());
				loginIdentifier = userRequest.getUserRecord().getEmail();
				identifierType = UserConstants.EMAIL;
			} else if ((null != userRequest.getUserRecord().getEmail())
					&& (!userRequest.getUserRecord().getEmail().isEmpty())) {

				openAmReq.getInput().getUser().setIdmsuid(userRequest.getUserRecord().getEmail());
				loginIdentifier = userRequest.getUserRecord().getEmail();
				identifierType = UserConstants.EMAIL;
			} else if ((null != userRequest.getUserRecord().getMobilePhone())
					&& (!userRequest.getUserRecord().getMobilePhone().isEmpty())) {
				openAmReq.getInput().getUser()
						.setIdmsuid(userRequest.getUserRecord().getMobilePhone() + "bridge-fo.com");
				loginIdentifier = userRequest.getUserRecord().getMobilePhone();
				identifierType = UserConstants.MOBILE;
			}

			LOGGER.info("LoginIdentifier Assigned SuccessFully  identifierType -> " + identifierType + " ,value -> "
					+ loginIdentifier);
			/**
			 * login identifier eq email or mobile call
			 * /se/users?_queryFilter=mail eq 'email value'
			 */
			LOGGER.info(AUDIT_REQUESTING_USER + AUDIT_TECHNICAL_USER + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN
					+ AUDIT_OPENAM_API + AUDIT_OPENAM_USER_EXISTS_CALL + loginIdentifier + AUDIT_LOG_CLOSURE);

			if (null != openAmReq.getInput().getUser().getRegisterationSource()
					&& UserConstants.UIMS.equalsIgnoreCase(openAmReq.getInput().getUser().getRegisterationSource())) {

				userExists = productService
						.checkUserExistsWithEmailMobile(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
								"federationID eq " + "\"" + openAmReq.getInput().getUser().getFederationID()
										+ "\" or loginid eq " + "\"" + URLEncoder.encode(loginIdentifier, "UTF-8")
										+ "\"");
			} else {
				userExists = productService.checkUserExistsWithEmailMobile(
						UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
						"loginid eq " + "\"" + URLEncoder.encode(loginIdentifier, "UTF-8") + "\"");
			}
			LOGGER.info(
					"UserServiceImpl:userRegistration -> productService.checkUserExistsWithEmailMobile :  userExists -> "
							+ userExists);
			productDocCtx = JsonPath.using(conf).parse(userExists);
			Integer resultCount = productDocCtx.read(JsonConstants.RESULT_COUNT);
			if (resultCount.intValue() > 0) {
				errorResponse.setStatus(errorStatus);
				errorResponse.setMessage(UserConstants.USER_EXISTS);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
				LOGGER.error("Error while creating user is " + errorResponse.getMessage());
				return Response.status(Response.Status.CONFLICT).entity(errorResponse).build();

			}

			LOGGER.info("CheckUserExistsWithEmailMobile Success");
			/**
			 * check login identifier e-email or mobile already handled in step2
			 */

			// Step 3:
			/**
			 * Check password exits and assign to openAM
			 */
			if (null != userRequest.getPassword() && !userRequest.getPassword().isEmpty()) {
				openAmReq.getInput().getUser().setUserPassword(userRequest.getPassword());
				openAmReq.getInput().getUser()
						.setTmp_password(new String(Base64.encodeBase64(userRequest.getPassword().getBytes())));
			} else {
				if (null != userRequest.getUserRecord().getIDMS_Federated_ID__c() && !userRequest.getUserRecord()
						.getIDMS_Federated_ID__c().startsWith(UserConstants.SOCIAL_LOGIN_PREFIX)) {
					openAmReq.getInput().getUser().setUserPassword(generateRamdomPassWord());
				} else if (null == userRequest.getUserRecord().getIDMS_Federated_ID__c()) {
					openAmReq.getInput().getUser().setUserPassword(generateRamdomPassWord());
				}
			}

			// new logic
			String userExistsInOpenam = productService.checkUserExistsWithEmailMobile(
					UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
					"mail eq " + "\"" + loginIdentifier + "\" or mobile eq " + "\"" + loginIdentifier + "\"");
			productDocCtxCheck = JsonPath.using(conf).parse(userExistsInOpenam);
			Integer resultCountCheck = productDocCtxCheck.read(JsonConstants.RESULT_COUNT);
			if ((resultCountCheck.intValue() > 0) && ((null == userRequest.getUserRecord().getIDMS_Federated_ID__c()
					|| userRequest.getUserRecord().getIDMS_Federated_ID__c().isEmpty()))) {
				// deleting already existing id in openam

				String userIdFromOpenam = productDocCtxCheck.read("$.result[0].username");
				Response deleteResponse = productService
						.deleteUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userIdFromOpenam);

				if (deleteResponse.getStatus() == 200) {
					LOGGER.info("Deleted the old entry from openam"
							+ IOUtils.toString((InputStream) deleteResponse.getEntity()));
				} else {
					LOGGER.info("Failed to delete the old entry from openam"
							+ IOUtils.toString((InputStream) deleteResponse.getEntity()) + " Status="
							+ deleteResponse.getStatus());
				}

				userName = userIdFromOpenam;
				uimsAlreadyCreatedFlag = true;

			} else {
				// Step 4:
				/**
				 * Generate Random login ID and map it to Open AM Username
				 * attribute Condition added for social login issue // (null ==
				 * userRequest.getUserRecord().getIDMS_Federated_ID__c()||
				 * userRequest.getUserRecord().getIDMS_Federated_ID__c().isEmpty
				 * ())){
				 * 
				 */

				if ((!UserConstants.UIMS.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Registration_Source__c()))
						&& (!pickListValidator.validate(UserConstants.APPLICATIONS,
								userRequest.getUserRecord().getIDMS_Registration_Source__c().toUpperCase()))
						&& (null == userRequest.getUserRecord().getIDMS_Federated_ID__c()
								|| userRequest.getUserRecord().getIDMS_Federated_ID__c().isEmpty())) {

					// new logic to generate fedId/userId
					// userName = UserConstants.UID_PREFIX + UUID.randomUUID().toString();
					userName = ChinaIdmsUtil.generateFedId();
				} else {
					userName = userRequest.getUserRecord().getIDMS_Federated_ID__c();
				}

			}
			openAmReq.getInput().getUser().setUsername(userName);
			/**
			 * Adding below line for R4 Release
			 */
			openAmReq.getInput().getUser().setFederationID(userName);

			/*
			 * openAmReq.getInput().getUser().setIdmsail_c("[]");
			 * openAmReq.getInput().getUser().setIdmsail_Applications_c("[]");
			 * openAmReq.getInput().getUser().setIdmsail_Features_c("[]");
			 * openAmReq.getInput().getUser().setIdmsail_Programs_c("[]");
			 */
			openAmReq.getInput().getUser().setCn(
					userRequest.getUserRecord().getFirstName() + " " + userRequest.getUserRecord().getLastName());

			// Step 5:
			/**
			 * call /json/se/selfservice/userRegistration
			 */

			// setting isInternal value to false
			openAmReq.getInput().getUser().setIDMSisInternal__c("FALSE");
			openAmReq.getInput().getUser().setEmailcount("0");
			String json = objMapper.writeValueAsString(openAmReq);
			json = json.replace("\"\"", "[]");
			LOGGER.info("Open AM  user  Request ------------->" + json);
			LOGGER.info(AUDIT_REQUESTING_USER + AUDIT_TECHNICAL_USER + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN
					+ AUDIT_OPENAM_API + AUDIT_OPENAM_USER_REGISTRATION_CALL + userAction + AUDIT_LOG_CLOSURE);

			/**
			 * The below or condition added for social login scenario for update
			 * user
			 * 
			 */

			if ((!pickListValidator.validate(UserConstants.IDMS_BFO_profile,
					userRequest.getUserRecord().getIDMS_Registration_Source__c()))
					&& ((pickListValidator.validate(UserConstants.APPLICATIONS,
							userRequest.getUserRecord().getIDMS_Registration_Source__c().toUpperCase()))
							|| ((null != userRequest.getUserRecord().getIDMS_Federated_ID__c()
									&& !userRequest.getUserRecord().getIDMS_Federated_ID__c().isEmpty())
									&& !UserConstants.UIMS.equalsIgnoreCase(
											userRequest.getUserRecord().getIDMS_Registration_Source__c())))) {
				// openAmReq.getInput().getUser().setUsername(null);
				json = objMapper.writeValueAsString(openAmReq.getInput().getUser());
				json = json.replace("\"\"", "[]");
				LOGGER.info(
						"UserServiceImpl:userRegistration -> productService.userRegistration :  Request -> " + json);
				productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userName, json);

			} else {
				LOGGER.info(
						"UserServiceImpl:userRegistration -> productService.userRegistration :  Request -> " + json);
				userCreation = productService.userRegistration(iPlanetDirectoryKey, userAction, json);
				// return productDocCtx;
				if (userCreation.getStatus() != 200) {
					// productService.sessionLogout(UserConstants.IPLANET_DIRECTORY_PRO+iPlanetDirectoryKey,
					// "logout");
					throw new Exception("Exception while Registering User in Open "
							+ IOUtils.toString((InputStream) userCreation.getEntity()));
				}
				LOGGER.info("User Registered Succssfully :: -> "
						+ IOUtils.toString((InputStream) userCreation.getEntity()));
			}
			// log for user stats
			AsyncUtil.generateCSV(registrationCsvPath,
					new Date() + "," + userName + "," + userRequest.getUserRecord().getIDMS_User_Context__c() + ","
							+ userRequest.getUserRecord().getIDMS_Registration_Source__c());

			String version = "{" + "\"V_Old\": \"" + UserConstants.V_OLD + "\",\"V_New\": \"" + UserConstants.V_NEW
					+ "\"" + "}";
			// Adding v_old and v_new
			LOGGER.info("UserServiceImpl:userRegistration -> productService.updateUser :  version -> " + version);
			productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userName, version);

			// Checking profile update and update login id

			if (null == openAmReq.getInput().getUser().getRegisterationSource()
					|| !UserConstants.UIMS.equalsIgnoreCase(openAmReq.getInput().getUser().getRegisterationSource())) {

				if (UserConstants.EMAIL.equalsIgnoreCase(identifierType)) {

					// if Registration source is not PRM then send mail
					if (null != userRequest.getUserRecord().getIDMS_Registration_Source__c()
							&& (!pickListValidator.validate(UserConstants.IDMS_BFO_profile,
									userRequest.getUserRecord().getIDMS_Registration_Source__c()))) {

						String otp = sendEmail.generateOtp(userName);
						LOGGER.info("Successfully OTP generated for " + userName);
						sendEmail.sendOpenAmEmail(otp, EmailConstants.USERREGISTRATION_OPT_TYPE, userName,
								userRequest.getUserRecord().getIDMS_Registration_Source__c());
					} else if (null != userRequest.getUserRecord().getIDMS_Registration_Source__c()
							&& (pickListValidator.validate(UserConstants.IDMS_BFO_profile,
									userRequest.getUserRecord().getIDMS_Registration_Source__c()))) {

						// HashedToken field is to store the hashed pin which comes from global IDMS
						sendEmail.storePRMOtp(userName, userRequest.getUserRecord().getIdmsHashedToken());
					}
				} else if (identifierType.equalsIgnoreCase(UserConstants.MOBILE)) {

					/**
					 * we need check when we are working for mobile scenario
					 */

					String otp = sendEmail.generateOtp(userName);
					LOGGER.info("Successfully OTP generated for " + userName);
					sendEmail.sendSMSMessage(otp, EmailConstants.USERREGISTRATION_OPT_TYPE, userName,
							userRequest.getUserRecord().getIDMS_Registration_Source__c());
					sendEmail.sendOpenAmMobileEmail(otp, EmailConstants.USERREGISTRATION_OPT_TYPE, userName,
							userRequest.getUserRecord().getIDMS_Profile_update_source__c());
				}
			}
		} catch (BadRequestException e) {
			e.printStackTrace();
			userResponse.setMessage(UserConstants.ERROR_CREATE_USER);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
			LOGGER.error("Executing while user Registration :: -> " + e.getMessage());
			return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
		} catch (NotFoundException e) {
			e.printStackTrace();
			userResponse.setMessage(UserConstants.ERROR_CREATE_USER);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
			LOGGER.error("Executing while user Registration :: -> " + e.getMessage());
			return Response.status(Response.Status.NOT_FOUND).entity(userResponse).build();
		} catch (Exception e) {
			e.printStackTrace();
			userResponse.setMessage(UserConstants.ERROR_CREATE_USER);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
			LOGGER.error("Executing while user Registration :: -> " + e.getMessage());
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(userResponse).build();
		}

		userRequest.getUserRecord().setIDMS_Federated_ID__c(userName);
		LOGGER.info(
				"UserServiceImpl:userRegistration -> uimsUserManagerSoapService :  !uimsAlreadyCreatedFlag Value is -> "
						+ !uimsAlreadyCreatedFlag);
		if (!uimsAlreadyCreatedFlag && null != userRequest.getUserRecord().getIDMS_Registration_Source__c()
				&& !UserConstants.UIMS.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Registration_Source__c())) {
			// mapping IFW request to UserCompany
			CompanyV3 company = mapper.map(userRequest, CompanyV3.class);
			UserV6 identity = mapper.map(userRequest, UserV6.class);
			// forcedFederatedId = "cn00"+ UUID.randomUUID().toString();
			// Calling Async method createUIMSUserAndCompany
			uimsUserManagerSoapService.createUIMSUserAndCompany(UimsConstants.CALLER_FID, identity,
					userRequest.getUserRecord().getIDMS_User_Context__c(), company, userName,
					UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, UserConstants.V_NEW,
					userRequest.getPassword(), userName, userRequest);
			userRequest.getUserRecord().setIDMS_Federated_ID__c(userName);// federated
																			// id
																			// for
																			// IDMS
		}

		sucessRespone = new CreateUserResponse();
		sucessRespone.setStatus(successStatus);
		sucessRespone.setMessage(UserConstants.CREATE_USER_SUCCESS_MESSAGE);
		Attributes attributes = new Attributes();
		attributes.setType("User");
		userRequest.getUserRecord().setAttributes(attributes);
		userRequest.getUserRecord().setId(userName);
		userRequest.getUserRecord().setDefaultCurrencyIsoCode("CNY");
		IDMSUserResponse idmsResponse = mapper.map(userRequest, IDMSUserResponse.class);
		idmsResponse.setActive(userRequest.getUserRecord().isActive());
		sucessRespone.setIDMSUserRecord(idmsResponse);
		elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
		return Response.status(Response.Status.OK).entity(sucessRespone).build();
	}

}

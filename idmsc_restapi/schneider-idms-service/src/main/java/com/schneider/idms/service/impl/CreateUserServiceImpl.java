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
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.util.Date;

import javax.inject.Inject;
import javax.ws.rs.BadRequestException;
import javax.ws.rs.NotAuthorizedException;
import javax.ws.rs.NotFoundException;
import javax.ws.rs.core.Response;

import org.apache.cxf.helpers.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.idms.product.model.OpenAmUserRequest;
import com.idms.service.util.AsyncUtil;
import com.idms.service.util.ChinaIdmsUtil;
import com.idms.service.util.UserServiceUtil;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Option;
import com.schneider.idms.common.DirectApiConstants;
import com.schneider.idms.common.ErrorCodeConstants;
import com.schneider.idms.common.ResponseCodeStatus;
import com.schneider.idms.mapper.DirectApiIdmsMapper;
import com.schneider.idms.model.IdmsCreateUserResponse;
import com.schneider.idms.model.IdmsUserRequest;
import com.schneider.idms.service.ICreateUserService;
import com.schneider.ims.service.uimsv2.CompanyV3;
import com.se.idms.cache.utils.EmailConstants;
import com.se.idms.util.JsonConstants;
import com.se.idms.util.UserConstants;
import com.uims.authenticatedUsermanager.UserV6;

@Service("createUserService")
public class CreateUserServiceImpl extends IdmsCommonServiceImpl implements ICreateUserService {

	/**
	 * Logger instance.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(CreateUserServiceImpl.class);
	
	@Inject
	private DirectApiIdmsMapper mapper;
	
	//CODE-RE-STRUCTURING
	@Value("${caller.fid}")
	private String CALLER_FID;

	@Override
	public Response userRegistration(String authorization, String secretToken, String accept, String region,
			IdmsUserRequest userRequest) {
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		ResponseCodeStatus errorResponse = new ResponseCodeStatus();
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
		OpenAmUserRequest openAmReq = null;
		String longLiveHashedToken = null;
		try {
			
			
			/**
			 * Check the authorization is valid or not
			 */

			
			
			objMapper = new ObjectMapper();

			LOGGER.info("Entered userRegistration() -> Start");
			LOGGER.info("Parameter userRequest -> " + objMapper.writeValueAsString(userRequest));
			LOGGER.info("Parameter authorization -> " + authorization + " ,secretToken -> " + secretToken);
			LOGGER.info("Parameter accept -> " + accept + " ,region -> " + region);

			// Step 1:
			/**
			 * Check mandatory values and user type (home/work)
			 */

			try {
				
				
				if ((null == authorization || authorization.isEmpty())
						&& (null == secretToken || secretToken.isEmpty())) {
					errorResponse.setMessage(ErrorCodeConstants.BADREQUEST_MESSAGE);
					errorResponse.setStatus(ErrorCodeConstants.ERROR);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
					LOGGER.error("Authorization is null or empty");
					return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
				}
				
				
				if(!UserConstants.ACCEPT_TYPE_APP_JSON.equalsIgnoreCase(accept)){
					errorResponse.setMessage(ErrorCodeConstants.BADREQUEST_MESSAGE);
					errorResponse.setStatus(ErrorCodeConstants.ERROR);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
					LOGGER.error("Header Param Accept is invalid");
					return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
				}
				
				if(!DirectApiConstants.COUNTRY_CHINA.equalsIgnoreCase(region)){
					errorResponse.setMessage(ErrorCodeConstants.BADREQUEST_MESSAGE);
					errorResponse.setStatus(ErrorCodeConstants.ERROR);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
					LOGGER.error("Region is not China");
					return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
				}
				
				/**
				 * hashing secret token using RSA-256
				 */
				
				if(null != secretToken && !secretToken.isEmpty()){
					longLiveHashedToken = ChinaIdmsUtil.generateHashValue(secretToken);
					LOGGER.info("longLiveHashedToken="+longLiveHashedToken);
				}
				
				// Validation_authorization_token
				if (!((null!=secretToken && directApiSecretToken.equalsIgnoreCase(longLiveHashedToken)) ||
						(null!=authorization && getTechnicalUserDetails(DirectApiConstants.BEAR_STRING+authorization)))) {
					errorResponse.setMessage("Mandatory Validation: Authorization/IDMS-Authorization token is invalid");
					errorResponse.setStatus(ErrorCodeConstants.ERROR);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
					LOGGER.error("Unauthorized or session expired");
					return Response.status(Response.Status.UNAUTHORIZED).entity(errorResponse).build();
				}
				
				/**
				 * Validate the data quality I - check mandatory information
				 */

				if (checkMandatoryFieldsForDirectAPIRequest(userRequest, errorResponse, true)) {
					errorResponse.setMessage(errorResponse.getMessage());
					errorResponse.setStatus(ErrorCodeConstants.ERROR);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
					LOGGER.error("DirectAPI:userRegistration - Error is: " + errorResponse.getMessage());
					return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
				}
			} 
			catch (NotAuthorizedException e) {
				LOGGER.error("DirectAPI:userRegistration ->" + e.getMessage());
				errorResponse.setMessage(ErrorCodeConstants.UNAUTHORIZED_MESSAGE);
				errorResponse.setStatus(ErrorCodeConstants.ERROR);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
				LOGGER.error("Unauthorized or session expired"+e.getMessage());
				return Response.status(Response.Status.UNAUTHORIZED).entity(errorResponse).build();
			}
			catch (BadRequestException e) {
				LOGGER.error("DirectAPI:userRegistration ->" + e.getMessage());
				errorResponse.setMessage(UserConstants.ATTRIBUTE_NOT_AVAILABELE);
				errorResponse.setStatus(ErrorCodeConstants.ERROR);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
				LOGGER.error("DirectAPI:userRegistration Bad request:"+e.getMessage());
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
			}
			catch (IllegalArgumentException e) {
				
				LOGGER.error("DirectAPI:userRegistration ->" + e.getMessage());
				errorResponse.setMessage(ErrorCodeConstants.BADREQUEST_MESSAGE);
				errorResponse.setStatus(ErrorCodeConstants.ERROR);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
				LOGGER.error("DirectAPI:userRegistration - IllegalArgumentException:"+e.getMessage());
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
			}
			catch (Exception e) {
				
				LOGGER.error("DirectAPI:userRegistration ->" + e.getMessage(),e);
				errorResponse.setMessage(UserConstants.ATTRIBUTE_NOT_AVAILABELE);
				errorResponse.setStatus(ErrorCodeConstants.ERROR);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
				LOGGER.error("DirectAPI:userRegistration - some other Exception:"+e.getMessage());
				return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(errorResponse).build();
			}

			LOGGER.info("CheckMandatoryFieldsFromRequest Completed ");

			/**
			 * Checking primary contact is coming from source map the property
			 * or if it is not coming from UIMS set it false otherwise true
			 * 
			 */

			if (null == userRequest.getPrimaryContact()
					|| userRequest.getPrimaryContact().isEmpty()) {

				if (UserConstants.UIMS.equalsIgnoreCase(userRequest.getRegistrationSource())) {
					userRequest.setPrimaryContact(UserConstants.FALSE);
				} else {
					userRequest.setPrimaryContact(UserConstants.TRUE);
				}

			}

			if (null != userRequest.getAdminCompanyFederatedId()
					&& !userRequest.getAdminCompanyFederatedId().isEmpty()) {
				userRequest.setPrimaryContact(UserConstants.FALSE);
			}
			
			/**
			 * Checking companyFederation is not passing generating new one and adding
			 */
			
			if ((UserConstants.USER_CONTEXT_WORK.equalsIgnoreCase(userRequest.getUserContext())
					|| UserConstants.USER_CONTEXT_WORK_1.equalsIgnoreCase(userRequest.getUserContext()))
					&& (null == userRequest.getCompanyFederatedId() || userRequest.getCompanyFederatedId().isEmpty())) {

				userRequest.setCompanyFederatedId(ChinaIdmsUtil.generateFedId());

			}

			// Step 2:

			openAmReq = mapper.map(userRequest, OpenAmUserRequest.class);
			
			/**
			 * TODO RegistrationSource mapping not happening need to check why
			 * 
			 */
			
			openAmReq.getInput().getUser().setRegisterationSource(userRequest.getRegistrationSource());
			/*openAmReq.getInput().getUser().setChannel(userRequest.getChannel());
			openAmReq.getInput().getUser().setSubchannel(userRequest.getSubChannel());*/
			LOGGER.info("Mapped openAmRequest: "+objMapper.writeValueAsString(openAmReq));
			
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
			if ((null != userRequest.getEmail() && !userRequest.getEmail().isEmpty())
					&& (null != userRequest.getMobilePhone()
							&& !userRequest.getMobilePhone().isEmpty())) {

				openAmReq.getInput().getUser().setIdmsuid(userRequest.getEmail());
				loginIdentifier = userRequest.getEmail();
				identifierType = UserConstants.EMAIL;
			} else if ((null != userRequest.getEmail())
					&& (!userRequest.getEmail().isEmpty())) {

				openAmReq.getInput().getUser().setIdmsuid(userRequest.getEmail());
				loginIdentifier = userRequest.getEmail();
				identifierType = UserConstants.EMAIL;
			} else if ((null != userRequest.getMobilePhone())
					&& (!userRequest.getMobilePhone().isEmpty())) {
				openAmReq.getInput().getUser()
						.setIdmsuid(userRequest.getMobilePhone() + "bridge-fo.com");
				loginIdentifier = userRequest.getMobilePhone();
				identifierType = UserConstants.MOBILE;
				if(null != userRequest.getEmail() && userRequest.getEmail().isEmpty()){
					userRequest.setEmail(null);
				}
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
				LOGGER.info("Start: For reg source UIMS, calling checkUserExistsWithEmailMobile() of OpenAMService");
				userExists = UserServiceUtil.checkUserExistsBasedOnFRVersion(productService, frVersion, UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
								"federationID eq " + "\"" + openAmReq.getInput().getUser().getFederationID()
										+ "\" or loginid eq " + "\"" + URLEncoder.encode(URLDecoder.decode(loginIdentifier,"UTF-8"), "UTF-8")
										+ "\"");
				LOGGER.info("End: For reg source UIMS, calling checkUserExistsWithEmailMobile() of OpenAMService finished");
			} else {
				LOGGER.info("Start: For reg source non-UIMS, calling checkUserExistsWithEmailMobile() of OpenAMService");
				userExists = UserServiceUtil.checkUserExistsBasedOnFRVersion(productService, frVersion,
						UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
						"loginid eq " + "\"" + URLEncoder.encode(URLDecoder.decode(loginIdentifier,"UTF-8"), "UTF-8") + "\"");
				LOGGER.info("End: For reg source non-UIMS, calling checkUserExistsWithEmailMobile() of OpenAMService finished");
			}
			LOGGER.info(
					"DirectAPI:userRegistration -> productService.checkUserExistsWithEmailMobile :  userExists -> "
							+ userExists);
			productDocCtx = JsonPath.using(conf).parse(userExists);
			Integer resultCount = productDocCtx.read(JsonConstants.RESULT_COUNT);
			LOGGER.info("ResultCount from checkUserExist:"+resultCount);
			if (resultCount.intValue() > 0) {
				errorResponse.setStatus(ErrorCodeConstants.ERROR);
				errorResponse.setMessage(ErrorCodeConstants.CONFLICT_MESSAGE);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
				LOGGER.error("User already exist/registered in openAM");
				return Response.status(Response.Status.CONFLICT).entity(errorResponse).build();

			}

			LOGGER.info("CheckUserExistsWithEmailMobile Success");
			/**
			 * check login identifier e-email or mobile already handled in step2
			 */

			// new logic
			String userExistsInOpenam = UserServiceUtil.checkUserExistsBasedOnFRVersion(productService, frVersion,
					UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
					"mail eq " + "\"" + loginIdentifier + "\" or mobile eq " + "\"" + loginIdentifier + "\"");
			productDocCtxCheck = JsonPath.using(conf).parse(userExistsInOpenam);
			Integer resultCountCheck = productDocCtxCheck.read(JsonConstants.RESULT_COUNT);
			
			//delete records from OPENAM if application is PRM
			if ((resultCountCheck.intValue() > 0) && (null != userRequest.getRegistrationSource() && pickListValidator
					.validate(UserConstants.APPLICATIONS, userRequest.getRegistrationSource().toUpperCase()))) {
				for (int i = 0; i < resultCountCheck.intValue(); i++) {
					String isActivated = productDocCtxCheck.read("$.result[" + i + "].isActivated[0]");
					if (!Boolean.valueOf(isActivated)) {
						Response deleteResponse = UserServiceUtil.deleteUserBasedOnFRVersion(productService, frVersion,
								UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
								productDocCtxCheck.read("$.result[" + i + "].username"));
						if (deleteResponse.getStatus() == 200) {
							LOGGER.info("Deleted the old not activated PRM entry from openam"
									+ IOUtils.toString((InputStream) deleteResponse.getEntity()));
						} else {
							LOGGER.error("Failed to delete the old not activated PRM entry from openam"
									+ IOUtils.toString((InputStream) deleteResponse.getEntity()) + " Status="
									+ deleteResponse.getStatus());
						}
					}
				}
			}
			
			
			if ((resultCountCheck.intValue() > 0) && ((null == userRequest.getFederationId()
					|| userRequest.getFederationId().isEmpty()))) {
				// deleting already existing id in openam

				String userIdFromOpenam = productDocCtxCheck.read("$.result[0].username");
				Response deleteResponse = UserServiceUtil.deleteUserBasedOnFRVersion(productService, frVersion,
						UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userIdFromOpenam);

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
				 * userRequest.getIDMS_Federated_ID__c()||
				 * userRequest.getIDMS_Federated_ID__c().isEmpty
				 * ())){
				 * 
				 */

				if ((!UserConstants.UIMS.equalsIgnoreCase(userRequest.getRegistrationSource()))
						&& (!pickListValidator.validate(UserConstants.APPLICATIONS,
								userRequest.getRegistrationSource().toUpperCase()))
						&& (null == userRequest.getFederationId()
								|| userRequest.getFederationId().isEmpty())) {

					// new logic to generate fedId/userId
					// userName = UserConstants.UID_PREFIX + UUID.randomUUID().toString();
					userName = ChinaIdmsUtil.generateFedId();
				} else {
					userName = userRequest.getFederationId();
				}

			}
			openAmReq.getInput().getUser().setUsername(userName);
			openAmReq.getInput().getUser().setUserPassword(generateRamdomPassWord());
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
					userRequest.getFirstName() + " " + userRequest.getLastName());

			// Step 5:
			/**
			 * call /json/se/selfservice/userRegistration
			 */

			// setting isInternal value to false
			openAmReq.getInput().getUser().setIDMSisInternal__c("FALSE");
			openAmReq.getInput().getUser().setEmailcount("0");
			String json = objMapper.writeValueAsString(openAmReq);
			json = json.replace("\"\"", "[]");
			LOGGER.info("Open AM  new user  Request ------------->" + json);
			LOGGER.info(AUDIT_REQUESTING_USER + AUDIT_TECHNICAL_USER + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN
					+ AUDIT_OPENAM_API + AUDIT_OPENAM_USER_REGISTRATION_CALL + userAction + AUDIT_LOG_CLOSURE);

			/**
			 * The below or condition added for social login scenario for update
			 * user
			 * 
			 */

			if ((!pickListValidator.validate(UserConstants.IDMS_BFO_profile,
					userRequest.getRegistrationSource()))
					&& ((pickListValidator.validate(UserConstants.APPLICATIONS,
							userRequest.getRegistrationSource().toUpperCase()))
							|| ((null != userRequest.getFederationId()
									&& !userRequest.getFederationId().isEmpty())
									&& !UserConstants.UIMS.equalsIgnoreCase(
											userRequest.getRegistrationSource())))) {
				// openAmReq.getInput().getUser().setUsername(null);
				json = objMapper.writeValueAsString(openAmReq.getInput().getUser());
				json = json.replace("\"\"", "[]");
				LOGGER.info(
						"DirectAPI:userRegistration -> Json Request social login user -> " + json);
				LOGGER.info("Start: calling updateUser() of OPENAM for social login");
				UserServiceUtil.updateUserBasedOnFRVersion(productService, frVersion, UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userName, json);
				LOGGER.info("End: calling updateUser() of OPENAM for social login");

			} else {
				LOGGER.info(
						"Start: DirectAPI:userRegistration -> calling  userRegistration() to create new user:" + json);
				userCreation = UserServiceUtil.userRegistrationBasedOnFRVersion(productService, frVersion, iPlanetDirectoryKey, userAction, json);
				LOGGER.info(
						"End: DirectAPI:userRegistration -> finished  userRegistration() to create new user:");
				// return productDocCtx;
				if (userCreation.getStatus() != 200) {
					// productService.sessionLogout(UserConstants.IPLANET_DIRECTORY_PRO+iPlanetDirectoryKey,
					// "logout");
					throw new Exception("Exception while Registering User in OpenAM "
							+ IOUtils.toString((InputStream) userCreation.getEntity()));
				}
				LOGGER.info("User Registered Succssfully in OpenAM:: -> "
						+ IOUtils.toString((InputStream) userCreation.getEntity()));
			}
			// log for user stats
			AsyncUtil.generateCSV(registrationCsvPath,
					new Date() + "," + userName + "," + userRequest.getUserContext() + ","
							+ userRequest.getRegistrationSource());

			String version = "{" + "\"V_Old\": \"" + UserConstants.V_OLD + "\",\"V_New\": \"" + UserConstants.V_NEW
					+ "\"" + "}";
			// Adding v_old and v_new
			LOGGER.info("Start: DirectAPI:userRegistration -> For new user, updating version in openam-> " + version);
			UserServiceUtil.updateUserBasedOnFRVersion(productService, frVersion, UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userName, version);
			LOGGER.info("End: DirectAPI:userRegistration -> For new user, updating version in openam-> " + version);

			// Checking profile update and update login id

			if (null == openAmReq.getInput().getUser().getRegisterationSource()
					|| !UserConstants.UIMS.equalsIgnoreCase(openAmReq.getInput().getUser().getRegisterationSource())) {

				if (UserConstants.EMAIL.equalsIgnoreCase(identifierType)) {

					// if Registration source is not PRM then send mail
					if (null != userRequest.getRegistrationSource()
							&& (!pickListValidator.validate(UserConstants.IDMS_BFO_profile,
									userRequest.getRegistrationSource()))) {

						String otp = sendEmail.generateOtp(userName);
						LOGGER.info("Successfully OTP generated for email user, userName=" + userName+" ,OTP="+otp);
						sendEmail.sendOpenAmEmail(null, otp, EmailConstants.USERREGISTRATION_OPT_TYPE, userName,
								userRequest.getRegistrationSource(), null, null);
					} else if (null != userRequest.getRegistrationSource()
							&& (pickListValidator.validate(UserConstants.IDMS_BFO_profile,
									userRequest.getRegistrationSource()))) {

						// HashedToken field is to store the hashed pin which comes from global IDMS
						sendEmail.storePRMOtp(userName, userRequest.getHashedPin());
					}
				} else if (identifierType.equalsIgnoreCase(UserConstants.MOBILE)) {

					/**
					 * we need check when we are working for mobile scenario
					 */

					String otp = sendEmail.generateOtp(userName);
					LOGGER.info("Successfully OTP generated for mobile user, userName=" + userName+" ,OTP="+otp);
					sendEmail.sendSMSNewGateway(otp, EmailConstants.USERREGISTRATION_OPT_TYPE, userName,
							userRequest.getRegistrationSource());
					sendEmail.sendOpenAmMobileEmail(otp, EmailConstants.USERREGISTRATION_OPT_TYPE, userName,
							userRequest.getProfileLastUpdateSource());
				}
			}
		} catch (BadRequestException e) {
			
			errorResponse.setMessage(ErrorCodeConstants.BADREQUEST_MESSAGE);
			errorResponse.setStatus(ErrorCodeConstants.ERROR);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
			LOGGER.error("Executing while user Registration :: -> " + e.getMessage());
			return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
		} catch (NotFoundException e) {
			
			errorResponse.setMessage(ErrorCodeConstants.NOTFOUND_MESSAGE);
			errorResponse.setStatus(ErrorCodeConstants.ERROR);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
			LOGGER.error("Executing while user Registration :: -> " + e.getMessage());
			return Response.status(Response.Status.NOT_FOUND).entity(errorResponse).build();
		} catch (Exception e) {
			
			errorResponse.setMessage(ErrorCodeConstants.SERVER_ERROR_MESSAGE);
			errorResponse.setStatus(ErrorCodeConstants.ERROR);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
			LOGGER.error("Executing while user Registration :: -> " + e.getMessage(),e);
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(errorResponse).build();
		}

		userRequest.setFederationId(userName);
		LOGGER.info("DirectAPI:userRegistration -> !uimsAlreadyCreatedFlag Value is -> "+ !uimsAlreadyCreatedFlag);
		Integer resultCountCheck = checkCompanyMappedOtherUsers(userRequest.getCompanyFederatedId());
		if (!uimsAlreadyCreatedFlag && null != userRequest.getRegistrationSource()
				&& !UserConstants.UIMS.equalsIgnoreCase(userRequest.getRegistrationSource())) {
			// mapping IFW request to UserCompany
			CompanyV3 company = mapper.map(userRequest, CompanyV3.class);
			UserV6 identity = mapper.map(userRequest, UserV6.class);
			// forcedFederatedId = "cn00"+ UUID.randomUUID().toString();
			// Calling Async method createUIMSUserAndCompany
			LOGGER.info("Start: Async createUIMSUserAndCompany() of UIMS to create user & company");
			directUIMSUserManagerSoapService.createUIMSUserAndCompany(CALLER_FID, identity,
					userRequest.getUserContext(), company, userName,
					UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, UserConstants.V_NEW,
					userName,userRequest,resultCountCheck.intValue());
			LOGGER.info("End: Async createUIMSUserAndCompany() of UIMS to create user & company");
			userRequest.setFederationId(userName);
		}

		
		IdmsCreateUserResponse idmsResponse = mapper.map(userRequest, IdmsCreateUserResponse.class);
		idmsResponse.setUserStatus("Registered + Active + In bFO");
		idmsResponse.setUserId(userName);
		elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
		return Response.status(Response.Status.OK).entity(idmsResponse).build();
	}
	
	public Integer checkCompanyMappedOtherUsers(String companyId){
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		DocumentContext productDocCtxCheck = null;
		
		String iPlanetDirectoryKey = getSSOToken();
		String companyMapped = UserServiceUtil.checkUserExistsBasedOnFRVersion(productService, frVersion,
				UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, "companyFederatedID eq " + "\"" + companyId + "\"");
		productDocCtxCheck = JsonPath.using(conf).parse(companyMapped);
		Integer resultCountCheck = productDocCtxCheck.read(JsonConstants.RESULT_COUNT);
		
		return resultCountCheck;
	}

	public String getCALLER_FID() {
		return CALLER_FID;
	}

	public void setCALLER_FID(String cALLER_FID) {
		CALLER_FID = cALLER_FID;
	}
}

package com.schneider.idms.service.impl;

import static com.se.idms.util.UserConstants.AUDIT_API_ADMIN;
import static com.se.idms.util.UserConstants.AUDIT_IMPERSONATING_USER;
import static com.se.idms.util.UserConstants.AUDIT_LOG_CLOSURE;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_API;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_GET_CALL;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_UPDATE_CALL;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_USER_INFO_CALL;
import static com.se.idms.util.UserConstants.AUDIT_REQUESTING_USER;

import java.net.URLEncoder;

import javax.ws.rs.BadRequestException;
import javax.ws.rs.ClientErrorException;
import javax.ws.rs.NotAuthorizedException;
import javax.ws.rs.core.Response;

import org.json.simple.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.idms.model.ActivateUser;
import com.idms.model.ActivateUserRequest;
import com.idms.model.IFWUser;
import com.idms.model.UpdateUserRequest;
import com.idms.model.UpdateUserResponse;
import com.idms.product.model.OpenAmUser;
import com.idms.product.model.OpenAmUserRequest;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Option;
import com.schneider.idms.service.IUpdateUserService;
import com.se.idms.cache.utils.EmailConstants;
import com.se.idms.util.JsonConstants;
import com.se.idms.util.UserConstants;
import com.uims.companymanager.CompanyV3;

@Service("updateUserService")
public class UpdateUserServiceImpl extends IdmsCommonServiceImpl implements IUpdateUserService {

	/**
	 * Logger instance.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(CreateUserServiceImpl.class);

	@Override
	public Response updateUser(String authorizedToken, String clientId, String clientSecret,
			UpdateUserRequest userRequest) {
		LOGGER.info("Entered updateUser() -> Start");
		LOGGER.info("Parameter authorizedToken -> " + authorizedToken);
		LOGGER.info("Parameter clientId -> " + clientId + " ,clientSecret" + clientSecret);
		LOGGER.info("Parameter userRequest -> " + userRequest);
		UpdateUserResponse sucessRespone = null;
		String userName = null;
		String iPlanetDirectoryKey = null;
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		ObjectMapper objMapper = new ObjectMapper();
		userResponse.setStatus(errorStatus);

		try {
			LOGGER.info("UserServiceImpl:updateUser -> : Request -> ", objMapper.writeValueAsString(userRequest));

			Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
			DocumentContext productDocCtx = null;
			DocumentContext productDocCtxUser = null;
			String userId = null;
			String hotpService = null;
			String identifierType = null;
			String jsonRequset = null;
			String jsonResponse = null;
			OpenAmUserRequest openAmReq = null;
			String userData = null;
			String updatingUser = null;
			boolean userUpdateforSameUser = false;
			IFWUser userRecord = null;
			String loginIdentifier = null;
			OpenAmUser user = new OpenAmUser();
			StringBuilder contentBuilder = null;
			String firstName = null;
			boolean booleanTrue = true;
			String fedId = null;
			Integer vNewCntValue = 0;
			String usermail = "";
			boolean isUserFromSocialLogin = false;
			// Step 1:

			LOGGER.info(" UserServiceImpl :: updateUser getUserInfoByAccessToken ");

			/**
			 * Check mandatory values and user type (home/work)
			 */

			try {

				/**
				 * Get iPlanetDirectory Pro Admin token for admin
				 */
				LOGGER.info(" UserServiceImpl :: updateUser getSSOToken ");
				iPlanetDirectoryKey = getSSOToken();

				if ((null != userRequest.getUserRecord().getIDMSAnnualRevenue__c())
						&& (userRequest.getUserRecord().getIDMSAnnualRevenue__c().matches("^\\D+$") == true)) {
					userResponse.setStatus(errorStatus);
					userResponse.setMessage(UserConstants.INCORRECT_REVENUE);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.error("Error is " + userResponse.getMessage());
					LOGGER.info("Time taken by UserServiceImpl.userRegistration() : " + elapsedTime);
					return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
				}

				if (checkMandatoryFieldsFromRequest(userRequest.getUserRecord(), userResponse, false)) {
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by UserServiceImpl.updateUser() : " + elapsedTime);
					return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
				}
			} catch (Exception e) {
				e.printStackTrace();
				userResponse.setMessage(UserConstants.ATTRIBUTE_NOT_AVAILABELE);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.updateUser() : " + elapsedTime);
				LOGGER.error("UserServiceImpl.updateUser()->" + userResponse.getMessage());
				return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
			}

			// Need to check do we need pass the user FirstName
			// sendEmail.emailReadyToSendEmail("Suresh.Bachu@non.schneider-electric.com",
			// "bsuresh.infi@gmail.com", "EMAIL CHANGE NOTIFICATION",
			// UserConstants.EMAIL_BODY);

			if (null != userRequest.getUserRecord().getIDMS_Profile_update_source__c() && UserConstants.UIMS
					.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Profile_update_source__c())) {

				if (null == clientId || null == clientSecret) {
					userResponse.setStatus(errorStatus);
					userResponse.setMessage(UserConstants.UIMS_CLIENTID_SECRET);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.error("Error is " + userResponse.getMessage());
					LOGGER.info("Time taken by UserServiceImpl.updateUser() : " + elapsedTime);
					return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
				}

				if ((null != clientId && !clientId.equalsIgnoreCase(uimsClientId))
						|| (null != clientSecret && !clientSecret.equalsIgnoreCase(uimsClientSecret))) {
					userResponse.setStatus(errorStatus);
					userResponse.setMessage(UserConstants.INVALID_UIMS_CREDENTIALS);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.error("Error is " + userResponse.getMessage());
					LOGGER.info("Time taken by UserServiceImpl.updateUser() : " + elapsedTime);
					return Response.status(Response.Status.UNAUTHORIZED).entity(userResponse).build();
				}

				Response fedResponse = null;
				//R7//checkUserExistsWithFederationID(iPlanetDirectoryKey,userRequest.getUserRecord().getIDMS_Federated_ID__c(), startTime);
				if (fedResponse.getStatus() == 200) {
					openAmReq = mapper.map(userRequest, OpenAmUserRequest.class);
					JSONObject uimsResponse = (JSONObject) fedResponse.getEntity();
					if (("Email".equalsIgnoreCase((String) uimsResponse.get("loginIdentity")))
							&& null != userRequest.getUserRecord().getEmail()) {
						openAmReq.getInput().getUser().setLoginid(userRequest.getUserRecord().getEmail());
					} else if (("Mobile".equalsIgnoreCase((String) uimsResponse.get("loginIdentity")))
							&& null != userRequest.getUserRecord().getMobilePhone()) {
						openAmReq.getInput().getUser().setLoginid(userRequest.getUserRecord().getMobilePhone());
					}
					userId = (String) uimsResponse.get("userId");
				} else {
					return fedResponse;
				}

			} else {

				/**
				 * Calling UserInfoByAccessToken to validate user accessToken
				 * and to get the user information
				 */
				LOGGER.info(AUDIT_REQUESTING_USER + userRequest.getUserRecord().getId() + AUDIT_IMPERSONATING_USER
						+ AUDIT_API_ADMIN + AUDIT_OPENAM_API + AUDIT_OPENAM_USER_INFO_CALL + "/se" + userId
						+ AUDIT_LOG_CLOSURE);

				if (pickListValidator.validate(UserConstants.IDMS_BFO_profile,
						userRequest.getUserRecord().getIDMS_Profile_update_source__c())) {
					// userId =
					// userRequest.getUserRecord().getIDMS_Federated_ID__c();
					// fedId =
					// userRequest.getUserRecord().getIDMS_Federated_ID__c();
					String userExistsInOpenam = productService.checkUserExistsWithEmailMobile(
							UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, "loginid eq " + "\""
									+ URLEncoder.encode(userRequest.getUserRecord().getEmail(), "UTF-8") + "\"");

					productDocCtx = JsonPath.using(conf).parse(userExistsInOpenam);
					userId = productDocCtx.read("$.result[0].username");
					fedId = productDocCtx.read("$.result[0].federationID[0]");
				} else {
					String userInfoByAccessToken = openAMTokenService.getUserInfoByAccessToken(authorizedToken, "/se");
					productDocCtx = JsonPath.using(conf).parse(userInfoByAccessToken);
					userId = productDocCtx.read("$.sub");
					fedId = productDocCtx.read("$.federationID");

					usermail = productDocCtx.read("$.email");
					if (usermail == null) {
						usermail = productDocCtx.read("$.Email");
					}
				}
				userResponse.setId(userId);

				/**
				 * Getting the user to check user is updating for same user or
				 * other if updating for same user dont need to call provisional
				 * realm , directly user can update on se realm only
				 */

				LOGGER.info(AUDIT_REQUESTING_USER + userId + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN
						+ AUDIT_OPENAM_API + AUDIT_OPENAM_GET_CALL + userId + AUDIT_LOG_CLOSURE);
				userData = productService.getUser(iPlanetDirectoryKey, userId);

				LOGGER.info("UserServiceImpl:updateUser -> : productService.getUser : userData -> ", userData);

				productDocCtxUser = JsonPath.using(conf).parse(userData);
				updatingUser = productDocCtxUser.read(JsonConstants.LOGIN_ID_LOWER_0);
				if (null == updatingUser) {
					updatingUser = productDocCtxUser.read(JsonConstants.LOGIN_ID_UPPER_0);
				}
				userName = productDocCtxUser.read(JsonConstants.USER_NAME);
				openAmReq = mapper.map(userRequest, OpenAmUserRequest.class);
				openAmReq.getInput().setUser(user);

				/**
				 * Adding for social login
				 */

				if (updatingUser == null && (!userName.startsWith(UserConstants.UID_PREFIX))) {
					updatingUser = usermail;
					isUserFromSocialLogin = true;
				}

				//
				/**
				 * check email and mobile phone for login identifier
				 */

				if (((null != updatingUser) ? emailValidator.validate(updatingUser) : true)
						&& (null != userRequest.getUserRecord().getEmail()
								&& !userRequest.getUserRecord().getEmail().isEmpty())
						&& (null != userRequest.getUserRecord().getMobilePhone()
								&& !userRequest.getUserRecord().getMobilePhone().isEmpty())) {

					openAmReq.getInput().getUser().setIdmsuid(userRequest.getUserRecord().getEmail());
					hotpService = UserConstants.HOTP_EMAIL_UPDATE;
					identifierType = UserConstants.EMAIL;
					user.setMail(userRequest.getUserRecord().getEmail());
					user.setMobile(userRequest.getUserRecord().getMobilePhone());
					loginIdentifier = userRequest.getUserRecord().getEmail();
					if (null != updatingUser && updatingUser.equalsIgnoreCase(userRequest.getUserRecord().getEmail())) {
						userUpdateforSameUser = true;
					}

				} else if (((null != updatingUser) ? emailValidator.validate(updatingUser) : true)
						&& (null != userRequest.getUserRecord().getEmail())
						&& (!userRequest.getUserRecord().getEmail().isEmpty())) {
					user.setMail(userRequest.getUserRecord().getEmail());
					openAmReq.getInput().getUser().setIdmsuid(userRequest.getUserRecord().getEmail());
					hotpService = UserConstants.HOTP_EMAIL_UPDATE;
					identifierType = UserConstants.EMAIL;
					loginIdentifier = userRequest.getUserRecord().getEmail();
					if (null != updatingUser && updatingUser.equalsIgnoreCase(userRequest.getUserRecord().getEmail())) {
						userUpdateforSameUser = true;
					}
				} else if ((null != userRequest.getUserRecord().getMobilePhone())
						&& (!userRequest.getUserRecord().getMobilePhone().isEmpty())) {
					if (null != userRequest.getUserRecord().getIDMS_Profile_update_source__c() && !UserConstants.UIMS
							.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Profile_update_source__c())) {
						if (null != updatingUser) {// added if socialLogin id is
													// empty
							legthValidator.validate(UserConstants.MOBILE_PHONE, updatingUser);
						}
					}
					user.setMobile(userRequest.getUserRecord().getMobilePhone());
					openAmReq.getInput().getUser()
							.setIdmsuid(userRequest.getUserRecord().getMobilePhone() + "bridge-fo.com");
					hotpService = UserConstants.HOTP_MOBILE_UPDATE;
					identifierType = UserConstants.MOBILE;
					loginIdentifier = userRequest.getUserRecord().getMobilePhone();
					if (null != updatingUser
							&& updatingUser.equalsIgnoreCase(userRequest.getUserRecord().getMobilePhone())) {
						userUpdateforSameUser = true;
					}
				}

				String userExists = productService.checkUserExistsWithEmailMobile(
						UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
						"loginid eq " + "\"" + loginIdentifier + "\"");

				productDocCtx = JsonPath.using(conf).parse(userExists);
				Integer resultCount = productDocCtx.read(JsonConstants.RESULT_COUNT);
				if (resultCount.intValue() > 0 && ((null != loginIdentifier) && (null != updatingUser)
						&& (!loginIdentifier.equalsIgnoreCase(updatingUser)))) {
					userResponse.setStatus(errorStatus);
					userResponse.setMessage(UserConstants.NEW_USER_EXISTS);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.error("Error is " + userResponse.getMessage());
					LOGGER.info("Time taken by UserServiceImpl.updateUser() : " + elapsedTime);
					return Response.status(Response.Status.CONFLICT).entity(userResponse).build();

				}

				if ((UserConstants.EMAIL.equalsIgnoreCase(identifierType) && !userUpdateforSameUser)) {

					// Step 3:
					/**
					 * Check password exits and assign to openAM
					 */

					String product_json_string = "{" + "\"newmail\": \"" + userRequest.getUserRecord().getEmail() + "\""
							+ "}";

					productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userId,
							product_json_string);

					/**
					 * Adding the below condition for social Login
					 */

					// if(!isUserFromSocialLogin){
					String otp = sendEmail.generateOtp(userId);
					LOGGER.info("Successfully OTP generated for " + userId);
					sendEmail.sendOpenAmEmail(otp, EmailConstants.UPDATEUSERRECORD_OPT_TYPE, userId,
							userRequest.getUserRecord().getIDMS_Profile_update_source__c());
					
					/**
					 * To update authId in openAM extended attribute
					 */
					if (UserConstants.EMAIL.equalsIgnoreCase(identifierType) && null != updatingUser) {
						String prefferedLanguage = null != productDocCtx.read("$.preferredlanguage")
								? getValue(productDocCtx.read("$.preferredlanguage").toString()) : getDelimeter();
						;

						/**
						 * Adding for social lgoin
						 */

						if (isUserFromSocialLogin) {
							prefferedLanguage = UserConstants.LANGUAGE_CHINA;
						}

						// cal send email
						firstName = null != productDocCtxUser.read("$.givenName")
								? getValue(productDocCtxUser.read("$.givenName").toString()) : getDelimeter();
						if (null == firstName) { // added for socialLogin issue
							firstName = null != productDocCtxUser.read("$.cn")
									? getValue(productDocCtxUser.read("$.cn").toString()) : getDelimeter();
						}

						contentBuilder = getContentFromTemplate(UserConstants.UPDATE_EMAIL_NOTIFICATION,
								prefferedLanguage);
						int startName = contentBuilder.indexOf("{!User.FirstName},");
						int endName = startName + "{!User.FirstName}".length();
						contentBuilder.replace(startName, endName, firstName);// Need
																				// to
																				// check
																				// whether
																				// we
																				// need
																				// to
																				// pass
																				// FirstName
																				// of
																				// loginId
						try {
							// sending email to old user
							sendEmail.emailReadyToSendEmail(updatingUser, fromUserName,
									UserConstants.UPDATE_EMAIL_NOTIFICATION, contentBuilder.toString());
						} catch (Exception e) {
							e.getStackTrace();
							LOGGER.error("Exception while sending email to old User :: -> " + e.getMessage());
						}

					}
					// }

				} else if (UserConstants.MOBILE.equalsIgnoreCase(identifierType) && !userUpdateforSameUser) {
					// for mobile scenarios

					String product_json_string = "{" + "\"newmobile\": \""
							+ userRequest.getUserRecord().getMobilePhone() + "\"" + "}";
					productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userId,
							product_json_string);

					String otp = sendEmail.generateOtp(userId);
					LOGGER.info("Successfully OTP generated for " + userId);
					sendEmail.sendSMSMessage(otp, EmailConstants.UPDATEUSERRECORD_OPT_TYPE, userName,
							userRequest.getUserRecord().getIDMS_Registration_Source__c());

					sendEmail.sendOpenAmMobileEmail(otp, EmailConstants.UPDATEUSERRECORD_OPT_TYPE, userId,
							userRequest.getUserRecord().getIDMS_Profile_update_source__c());
					if (null != updatingUser) {
						// Need to check whether do we need to send message to
						// existing mobile number
					}

				}

				openAmReq = mapper.map(userRequest, OpenAmUserRequest.class);

				if (UserConstants.MOBILE.equalsIgnoreCase(identifierType) && !userUpdateforSameUser) {
					openAmReq.getInput().getUser().setMobile(null);
					userRecord = new IFWUser();
					userRecord.setMobilePhone(userRequest.getUserRecord().getMobilePhone());
				} else if (UserConstants.EMAIL.equalsIgnoreCase(identifierType) && !userUpdateforSameUser) {
					openAmReq.getInput().getUser().setMail(null);
					userRecord = new IFWUser();
					userRecord.setEmail(userRequest.getUserRecord().getEmail());
				}
			}

			// convert Ifw to open am
			jsonRequset = objMapper.writeValueAsString(openAmReq.getInput().getUser());
			jsonRequset = jsonRequset.replace("\"\"", "[]");

			/**
			 * Call updateuser for all attributes except email and mobile
			 */
			LOGGER.info(" UserServiceImpl :: updateUser productService.updateUser ");
			LOGGER.info(AUDIT_REQUESTING_USER + userId + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN + AUDIT_OPENAM_API
					+ AUDIT_OPENAM_UPDATE_CALL + userId + AUDIT_LOG_CLOSURE);
			LOGGER.info("Json  REquest  for  updtae  user ------------->" + jsonRequset);
			jsonResponse = productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userId,
					jsonRequset);
			LOGGER.info("UserServiceImpl:userRegistration -> productService.updateUser : Response -> " + jsonResponse);
			productDocCtx = JsonPath.using(conf).parse(jsonResponse);
			String openamVnew = null != productDocCtx.read("$.V_New[0]") ? getValue(productDocCtx.read("$.V_New[0]"))
					: getDelimeter();
			if (null != openamVnew) {
				vNewCntValue = Integer.parseInt(openamVnew) + 1;
			}
			String version = "{\"V_New\": \"" + vNewCntValue + "\"" + "}";

			LOGGER.info("Json Response " + jsonResponse);

			if (booleanTrue == userRequest.getUserRecord().isActive() && UserConstants.UIMS
					.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Profile_update_source__c())) {

				ActivateUserRequest activateUserRequest = new ActivateUserRequest();
				ActivateUser activateUser = new ActivateUser();
				activateUserRequest.setUserRecord(activateUser);
				activateUser.setIDMS_Federated_ID__c(userRequest.getUserRecord().getIDMS_Federated_ID__c());
				activateUser.setIDMS_Registration_Source__c(UserConstants.UIMS);
				//*R7
				//activateUser(iPlanetDirectoryKey, clientId, clientSecret, activateUserRequest);
			}

			// calling UIMS update user
			if ((!isUserFromSocialLogin)
					&& (null != userRequest.getUserRecord().getIDMS_Profile_update_source__c() && !UserConstants.UIMS
							.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Profile_update_source__c()))) {
				// Adding V_New
				productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userId, version);
				// mapping IFW request to UserCompany
				CompanyV3 company = mapper.map(userRequest, CompanyV3.class);
				com.se.uims.usermanager.UserV6 identity = mapper.map(userRequest, com.se.uims.usermanager.UserV6.class);

				// calling Async method updateUIMSUserAndCompany
				uimsUserManagerSoapService.updateUIMSUserAndCompany(fedId, identity,
						userRequest.getUserRecord().getIDMS_User_Context__c(), company, vNewCntValue.toString(),
						productService, UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userId, usermail);
			}

			//R7
			Response response = null;//getUser(userId);
			Object responseObject = response.getEntity();

			sucessRespone = new UpdateUserResponse();
			sucessRespone.setStatus(successStatus);
			sucessRespone.setMessage(UserConstants.UPDATE_USER_SUCCESS_MESSAGE);
			userRequest.getUserRecord().setId(userId);
			sucessRespone.setIDMSUserRecord(responseObject);

		} catch (BadRequestException e) {
			e.printStackTrace();
			userResponse.setMessage(UserConstants.ERROR_UPDATE_USER);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.updateUser() : " + elapsedTime);
			LOGGER.error("Executing while Updating the User :: -> " + e.getMessage());
			return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
		} catch (NotAuthorizedException e) {
			e.printStackTrace();
			userResponse.setMessage("Session expired or invalid");
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.updateUser() : " + elapsedTime);
			LOGGER.error("Executing while Updating the User :: -> " + e.getMessage());
			return Response.status(Response.Status.UNAUTHORIZED).entity(userResponse).build();
		} catch (ClientErrorException e) {
			e.printStackTrace();
			userResponse.setMessage(UserConstants.NEW_USER_EXISTS);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.updateUser() : " + elapsedTime);
			LOGGER.error("Exception while updating the User :: -> " + userResponse.getMessage());
			return Response.status(Response.Status.CONFLICT).entity(userResponse).build();
		} catch (Exception e) {
			e.printStackTrace();
			userResponse.setMessage(UserConstants.ERROR_UPDATE_USER);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.updateUser() : " + elapsedTime);
			LOGGER.error("Executing while Updating the User :: -> " + e.getMessage());
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(userResponse).build();
		}

		LOGGER.info(" UserServiceImpl :: updateUser End");
		elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		LOGGER.info("Time taken by UserServiceImpl.updateUser() : " + elapsedTime);
		return Response.status(Response.Status.OK).entity(sucessRespone).build();
	}
}

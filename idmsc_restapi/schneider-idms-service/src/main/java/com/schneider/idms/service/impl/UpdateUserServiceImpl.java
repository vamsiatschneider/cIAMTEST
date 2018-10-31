package com.schneider.idms.service.impl;

import static com.se.idms.util.UserConstants.AUDIT_API_ADMIN;
import static com.se.idms.util.UserConstants.AUDIT_IMPERSONATING_USER;
import static com.se.idms.util.UserConstants.AUDIT_LOG_CLOSURE;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_API;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_GET_CALL;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_UPDATE_CALL;
import static com.se.idms.util.UserConstants.AUDIT_REQUESTING_USER;

import java.net.URLDecoder;
import java.net.URLEncoder;

import javax.inject.Inject;
import javax.ws.rs.BadRequestException;
import javax.ws.rs.ClientErrorException;
import javax.ws.rs.NotAuthorizedException;
import javax.ws.rs.core.Response;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.idms.model.IFWUser;
import com.idms.product.model.OpenAmUser;
import com.idms.product.model.OpenAmUserRequest;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Option;
import com.schneider.idms.common.DirectApiConstants;
import com.schneider.idms.common.ErrorCodeConstants;
import com.schneider.idms.common.ResponseCodeStatus;
import com.schneider.idms.mapper.DirectApiIdmsMapper;
import com.schneider.idms.model.IdmsUpdateUserResponse;
import com.schneider.idms.model.IdmsUserRequest;
import com.schneider.idms.service.IUpdateUserService;
import com.schneider.ims.service.uimsv2.CompanyV3;
import com.se.idms.cache.utils.EmailConstants;
import com.se.idms.util.JsonConstants;
import com.se.idms.util.UserConstants;

@Service("updateUserService")
public class UpdateUserServiceImpl extends IdmsCommonServiceImpl implements IUpdateUserService {

	/**
	 * Logger instance.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(UpdateUserServiceImpl.class);
	
	@Inject
	private DirectApiIdmsMapper mapper;

	@Override
	public Response updateUser(String authorizedToken, String accept, String region, IdmsUserRequest userRequest) {

		LOGGER.info("Entered updateUser() -> Start");
		LOGGER.info("Parameter authorizedToken -> " + authorizedToken);
		LOGGER.info("Parameter userRequest -> " + userRequest);
		IdmsUpdateUserResponse sucessRespone = null;
		ResponseCodeStatus errorResponse = new ResponseCodeStatus();
		String userName = null;
		String iPlanetDirectoryKey = null;
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		ObjectMapper objMapper = new ObjectMapper();

		try {
			LOGGER.info("UserServiceImpl:updateUser -> : Request -> ", objMapper.writeValueAsString(userRequest));

			Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
			DocumentContext productDocCtx = null;
			DocumentContext productDocCtxUser = null;
			String userId = null;
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
			String companyFedId="";
			String usermail = "";
			boolean isUserFromSocialLogin = false;
			// Step 1:

			LOGGER.info(" UserServiceImpl :: updateUser getUserInfoByAccessToken ");

			/**
			 * Check mandatory values and user type (home/work)
			 */

			try {
				
				if(null == authorizedToken || authorizedToken.isEmpty()){
					errorResponse.setMessage(ErrorCodeConstants.BADREQUEST_MESSAGE);
					errorResponse.setStatus(ErrorCodeConstants.ERROR);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
					LOGGER.error("Error while processing is " + errorResponse.getMessage());
					return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
				}
				
				
				if(!UserConstants.ACCEPT_TYPE_APP_JSON.equalsIgnoreCase(accept)){
					errorResponse.setMessage(ErrorCodeConstants.BADREQUEST_MESSAGE);
					errorResponse.setStatus(ErrorCodeConstants.ERROR);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
					LOGGER.error("Error while processing is " + errorResponse.getMessage());
					return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
				}
				
				if(!DirectApiConstants.COUNTRY_CHINA.equalsIgnoreCase(region)){
					errorResponse.setMessage(ErrorCodeConstants.BADREQUEST_MESSAGE);
					errorResponse.setStatus(ErrorCodeConstants.ERROR);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
					LOGGER.error("Error while processing is " + errorResponse.getMessage());
					return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
				}

				authorizedToken = DirectApiConstants.BEAR_STRING+authorizedToken;
				/**
				 * Get iPlanetDirectory Pro Admin token for admin
				 */
				LOGGER.info(" UserServiceImpl :: updateUser getSSOToken ");
				iPlanetDirectoryKey = getSSOToken();

				if ((null != userRequest.getAnnualRevenue())
						&& (userRequest.getAnnualRevenue().matches("^\\D+$") == true)) {
					errorResponse.setStatus(ErrorCodeConstants.ERROR);
					errorResponse.setMessage(UserConstants.INCORRECT_REVENUE);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.error("Error is " + errorResponse.getMessage());
					LOGGER.info("Time taken by UserServiceImpl.userRegistration() : " + elapsedTime);
					return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
				}
				

				/**
				 * Need to check with for validation
				 */

				if (checkMandatoryFieldsForDirectAPIRequest(userRequest, errorResponse, false)) {
					errorResponse.setMessage(errorResponse.getMessage());
					errorResponse.setStatus(ErrorCodeConstants.ERROR);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
					LOGGER.error("Error while processing is " + errorResponse.getMessage());
					return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
				}

			} catch (NotAuthorizedException e) {
				e.printStackTrace();
				LOGGER.error("UserServiceImpl:userRegistration ->" + e.getMessage());
				errorResponse.setMessage(ErrorCodeConstants.UNAUTHORIZED_MESSAGE);
				errorResponse.setStatus(ErrorCodeConstants.ERROR);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
				LOGGER.error("Error while processing is " + errorResponse.getMessage());
				return Response.status(Response.Status.UNAUTHORIZED).entity(errorResponse).build();
			} catch (BadRequestException e) {
				e.printStackTrace();
				LOGGER.error("UserServiceImpl:userRegistration ->" + e.getMessage());
				errorResponse.setMessage(UserConstants.ATTRIBUTE_NOT_AVAILABELE);
				errorResponse.setStatus(ErrorCodeConstants.ERROR);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
				LOGGER.error("Error while processing is " + errorResponse.getMessage());
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
			} catch (IllegalArgumentException e) {
				e.printStackTrace();
				LOGGER.error("UserServiceImpl:userRegistration ->" + e.getMessage());
				errorResponse.setMessage(ErrorCodeConstants.BADREQUEST_MESSAGE);
				errorResponse.setStatus(ErrorCodeConstants.ERROR);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
				LOGGER.error("Error while processing is " + errorResponse.getMessage());
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
			} catch (Exception e) {
				e.printStackTrace();
				LOGGER.error("UserServiceImpl:userRegistration ->" + e.getMessage());
				errorResponse.setMessage(UserConstants.ATTRIBUTE_NOT_AVAILABELE);
				errorResponse.setStatus(ErrorCodeConstants.ERROR);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
				LOGGER.error("Error while processing is " + errorResponse.getMessage());
				return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(errorResponse).build();
			}

			// Need to check do we need pass the user FirstName
			// sendEmail.emailReadyToSendEmail("Suresh.Bachu@non.schneider-electric.com",
			// "bsuresh.infi@gmail.com", "EMAIL CHANGE NOTIFICATION",
			// UserConstants.EMAIL_BODY);

			/**
			 * Calling UserInfoByAccessToken to validate user accessToken and to
			 * get the user information
			 */
			if (pickListValidator.validate(UserConstants.IDMS_BFO_profile, userRequest.getProfileLastUpdateSource())) {
				String userInfoByAccessToken = openAMTokenService.getUserInfoByAccessToken(authorizedToken, "/se");
				productDocCtx = JsonPath.using(conf).parse(userInfoByAccessToken);

				if(null != productDocCtx.read("$.email") && productDocCtx.read("$.email").toString().contains(UserConstants.TECHNICAL_USER)){
					String userExistsInOpenam = productService.checkUserExistsWithEmailMobile(
							UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
							"loginid eq " + "\"" + URLEncoder.encode(URLDecoder.decode(userRequest.getEmail(),"UTF-8"),"UTF-8") + "\"");

					productDocCtx = JsonPath.using(conf).parse(userExistsInOpenam);
					userId = productDocCtx.read("$.result[0].username");
					fedId = productDocCtx.read("$.result[0].federationID[0]");
				}else{
					userId = productDocCtx.read("$.sub");
					fedId = productDocCtx.read("$.federationID");
				}
				usermail = productDocCtx.read("$.email");
				if (usermail == null) {
					usermail = productDocCtx.read("$.Email");
				}
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

			/**
			 * Getting the user to check user is updating for same user or other
			 * if updating for same user dont need to call provisional realm ,
			 * directly user can update on se realm only
			 */

			LOGGER.info(AUDIT_REQUESTING_USER + userId + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN + AUDIT_OPENAM_API
					+ AUDIT_OPENAM_GET_CALL + userId + AUDIT_LOG_CLOSURE);
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
			companyFedId = productDocCtxUser.read("$.companyFederatedID[0]");

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
					&& (null != userRequest.getEmail() && !userRequest.getEmail().isEmpty())
					&& (null != userRequest.getMobilePhone() && !userRequest.getMobilePhone().isEmpty())) {

				openAmReq.getInput().getUser().setIdmsuid(userRequest.getEmail());
				identifierType = UserConstants.EMAIL;
				user.setMail(userRequest.getEmail());
				user.setMobile(userRequest.getMobilePhone());
				loginIdentifier = userRequest.getEmail();
				if (null != updatingUser && updatingUser.equalsIgnoreCase(userRequest.getEmail())) {
					userUpdateforSameUser = true;
				}

			} else if (((null != updatingUser) ? emailValidator.validate(updatingUser) : true)
					&& (null != userRequest.getEmail()) && (!userRequest.getEmail().isEmpty())) {
				user.setMail(userRequest.getEmail());
				openAmReq.getInput().getUser().setIdmsuid(userRequest.getEmail());
				identifierType = UserConstants.EMAIL;
				loginIdentifier = userRequest.getEmail();
				if (null != updatingUser && updatingUser.equalsIgnoreCase(userRequest.getEmail())) {
					userUpdateforSameUser = true;
				}
			} else if ((null != userRequest.getMobilePhone()) && (!userRequest.getMobilePhone().isEmpty())) {
				if (null != userRequest.getProfileLastUpdateSource()
						&& !UserConstants.UIMS.equalsIgnoreCase(userRequest.getProfileLastUpdateSource())) {
					if (null != updatingUser) {// added if socialLogin id is
												// empty
						legthValidator.validate(UserConstants.MOBILE_PHONE, updatingUser);
					}
				}
				user.setMobile(userRequest.getMobilePhone());
				openAmReq.getInput().getUser().setIdmsuid(userRequest.getMobilePhone() + "bridge-fo.com");
				identifierType = UserConstants.MOBILE;
				loginIdentifier = userRequest.getMobilePhone();
				if (null != updatingUser && updatingUser.equalsIgnoreCase(userRequest.getMobilePhone())) {
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
				errorResponse.setMessage(UserConstants.NEW_USER_EXISTS);
				errorResponse.setStatus(ErrorCodeConstants.ERROR);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error is " + errorResponse.getMessage());
				LOGGER.info("Time taken by UserServiceImpl.updateUser() : " + elapsedTime);
				return Response.status(Response.Status.CONFLICT).entity(errorResponse).build();

			}

			if ((UserConstants.EMAIL.equalsIgnoreCase(identifierType) && !userUpdateforSameUser)) {

				// Step 3:
				/**
				 * Check password exits and assign to openAM
				 */

				String product_json_string = "{" + "\"newmail\": \"" + userRequest.getEmail() + "\"" + "}";

				productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userId,
						product_json_string);

				/**
				 * Adding the below condition for social Login
				 */

				// if(!isUserFromSocialLogin){
				String otp = sendEmail.generateOtp(userId);
				LOGGER.info("Successfully OTP generated for " + userId);
				sendEmail.sendOpenAmEmail(otp, EmailConstants.UPDATEUSERRECORD_OPT_TYPE, userId,
						userRequest.getProfileLastUpdateSource());

				/**
				 * To update authId in openAM extended attribute
				 */
				if (UserConstants.EMAIL.equalsIgnoreCase(identifierType) && null != updatingUser) {
					String prefferedLanguage = null != productDocCtxUser.read("$.preferredlanguage")
							? getValue(productDocCtxUser.read("$.preferredlanguage").toString()) : getDelimeter();
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

					contentBuilder = getContentFromTemplate(UserConstants.UPDATE_EMAIL_NOTIFICATION, prefferedLanguage);
					int startName = contentBuilder.indexOf("{!User.FirstName},");
					int endName = startName + "{!User.FirstName}".length();
					contentBuilder.replace(startName, endName, firstName);
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

				String product_json_string = "{" + "\"newmobile\": \"" + userRequest.getMobilePhone() + "\"" + "}";
				productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userId,
						product_json_string);

				String otp = sendEmail.generateOtp(userId);
				LOGGER.info("Successfully OTP generated for " + userId);
				sendEmail.sendSMSNewGateway(otp, EmailConstants.UPDATEUSERRECORD_OPT_TYPE, userName,
						userRequest.getProfileLastUpdateSource());

				sendEmail.sendOpenAmMobileEmail(otp, EmailConstants.UPDATEUSERRECORD_OPT_TYPE, userId,
						userRequest.getProfileLastUpdateSource());
				if (null != updatingUser) {
					// Need to check whether do we need to send message to
					// existing mobile number
				}

			}

			openAmReq = mapper.map(userRequest, OpenAmUserRequest.class);

			if (UserConstants.MOBILE.equalsIgnoreCase(identifierType) && !userUpdateforSameUser) {
				openAmReq.getInput().getUser().setMobile(null);
				userRecord = new IFWUser();
				userRecord.setMobilePhone(userRequest.getMobilePhone());
			} else if (UserConstants.EMAIL.equalsIgnoreCase(identifierType) && !userUpdateforSameUser) {
				openAmReq.getInput().getUser().setMail(null);
				userRecord = new IFWUser();
				userRecord.setEmail(userRequest.getEmail());
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

			// calling UIMS update user
			if ((!isUserFromSocialLogin) && (null != userRequest.getProfileLastUpdateSource()
					&& !UserConstants.UIMS.equalsIgnoreCase(userRequest.getProfileLastUpdateSource()))) {
				// Adding V_New
				productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userId, version);
				// mapping IFW request to UserCompany
				CompanyV3 company = mapper.map(userRequest, CompanyV3.class);

				if (null != company.getLanguageCode()) {
					company.setLanguageCode(company.getLanguageCode().toLowerCase());
				}
				company.setFederatedId(companyFedId);
				
				com.se.uims.usermanager.UserV6 identity = mapper.map(userRequest, com.se.uims.usermanager.UserV6.class);

				if (null != identity.getLanguageCode()) {
					identity.setLanguageCode(identity.getLanguageCode().toLowerCase());
				}

				// calling Async method updateUIMSUserAndCompany

				directUIMSUserManagerSoapService.updateUIMSUserAndCompany(fedId, identity, userRequest.getUserContext(),
						company, vNewCntValue.toString(), productService,
						UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userId, companyFedId, usermail);

			}

			// R7 Need to change the below one
			sucessRespone = mapper.map(userRequest, IdmsUpdateUserResponse.class);

		} catch (BadRequestException e) {
			e.printStackTrace();
			errorResponse.setStatus(ErrorCodeConstants.ERROR);
			errorResponse.setMessage(UserConstants.ERROR_UPDATE_USER);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.updateUser() : " + elapsedTime);
			LOGGER.error("Executing while Updating the User :: -> " + e.getMessage());
			return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
		} catch (NotAuthorizedException e) {
			e.printStackTrace();
			errorResponse.setStatus(ErrorCodeConstants.ERROR);
			errorResponse.setMessage("Session expired or invalid");
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.updateUser() : " + elapsedTime);
			LOGGER.error("Executing while Updating the User :: -> " + e.getMessage());
			return Response.status(Response.Status.UNAUTHORIZED).entity(errorResponse).build();
		} catch (ClientErrorException e) {
			e.printStackTrace();
			errorResponse.setStatus(ErrorCodeConstants.ERROR);
			errorResponse.setMessage(UserConstants.NEW_USER_EXISTS);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.updateUser() : " + elapsedTime);
			LOGGER.error("Exception while updating the User :: -> " + errorResponse.getMessage());
			return Response.status(Response.Status.CONFLICT).entity(errorResponse).build();
		} catch (Exception e) {
			e.printStackTrace();
			errorResponse.setStatus(ErrorCodeConstants.ERROR);
			errorResponse.setMessage(UserConstants.ERROR_UPDATE_USER);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.updateUser() : " + elapsedTime);
			LOGGER.error("Executing while Updating the User :: -> " + e.getMessage());
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(errorResponse).build();
		}

		LOGGER.info(" UserServiceImpl :: updateUser End");
		elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		LOGGER.info("Time taken by UserServiceImpl.updateUser() : " + elapsedTime);
		return Response.status(Response.Status.OK).entity(sucessRespone).build();
	}

	public IdmsUpdateUserResponse convertUserDataToUserResponse(String userData) {

		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		DocumentContext userProductDocCtx = null;
		IdmsUpdateUserResponse updatedUserResponse = new IdmsUpdateUserResponse();
		userProductDocCtx = JsonPath.using(conf).parse(userData);

		updatedUserResponse.setSalutation(null != userProductDocCtx.read("$.initials")
				? getValue(userProductDocCtx.read("$.initials").toString()) : getDelimeter());

		String firstNameValue = null != userProductDocCtx.read("$.givenName")
				? getValue(userProductDocCtx.read("$.givenName").toString()) : getDelimeter();
		updatedUserResponse.setFirstName(firstNameValue);

		updatedUserResponse.setMiddleName(null != userProductDocCtx.read("$.middleName")
				? getValue(userProductDocCtx.read("$.middleName").toString()) : getDelimeter());

		String lastNameValue = null != userProductDocCtx.read("$.sn")
				? getValue(userProductDocCtx.read("$.sn").toString()) : getDelimeter();
		updatedUserResponse.setLastName(lastNameValue);

		String countryValue = null != userProductDocCtx.read("$.c") ? getValue(userProductDocCtx.read("$.c").toString())
				: getDelimeter();
		updatedUserResponse.setCountryCode(countryValue);

		String emailValue = null != userProductDocCtx.read("$.mail")
				? getValue(userProductDocCtx.read("$.mail").toString()) : getDelimeter();
		updatedUserResponse.setEmail(emailValue);

		String mobileValue = null != userProductDocCtx.read("$.mobile")
				? getValue(userProductDocCtx.read("$.mobile").toString()) : getDelimeter();
		updatedUserResponse.setMobilePhone(mobileValue);

		updatedUserResponse.setLanguageCode(null != userProductDocCtx.read("$.preferredlanguage")
				? getValue(userProductDocCtx.read("$.preferredlanguage").toString()) : getDelimeter());

		String emailOptInValue = null != userProductDocCtx.read("$.emailOptIn")
				? getValue(userProductDocCtx.read("$.emailOptIn").toString()) : getDelimeter();
		updatedUserResponse.setEmailOptIn(emailOptInValue);

		updatedUserResponse.setAboutMe(null != userProductDocCtx.read("$.AboutMe")
				? getValue(userProductDocCtx.read("$.AboutMe").toString()) : getDelimeter());

		updatedUserResponse.setStreet(null != userProductDocCtx.read("$.street")
				? getValue(userProductDocCtx.read("$.street").toString()) : getDelimeter());

		updatedUserResponse.setCity(null != userProductDocCtx.read("$.l")
				? getValue(userProductDocCtx.read("$.l").toString()) : getDelimeter());

		updatedUserResponse.setZipCode(null != userProductDocCtx.read("$.postalCode")
				? getValue(userProductDocCtx.read("$.postalCode").toString()) : getDelimeter());

		updatedUserResponse.setStateOrProvinceCode(null != userProductDocCtx.read("$.st")
				? getValue(userProductDocCtx.read("$.st").toString()) : getDelimeter());

		updatedUserResponse.setCounty(null != userProductDocCtx.read("$.county")
				? getValue(userProductDocCtx.read("$.county").toString()) : getDelimeter());

		updatedUserResponse.setpOBox(null != userProductDocCtx.read("$.postOfficeBox")
				? getValue(userProductDocCtx.read("$.postOfficeBox").toString()) : getDelimeter());

		updatedUserResponse.setAdditionalAddress(null != userProductDocCtx.read("$.additionalInfo")
				? getValue(userProductDocCtx.read("$.additionalInfo").toString()) : getDelimeter());

		/**
		 * Home phone missing
		 */

		updatedUserResponse.setSuffix(null != userProductDocCtx.read("$.suffix")
				? getValue(userProductDocCtx.read("$.suffix").toString()) : getDelimeter());

		updatedUserResponse.setFax(null != userProductDocCtx.read("$.fax")
				? getValue(userProductDocCtx.read("$.fax").toString()) : getDelimeter());

		updatedUserResponse.setProfileLastUpdateSource(null != userProductDocCtx.read("$.updateSource")
				? getValue(userProductDocCtx.read("$.updateSource").toString()) : getDelimeter());

		updatedUserResponse.setCurrency(null != userProductDocCtx.read("$.currency")
				? getValue(userProductDocCtx.read("$.currency").toString()) : getDelimeter());

		updatedUserResponse.setCompanyName(null != userProductDocCtx.read("$.companyName")
				? getValue(userProductDocCtx.read("$.companyName").toString()) : getDelimeter());

		updatedUserResponse.setCompanyStreet(null != userProductDocCtx.read("$.companyStreet")
				? getValue(userProductDocCtx.read("$.companyStreet").toString()) : getDelimeter());

		updatedUserResponse.setCompanyCity(null != userProductDocCtx.read("$.companyCity")
				? getValue(userProductDocCtx.read("$.companyCity").toString()) : getDelimeter());

		updatedUserResponse.setCompanyZipCode(null != userProductDocCtx.read("$.companyPostalCode")
				? getValue(userProductDocCtx.read("$.companyPostalCode").toString()) : getDelimeter());

		updatedUserResponse.setCompanyStateOrProvinceCode(null != userProductDocCtx.read("$.companyState")
				? getValue(userProductDocCtx.read("$.companyState").toString()) : getDelimeter());

		updatedUserResponse.setCompanyPOBox(null != userProductDocCtx.read("$.companyPostOfficeBox")
				? getValue(userProductDocCtx.read("$.companyPostOfficeBox").toString()) : getDelimeter());

		updatedUserResponse.setCompanyCounty(null != userProductDocCtx.read("$.companyCounty")
				? getValue(userProductDocCtx.read("$.companyCounty").toString()) : getDelimeter());

		updatedUserResponse.setCompanyCountryCode(null != userProductDocCtx.read("$.companyCountry")
				? getValue(userProductDocCtx.read("$.companyCountry").toString()) : getDelimeter());

		updatedUserResponse.setCompanyAdditionalAddress(null != userProductDocCtx.read("$.companyAdditionalInfo")
				? getValue(userProductDocCtx.read("$.companyAdditionalInfo").toString()) : getDelimeter());

		updatedUserResponse.setCompanyWebsite(null != userProductDocCtx.read("$.companyWebSite")
				? getValue(userProductDocCtx.read("$.companyWebSite").toString()) : getDelimeter());

		updatedUserResponse.setClassLevel1(null != userProductDocCtx.read("$.iam1")
				? getValue(userProductDocCtx.read("$.iam1").toString()) : getDelimeter());

		updatedUserResponse.setClassLevel2(null != userProductDocCtx.read("$.iam2")
				? getValue(userProductDocCtx.read("$.iam2").toString()) : getDelimeter());

		updatedUserResponse.setMarketSegment(null != userProductDocCtx.read("$.industrySegment")
				? getValue(userProductDocCtx.read("$.industrySegment").toString()) : getDelimeter());

		updatedUserResponse.setMarketSubSegment(null != userProductDocCtx.read("$.industrySubSegment")
				? getValue(userProductDocCtx.read("$.industrySubSegment").toString()) : getDelimeter());

		updatedUserResponse.setMarketServed(null != userProductDocCtx.read("$.industries")
				? getValue(userProductDocCtx.read("$.industries").toString()) : getDelimeter());

		updatedUserResponse.setEmployeeSize(null != userProductDocCtx.read("$.employeeSize")
				? getValue(userProductDocCtx.read("$.employeeSize").toString()) : getDelimeter());

		updatedUserResponse.setDepartment(null != userProductDocCtx.read("$.departmentNumber")
				? getValue(userProductDocCtx.read("$.departmentNumber").toString()) : getDelimeter());

		updatedUserResponse.setHeadquarter(null != userProductDocCtx.read("$.headquarters")
				? getValue(userProductDocCtx.read("$.headquarters").toString()) : getDelimeter());

		updatedUserResponse.setAnnualRevenue(null != userProductDocCtx.read("$.annualRevenue")
				? getValue(userProductDocCtx.read("$.annualRevenue").toString()) : getDelimeter());

		updatedUserResponse.setTaxIdentificationNumber(null != userProductDocCtx.read("$.taxID")
				? getValue(userProductDocCtx.read("$.taxID").toString()) : getDelimeter());

		updatedUserResponse.setJobTitle(null != userProductDocCtx.read("$.title")
				? getValue(userProductDocCtx.read("$.title").toString()) : getDelimeter());

		updatedUserResponse.setJobFunction(null != userProductDocCtx.read("$.jobFunction")
				? getValue(userProductDocCtx.read("$.jobFunction").toString()) : getDelimeter());

		updatedUserResponse.setJobDescription(null != userProductDocCtx.read("$.jobDescription")
				? getValue(userProductDocCtx.read("$.jobDescription").toString()) : getDelimeter());

		String workPhoneValue = null != userProductDocCtx.read("$.telephoneNumber")
				? getValue(userProductDocCtx.read("$.telephoneNumber").toString()) : null;
		updatedUserResponse.setWorkPhone(workPhoneValue);

		updatedUserResponse.setTrustedAdmin(null != userProductDocCtx.read("$.trustedAdmin")
				? getValue(userProductDocCtx.read("$.trustedAdmin").toString()) : getDelimeter());

		updatedUserResponse.setUserId(null != userProductDocCtx.read("$.uid")
				? getValue(userProductDocCtx.read("$.uid").toString()) : getDelimeter());

		/**
		 * Account id need to map
		 */

		updatedUserResponse.setFederatedId(null != userProductDocCtx.read("$.federationID")
				? getValue(userProductDocCtx.read("$.federationID").toString()) : getDelimeter());

		updatedUserResponse.setIdmsFederatedId(null != userProductDocCtx.read("$.federationID")
				? getValue(userProductDocCtx.read("$.federationID").toString()) : getDelimeter());

		String employeeTypeValue = null != userProductDocCtx.read(JsonConstants.EMPLOYEE_TYPE)
				? getValue(userProductDocCtx.read(JsonConstants.EMPLOYEE_TYPE).toString()) : getDelimeter();
		updatedUserResponse.setUserContext(employeeTypeValue);

		updatedUserResponse.setRegistrationSource(null != userProductDocCtx.read("$.registerationSource")
				? getValue(userProductDocCtx.read("$.registerationSource").toString()) : getDelimeter());

		/**
		 * Trust status ,trustLevel ,rejectionReason,rejectionComment, missing
		 */

		updatedUserResponse.setDelegatedIdp(null != userProductDocCtx.read("$.delegatedIDP")
				? getValue(userProductDocCtx.read("$.delegatedIDP").toString()) : getDelimeter());

		updatedUserResponse.setIdentityType(null != userProductDocCtx.read("$.identityType")
				? getValue(userProductDocCtx.read("$.identityType").toString()) : getDelimeter());

		updatedUserResponse.setIsInternal(null != userProductDocCtx.read("$.IDMSisInternal__c")
				? getValue(userProductDocCtx.read("$.IDMSisInternal__c").toString()) : getDelimeter());

		updatedUserResponse.setAil(null != userProductDocCtx.read("$.IDMSAil_c")
				? getValue(userProductDocCtx.read("$.IDMSAil_c").toString()) : getDelimeter());

		updatedUserResponse.setAilApplications(null != userProductDocCtx.read("$.IDMSAIL_Applications_c")
				? getValue(userProductDocCtx.read("$.IDMSAIL_Applications_c").toString()) : getDelimeter());

		updatedUserResponse.setAilFeatures(null != userProductDocCtx.read("$.IDMSAIL_Features_c")
				? getValue(userProductDocCtx.read("$.IDMSAIL_Features_c").toString()) : getDelimeter());

		updatedUserResponse.setAilPrograms(null != userProductDocCtx.read("$.IDMSAIL_Programs_c")
				? getValue(userProductDocCtx.read("$.IDMSAIL_Programs_c").toString()) : getDelimeter());

		/**
		 * division is missing
		 */

		updatedUserResponse.setTitle(null != userProductDocCtx.read("$.title")
				? getValue(userProductDocCtx.read("$.title").toString()) : getDelimeter());

		/**
		 * businessUnit,userStatus,socialProviders is missing
		 */

		updatedUserResponse.setCompanyFederatedId(null != userProductDocCtx.read("$.companyFederatedID")
				? getValue(userProductDocCtx.read("$.companyFederatedID").toString()) : getDelimeter());

		updatedUserResponse.setContactGoldenID("");

		updatedUserResponse.setAccountGoldenID("");

		return updatedUserResponse;
	}
}

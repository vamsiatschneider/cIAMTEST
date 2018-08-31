package com.schneider.idms.service.impl;

import java.net.URLDecoder;
import java.net.URLEncoder;

import javax.ws.rs.core.Response;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.idms.model.CheckUserExistsRequest;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.schneider.idms.common.ErrorCodeConstants;
import com.schneider.idms.common.ResponseCodeStatus;
import com.schneider.idms.service.CheckUserExistService;
import com.se.idms.util.EmailValidator;
import com.se.idms.util.UserConstants;

/**
 * 
 * @author SESA508936 For Direct API Call
 */
@Service("checkUserExistService")
public class CheckUserExistServiceImpl extends IdmsCommonServiceImpl implements CheckUserExistService {
	private static final Logger LOGGER = LoggerFactory.getLogger(CheckUserExistServiceImpl.class);

	@Override
	public Response idmsCheckUserExists(String authorization, String accept, String region,
			CheckUserExistsRequest checkUserExistsRequest) {
		LOGGER.info("Entered idmsCheckUserExists() -> Start");
		LOGGER.info("Paramters are Authorization=" + authorization + " ,IDMS-Region=" + region);
		LOGGER.info("Paramters are Accept=" + accept);
		LOGGER.info("Paramters are checkUserExistsRequest=" + checkUserExistsRequest);

		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		ObjectMapper objectMapper = new ObjectMapper();
		ResponseCodeStatus responseCode = new ResponseCodeStatus();
		String emailOrMobile = null;
		String email = null, mobile = null;
		String withGlobalUsers = checkUserExistsRequest.getWithGlobalUsers();
		DocumentContext productDocCtx = null;

		// Authorization
		if (null == authorization || authorization.isEmpty()) {
			responseCode.setStatus(ErrorCodeConstants.ERROR);
			responseCode.setMessage("Mandatory Check: IDMS-Authorization token is null or empty");
			LOGGER.error("Mandatory check: Header field Authorization is Missing or Null/Empty");
			return Response.status(Response.Status.BAD_REQUEST).entity(responseCode).build();
		}
		
		// Validate_Authorization
		if (!getTechnicalUserDetails(authorization)) {
			responseCode.setMessage("Mandatory Validation: IDMS-Authorization token is invalid");
			responseCode.setStatus(ErrorCodeConstants.ERROR);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
			LOGGER.error("Error while processing is " + responseCode.getMessage());
			return Response.status(Response.Status.BAD_REQUEST).entity(responseCode).build();
		}

		// accept
		if (null == accept || accept.isEmpty()) {
			responseCode.setStatus(ErrorCodeConstants.ERROR);
			responseCode.setMessage("Header field Accept is Mandatory");
			LOGGER.error("Mandatory check: Header field Accept is Missing or Null/Empty");
			return Response.status(Response.Status.BAD_REQUEST).entity(responseCode).build();
		}

		if (null != accept && !accept.equalsIgnoreCase("application/json")) {
			responseCode.setStatus(ErrorCodeConstants.ERROR);
			responseCode.setMessage("Header field Accept must be application/json");
			LOGGER.error("Mandatory check: Header field Accept:" + accept + " is not permitted");
			return Response.status(Response.Status.BAD_REQUEST).entity(responseCode).build();
		}

		// region
		if (null != region && !region.equalsIgnoreCase("CN")) {
			responseCode.setStatus(ErrorCodeConstants.ERROR);
			responseCode.setMessage("Region is Invalid");
			LOGGER.error("Mandatory check: Header field IDMS-Region:" + region + " is not permitted");
			return Response.status(Response.Status.BAD_REQUEST).entity(responseCode).build();
		}
		// mobile
		if (null != checkUserExistsRequest.getMobile() && !checkUserExistsRequest.getMobile().isEmpty()) {
			String mobileNumber = checkUserExistsRequest.getMobile().trim();
			if (null != mobileNumber && !mobileNumber.isEmpty()) {
				mobile = mobileNumber.replaceAll("[\\(\\)\\-\\+]", "");
				LOGGER.info("mobileNumber==" + mobile);

				if (!StringUtils.isNumeric(mobile)) {
					responseCode.setStatus(ErrorCodeConstants.ERROR);
					responseCode.setMessage("Mobile number is Non-numberic");
					LOGGER.error("Mandatory check: Mobile number is Non-numberic:" + mobileNumber);
					return Response.status(Response.Status.BAD_REQUEST).entity(responseCode).build();
				}

				if (mobile.length() > 11) {
					responseCode.setStatus(ErrorCodeConstants.ERROR);
					responseCode.setMessage("Mobile number should not be greater than 11 digits");
					LOGGER.error("Mandatory check: Mobile number should not exceed 11 digits:" + mobileNumber);
					return Response.status(Response.Status.BAD_REQUEST).entity(responseCode).build();
				}
			}
		}

		// Email
		if (null != checkUserExistsRequest.getEmail() && !checkUserExistsRequest.getEmail().isEmpty()) {
			email = checkUserExistsRequest.getEmail().trim();
			if (null != email && !email.isEmpty()) {
				boolean validEmail = EmailValidator.getInstance().validate(email);
				if (!validEmail) {
					responseCode.setStatus(ErrorCodeConstants.ERROR);
					responseCode.setMessage("Email is not valid");
					LOGGER.error("Mandatory check Email: Not a valid email:" + email);
					return Response.status(Response.Status.BAD_REQUEST).entity(responseCode).build();
				}
			}
		}

		// withGlobalUsers
		if ((null != withGlobalUsers && !withGlobalUsers.isEmpty()) && (!UserConstants.TRUE.equalsIgnoreCase(withGlobalUsers)
				&& !UserConstants.FALSE.equalsIgnoreCase(withGlobalUsers))) {
			responseCode.setStatus(ErrorCodeConstants.ERROR);
			responseCode.setMessage(UserConstants.GLOBAL_USER_BOOLEAN);
			LOGGER.error("Mandatory check GlobalUSerField:" + UserConstants.GLOBAL_USER_BOOLEAN);
			return Response.status(Response.Status.BAD_REQUEST).entity(responseCode).build();
		}

		try {
			LOGGER.info("userAilRequest=" + objectMapper.writeValueAsString(checkUserExistsRequest));
			email = checkUserExistsRequest.getEmail();
			mobile = checkUserExistsRequest.getMobile();

			if (null != email && !email.trim().isEmpty() && email.trim().length() > 0) {
				emailOrMobile = email.trim();
			}
			if (null == emailOrMobile) {
				if (null != mobile && !mobile.trim().isEmpty() && mobile.trim().length() > 0) {
					emailOrMobile = mobile.trim();
				}
			}
			if (null == emailOrMobile || emailOrMobile.isEmpty()) {
				responseCode.setStatus(ErrorCodeConstants.ERROR);
				responseCode.setMessage("Both email & mobile can not be empty");
				LOGGER.error("Mandatory check: Atleast email or mobile should have value");
				return Response.status(Response.Status.BAD_REQUEST).entity(responseCode).build();
			}
			LOGGER.info("--------after emailOrMobile----------");
			// start
			String iPlanetDirectoryKey = getSSOToken();
			LOGGER.info("Start:Calling checkUserExistsWithEmailMobile() of OpenAMService for user having emailOrMobile: "+emailOrMobile);
			String userExists = productService.checkUserExistsWithEmailMobile(
					UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, "loginid eq " + "\"" + URLEncoder.encode(URLDecoder.decode(emailOrMobile,"UTF-8"),"UTF-8") + "\"");
			LOGGER.info("End:checkUserExistsWithEmailMobile() of OpenAMService Finished for user having emailOrMobile: "+emailOrMobile);
			productDocCtx = JsonPath.using(conf).parse(userExists);
			Integer resultCount = productDocCtx.read("$.resultCount");
			LOGGER.info("Result count="+resultCount);
			if (resultCount.intValue() > 0) {
				responseCode.setMessage(UserConstants.TRUE);
				return Response.status(Response.Status.OK).entity(responseCode).build();
			}else{
				if (UserConstants.TRUE.equalsIgnoreCase(withGlobalUsers.trim())) {
					String bfoAuthorization = salesForceService.getSalesForceToken(
							UserConstants.CONTENT_TYPE_URL_FROM, UserConstants.PR_GRANT_TYPE, salesForceClientId,
							salesForceClientSecret, salesForceUserName, salesForcePassword);
					productDocCtx = JsonPath.using(conf).parse(bfoAuthorization);
					String bfoAuthorizationToken = productDocCtx.read("$.access_token");
					
				}
			

			}
			responseCode.setStatus(ErrorCodeConstants.ERROR);
			responseCode.setMessage("Direct API: CheckUserExists still in development phase");
			LOGGER.error("Mandatory check: Direct API Still in development phase");
			return Response.status(Response.Status.BAD_REQUEST).entity(responseCode).build();
			
		} catch (Exception e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		return null;
	}

}

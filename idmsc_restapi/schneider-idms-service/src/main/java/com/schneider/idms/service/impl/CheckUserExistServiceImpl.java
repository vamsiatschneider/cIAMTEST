package com.schneider.idms.service.impl;

import javax.ws.rs.core.Response;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.idms.model.CheckUserExistsRequest;
import com.schneider.idms.common.ErrorCodeConstants;
import com.schneider.idms.common.ErrorResponseCode;
import com.schneider.idms.service.CheckUserExistService;

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

		ObjectMapper objectMapper = new ObjectMapper();
		ErrorResponseCode errorResponseCode = new ErrorResponseCode();
		String emailOrMobile = null;
		String email = null, mobile = null;
		boolean withGlobalUsers = false;

		// TODO - to replace true with Authorization check logic
		if (true) {
			// Authorization
			if (null == authorization || authorization.isEmpty()) {
				errorResponseCode.setCode(ErrorCodeConstants.BAD_REQUEST);
				errorResponseCode.setMessage("Header field Authorization is Mandatory");
				LOGGER.error("Mandatory check: Header field Authorization is Missing or Null/Empty");
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponseCode).build();
			}

			// accept
			if (null == accept || accept.isEmpty()) {
				errorResponseCode.setCode(ErrorCodeConstants.BAD_REQUEST);
				errorResponseCode.setMessage("Header field Accept is Mandatory");
				LOGGER.error("Mandatory check: Header field Accept is Missing or Null/Empty");
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponseCode).build();
			}

			if (null != accept && !accept.equalsIgnoreCase("application/json")) {
				errorResponseCode.setCode(ErrorCodeConstants.BAD_REQUEST);
				errorResponseCode.setMessage("Header field Accept must be application/json");
				LOGGER.error("Mandatory check: Header field Accept:" + accept + " is not permitted");
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponseCode).build();
			}

			// region
			if (null != region && !region.equalsIgnoreCase("CN")) {
				errorResponseCode.setCode(ErrorCodeConstants.BAD_REQUEST);
				errorResponseCode.setMessage("Header field IDMS-Region must be CN");
				LOGGER.error("Mandatory check: Header field IDMS-Region:" + region + " is not permitted");
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponseCode).build();
			}
			//check mobile length, should be 11 digits
			if(null != checkUserExistsRequest.getMobile()){
				String mobileNumber = checkUserExistsRequest.getMobile().trim();
				mobile=mobileNumber.replaceAll("-", "");
				mobile=mobile.replaceAll("(", "");
				mobile=mobile.replaceAll(")", "");
				mobile=mobile.replaceAll("+", "");
				LOGGER.info("mobileNumber=="+mobile);
				if(!StringUtils.isNumeric(mobile)){
					errorResponseCode.setCode(ErrorCodeConstants.BAD_REQUEST);
					errorResponseCode.setMessage("Mobile number is Non-numberic");
					LOGGER.error("Mandatory check: Mobile number is Non-numberic:" + mobileNumber);
					return Response.status(Response.Status.BAD_REQUEST).entity(errorResponseCode).build();
				}
				
				if(mobile.length()>11){
					errorResponseCode.setCode(ErrorCodeConstants.BAD_REQUEST);
					errorResponseCode.setMessage("Mobile number should not be greater than 11 digits");
					LOGGER.error("Mandatory check: Mobile number should not exceed 11 digits:" + mobileNumber);
					return Response.status(Response.Status.BAD_REQUEST).entity(errorResponseCode).build();
				}
			}

			try {
				LOGGER.info("userAilRequest=" + objectMapper.writeValueAsString(checkUserExistsRequest));
				email = checkUserExistsRequest.getEmail();
				mobile = checkUserExistsRequest.getMobile();
				String tempGlobalUsers = checkUserExistsRequest.getWithGlobalUsers();

				if (null != email) {
					emailOrMobile = email;
				}
				if (null == emailOrMobile) {
					emailOrMobile = mobile;
				}
				if(null == emailOrMobile){
					errorResponseCode.setCode(ErrorCodeConstants.BAD_REQUEST);
					errorResponseCode.setMessage("Both email & mobile can not be empty");
					LOGGER.error("Mandatory check: Atleast email or mobile should have value");
					return Response.status(Response.Status.BAD_REQUEST).entity(errorResponseCode).build();
				}
				LOGGER.info("--------after emailOrMobile----------");
				
				errorResponseCode.setCode(ErrorCodeConstants.BAD_REQUEST);
				errorResponseCode.setMessage("Direct API: CheckUserExists still in development phase");
				LOGGER.error("Mandatory check: Direct API Still in development phase");
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponseCode).build();

			} catch (Exception e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}

		}

		return null;
	}

}

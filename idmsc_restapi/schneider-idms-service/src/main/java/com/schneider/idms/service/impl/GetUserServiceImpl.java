package com.schneider.idms.service.impl;

import static com.se.idms.util.UserConstants.AUDIT_API_ADMIN;
import static com.se.idms.util.UserConstants.AUDIT_IMPERSONATING_USER;
import static com.se.idms.util.UserConstants.AUDIT_LOG_CLOSURE;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_API;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_GET_CALL;
import static com.se.idms.util.UserConstants.AUDIT_REQUESTING_USER;
import static com.se.idms.util.UserConstants.AUDIT_TECHNICAL_USER;

import javax.ws.rs.NotAuthorizedException;
import javax.ws.rs.core.Response;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Option;
import com.schneider.idms.common.ErrorCodeConstants;
import com.schneider.idms.common.ResponseCodeStatus;
import com.schneider.idms.mapper.UserInfoMapper;
import com.schneider.idms.model.UserInfoDTO;
import com.schneider.idms.service.GetUserService;

/**
 * @author SESA508936
 * For Direct API
 */
@Service("getUserService")
public class GetUserServiceImpl extends IdmsCommonServiceImpl implements GetUserService {
	private static final Logger LOGGER = LoggerFactory.getLogger(GetUserServiceImpl.class);

	@Override
	public Response getUser(String authorization, String accept, String region) {
		LOGGER.info("Entered getUser() -> Start");
		LOGGER.info("Parameter IDMS-Authorization -> " + authorization);
		LOGGER.info("Parameter Accept -> " + accept);
		LOGGER.info("Parameter IDMS-region -> " + region);

		String userData = null;
		long startTime = System.currentTimeMillis();
		ResponseCodeStatus errorResponse = new ResponseCodeStatus();

		try {
			if (null == authorization || authorization.isEmpty()) {
				LOGGER.error("IDMS-Authorization(Bearer) is either empty or null");
				errorResponse.setStatus(ErrorCodeConstants.ERROR);
				errorResponse.setMessage("Mandatory Check: IDMS-Authorization token is null or empty");
				return Response.status(Response.Status.NOT_FOUND.getStatusCode()).entity(errorResponse).build();
			} else if (null != authorization.trim() && !authorization.trim().isEmpty()) {
				LOGGER.info(AUDIT_REQUESTING_USER.concat(AUDIT_TECHNICAL_USER).concat(AUDIT_IMPERSONATING_USER)
						.concat(AUDIT_API_ADMIN).concat(AUDIT_OPENAM_API).concat(AUDIT_OPENAM_GET_CALL).concat(authorization)
						.concat(AUDIT_LOG_CLOSURE));
				if(null != region && !region.equalsIgnoreCase("CN")){
					errorResponse.setStatus(ErrorCodeConstants.ERROR);
					errorResponse.setMessage("Invalid Region");					
					LOGGER.error("Requested region is "+region+". It is not supported");
					return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
				}				
				if(null == region || region.equalsIgnoreCase("CN")){
					LOGGER.info("Start: calling getUserDetails() of OpenDjService to fetch User Details for bearer id:"+authorization);
					authorization = "Bearer "+authorization.trim();
					userData = openAMTokenService.getUserDetails(authorization);
					LOGGER.info("End: getUserDetails() of OpenDjService to fetch User Details Finished for bearer id:"+authorization);
					LOGGER.info("User Data from Openam: " + userData);
					LOGGER.info("Time took in Processing:" + (System.currentTimeMillis() - startTime));
				}
			}

			if (userData == null) {
				LOGGER.info("Time took in Processing:" + (System.currentTimeMillis() - startTime));
				LOGGER.error("UserData received from OpenAM is NULL for authorization id:" + authorization);
				errorResponse.setStatus(ErrorCodeConstants.ERROR);
				errorResponse.setMessage("User does not exists in OPENAM for authorization token:"+authorization);
				return Response.status(Response.Status.NOT_FOUND.getStatusCode()).entity(errorResponse).build();
			} else {
				UserInfoMapper userInfoMapper = new UserInfoMapper();
				UserInfoDTO userInfoDTO = new UserInfoDTO();

				Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
				DocumentContext productDocCtx = JsonPath.using(conf).parse(userData);

				userInfoMapper.parseGetUserResponse(userInfoDTO, productDocCtx);
				return Response.status(Response.Status.OK.getStatusCode()).entity(userInfoDTO).build();
			}
		} catch (NotAuthorizedException e) {
			LOGGER.error("Direct API NotAuthorizedException ="+e.getMessage());
			errorResponse.setStatus(ErrorCodeConstants.ERROR);
			errorResponse.setMessage("Session expired or Invalid token");
			return Response.status(Response.Status.UNAUTHORIZED.getStatusCode()).entity(errorResponse).build();
		}catch (Exception e) {
			LOGGER.error("Error in Direct API getUser() OpenDjService ->" + e.getMessage(),e);
			errorResponse.setStatus(ErrorCodeConstants.ERROR);
			errorResponse.setMessage("Error in Calling GetUser API, Please try again");
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(errorResponse).build();
		}
	}
}

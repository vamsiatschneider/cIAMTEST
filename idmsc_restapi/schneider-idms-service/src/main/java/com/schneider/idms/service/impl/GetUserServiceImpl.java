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
import com.schneider.idms.common.ErrorResponseCode;
import com.schneider.idms.common.JsonBody;
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
		ErrorResponseCode errorResponseCode = new ErrorResponseCode();

		try {
			if (null == authorization || authorization.isEmpty()) {
				LOGGER.error("IDMS-Authorization(Bearer) is either empty or null");
				return Response.status(Response.Status.NOT_FOUND.getStatusCode()).entity(JsonBody.EmptyJsonArray(authorization)).build();
			} else if (null != authorization) {
				LOGGER.info(AUDIT_REQUESTING_USER.concat(AUDIT_TECHNICAL_USER).concat(AUDIT_IMPERSONATING_USER)
						.concat(AUDIT_API_ADMIN).concat(AUDIT_OPENAM_API).concat(AUDIT_OPENAM_GET_CALL).concat(authorization)
						.concat(AUDIT_LOG_CLOSURE));
				if(null != region && !region.equalsIgnoreCase("CN")){
					errorResponseCode.setMessage("Other than China region is not supported");
					errorResponseCode.setCode("BAD_REQUEST");
					LOGGER.error("Requested region is "+region+". It is not supported");
					return Response.status(Response.Status.BAD_REQUEST).entity(errorResponseCode).build();
				}
				if(null == region || region.equalsIgnoreCase("CN")){
					LOGGER.info("Going to call getUserDetails() of OpenDjService for bearer id:"+authorization);
					userData = openDJService.getUserDetails(authorization);
					LOGGER.info("getUserDetails() of OpenDjService finished for bearer id:"+authorization);
					LOGGER.info("User Data from Openam: " + userData);
					LOGGER.info("Time took in Processing:" + (System.currentTimeMillis() - startTime));
				}

			}

			if (userData == null) {
				LOGGER.info("Time took in Processing:" + (System.currentTimeMillis() - startTime));
				LOGGER.error("UserData received from OpenAM is NULL for authorization id:" + authorization);
				return Response.status(Response.Status.NOT_FOUND.getStatusCode()).entity(JsonBody.EmptyJsonArray(authorization)).build();
			} else {
				UserInfoMapper userInfoMapper = new UserInfoMapper();
				UserInfoDTO userInfoDTO = new UserInfoDTO();

				Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
				DocumentContext productDocCtx = JsonPath.using(conf).parse(userData);

				userInfoMapper.parseGetUserResponse(userInfoDTO, productDocCtx);
				return Response.status(Response.Status.OK.getStatusCode()).entity(userInfoDTO).build();
			}
		} catch (NotAuthorizedException e) {
			e.printStackTrace();			
			LOGGER.error("Direct API NotAuthorizedException ="+e.getMessage());
			return Response.status(Response.Status.UNAUTHORIZED.getStatusCode()).entity(JsonBody.InvalidSessionJsonArray()).build();
		}catch (Exception e) {
			LOGGER.error("Error in Direct API getUser() OpenDjService ->" + e.getMessage());
			e.printStackTrace();
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode()).build();
		}
	}
}

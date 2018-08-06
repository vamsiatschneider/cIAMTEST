package com.schneider.idms.service.impl;


import static com.se.idms.util.UserConstants.AUDIT_API_ADMIN;
import static com.se.idms.util.UserConstants.AUDIT_IMPERSONATING_USER;
import static com.se.idms.util.UserConstants.AUDIT_LOG_CLOSURE;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_API;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_AUTHENTICATE_CALL;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_GET_CALL;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_USER_EXISTS_CALL;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_USER_INFO_CALL;
import static com.se.idms.util.UserConstants.AUDIT_REQUESTING_USER;
import static com.se.idms.util.UserConstants.AUDIT_TECHNICAL_USER;
import static com.se.idms.util.UserConstants.GET_USER_BY_TOKEN_TIME_LOG;
import static com.se.idms.util.UserConstants.GET_USER_TIME_LOG;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.net.URLEncoder;

import javax.annotation.Resource;
import javax.inject.Inject;
import javax.ws.rs.NotAuthorizedException;
import javax.ws.rs.core.Response;

import org.json.simple.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.idms.product.client.OpenAMService;
import com.idms.product.client.OpenAMTokenService;
import com.idms.product.model.Attributes;
import com.idms.product.model.OpenAMGetUserHomeResponse;
import com.idms.product.model.OpenAMGetUserWorkResponse;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Option;
import com.schneider.idms.common.CommonClass;
import com.schneider.idms.common.JsonBody;
import com.schneider.idms.service.GetUserService;
import com.se.idms.dto.GetUserHomeByOauthResponse;
import com.se.idms.dto.GetUserWorkByOauthResponse;
import com.se.idms.dto.ParseValuesByOauthHomeWorkContextDto;
import com.se.idms.util.JsonConstants;
import com.se.idms.util.UserConstants;

/**
 * @author SESA508936
 *
 */
@Service("getUserService")
public class GetUserServiceImpl implements GetUserService {
	private static final Logger LOGGER = LoggerFactory.getLogger(GetUserServiceImpl.class);
	
	@Inject
	private OpenAMService productService;
	
	@Inject
	private OpenAMTokenService openAMTokenService;
	
	@Resource(name="cacheManager")
	private org.springframework.cache.ehcache.EhCacheCacheManager cacheManager;
	
	@Value("${adminUserName}")
	private String adminUserName;

	@Value("${adminPassword}")
	private String adminPassword;
	
	@Autowired
	private ParseValuesByOauthHomeWorkContextDto valuesByOauthHomeWorkContext;
	
	

	@Override
	public Response getUser(String userId) {
		LOGGER.info("Entered getUser() -> Start");
		LOGGER.info("Parameter userId -> " + userId);

		String userData = null;
		long startTime = System.currentTimeMillis();

		try {
			if(null ==userId || userId.isEmpty()){
				LOGGER.error("UserId is either empty or null");
				return Response.status(Response.Status.NOT_FOUND.getStatusCode()).entity(JsonBody.EmptyJsonArray(userId)).build();
			}else if (null != userId) {
				LOGGER.info(AUDIT_REQUESTING_USER.concat(AUDIT_TECHNICAL_USER).concat(AUDIT_IMPERSONATING_USER)
						.concat(AUDIT_API_ADMIN).concat(AUDIT_OPENAM_API).concat(AUDIT_OPENAM_GET_CALL).concat(userId)
						.concat(AUDIT_LOG_CLOSURE));
				String token = getSSOToken();
				userData = productService.getUser(token, userId);
				LOGGER.info("User Data from Openam: " + userData);
				LOGGER.info("Time took in Processing:"+(System.currentTimeMillis()-startTime));
			}

			if (userData == null) {
				LOGGER.info("Time took in Processing:"+(System.currentTimeMillis()-startTime));
				LOGGER.error("UserData received from OpenAM is NULL for UserID:"+userId);
				return Response.status(Response.Status.NOT_FOUND.getStatusCode()).entity(JsonBody.EmptyJsonArray(userId)).build();
			}else{
				DocumentContext productDocCtx = CommonClass.JsonParsedData(userData);
				String context = null != productDocCtx.read(JsonConstants.EMPLOYEE_TYPE)? CommonClass.getValue(productDocCtx.read(JsonConstants.EMPLOYEE_TYPE).toString()) : null;
				LOGGER.info("context=" + context);

				OpenAMGetUserHomeResponse userHomeResponse = new OpenAMGetUserHomeResponse();
				OpenAMGetUserWorkResponse userWorkResponse = new OpenAMGetUserWorkResponse();
				Attributes attributes = new Attributes();
				userHomeResponse.setAttributes(attributes);
				userWorkResponse.setAttributes(attributes);
				if ("@home".equalsIgnoreCase(context)|| "home".equalsIgnoreCase(context)) {		
					valuesByOauthHomeWorkContext.parseValuesHomeContext(userHomeResponse, productDocCtx);
					return Response.status(Response.Status.OK.getStatusCode()).entity(userHomeResponse).build();
				} else if ("@work".equalsIgnoreCase(context)|| "work".equalsIgnoreCase(context)) {
					valuesByOauthHomeWorkContext.parseValuesWorkContext(userWorkResponse, productDocCtx);
					return Response.status(Response.Status.OK.getStatusCode()).entity(userWorkResponse).build();
				} else if (null == context || "".equals(context)) {
					valuesByOauthHomeWorkContext.parseValuesHomeContext(userHomeResponse, productDocCtx);
					return Response.status(Response.Status.OK.getStatusCode()).entity(userHomeResponse).build();
				}
			}
		} catch (Exception e) {
			LOGGER.error("Error in getUser() openam service->"+e.getMessage());
			e.printStackTrace();
			return Response.status(Response.Status.NOT_FOUND.getStatusCode()).entity(JsonBody.EmptyJsonArray(userId)).build();
		}
		return Response.status(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode()).build();
	}


	@Override
	public Response getUserByOauth(String token) {
		return getUserbyToken(token);
	}


	@Override
	public Response getUserbyToken(String token) {
		LOGGER.info("Entered getUserbyToken() -> Start");
		LOGGER.info("Parameter token -> "+token);

		long startTime = System.currentTimeMillis();
		long elapsedTime;
		Response userResponse = null;
		try {
			if (null != token) {

				LOGGER.info(AUDIT_REQUESTING_USER + AUDIT_TECHNICAL_USER + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN
						+ AUDIT_OPENAM_API + AUDIT_OPENAM_USER_INFO_CALL + "/se" + AUDIT_LOG_CLOSURE);
				LOGGER.info("Going to call getUserInfoByAccessToken() of OpenAMTokenService");
				String userInfoByAccessToken = openAMTokenService.getUserInfoByAccessToken(token, "/se");
				LOGGER.info("getUserInfoByAccessToken() of OpenAMTokenService finished");
				LOGGER.info("Accesstoken from the API call: " + userInfoByAccessToken);

				Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
				DocumentContext productDocCtx = JsonPath.using(conf).parse(userInfoByAccessToken);
				String userId = productDocCtx.read("$.sub");
				userResponse = getUserByOauthToken(userId);
				LOGGER.info("User details derived from access token: " + userId);
			}
		} catch (NotAuthorizedException e) {
			e.printStackTrace();
			//LOGGER.debug("InvalidSessionId!");
			/*JSONObject jsonObject = new JSONObject();
			jsonObject.put("errorCode", "INVALID_SESSION_ID");
			jsonObject.put("message", "Session expired or invalid");

			JSONArray jsonArray = new JSONArray();
			jsonArray.add(jsonObject);*/
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info(GET_USER_BY_TOKEN_TIME_LOG + elapsedTime);
			LOGGER.error("Error in getUserInfoByAccessToken() of OpenAMTokenService:"+e.getMessage());
			return Response.status(Response.Status.UNAUTHORIZED.getStatusCode()).entity(JsonBody.InvalidSessionJsonArray()).build();
		} catch (Exception e) {
			e.printStackTrace();
			//LOGGER.debug("Unauthorized!");
			/*JSONObject jsonObject = new JSONObject();
			jsonObject.put("errorCode", "Unauthorized");
			jsonObject.put("message", "Provided external ID field does not exist or is  not accessible ");

			JSONArray jsonArray = new JSONArray();
			jsonArray.add(jsonObject);*/
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info(GET_USER_BY_TOKEN_TIME_LOG + elapsedTime);
			LOGGER.error("Exception in getUserInfoByAccessToken() of OpenAMTokenService->"+e.getMessage());
			return Response.status(Response.Status.NOT_FOUND.getStatusCode()).entity(JsonBody.UnauthorizedJsonArray()).build();
			
		}
		elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		LOGGER.info(GET_USER_BY_TOKEN_TIME_LOG + elapsedTime);
		return userResponse;
	}



	@SuppressWarnings("unchecked")
	@Override
	public Response getUserByLoginIdentifier(String loginIdentifier) {
		LOGGER.info("Entered getUserByLoginIdentifier() -> Start");
		LOGGER.info("Parameter loginIdentifier -> " + loginIdentifier);

		DocumentContext productDocCtx = null;
		String iPlanetDirectoryKey = null;
		String userExists=null;
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		JSONObject response = new JSONObject();

		iPlanetDirectoryKey = getSSOToken();

		if (null != loginIdentifier) {
			LOGGER.info(AUDIT_REQUESTING_USER + AUDIT_TECHNICAL_USER + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN
					+ AUDIT_OPENAM_API + AUDIT_OPENAM_USER_EXISTS_CALL + loginIdentifier + AUDIT_LOG_CLOSURE);

			try {
				userExists = productService.checkUserExistsWithEmailMobile(
						UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
						"mail eq " + "\"" + URLEncoder.encode(URLDecoder.decode(loginIdentifier,"UTF-8"),"UTF-8") + "\" or mobile eq " + "\"" + URLEncoder.encode(URLDecoder.decode(loginIdentifier,"UTF-8"),"UTF-8") + "\"");
			} catch (UnsupportedEncodingException e) {
				e.printStackTrace();
				LOGGER.error("Error is "+e.getMessage());
			}


			productDocCtx = JsonPath.using(conf).parse(userExists);
			Integer resultCount = productDocCtx.read("$.resultCount");
			if (resultCount.intValue() > 0) {
				response.put("userId", productDocCtx.read("$.result[0].username"));
				response.put("fedId", productDocCtx.read("$.result[0].federationID[0]"));
				response.put("regSource", productDocCtx.read("$.result[0].registerationSource[0]"));
				//productService.sessionLogout(UserConstants.IPLANET_DIRECTORY_PRO+iPlanetDirectoryKey, "logout");
				return Response.status(Response.Status.OK).entity(response).build();

			} else {
				//productService.sessionLogout(UserConstants.IPLANET_DIRECTORY_PRO+iPlanetDirectoryKey, "logout");
				return Response.status(Response.Status.OK).entity(response).build();
			}
		}

		return null;
	}



	@Override
	public Response getUserByOauthFromUI(String token) {
		return getUserbyTokenUI(token);
		}
	
	
	/**
	 * 
	 * @param userId
	 * @return
	 */
	private Response getUserByOauthToken(String userId) {
		LOGGER.info("Entered getUserByOauthToken() -> Start");
		LOGGER.info("Parameter userId -> " + userId);
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		String userData = null;
		

		try {
			if (null != userId) {
				String token = getSSOToken();
				LOGGER.info(AUDIT_REQUESTING_USER.concat(AUDIT_TECHNICAL_USER).concat(AUDIT_IMPERSONATING_USER)
						.concat(AUDIT_API_ADMIN).concat(AUDIT_OPENAM_API).concat(AUDIT_OPENAM_GET_CALL).concat(userId)
						.concat(AUDIT_LOG_CLOSURE));
				LOGGER.info("Going to call getUser() of OpenAMService with userId:"+userId);
				userData = productService.getUser(token, userId);
				LOGGER.info("getUser() call of OpenAMService finished with userdata: " + userData);
			}
		} catch (Exception e) {
			e.getStackTrace();
			LOGGER.error(e.toString());
			LOGGER.error("UserServiceImpl:getUserByOauthToken() ->"+e.getMessage());
			if (userData == null) {
				/*JSONObject jsonObject = new JSONObject();
				jsonObject.put("errorCode", "NOT_FOUND");
				jsonObject.put("message", "Provided external ID field does not exist or is  not accessible: " + userId);

				JSONArray jsonArray = new JSONArray();
				jsonArray.add(jsonObject);*/
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info(GET_USER_TIME_LOG + elapsedTime);
				//productService.sessionLogout(UserConstants.IPLANET_DIRECTORY_PRO+token, "logout");
				return Response.status(Response.Status.NOT_FOUND.getStatusCode()).entity(JsonBody.EmptyJsonArray(userId)).build();
			}
		}
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		// getting the context
		DocumentContext productDocCtx = JsonPath.using(conf).parse(userData);
		String context = null != productDocCtx.read(JsonConstants.EMPLOYEE_TYPE)
				? CommonClass.getValue(productDocCtx.read(JsonConstants.EMPLOYEE_TYPE).toString()) : null;
		LOGGER.info("context=" + context);

		GetUserHomeByOauthResponse userHomeResponse = new GetUserHomeByOauthResponse();
		GetUserWorkByOauthResponse userWorkResponse = new GetUserWorkByOauthResponse();
		//DocumentContext userProductDocCtx = JsonPath.using(conf).parse(userData);
		if ("@home".equalsIgnoreCase(context)) {
			valuesByOauthHomeWorkContext.parseValuesByOauthHomeContext(userHomeResponse, productDocCtx);
			//return returnGetUserByOauthHomeContext(startTime, userHomeResponse, userProductDocCtx);
		} else if ("@work".equalsIgnoreCase(context)) {
			valuesByOauthHomeWorkContext.parseValuesByOauthWorkContext(userWorkResponse, productDocCtx);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info(GET_USER_TIME_LOG + elapsedTime);
			return Response.status(Response.Status.OK.getStatusCode()).entity(userWorkResponse).build();
		} else if (null == context || "".equals(context)) {
			valuesByOauthHomeWorkContext.parseValuesByOauthHomeContext(userHomeResponse, productDocCtx);
			//return returnGetUserByOauthHomeContext(startTime, userHomeResponse, productDocCtx);
		}
		elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		LOGGER.info(GET_USER_TIME_LOG + elapsedTime);
		return Response.status(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode()).build();
	}
	
	/**
	 * 
	 * @param token
	 * @return
	 */
	private Response getUserbyTokenUI(String token) {
		LOGGER.info("Entered getUserbyTokenUI() -> Start");
		LOGGER.info("Parameter token -> "+token);

		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		Response userResponse = null;
		try {
			if (null != token) {

				LOGGER.info(AUDIT_REQUESTING_USER + AUDIT_TECHNICAL_USER + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN
						+ AUDIT_OPENAM_API + AUDIT_OPENAM_USER_INFO_CALL + "/se" + AUDIT_LOG_CLOSURE);
				LOGGER.info("Going to call getUserInfoByAccessToken() of OpenAMTokenService");
				String userInfoByAccessToken = openAMTokenService.getUserInfoByAccessToken(token, "/se");
				LOGGER.info("getUserInfoByAccessToken() of OpenAMTokenService finished");
				LOGGER.info("Accesstoken from the API call: " + userInfoByAccessToken);

				Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
				DocumentContext productDocCtx = JsonPath.using(conf).parse(userInfoByAccessToken);
				String userId = productDocCtx.read("$.sub");
				userResponse = getUser(userId);
				LOGGER.info("User details derived from access token: " + userId);
			}
		} catch (NotAuthorizedException e) {
			//LOGGER.debug("Unauthorized!");
			/*JSONObject jsonObject = new JSONObject();
			jsonObject.put("errorCode", "Unauthorized");
			jsonObject.put("message", "Provided external ID field does not exist or is  not accessible ");

			JSONArray jsonArray = new JSONArray();
			jsonArray.add(jsonObject);*/
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info(GET_USER_BY_TOKEN_TIME_LOG + elapsedTime);
			LOGGER.error("Error in getUserInfoByAccessToken() of OpenAMTokenService:"+e.getMessage());
			return Response.status(Response.Status.NOT_FOUND.getStatusCode()).entity(JsonBody.UnauthorizedJsonArray()).build();
		}
		elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		LOGGER.info(GET_USER_BY_TOKEN_TIME_LOG + elapsedTime);
		return userResponse;
	}
	
	
	/**
	 * Authorization token from OPENAM
	 * @return
	 */
	private String getSSOToken() {
		LOGGER.info(AUDIT_REQUESTING_USER.concat(AUDIT_TECHNICAL_USER).concat(AUDIT_IMPERSONATING_USER)
				.concat(AUDIT_API_ADMIN).concat(AUDIT_OPENAM_API).concat(AUDIT_OPENAM_AUTHENTICATE_CALL)
				.concat(AUDIT_LOG_CLOSURE));

		String tokenResponse = productService.authenticateUser(adminUserName, adminPassword, UserConstants.REALM);
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		DocumentContext productDocCtx = JsonPath.using(conf).parse(tokenResponse);
		return productDocCtx.read(JsonConstants.TOKEN_ID);

	}

}

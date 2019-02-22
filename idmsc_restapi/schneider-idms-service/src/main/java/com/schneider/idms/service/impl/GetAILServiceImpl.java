/**
 * 
 */
package com.schneider.idms.service.impl;

import static com.se.idms.util.UserConstants.GET_USER_TIME_LOG;

import java.net.URLDecoder;
import java.net.URLEncoder;

import javax.ws.rs.NotAuthorizedException;
import javax.ws.rs.NotFoundException;
import javax.ws.rs.core.Response;

import org.json.simple.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.idms.product.model.Attributes;
import com.idms.product.model.OpenAMGetUserHomeResponse;
import com.idms.product.model.OpenAMGetUserWorkResponse;
import com.idms.service.util.ChinaIdmsUtil;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Option;
import com.schneider.idms.common.ErrorCodeConstants;
import com.schneider.idms.common.ResponseCodeStatus;
import com.schneider.idms.service.GetAILService;
import com.se.idms.util.JsonConstants;
import com.se.idms.util.UserConstants;

/**
 * @author SESA508936
 *
 */
@Service("getAILService")
public class GetAILServiceImpl extends IdmsCommonServiceImpl implements GetAILService {
	private static final Logger LOGGER = LoggerFactory.getLogger(GetAILServiceImpl.class);
	
	private ObjectMapper mapper = new ObjectMapper();

	@Override
	public Response getUserAIL(String authorization, String accept, String region, String federationId) {
		LOGGER.info("Entered getUserAIL() -> Start");
		LOGGER.info("Parameter Authorization -> " + authorization);
		LOGGER.info("Parameter Accept -> " + accept);
		LOGGER.info("Parameter IDMS-region -> " + region);
		LOGGER.info("Parameter federationId -> " + federationId);

		ResponseCodeStatus responseCode = new ResponseCodeStatus();
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		String longLiveHashedToken = null;
		String iPlanetDirectoryKey = null;
		String userData = null;
		JsonNode responseJason = mapper.createObjectNode();
		String idmsAIL_Features = null, idmsAIL_Programs = null, idmsAIL_Applications = null;

		try {
			// Authorization
			if (null == authorization || authorization.isEmpty()) {
				responseCode.setStatus(ErrorCodeConstants.ERROR);
				responseCode.setMessage("Mandatory Check: Authorization token is null or empty");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
				LOGGER.error("Mandatory check: Header field authorization is Missing or Null/Empty");
				return Response.status(Response.Status.BAD_REQUEST).entity(responseCode).build();
			}

			if(null != authorization && !authorization.isEmpty()){
				longLiveHashedToken = ChinaIdmsUtil.generateHashValue(authorization);
			}

			// Validation_authorization_token
			if (null!=authorization && !directApiSecretToken.equalsIgnoreCase(longLiveHashedToken)) {
				responseCode.setMessage("Mandatory Validation: Authorization token is invalid or session expired");
				responseCode.setStatus(ErrorCodeConstants.ERROR);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
				LOGGER.error("Validation check: Not authorized token or session expired");
				return Response.status(Response.Status.UNAUTHORIZED).entity(responseCode).build();
			}

			// accept
			if (null != accept && !UserConstants.ACCEPT_TYPE_APP_JSON.equalsIgnoreCase(accept)) {
				responseCode.setStatus(ErrorCodeConstants.ERROR);
				responseCode.setMessage("Header field accept must be application/json");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
				LOGGER.error("Mandatory check: Header field accept:" + accept + " is not permitted");
				return Response.status(Response.Status.BAD_REQUEST).entity(responseCode).build();
			}

			// region
			if (null != region && !region.equalsIgnoreCase("CN")) {
				responseCode.setStatus(ErrorCodeConstants.ERROR);
				responseCode.setMessage("Header field Region must be CN");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Mandatory check: Header field Region:" + region + " is not permitted");
				return Response.status(Response.Status.BAD_REQUEST).entity(responseCode).build();
			}

			iPlanetDirectoryKey = getSSOToken();

			LOGGER.info("Start: calling getUser() of OpenAMService to fetch AIL values for federationId=" + federationId);
			userData = productService.getUser(iPlanetDirectoryKey, federationId);
			LOGGER.info("End: getUser() of OpenAMService to fetch AIL values finsihed for federationId=" + federationId);

			Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
			DocumentContext productDocCtx = JsonPath.using(conf).parse(userData);
			LOGGER.info("productDocCtx = "+productDocCtx.jsonString());
			
			idmsAIL_Features = (null != productDocCtx.read("$.IDMSAIL_Features_c")
					? getValue(productDocCtx.read("$.IDMSAIL_Features_c").toString()) : getDelimeter());
			idmsAIL_Programs = (null != productDocCtx.read("$.IDMSAIL_Programs_c")
					? getValue(productDocCtx.read("$.IDMSAIL_Programs_c").toString()) : getDelimeter());
			idmsAIL_Applications = (null != productDocCtx.read("$.IDMSAIL_Applications_c")
					? getValue(productDocCtx.read("$.IDMSAIL_Applications_c").toString()) : getDelimeter());

			if(null == idmsAIL_Features && null == idmsAIL_Programs && null == idmsAIL_Applications){
				responseCode.setStatus(ErrorCodeConstants.ERROR);
				responseCode.setMessage("No AIL Data Available");				
				return Response.status(Response.Status.BAD_REQUEST).entity(responseCode).build();
			}else{			
			responseJason = successAILResponse(productDocCtx);			
			return Response.status(Response.Status.OK.getStatusCode()).entity(responseJason).build();
			}
		} catch (NotFoundException e) {
			
			responseCode.setStatus(ErrorCodeConstants.ERROR);
			responseCode.setMessage("User not found.");
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by getUserAIL() : " + elapsedTime);
			LOGGER.error("NotFoundException in getUserAIL() :: -> " + e.getMessage());
			return Response.status(Response.Status.NOT_FOUND).entity(responseCode).build();
		} catch (NotAuthorizedException e) {
			
			responseCode.setStatus(ErrorCodeConstants.ERROR);
			responseCode.setMessage("HTTP 401 Unauthorized or Session expired");
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by getUserAIL() : " + elapsedTime);
			LOGGER.error("NotAuthorizedException in getUserAIL() :: -> " + e.getMessage());
			return Response.status(Response.Status.UNAUTHORIZED).entity(responseCode).build();
		}catch (Exception e) {
			
			responseCode.setStatus(ErrorCodeConstants.ERROR);
			responseCode.setMessage("Internal Server Error");
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by getUserAIL() : " + elapsedTime);
			LOGGER.error("Exception in getUserAIL() :: -> " + e.getMessage());
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(responseCode).build();
		}
	}

	@SuppressWarnings("deprecation")
	private JsonNode successAILResponse(DocumentContext productDocCtx){
		JsonNode AILInfo = mapper.createObjectNode();
		((ObjectNode) AILInfo).put("status", "Success");
		((ObjectNode) AILInfo).put("message", "AIL retrieved successfully");
		((ObjectNode) AILInfo).put("federationidentifier",null != productDocCtx.read("$.uid")
				? getValue(productDocCtx.read("$.uid").toString()) : getDelimeter());
		ArrayNode array = mapper.createArrayNode();
		String appType[] = {"Application","Program","Feature"};

		for(String appName:appType){
			JsonNode dataAIL = mapper.createObjectNode();

			if(null != productDocCtx.read("$.IDMSAIL_"+appName+"s_c")){
				((ObjectNode) dataAIL).put("aclType", appName);
				((ObjectNode) dataAIL).put("acl", getValue(productDocCtx.read("$.IDMSAIL_"+appName+"s_c").toString()));
				array.add(dataAIL);
			}
		}

		((ObjectNode) AILInfo).put("ails", array);

		return AILInfo;
	}
	
/*	private JsonNode failureAILResponse(){
		JsonNode AILInfo = mapper.createObjectNode();
		((ObjectNode) AILInfo).put("status", "Error");
		((ObjectNode) AILInfo).put("message", "No AIL Data Available");
		
		return AILInfo;
	}*/
	
/*class UserAILInfoDTO {

	private String idmsAil_c;
	private String idmsAIL_Features;
	private String idmsAIL_Programs;
	private String idmsAIL_Applications;
	
	public String getIdmsAil_c() {
		return idmsAil_c;
	}
	public void setIdmsAil_c(String idmsAil_c) {
		this.idmsAil_c = idmsAil_c;
	}
	public String getIdmsAIL_Features() {
		return idmsAIL_Features;
	}
	public void setIdmsAIL_Features(String idmsAIL_Features) {
		this.idmsAIL_Features = idmsAIL_Features;
	}
	public String getIdmsAIL_Programs() {
		return idmsAIL_Programs;
	}
	public void setIdmsAIL_Programs(String idmsAIL_Programs) {
		this.idmsAIL_Programs = idmsAIL_Programs;
	}
	public String getIdmsAIL_Applications() {
		return idmsAIL_Applications;
	}
	public void setIdmsAIL_Applications(String idmsAIL_Applications) {
		this.idmsAIL_Applications = idmsAIL_Applications;
	}
}*/

@SuppressWarnings("unchecked")
public Response getUserResponse (String userId,String iPlanetDirectoryToken ){
	long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
	String userData = null;
	try{
		userData = productService.getUser(iPlanetDirectoryToken, userId);
	}
	catch (Exception e) {
		
		LOGGER.error("Error in getUser() openam service->"+e.getMessage());
		LOGGER.error(e.toString());
		if (userData == null) {
			JSONObject jsonObject = new JSONObject();
			jsonObject.put("errorCode", "NOT_FOUND");
			jsonObject.put("message", "Provided external ID field does not exist or is  not accessible: " + userId);
			return Response.status(Response.Status.NOT_FOUND.getStatusCode()).entity(jsonObject).build();
		}
	}
	Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
	// getting the context
	DocumentContext productDocCtx = JsonPath.using(conf).parse(userData);
	String context = null != productDocCtx.read(JsonConstants.EMPLOYEE_TYPE)
			? getValue(productDocCtx.read(JsonConstants.EMPLOYEE_TYPE).toString()) : null;
	LOGGER.info("context=" + context);
	OpenAMGetUserHomeResponse userHomeResponse = new OpenAMGetUserHomeResponse();
	OpenAMGetUserWorkResponse userWorkResponse = new OpenAMGetUserWorkResponse();
	DocumentContext userProductDocCtx = JsonPath.using(conf).parse(userData);
	Attributes attributes = new Attributes();
	userHomeResponse.setAttributes(attributes);
	userWorkResponse.setAttributes(attributes);
	if ("@home".equalsIgnoreCase(context)|| "home".equalsIgnoreCase(context)) {
		return returnGetUserHomeContext(startTime, userHomeResponse, userProductDocCtx);
	} else if ("@work".equalsIgnoreCase(context)|| "work".equalsIgnoreCase(context)) {
		valuesByOauthHomeWorkContext.parseValuesWorkContext(userWorkResponse, userProductDocCtx);
		
		return Response.status(Response.Status.OK.getStatusCode()).entity(userWorkResponse).build();
	} else if (null == context || "".equals(context)) {
		return returnGetUserHomeContext(startTime, userHomeResponse, userProductDocCtx);
	}
	return Response.status(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode()).build();
}
private Response returnGetUserHomeContext(long startTime, OpenAMGetUserHomeResponse userHomeResponse,
		DocumentContext userProductDocCtx) {
	long elapsedTime;
	valuesByOauthHomeWorkContext.parseValuesHomeContext(userHomeResponse, userProductDocCtx);
	elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
	LOGGER.info(GET_USER_TIME_LOG + elapsedTime);
	return Response.status(Response.Status.OK.getStatusCode()).entity(userHomeResponse).build();
}

/**
 * API to search IDMS CHINA based on userID/federationID/email/Mobile
 */
@SuppressWarnings("unchecked")
@Override
public Response getUserBySearch(String authorization, String region, String federationId, String userId, String email,
		String mobile) {
	// TODO Auto-generated method stub
	LOGGER.info("Entered getUserBySearch() -> Start");
	LOGGER.info("Parameter Authorization -> " + authorization);
	LOGGER.info("Parameter IDMS-region -> " + region);
	LOGGER.info("Parameter federationId -> " + federationId);
	LOGGER.info("Parameter UserId -> " + userId);
	LOGGER.info("Parameter email -> " + email);
	LOGGER.info("Parameter mobile -> " + mobile);
	ResponseCodeStatus responseCode = new ResponseCodeStatus();
	String longLiveHashedToken = null;
	String iPlanetDirectoryToken = null;
	Response response =null;
	DocumentContext productDocCtx = null;
	JSONObject uimsResponse = new JSONObject();
	// Authorization
	if (null == authorization || authorization.isEmpty()) {
		responseCode.setStatus(ErrorCodeConstants.ERROR);
		responseCode.setMessage("Mandatory Check: Authorization token is null or empty");
		LOGGER.error("Mandatory check: Header field authorization is Missing or Null/Empty");
		return Response.status(Response.Status.BAD_REQUEST).entity(responseCode).build();
	}
	if(null != authorization && !authorization.isEmpty()){
		longLiveHashedToken = ChinaIdmsUtil.generateHashValue(authorization);
	}
	// Validation_authorization_token
	if (null!=authorization && !directApiSecretToken.equalsIgnoreCase(longLiveHashedToken)) {
		responseCode.setMessage("Mandatory Validation: Authorization token is invalid or session expired");
		responseCode.setStatus(ErrorCodeConstants.ERROR);
		LOGGER.error("Validation check: Not authorized token or session expired");
		return Response.status(Response.Status.UNAUTHORIZED).entity(responseCode).build();
	}
	// region
	if (null != region && !region.equalsIgnoreCase("CN")) {
		responseCode.setStatus(ErrorCodeConstants.ERROR);
		responseCode.setMessage("Header field Region must be CN");
		LOGGER.error("Mandatory check: Header field Region:" + region + " is not permitted");
		return Response.status(Response.Status.BAD_REQUEST).entity(responseCode).build();
	}
	iPlanetDirectoryToken = getSSOToken();
	LOGGER.info("iPlanetDirectoryKey: "+iPlanetDirectoryToken);
	if(null != userId && !userId.isEmpty()){
		response=getUserResponse(userId,iPlanetDirectoryToken);
	}
	else if(null != federationId && ! federationId.isEmpty()){
		try{
		String userExists = productService.checkUserExistsWithEmailMobile(
				UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryToken, "federationID eq " + "\"" + federationId + "\" or uid eq " + "\"" + federationId + "\"");
		LOGGER.info("End: checkUserExistsWithEmailMobile() of openam finished for federationId:"+federationId);
		productDocCtx = JsonPath.using(conf).parse(userExists);
		LOGGER.info("productDocCtx = "+productDocCtx.jsonString());
		Integer resultCount = productDocCtx.read(JsonConstants.RESULT_COUNT);
		LOGGER.info("resultCount="+resultCount);
		if (resultCount.intValue() == 0) {
			uimsResponse = new JSONObject();
			uimsResponse.put(UserConstants.STATUS, errorStatus);
			uimsResponse.put(UserConstants.MESSAGE, UserConstants.USER_NOT_EXISTS);
			uimsResponse.put(UserConstants.FEDERATION_IDENTIFIER, federationId);
			LOGGER.error("Error is -> " + UserConstants.USER_NOT_EXISTS);
			return Response.status(Response.Status.NOT_FOUND).entity(uimsResponse).build();
		} else {
			userId = productDocCtx.read(JsonConstants.RESULT);
		}
	}
	catch (Exception e) {
		userResponse.setStatus(errorStatus);
		userResponse.setMessage("Exception occured in getUserBySearch");
		return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
	}
	response=getUserResponse(userId,iPlanetDirectoryToken);
	}
	else if(null != email && !email .isEmpty()){
		//call using email id
		try{
		String	userExists = productService.checkUserExistsWithEmailMobile(
					UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryToken, "mail eq " + "\"" + URLEncoder.encode(URLDecoder.decode(email, "UTF-8"), "UTF-8")  + "\"");
		LOGGER.info("End: checkUserExistsWithEmailMobile() of openam finished for email:"+email);
		productDocCtx = JsonPath.using(conf).parse(userExists);
		LOGGER.info("productDocCtx = "+productDocCtx.jsonString());
		Integer resultCount = productDocCtx.read(JsonConstants.RESULT_COUNT);
		LOGGER.info("resultCount="+resultCount);
		if (resultCount.intValue() == 0) {
			uimsResponse = new JSONObject();
			uimsResponse.put(UserConstants.STATUS, errorStatus);
			uimsResponse.put(UserConstants.MESSAGE, UserConstants.USER_NOT_EXISTS);
			uimsResponse.put(UserConstants.EMAIL_OR_MOBILE_NOT_MATCHING, email);
			LOGGER.error("Error is -> " + UserConstants.USER_NOT_EXISTS);
			return Response.status(Response.Status.NOT_FOUND).entity(uimsResponse).build();
		}
		else {
			userId = productDocCtx.read(JsonConstants.RESULT);
		}
		}
		catch (Exception e) {
			userResponse.setStatus(errorStatus);
			userResponse.setMessage("Exception occured in getUserBySearch");
			return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
		}
		response=getUserResponse(userId,iPlanetDirectoryToken);
	}
	else if(null != mobile && !mobile .isEmpty()){
	    //call using mobile id
		try{
		String	userExists = productService.checkUserExistsWithEmailMobile(
					UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryToken, "mobile_reg eq " + "\"" + URLEncoder.encode(URLDecoder.decode(mobile, "UTF-8"), "UTF-8")
					+ "\" or mobile eq " + "\"" + URLEncoder.encode(URLDecoder.decode(mobile, "UTF-8"), "UTF-8")  + "\"");
		LOGGER.info("End: checkUserExistsWithEmailMobile() of openam finished for mobile:"+mobile);
		productDocCtx = JsonPath.using(conf).parse(userExists);
		LOGGER.info("productDocCtx = "+productDocCtx.jsonString());
		Integer resultCount = productDocCtx.read(JsonConstants.RESULT_COUNT);
		LOGGER.info("resultCount="+resultCount);
		if (resultCount.intValue() == 0) {
			uimsResponse = new JSONObject();
			uimsResponse.put(UserConstants.STATUS, errorStatus);
			uimsResponse.put(UserConstants.MESSAGE, UserConstants.USER_NOT_EXISTS);
			uimsResponse.put(UserConstants.EMAIL_OR_MOBILE_NOT_MATCHING, mobile);
			LOGGER.error("Error is -> " + UserConstants.USER_NOT_EXISTS);
			return Response.status(Response.Status.NOT_FOUND).entity(uimsResponse).build();
		} else {
			userId = productDocCtx.read(JsonConstants.RESULT);
		}
		}
		catch (Exception e) {
			userResponse.setStatus(errorStatus);
			userResponse.setMessage("Exception occured in getUserBySearch");
			return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
		}
		response=getUserResponse(userId,iPlanetDirectoryToken);
	}
	else {
		userResponse.setStatus(errorStatus);
		userResponse.setMessage("Invalid Request");
		return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
	}
	return response;
	}
}
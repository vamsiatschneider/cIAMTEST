/**
 * 
 */
package com.schneider.idms.service.impl;

import javax.ws.rs.NotAuthorizedException;
import javax.ws.rs.NotFoundException;
import javax.ws.rs.core.Response;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.idms.service.util.ChinaIdmsUtil;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Option;
import com.schneider.idms.common.ErrorCodeConstants;
import com.schneider.idms.common.ResponseCodeStatus;
import com.schneider.idms.service.GetAILService;
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

			LOGGER.info("productDocCtx="+productDocCtx);
			
			responseJason = successAILResponse(productDocCtx);
			
			return Response.status(Response.Status.OK.getStatusCode()).entity(responseJason).build();
		} catch (NotFoundException e) {
			e.printStackTrace();
			responseCode.setStatus(ErrorCodeConstants.ERROR);
			responseCode.setMessage("User not found based on federationId");
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by getUserAIL() : " + elapsedTime);
			LOGGER.error("NotFoundException in getUserAIL() :: -> " + e.getMessage());
			return Response.status(Response.Status.NOT_FOUND).entity(responseCode).build();
		} catch (NotAuthorizedException e) {
			e.printStackTrace();
			responseCode.setStatus(ErrorCodeConstants.ERROR);
			responseCode.setMessage("HTTP 401 Unauthorized or Session expired");
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by getUserAIL() : " + elapsedTime);
			LOGGER.error("NotAuthorizedException in getUserAIL() :: -> " + e.getMessage());
			return Response.status(Response.Status.UNAUTHORIZED).entity(responseCode).build();
		}catch (Exception e) {
			e.printStackTrace();
			responseCode.setStatus(ErrorCodeConstants.ERROR);
			responseCode.setMessage("Error in Getting the AIL Object is="+e.getMessage());
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
			((ObjectNode) dataAIL).put("aclType", appName);
			((ObjectNode) dataAIL).put("acl", null != productDocCtx.read("$.IDMSAIL_"+appName+"s_c")
					? getValue(productDocCtx.read("$.IDMSAIL_"+appName+"s_c").toString()) : getDelimeter());
			array.add(dataAIL);
		}
		
		((ObjectNode) AILInfo).put("ails", array);
		
		return AILInfo;
	}
	/*private JsonObject failureAILResponse(){
		JsonObject AILInfo = new JsonObject();
		AILInfo.addProperty(property, value);
		
		return AILInfo;
	}*/

}




class UserAILInfoDTO {

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
}

package com.schneider.idms.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.ws.rs.NotFoundException;
import javax.ws.rs.core.Response;

import org.json.simple.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.idms.model.AILRequest;
import com.idms.product.model.Attributes;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Option;
import com.schneider.idms.service.UpdateAILService;
import com.se.idms.dto.AILResponse;
import com.se.idms.dto.IDMSUserAIL;
import com.se.idms.dto.IDMSUser__r;
import com.se.idms.util.JsonConstants;
import com.se.idms.util.UserConstants;

/**
 * @author SESA508936
 *
 */
@Service("updateAILService")
public class UpdateAILServiceImpl extends IdmsCommonServiceImpl implements UpdateAILService {
	private static final Logger LOGGER = LoggerFactory.getLogger(UpdateAILServiceImpl.class);


	/* (non-Javadoc)
	 * @see com.schneider.idms.service.UpdateAILService#updateAIL(java.lang.String, java.lang.String, com.idms.model.AILRequest)
	 */
	@Override
	public Response updateAIL(String clientId, String clientSecret, AILRequest ailRequest) {
		LOGGER.info("Entered updateAIL() -> Start");
		LOGGER.info("Parameter clientId -> "+clientId+" ,clientSecret -> "+clientSecret);
		LOGGER.info("Parameter ailRequest -> "+ailRequest);

		String IDMSAil__c = "";
		String userData = "";
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		String idmsAclType_c = null;
		String userId = null;
		String openamVnew=null;
		String iPlanetDirectoryKey = null;
		String userName="";
		Integer vNewCntValue=0;
		List<String> listOfAil_c =null;
		String PRODUCT_JSON_STRING = "";
		String usermail = "";
		// Validate Input Paramenters
		
		ObjectMapper objMapper = new ObjectMapper();
		

		// Checking Profile Update Source is empty
		if (null == ailRequest.getUserAILRecord().getIDMS_Profile_update_source__c()
				|| ailRequest.getUserAILRecord().getIDMS_Profile_update_source__c().isEmpty()) {
			userResponse.setStatus(errorStatus);
			userResponse.setMessage(UserConstants.MANDATORY_PROFILE_UPDATE_SOURCE);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.error("Error is "+userResponse.getMessage());
			LOGGER.info("Time taken by UserServiceImpl.updateAIL() : " + elapsedTime);
			return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
		}

		// UID
		if (null != ailRequest.getUserAILRecord().getIDMS_Profile_update_source__c() && !UserConstants.UIMS
				.equalsIgnoreCase(ailRequest.getUserAILRecord().getIDMS_Profile_update_source__c())) {
			if ((null == ailRequest.getUserAILRecord().getIDMS_Federated_ID__c()
					|| ailRequest.getUserAILRecord().getIDMS_Federated_ID__c().isEmpty())
					&& (null == ailRequest.getUserAILRecord().getIDMSUser__c()
							|| ailRequest.getUserAILRecord().getIDMSUser__c().isEmpty())) {
				userResponse.setStatus(errorStatus);
				userResponse.setMessage(UserConstants.MANDATORY_ID);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error is "+userResponse.getMessage());
				LOGGER.info("Time taken by UserServiceImpl.updateAIL() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
			}
		}

		// FedrationID
		if (null != ailRequest.getUserAILRecord().getIDMS_Profile_update_source__c() && UserConstants.UIMS
				.equalsIgnoreCase(ailRequest.getUserAILRecord().getIDMS_Profile_update_source__c())) {
			if (null == ailRequest.getUserAILRecord().getIDMS_Federated_ID__c()
					|| ailRequest.getUserAILRecord().getIDMS_Federated_ID__c().isEmpty()) {
				userResponse.setStatus(errorStatus);
				userResponse.setMessage(UserConstants.MANDATORY_FEDERATION_ID);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error is "+userResponse.getMessage());
				LOGGER.info("Time taken by UserServiceImpl.updateAIL() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
			}
			
			if (null == clientId || null == clientSecret) {
				userResponse.setStatus(errorStatus);
				userResponse.setMessage(UserConstants.UIMS_CLIENTID_SECRET);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error is "+userResponse.getMessage());
				LOGGER.info("Time taken by UserServiceImpl.updateUser() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
			}

			if ((null != clientId && !clientId.equalsIgnoreCase(uimsClientId))
					|| (null != clientSecret && !clientSecret.equalsIgnoreCase(uimsClientSecret))) {
				userResponse.setStatus(errorStatus);
				userResponse.setMessage(UserConstants.INVALID_UIMS_CREDENTIALS);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error is "+userResponse.getMessage());
				LOGGER.info("Time taken by UserServiceImpl.updateUser() : " + elapsedTime);
				return Response.status(Response.Status.UNAUTHORIZED).entity(userResponse).build();
			}
		}

		// IDMSAclType__c
		if (null == ailRequest.getUserAILRecord().getIDMSAclType__c()
				|| ailRequest.getUserAILRecord().getIDMSAclType__c().isEmpty() || (!pickListValidator
						.validate(UserConstants.IDMS_ACL_TYPE_C, ailRequest.getUserAILRecord().getIDMSAclType__c()))) {
			userResponse.setStatus(errorStatus);
			userResponse.setMessage(UserConstants.INVALID_ACL_TYPE);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.error("Error is "+userResponse.getMessage());
			LOGGER.info("Time taken by UserServiceImpl.updateAIL() : " + elapsedTime);
			return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
		}

		// IDMSAcl__c
		if (null == ailRequest.getUserAILRecord().getIDMSAcl__c()
				|| ailRequest.getUserAILRecord().getIDMSAcl__c().isEmpty()) {
			userResponse.setStatus(errorStatus);
			userResponse.setMessage(UserConstants.MANDATORY_ACL);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.error("Error is "+userResponse.getMessage());
			LOGGER.info("Time taken by UserServiceImpl.updateAIL() : " + elapsedTime);
			return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
		}

		// Operation
		if (null == ailRequest.getUserAILRecord().getIDMSOperation__c()
				|| ailRequest.getUserAILRecord().getIDMSOperation__c().isEmpty()
				|| (!pickListValidator.validate(UserConstants.IDMS_OPERATION_C,
						ailRequest.getUserAILRecord().getIDMSOperation__c()))) {
			userResponse.setStatus(errorStatus);
			userResponse.setMessage(UserConstants.INVALID_OPERATION);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.error("Error is "+userResponse.getMessage());
			LOGGER.info("Time taken by UserServiceImpl.updateAIL() : " + elapsedTime);
			return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
		}

		try {
			LOGGER.info("UserServiceImpl:updateAIL -> : Requset :  -> "+objMapper.writeValueAsString(ailRequest));
			
			idmsAclType_c = getIDMSAclType(ailRequest.getUserAILRecord().getIDMSAclType__c());
			LOGGER.info("AIL type="+idmsAclType_c);
			// Getting the user data
			LOGGER.info("Going to call getSSOToken()");
			iPlanetDirectoryKey = getSSOToken();
			LOGGER.error("call getSSOToken() finished");

			if (null != ailRequest.getUserAILRecord().getIDMS_Profile_update_source__c() && UserConstants.UIMS
					.equalsIgnoreCase(ailRequest.getUserAILRecord().getIDMS_Profile_update_source__c())) {
				
				Response fedResponse = checkUserExistsWithFederationID(iPlanetDirectoryKey,
						ailRequest.getUserAILRecord().getIDMS_Federated_ID__c(), startTime);

				if (fedResponse.getStatus() == 200) {
					JSONObject uimsResponse = (JSONObject) fedResponse.getEntity();
					userId = (String) uimsResponse.get("userId");
				} else {
					return fedResponse;
				}
			} else {
				userId = ailRequest.getUserAILRecord().getIDMSUser__c();
				if(null == userId ){
					userId = ailRequest.getUserAILRecord().getIDMS_Federated_ID__c();
				}
			}

			if (null != userId) {
				LOGGER.info("AUDIT:requestingUser" + userId + "," + "impersonatingUser : amadmin,"
						+ "openAMApi:GET/getUser/{userId}");

				LOGGER.info("Going to call getUser() of OpenAMService for userId="+userId);
				userData = productService.getUser(iPlanetDirectoryKey, userId);
				LOGGER.info("getUser() of OpenAMService finsihed for userId="+userId);
				LOGGER.info("productService.getUser : Response -> "+userData);
			}
			Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
			DocumentContext productDocCtx = JsonPath.using(conf).parse(userData);
			//LOGGER.info("SSOTOKEN--------------------------->" + iPlanetDirectoryKey);
			IDMSAil__c = productDocCtx.read("$.IDMSAil_c[0]");
			
			
			//IDMSAil__c = IDMSAil__c.replace("\"", "");
			//LOGGER.info("1st  var IDMSAil_c" + IDMSAil__c);
			
			if (null != IDMSAil__c) {
				listOfAil_c = Arrays.asList(IDMSAil__c.replaceAll("[\\(\\)\\[\\]\\{\\}]", "").split(","));
			} else{
				listOfAil_c = new ArrayList<String>();
			}

			usermail = productDocCtx.read("$.mail[0]");
			
			// Updating the IDMSAil__c attribute based on the provided operation
			if ((!listOfAil_c.contains(ailRequest.getUserAILRecord().getIDMSAclType__c() + ";"
					+ ailRequest.getUserAILRecord().getIDMSAcl__c()))
					&&("GRANT".equalsIgnoreCase(ailRequest.getUserAILRecord().getIDMSOperation__c()))) {
				String aclType_c = productDocCtx.read("$.IDMSAIL_" + idmsAclType_c + "_c[0]");
			
				// Checking the value does not contain null value
				if (!(aclType_c == null || aclType_c.length() == 0))
					aclType_c = aclType_c + "," + ailRequest.getUserAILRecord().getIDMSAcl__c();
				else
					aclType_c = ailRequest.getUserAILRecord().getIDMSAcl__c();
				PRODUCT_JSON_STRING = "{" + "\"IDMSAIL_" + idmsAclType_c + "_c\": \"" + aclType_c.trim() + "\""
						+ "}";
				LOGGER.info("AUDIT:requestingUser" + userId + "," + "impersonatingUser : amadmin,"
						+ "openAMApi:GET/updateUser/{userId}");
				LOGGER.info("Grant Operation: UpdateAIL : Request -> "+PRODUCT_JSON_STRING);
				LOGGER.info("Going to call updateUser() of OpenAMService for userId="+userId);
				productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userId,
						PRODUCT_JSON_STRING);
				LOGGER.info("updateUser() of OpenAMService finished for userId="+userId);

				/*String tempString = IDMSAil__c.substring(0, IDMSAil__c.length() - 2);
				tempString = tempString.replaceAll("\\[", "");
				*///LOGGER.info("tempString---------->" + tempString);
				// tempString=tempString.substring(1);
				if (null != IDMSAil__c && !IDMSAil__c.isEmpty())
					IDMSAil__c = "" + IDMSAil__c + ",(" + ailRequest.getUserAILRecord().getIDMSAclType__c() + ";"
							+ ailRequest.getUserAILRecord().getIDMSAcl__c() + ")";
				else
					IDMSAil__c = "(" + ailRequest.getUserAILRecord().getIDMSAclType__c() + ";"
							+ ailRequest.getUserAILRecord().getIDMSAcl__c() + ")" ;
				
				// Update the IDMSAil__c in OpenAm
				PRODUCT_JSON_STRING = "{" + "\"IDMSAil_c\": \"" + IDMSAil__c.trim() + "\"" + "}";
				LOGGER.info("AUDIT:requestingUser" + userId + "," + "impersonatingUser : amadmin,"
						+ "openAMApi:GET/getUser/{userId}");
				LOGGER.info("productService.updateUser : Request -> "+PRODUCT_JSON_STRING);
				LOGGER.info("Going to call updateUser() of OpenAMService for userId="+userId);
				productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userId, PRODUCT_JSON_STRING);
				LOGGER.info("updateUser() of OpenAMService finished for userId="+userId);
				LOGGER.info("IDMSAil__c Modified After Grant Operation -------------->" + IDMSAil__c);
			} else if ((listOfAil_c.contains(ailRequest.getUserAILRecord().getIDMSAclType__c() + ";"
					+ ailRequest.getUserAILRecord().getIDMSAcl__c()))&&("REVOKE".equalsIgnoreCase(ailRequest.getUserAILRecord().getIDMSOperation__c()))) {
				IDMSAil__c = productDocCtx.read("$.IDMSAil_c[0]");
				IDMSAil__c = IDMSAil__c.replaceAll("\\[", "");
				IDMSAil__c = IDMSAil__c.replaceAll("\\]", "");
				String[] ailParts = IDMSAil__c.split(",");
				IDMSAil__c = "";
				for (String pair : ailParts) {
					pair = pair.replace("\"", "");
					String revokepair = "(" + ailRequest.getUserAILRecord().getIDMSAclType__c() + ";"
							+ ailRequest.getUserAILRecord().getIDMSAcl__c() + ")";
					if (!pair.equalsIgnoreCase(revokepair)) {
						IDMSAil__c = IDMSAil__c + pair + ",";
					}
				}
				IDMSAil__c = IDMSAil__c.replace("\"", "");
				if (!(IDMSAil__c == null || IDMSAil__c.length() == 0))
					IDMSAil__c = IDMSAil__c.substring(0, IDMSAil__c.length() - 1);
				String aclType = productDocCtx.read("$.IDMSAIL_" + idmsAclType_c + "_c[0]");				
				ailParts = aclType.split(",");
				aclType = "";
				for (String pair : ailParts) {
					String revokepair = ailRequest.getUserAILRecord().getIDMSAcl__c();
					if (!pair.equalsIgnoreCase(revokepair)) {
						aclType = aclType + pair + ",";
					}
				}

				if (!(aclType == null || aclType.length() == 0)) {
					aclType = aclType.substring(0, aclType.length() - 1);
					aclType = aclType.replaceAll("\\[", "");
					aclType = aclType.replaceAll("\\]", "");
				}
								
				if(null == aclType || aclType.isEmpty()){
					PRODUCT_JSON_STRING = "{" + "\"IDMSAIL_" + idmsAclType_c + "_c\":".concat("[]}");
				}else{
					PRODUCT_JSON_STRING = "{" + "\"IDMSAIL_" + idmsAclType_c + "_c\": \"" + aclType + "\""
							+ "}";	
				}
				LOGGER.info("AUDIT:requestingUser" + userId + "," + "impersonatingUser : amadmin,"
						+ "openAMApi:GET/getUser/{userId}");
				LOGGER.info("Revoke Operation: UpdateAIL : Request -> "+PRODUCT_JSON_STRING);
				LOGGER.info("Going to call updateUser() of OpenAMService for userId="+userId);
				productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userId,
						PRODUCT_JSON_STRING);
				LOGGER.info("updateUser() of OpenAMService finished for userId="+userId);

				// Update the IDMSAil__c in OpenAm
				if(null == IDMSAil__c || IDMSAil__c.isEmpty()){
					PRODUCT_JSON_STRING = "{" + "\"IDMSAil_c\":".concat("[]}");
				}else{
				PRODUCT_JSON_STRING = "{" + "\"IDMSAil_c\": \"" + IDMSAil__c.trim() + "\"" + "}";
				}
				LOGGER.info("AUDIT:requestingUser" + userId + "," + "impersonatingUser : amadmin,"
						+ "openAMApi:GET/getUser/{userId}");
				LOGGER.info("Revoke Operation -> : productService.updateUser : Request -> "+PRODUCT_JSON_STRING);
				LOGGER.info("Going to call updateUser() of OpenAMService for userId="+userId);
				productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userId, PRODUCT_JSON_STRING);
				LOGGER.info("updateUser() of OpenAMService finished for userId="+userId);
			}

			// Building the Response
			Attributes idmsUser_rAttributes = new Attributes();
			IDMSUser__r idmsUser__r = new IDMSUser__r();
			idmsUser__r.setAttributes(idmsUser_rAttributes);
			idmsUser__r.setId(userId);
			idmsUser__r.setIDMS_Federated_ID__c(userId);

			IDMSUserAIL idmsUserAIL = new IDMSUserAIL(idmsUser__r);
			Attributes attributes = new Attributes();
			attributes.setType("IDMSUserAIL__c");
			idmsUserAIL.setAttributes(attributes);
			idmsUserAIL.setId(userId);
			idmsUserAIL
					.setIdms_Profile_update_source__c(ailRequest.getUserAILRecord().getIDMS_Profile_update_source__c());
			idmsUserAIL.setIdmsaclType__c(ailRequest.getUserAILRecord().getIDMSAclType__c());
			if ("REVOKE".equalsIgnoreCase(ailRequest.getUserAILRecord().getIDMSOperation__c()))
				idmsUserAIL.setIdmsisRevokedOperation__c(true);
			else
				idmsUserAIL.setIdmsisRevokedOperation__c(false);
			idmsUserAIL.setIdmsoperation__c(ailRequest.getUserAILRecord().getIDMSOperation__c());
			idmsUserAIL.setIdmsacl__c(ailRequest.getUserAILRecord().getIDMSAcl__c());
			idmsUserAIL.setIdmsuser__c(userId);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.updateAIL() : " + elapsedTime);
			userName=productDocCtx.read("$.result[0].username");
			openamVnew = null != productDocCtx.read("$.V_New[0]") ? getValue(productDocCtx.read("$.V_New[0]")) : getDelimeter();
			
			if(null != vNewCntValue && null != openamVnew){
				vNewCntValue = Integer.parseInt(openamVnew)+ 1;
			}
			String version = "{\"V_New\": \"" + vNewCntValue + "\"" + "}";
			
			//calling Async methods of UIMS api in updateUserAil IDMS api
			if (null != ailRequest.getUserAILRecord().getIDMS_Profile_update_source__c() && 
					!UserConstants.UIMS.equalsIgnoreCase(ailRequest.getUserAILRecord().getIDMS_Profile_update_source__c())) {
				// Adding V_New
				LOGGER.info("version -> "+version);
				LOGGER.info("Going to call updateUser() of OpenAMService for userId="+userId+" ,version="+version);
				productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userId, version);
				LOGGER.info("updateUser() of OpenAMService finished for userId="+userId+" ,version="+version);
				LOGGER.info("Going to call updateUIMSUserAIL() of UIMSAccessManagerSoapService for usermail="+usermail);
				/*directUIMSUserManagerSoapService.updateUIMSUserAIL(ailRequest, idmsUserAIL,vNewCntValue.toString(),productService,
						UserConstants.CHINA_IDMS_TOKEN +iPlanetDirectoryKey, usermail);*/
				LOGGER.info("updateUIMSUserAIL() of UIMSAccessManagerSoapService finished for usermail="+usermail);
			} else {
				//productService.sessionLogout(UserConstants.IPLANET_DIRECTORY_PRO+iPlanetDirectoryKey, "logout");
			}
			return updateAILSuccessResponse(idmsUserAIL);
		} catch (NotFoundException e) {
			e.printStackTrace();
			userResponse.setStatus(errorStatus);
			userResponse.setMessage("User not found based on user Id");
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.updateAIL() : " + elapsedTime);
			LOGGER.error("Executing while updateAIL() :: -> " + userResponse.getMessage());
			LOGGER.error("Executing while updateAIL() :: -> " + e.getMessage());
			//productService.sessionLogout(UserConstants.IPLANET_DIRECTORY_PRO+iPlanetDirectoryKey, "logout");
			return Response.status(Response.Status.NOT_FOUND).entity(userResponse).build();
		} catch (Exception e) {
			e.printStackTrace();
			userResponse.setStatus(errorStatus);
			userResponse.setMessage("Error in Updating the AIL Object");
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.updateAIL() : " + elapsedTime);
			LOGGER.error("Executing while updateAIL() :: -> " + userResponse.getMessage());
			LOGGER.error("Executing while updateAIL() :: -> " + e.getMessage());
			//productService.sessionLogout(UserConstants.IPLANET_DIRECTORY_PRO+iPlanetDirectoryKey, "logout");
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(userResponse).build();
		}
	}
	
	
	/**
	 * 
	 * @param iPlanetDirectoryToken
	 * @param federationId
	 * @param startTime
	 * @return
	 */
	@SuppressWarnings({ "unchecked" })
	private Response checkUserExistsWithFederationID(String iPlanetDirectoryToken, String federationId,
			long startTime) {
		LOGGER.info("Entered checkUserExistsWithFederationID() -> Start");
		//LOGGER.info("Parameter iPlanetDirectoryToken -> " + iPlanetDirectoryToken);
		LOGGER.info("Parameter federationId -> " + federationId);
		//LOGGER.info("Parameter startTime -> " + startTime);
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		DocumentContext productDocCtx = null;
		JSONObject uimsResponse = new JSONObject();
		long elapsedTime;
		String userId = null;
		String loginIdentifierType = "";

		LOGGER.info("going to call checkUserExistsWithEmailMobile() of OpenAMService for federationId="+federationId);
		String userExists = productService.checkUserExistsWithEmailMobile(
				UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryToken, "federationID eq " + "\"" + federationId + "\"");
		LOGGER.info("checkUserExistsWithEmailMobile() of OpenAMService finished for federationId="+federationId);
		LOGGER.info("User Exist with fed ID= " + userExists);

		productDocCtx = JsonPath.using(conf).parse(userExists);
		Integer resultCount = productDocCtx.read(JsonConstants.RESULT_COUNT);

		if (resultCount.intValue() == 0) {
			uimsResponse = new JSONObject();
			uimsResponse.put(UserConstants.STATUS, errorStatus);
			uimsResponse.put(UserConstants.MESSAGE, UserConstants.USER_NOT_EXISTS);
			uimsResponse.put(UserConstants.FEDERATION_IDENTIFIER, federationId);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.error("Error is -> " + UserConstants.USER_NOT_EXISTS);
			LOGGER.info("Time taken by UserServiceImpl.updateUser() : " + elapsedTime);
			return Response.status(Response.Status.NOT_FOUND).entity(uimsResponse).build();
		} else {
			userId = productDocCtx.read(JsonConstants.RESULT);
			loginIdentifierType = productDocCtx.read(JsonConstants.RESULT_Loginid);
			if (null != loginIdentifierType && !loginIdentifierType.isEmpty()) {
				if (emailValidator.validate(loginIdentifierType)) {
					uimsResponse.put("loginIdentity", "Email");
				} else {
					uimsResponse.put("loginIdentity", "Mobile");
				}
			}
			uimsResponse.put("userId", userId);
		}
		return Response.status(Response.Status.OK).entity(uimsResponse).build();
	}
	
	private String getIDMSAclType(String aclType) {
		if (UserConstants.ACLTYPE_APPLICATION.equalsIgnoreCase(aclType))
			return UserConstants.ACLTYPE_APPLICATIONS;
		else if (UserConstants.ACLTYPE_PROGRAM.equalsIgnoreCase(aclType))
			return UserConstants.ACLTYPE_PROGRAMS;
		else if (UserConstants.ACLTYPE_FEATURE.equalsIgnoreCase(aclType))
			return UserConstants.ACLTYPE_FEATURES;

		return null;
	}
	
	private Response updateAILSuccessResponse(IDMSUserAIL idmsUserAIL) {
		LOGGER.info("Entered updateAILSuccessResponse() -> Start");
		LOGGER.info("Parameter idmsUserAIL -> "+idmsUserAIL);

		AILResponse ailResponse;
		ailResponse = new AILResponse(idmsUserAIL);
		ailResponse.setStatus(successStatus);
		ailResponse.setMessage("User AIL updated successfully");
		LOGGER.info("updateAILSuccessResponse() -> End");
		return Response.status(Response.Status.OK).entity(ailResponse).build();

	}

}

package com.schneider.idms.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.ws.rs.NotFoundException;
import javax.ws.rs.core.Response;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Option;
import com.schneider.idms.common.ErrorCodeConstants;
import com.schneider.idms.common.ResponseCodeStatus;
import com.schneider.idms.model.IdmsUserAilRequest;
import com.schneider.idms.service.UpdateAILService;
import com.se.idms.util.UserConstants;

/**
 * @author SESA508936 For Direct Call API
 */
@Service("updateAILService")
public class UpdateAILServiceImpl extends IdmsCommonServiceImpl implements UpdateAILService {
	private static final Logger LOGGER = LoggerFactory.getLogger(UpdateAILServiceImpl.class);

	@Override
	public Response updateAIL(String authorization, String accept, String region, IdmsUserAilRequest userAilRequest) {
		LOGGER.info("Entered updateAIL() -> Start");
		LOGGER.info("Paramters are region=" + region);
		LOGGER.info("Paramters are authorization=" + authorization + " ,accept=" + accept);

		ObjectMapper objectMapper = new ObjectMapper();
		ResponseCodeStatus responseCode = new ResponseCodeStatus();
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		String userId = null;
		String userData = null;
		String iPlanetDirectoryKey = null;
		String inOpenAMAILFullValues = null, modifiedAILFullValues = "";
		String inRequestAILTypeValPair = null;
		String aclType = null, aclValue = null, aclOperation = null;
		String inOpenAMAILType = null;
		String inOpenAMAILValue = null, modifiedAILValue = "";
		List<String> listOfAilValues = null;
		String PRODUCT_JSON_STRING = "";
		ResponseCodeStatus responseCodeStatus = new ResponseCodeStatus();
		String federatedId = null;

		try {
			LOGGER.info("userAilRequest=" + objectMapper.writeValueAsString(userAilRequest));
			federatedId = userAilRequest.getFederatedId();

			// FederatedId
			if (null == federatedId || federatedId.isEmpty()) {
				responseCode.setStatus(ErrorCodeConstants.ERROR);
				responseCode.setMessage("Mandatory Check: federatedId is Mandatory");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
				LOGGER.error("Mandatory check: Path field federatedId is Missing or Null/Empty");
				return Response.status(Response.Status.BAD_REQUEST).entity(responseCode).build();
			}

			// Authorization
			if (null == authorization || authorization.isEmpty()) {
				responseCode.setStatus(ErrorCodeConstants.ERROR);
				responseCode.setMessage("Mandatory Check: Authorization token is null or empty");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
				LOGGER.error("Mandatory check: Header field authorization is Missing or Null/Empty");
				return Response.status(Response.Status.BAD_REQUEST).entity(responseCode).build();
			}
			
			// Validation_authorization_token
			if (!getTechnicalUserDetails(authorization)) {
				responseCode.setMessage("Mandatory Validation: Authorization token is invalid");
				responseCode.setStatus(ErrorCodeConstants.ERROR);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
				LOGGER.error("Error while processing is " + responseCode.getMessage());
				return Response.status(Response.Status.BAD_REQUEST).entity(responseCode).build();
			}

			// accept
			if (null == accept || accept.isEmpty()) {
				responseCode.setStatus(ErrorCodeConstants.ERROR);
				responseCode.setMessage("Header field accept is Mandatory");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
				LOGGER.error("Mandatory check: Header field accept is Missing or Null/Empty");
				return Response.status(Response.Status.BAD_REQUEST).entity(responseCode).build();
			}

			if (null != accept && !accept.equals("*/*") && !accept.equalsIgnoreCase("application/json")) {
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
				responseCode.setMessage("Header field IDMS-Region must be CN");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Mandatory check: Header field IDMS-Region:" + region + " is not permitted");
				return Response.status(Response.Status.BAD_REQUEST).entity(responseCode).build();
			}

			// Profile Update Source
			if (null == userAilRequest.getProfileLastUpdateSource()
					|| userAilRequest.getProfileLastUpdateSource().isEmpty()) {
				responseCode.setStatus(ErrorCodeConstants.ERROR);
				responseCode.setMessage("ProfileLastUpdateSource is Mandatory");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
				LOGGER.error("Mandatory check: ProfileLastUpdateSource is Missing or Null/Empty");
				return Response.status(Response.Status.BAD_REQUEST).entity(responseCode).build();
			}

			// acl
			if (null == userAilRequest.getAcl() || userAilRequest.getAcl().isEmpty()) {
				responseCode.setStatus(ErrorCodeConstants.ERROR);
				responseCode.setMessage("Acl is Mandatory");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
				LOGGER.error("Mandatory check: Acl is Missing or Null/Empty");
				return Response.status(Response.Status.BAD_REQUEST).entity(responseCode).build();
			}

			// aclType
			if (null == userAilRequest.getAclType() || userAilRequest.getAclType().isEmpty()) {
				responseCode.setStatus(ErrorCodeConstants.ERROR);
				responseCode.setMessage("AclType is Mandatory");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
				LOGGER.error("Mandatory check: AclType is Missing or Null/Empty");
				return Response.status(Response.Status.BAD_REQUEST).entity(responseCode).build();
			}

			if (null != userAilRequest.getAclType()
					&& (!pickListValidator.validate(UserConstants.IDMS_ACL_TYPE_C, userAilRequest.getAclType()))) {
				responseCode.setStatus(ErrorCodeConstants.ERROR);
				responseCode.setMessage("AclType is Not Valid");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
				LOGGER.error("Mandatory check: AclType " + userAilRequest.getAclType() + " is Not Valid");
				return Response.status(Response.Status.BAD_REQUEST).entity(responseCode).build();
			}

			// operation
			if (null == userAilRequest.getOperation() || userAilRequest.getOperation().isEmpty()) {
				responseCode.setStatus(ErrorCodeConstants.ERROR);
				responseCode.setMessage("AIL Operation is Mandatory");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
				LOGGER.error("Mandatory check: Operation is Null or Empty");
				return Response.status(Response.Status.BAD_REQUEST).entity(responseCode).build();
			}

			if (null != userAilRequest.getOperation()
					&& (!pickListValidator.validate(UserConstants.IDMS_OPERATION_C, userAilRequest.getOperation()))) {
				responseCode.setStatus(ErrorCodeConstants.ERROR);
				responseCode.setMessage("Operation is Not Valid");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
				LOGGER.error("Mandatory check: AIL Operation " + userAilRequest.getOperation() + " is Not Valid");
				return Response.status(Response.Status.BAD_REQUEST).entity(responseCode).build();
			}

			// writing business logic - start
			userId = federatedId;
			iPlanetDirectoryKey = getSSOToken();

			aclType = getIDMSAclType(userAilRequest.getAclType().trim());
			aclValue = userAilRequest.getAcl().trim();
			aclOperation = userAilRequest.getOperation().trim();

			LOGGER.info("ACL type=" + aclType + " ,aclValue=" + aclValue + " ,aclOperation=" + aclOperation);

			LOGGER.info("AUDIT:requestingUser->" + userId + "," + "impersonatingUser : amadmin,"
					+ "openAMApi:GET/getUser/{userId}");

			LOGGER.info("Start: calling getUser() of OpenAMService to fetch AIL values for userId=" + userId);
			userData = productService.getUser(iPlanetDirectoryKey, userId);
			LOGGER.info("End: getUser() of OpenAMService to fetch AIL values finsihed for userId=" + userId);
			LOGGER.info("Returned User Data from OpenAM=" + userData);
			Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
			DocumentContext productDocCtx = JsonPath.using(conf).parse(userData);

			inOpenAMAILFullValues = productDocCtx.read("$.IDMSAil_c[0]");
			inOpenAMAILType = "IDMSAIL_" + aclType + "_c";
			inOpenAMAILValue = productDocCtx.read("$.IDMSAIL_" + aclType + "_c[0]");

			LOGGER.info("In OpenAM AIL Full Values=" + inOpenAMAILFullValues);
			LOGGER.info("In OpenAM AIL Type=" + inOpenAMAILType + " ,Values=" + inOpenAMAILValue);

			if (null != inOpenAMAILFullValues && !inOpenAMAILFullValues.isEmpty()) {
				listOfAilValues = Arrays.asList(inOpenAMAILFullValues.split(","));
			} else {
				listOfAilValues = new ArrayList<String>();
			}

			inRequestAILTypeValPair = userAilRequest.getAclType().trim() + ";" + userAilRequest.getAcl().trim();

			// Grant OPeration
			if ((!listOfAilValues.contains("(" + inRequestAILTypeValPair + ")"))
					&& ("GRANT".equalsIgnoreCase(aclOperation))) {

				if (!(inOpenAMAILValue == null || inOpenAMAILValue.length() == 0))
					modifiedAILValue = inOpenAMAILValue + "," + aclValue;
				else
					modifiedAILValue = aclValue;

				if (null != inOpenAMAILFullValues && !inOpenAMAILFullValues.isEmpty())
					modifiedAILFullValues = "" + inOpenAMAILFullValues + ",(" + inRequestAILTypeValPair + ")";
				else
					modifiedAILFullValues = "(" + inRequestAILTypeValPair + ")";

				PRODUCT_JSON_STRING = "{" + "\"" + inOpenAMAILType + "\": \"" + modifiedAILValue.trim() + "\""
						+ ",\"IDMSAil_c\": \"" + modifiedAILFullValues.trim() + "\"}";

				LOGGER.info("Grant Operation: JSON string to update -> " + PRODUCT_JSON_STRING);
				LOGGER.info("AUDIT:requestingUser->" + userId + "," + "impersonatingUser : amadmin,"
						+ "openAMApi:GET/getUser/{userId}");
				LOGGER.info("Start: Calling updateUser() of OpenAMService to update AIL for userId=" + userId);
				productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userId,
						PRODUCT_JSON_STRING);
				LOGGER.info("End: updateUser() of OpenAMService to update AIL finished for userId=" + userId);
				// response
				responseCodeStatus.setStatus(ErrorCodeConstants.SUCCESS);
				responseCodeStatus.setMessage("AIL was granted successfully ");

				return Response.status(Response.Status.OK.getStatusCode()).entity(responseCodeStatus).build();

			} else
				// Revoke Operation
				if ((listOfAilValues.contains("(" + inRequestAILTypeValPair + ")"))
						&& ("REVOKE".equalsIgnoreCase(aclOperation))) {
					modifiedAILFullValues = "";
					List<String> ailValList = null;
					modifiedAILValue = "";
					PRODUCT_JSON_STRING = "";

					for (String pair : listOfAilValues) {
						if (!pair.equalsIgnoreCase("(" + inRequestAILTypeValPair + ")")) {
							modifiedAILFullValues = modifiedAILFullValues + pair + ",";
						}
					}
					if (null != modifiedAILFullValues && !modifiedAILFullValues.isEmpty()) {
						modifiedAILFullValues = modifiedAILFullValues.substring(0, modifiedAILFullValues.length() - 1);
					}

					if (null != inOpenAMAILValue) {
						ailValList = new ArrayList<String>(Arrays.asList(inOpenAMAILValue.split(",")));
					}

					for (String val : ailValList) {
						if (!val.equalsIgnoreCase(aclValue)) {
							modifiedAILValue = modifiedAILValue + val + ",";
						}
					}
					if (null != modifiedAILValue && !modifiedAILValue.isEmpty()) {
						modifiedAILValue = modifiedAILValue.substring(0, modifiedAILValue.length() - 1);
					}

					if (null == modifiedAILValue || modifiedAILValue.isEmpty()) {
						PRODUCT_JSON_STRING = "{\"" + inOpenAMAILType + "\": ".concat("[]}");
					} else {
						PRODUCT_JSON_STRING = "{\"" + inOpenAMAILType + "\": \"" + modifiedAILValue.trim() + "\"" + "}";
					}

					if (null == modifiedAILFullValues || modifiedAILFullValues.isEmpty()) {
						PRODUCT_JSON_STRING = PRODUCT_JSON_STRING.substring(0, PRODUCT_JSON_STRING.length() - 1)
								.concat(",\"IDMSAil_c\": ".concat("[]}"));
					} else {
						PRODUCT_JSON_STRING = PRODUCT_JSON_STRING.substring(0, PRODUCT_JSON_STRING.length() - 1)
								.concat(",\"IDMSAil_c\": \"" + modifiedAILFullValues.trim() + "\"}");
					}

					LOGGER.info("Revoke Operation: JSON String to Update=" + PRODUCT_JSON_STRING);
					LOGGER.info("Start: calling updateUser() of OpenAMService to update AIL values for userId=" + userId);

					productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userId,
							PRODUCT_JSON_STRING);
					LOGGER.info("End: updateUser() of OpenAMService to update AIL values finished for userId=" + userId);

					responseCodeStatus.setStatus(ErrorCodeConstants.SUCCESS);
					responseCodeStatus.setMessage("AIL was revoked successfully ");

					return Response.status(Response.Status.OK.getStatusCode()).entity(responseCodeStatus).build();
				} else {
					responseCode
					.setMessage("Can not perform Grant/Revoke operation. AIL Value/s already present/removed.");
					responseCode.setStatus(ErrorCodeConstants.ERROR);
					LOGGER.error("Can not perform Grant/Revoke operation. AIL Value/s already present/removed.");
					return Response.status(Response.Status.BAD_REQUEST).entity(responseCode).build();
				}
			// writing business logic - end
		} catch (JsonProcessingException e) {
			e.printStackTrace();
			LOGGER.error("JsonProcessingException = " + e.getMessage());
			responseCode.setStatus(ErrorCodeConstants.ERROR);
			responseCode.setMessage("JSON Parsing error, Please try again");
			LOGGER.error("JSON Parsing error, Please try again");
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(responseCode).build();
		} catch (NotFoundException e) {
			e.printStackTrace();
			responseCode.setStatus(ErrorCodeConstants.ERROR);
			responseCode.setMessage("User not found based on user Id");
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.updateAIL() : " + elapsedTime);
			LOGGER.error("Executing while updateAIL() :: -> " + responseCode.getMessage());
			LOGGER.error("Executing while updateAIL() :: -> " + e.getMessage());
			return Response.status(Response.Status.NOT_FOUND).entity(responseCode).build();
		} catch (Exception e) {
			e.printStackTrace();
			responseCode.setStatus(ErrorCodeConstants.ERROR);
			responseCode.setMessage("Error in Updating the AIL Object");
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.updateAIL() : " + elapsedTime);
			LOGGER.error("Executing while updateAIL() :: -> " + responseCode.getMessage());
			LOGGER.error("Executing while updateAIL() :: -> " + e.getMessage());
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(responseCode).build();
		}

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

}

package com.schneider.idms.service.impl;

import static com.se.idms.util.UserConstants.AUDIT_API_ADMIN;
import static com.se.idms.util.UserConstants.AUDIT_IMPERSONATING_USER;
import static com.se.idms.util.UserConstants.AUDIT_LOG_CLOSURE;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_API;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_GET_CALL;
import static com.se.idms.util.UserConstants.AUDIT_REQUESTING_USER;
import static com.se.idms.util.UserConstants.AUDIT_TECHNICAL_USER;

import javax.ws.rs.BadRequestException;
import javax.ws.rs.NotFoundException;
import javax.ws.rs.core.Response;

import org.apache.commons.codec.binary.Base64;
import org.json.simple.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.idms.model.ResendPinRequest;
import com.idms.service.util.UserServiceUtil;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Option;
import com.schneider.idms.service.IResendPinService;
import com.se.idms.cache.utils.EmailConstants;
import com.se.idms.util.JsonConstants;
import com.se.idms.util.UserConstants;

//@Service("resendPinService")
public class ResendPinServiceImpl extends IdmsCommonServiceImpl implements IResendPinService {
	
	/**
	 * Logger instance.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(ResendPinServiceImpl.class);


	@SuppressWarnings("unchecked")
	@Override
	public Response resendPIN(String token, ResendPinRequest resendPinRequest) {

		LOGGER.info("Entered resendPIN() -> Start");
		LOGGER.info("Parameter token -> " + token);
		LOGGER.info("Parameter resendPinRequest -> " + resendPinRequest);
		

		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		JSONObject response = new JSONObject();
		DocumentContext productDocCtx = null;
		String userData = null;
		String loginIdentifier = null;
		String tmpPR = null;
		String PRODUCT_JSON_STRING = null;
		String iPlanetDirectoryKey = null;
		String sendEmailOptType = "";
		String resendId= "";
		ObjectMapper objMapper=new ObjectMapper();

		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		response.put(UserConstants.STATUS, UserConstants.STATUS_FAILD);
		try {
			LOGGER.info("UserServiceImpl:resendPIN : Request   -> " + objMapper.writeValueAsString(resendPinRequest));
			iPlanetDirectoryKey = getSSOToken();
			
			if ((null == resendPinRequest.getIdmsUserId() || resendPinRequest.getIdmsUserId().isEmpty())
					&& (null == resendPinRequest.getIDMS_Federated_ID__c()|| resendPinRequest.getIDMS_Federated_ID__c().isEmpty())
					&& (null == resendPinRequest.getFederationIdentifier()|| resendPinRequest.getFederationIdentifier().isEmpty())
					&& (null == resendPinRequest.getFederationId()|| resendPinRequest.getFederationId().isEmpty())) {
				response.put(UserConstants.STATUS, UserConstants.STATUS_FAILD);
				response.put(UserConstants.MESSAGE, UserConstants.RESPONSE_MESSAGE_NULL);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error(UserConstants.RESPONSE_MESSAGE_NULL);
				LOGGER.info("Time taken by UserServiceImpl.resendPIN() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();

			}else{
				
				if(null != resendPinRequest.getIdmsUserId() && !resendPinRequest.getIdmsUserId().isEmpty()){
					resendId = resendPinRequest.getIdmsUserId();
				} else if(null != resendPinRequest.getIDMS_Federated_ID__c() && !resendPinRequest.getIDMS_Federated_ID__c().isEmpty()){
					resendId = resendPinRequest.getIDMS_Federated_ID__c();
				}else if(null != resendPinRequest.getFederationId() && !resendPinRequest.getFederationId().isEmpty()){
					resendId = resendPinRequest.getFederationId();
				}else{
					resendId = resendPinRequest.getFederationIdentifier();
				}
				/*if((null == resendId)|| (resendId.isEmpty()) || "".equalsIgnoreCase(resendId)){
					resendId = resendPinRequest.getIDMS_Federated_ID__c();
				}*/
			}
			// Federation Identifier
			/*if (null == resendPinRequest.getFederationId() || resendPinRequest.getFederationId().isEmpty()) {
				response.put(UserConstants.STATUS, UserConstants.STATUS_FAILD);
				response.put(UserConstants.MESSAGE, UserConstants.MANDATORY_FEDERATION_ID);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.setPassword() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}*/

			if (null != resendId) {
				LOGGER.info(AUDIT_REQUESTING_USER + AUDIT_TECHNICAL_USER + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN
						+ AUDIT_OPENAM_API + AUDIT_OPENAM_GET_CALL + AUDIT_LOG_CLOSURE);
				userData = UserServiceUtil.getUserBasedOnFRVersion(productService, frVersion, iPlanetDirectoryKey, resendId);
				LOGGER.info("user data from Openam: " + userData);

				productDocCtx = JsonPath.using(conf).parse(userData);

				if (UserConstants.USER_REGISTRATION.equalsIgnoreCase(resendPinRequest.getOperation())) {
					loginIdentifier = productDocCtx.read("$.mobile[0]");
				} else if(UserConstants.UPDATE_USER_RECORD.equals(resendPinRequest.getOperation())){
					loginIdentifier = productDocCtx.read("$.newmobile[0]");
				}else{
					loginIdentifier = productDocCtx.read(JsonConstants.LOGIN_ID_LOWER_0);
					if (null == loginIdentifier) {
						loginIdentifier = productDocCtx.read(JsonConstants.LOGIN_ID_UPPER_0);
					}
				}
				if (null != loginIdentifier && validateMobile(loginIdentifier)) {

					tmpPR = productDocCtx.read("$.tmp_password[0]");
					if (null == tmpPR || tmpPR.isEmpty()) {
						tmpPR = generateRamdomPassWord();
					} else {
						tmpPR = new String(Base64.decodeBase64(tmpPR));
					}

					if (UserConstants.USER_REGISTRATION.equalsIgnoreCase(resendPinRequest.getOperation())) {
						sendEmailOptType = EmailConstants.USERREGISTRATION_OPT_TYPE;
					} else if (UserConstants.UPDATE_USER_RECORD.equalsIgnoreCase(resendPinRequest.getOperation())) {
						sendEmailOptType = EmailConstants.UPDATEUSERRECORD_OPT_TYPE;
					} else {
						sendEmailOptType = EmailConstants.SETUSERPWD_OPT_TYPE;
					}


					LOGGER.info("UserServiceImpl:resendPIN : productService.updateUser : Request   -> " + PRODUCT_JSON_STRING);
					String regestrationSource=productDocCtx.read("$.registerationSource[0]");
					if ((EmailConstants.UPDATEUSERRECORD_OPT_TYPE.equalsIgnoreCase(sendEmailOptType) && null != productDocCtx.read("$.newmail[0]"))
							|| EmailConstants.USERREGISTRATION_OPT_TYPE.equalsIgnoreCase(sendEmailOptType)
							|| EmailConstants.SETUSERPWD_OPT_TYPE.equalsIgnoreCase(sendEmailOptType)) {
						String otp = sendEmail.generateOtp(resendId);
						LOGGER.info("Successfully OTP generated for "+resendId);
						sendEmail.sendSMSNewGateway(otp, sendEmailOptType, resendId, regestrationSource);
						
						sendEmail.sendOpenAmMobileEmail(otp, sendEmailOptType, resendId, regestrationSource);
						//sendEmail.sendOpenAmEmail(otp, sendEmailOptType, resendId, regestrationSource);
					} else {
						response.put(UserConstants.STATUS, UserConstants.STATUS_FAILD);
						response.put(UserConstants.MESSAGE, UserConstants.RESEND_UPDATEOPTTYPE_ERROR);
						elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
						LOGGER.error(UserConstants.RESEND_UPDATEOPTTYPE_ERROR);
						LOGGER.info("Time taken by UserServiceImpl.resendPIN() : " + elapsedTime);
						return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(response).build();
					}
				} else {
					response.put(UserConstants.STATUS, UserConstants.STATUS_FAILD);
					response.put(UserConstants.MESSAGE, UserConstants.RESEND_ONLYMOBILE_ERROR_MESSAGE);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.error(UserConstants.RESEND_ONLYMOBILE_ERROR_MESSAGE);
					LOGGER.info("Time taken by UserServiceImpl.resendPIN() : " + elapsedTime);
					return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(response).build();
				}

			} else {
				response.put(UserConstants.STATUS, UserConstants.STATUS_FAILD);
				response.put(UserConstants.MESSAGE, UserConstants.RESPONSE_MESSAGE);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error(UserConstants.RESPONSE_MESSAGE);
				LOGGER.info("Time taken by UserServiceImpl.resendPIN() : " + elapsedTime);
				return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(response).build();
			}
		} catch (NotFoundException e) {
			response.put(UserConstants.MESSAGE, UserConstants.ERROR_RESEND_PIN);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.resendPIN() : " + elapsedTime);
			LOGGER.error("Executing while Resending User PIN :: -> " + e.getMessage());
			return Response.status(Response.Status.NOT_FOUND).entity(response).build();
		} catch (BadRequestException e) {
			response.put(UserConstants.MESSAGE, UserConstants.ERROR_RESEND_PIN);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.resendPIN() : " + elapsedTime);
			LOGGER.error("Executing while Resending User PIN :: -> " + e.getMessage());
			return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
		} catch (Exception e) {
			response.put(UserConstants.MESSAGE, UserConstants.ERROR_RESEND_PIN);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.resendPIN() : " + elapsedTime);
			LOGGER.error("Executing while Resending User PIN :: -> " + e.getMessage(),e);
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(response).build();
		}
		response.put(UserConstants.STATUS, successStatus);
		response.put(UserConstants.MESSAGE, "Pin Code has been sent successfully");
		elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		LOGGER.info("Time taken by UserServiceImpl.resendPIN() : " + elapsedTime);
		return Response.status(Response.Status.OK).entity(response).build();
	
	}

	

}

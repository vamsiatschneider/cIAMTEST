package com.idms.service.uims.sync;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;

import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.rholder.retry.RetryException;
import com.github.rholder.retry.Retryer;
import com.github.rholder.retry.RetryerBuilder;
import com.github.rholder.retry.StopStrategies;
import com.google.common.base.Predicates;
import com.idms.mapper.IdmsMapper;
import com.idms.model.CreateUserRequest;
import com.idms.model.UserRegistrationInfoRequest;
import com.idms.model.digital.Authentication;
import com.idms.product.client.OpenAMService;
import com.idms.service.SendEmail;
import com.idms.service.digital.GoDigitalUserService;
import com.se.idms.util.UimsConstants;
import com.se.idms.util.UserConstants;
import com.uims.authenticatedUsermanager.AccessElement;
import com.uims.authenticatedUsermanager.Type;
import com.uims.companymanager.CompanyV3;

/**
 * The Soap Service interface layer to call the UIMS user manager stubs.
 * 
 * @author Aravindh Kumar
 *
 */
@org.springframework.stereotype.Service("uimsUserManagerSoapServiceSync")
public class UIMSUserManagerSoapServiceSync {
	private static final Logger LOGGER = LoggerFactory.getLogger(UIMSUserManagerSoapServiceSync.class);
	
	@Autowired
	private UIMSAuthenticatedUserManagerSoapServiceSync authenticatedUserManagerSoapServiceSync;
	
	@Inject
	private OpenAMService productService;
	
	@Inject
	private IdmsMapper mapper;
	
	@Autowired
	private UIMSCompanyManagerSoapServiceSync companyManagerSoapServiceSync;
	
	@Value("${supportUser}")
	private String supportUser;
	
	@Value("${fromUserName}")
	private String fromUserName;
	
	@Value("${goDigitalValue}")
	private String goDigitalValue;
	
	@Value("${goDitalToken}")
	private String goDitalToken;
	
	@Autowired
	private GoDigitalUserService goDigitalUserService;
	
	private String applicationName = "Uims";
	private String createdFedId = null;
	private SendEmail sendEmail;
	String createdCompanyFedId = null;
	
	public String createUIMSUserAndCompany(String callerFid, com.uims.authenticatedUsermanager.UserV6 identity,
			String context, CompanyV3 company, String userName,
			String iPlanetDirectoryKey, String v_new, String password, String forcedFederatedId,
			CreateUserRequest userRequest) {
		LOGGER.info("Entered createUIMSUserAndCompany() -> Start");
		LOGGER.info("Parameter callerFid -> " + callerFid);
		LOGGER.info("Parameter context -> " + context);
		LOGGER.info("Parameter userName -> " + userName+" ,iPlanetDirectoryKey -> "+iPlanetDirectoryKey);
		LOGGER.info("Parameter v_new -> " + v_new+" ,forcedFederatedId -> "+forcedFederatedId);
			
		
		Boolean companyCreated = false;
		Boolean userCreated = false;
		AccessElement application = new AccessElement();
		application.setId(applicationName);
		application.setType(Type.APPLICATION);
		
		ObjectMapper objMapper = new ObjectMapper();
		String userRequestjsonString = "";
		try {
			LOGGER.info("Parameter userRequest -> " + objMapper.writeValueAsString(userRequest));
			LOGGER.info("Parameter identity -> " + objMapper.writeValueAsString(identity));
			LOGGER.info("Parameter company -> " + objMapper.writeValueAsString(company));
			
			userRequestjsonString = objMapper.writeValueAsString(userRequest);
			userRequestjsonString = userRequestjsonString.replace("\"\"", "[]");
		} catch (JsonProcessingException e) {
			LOGGER.error("Error while converting the userRequest to Json" + e.getMessage());
			e.printStackTrace();
		}finally{
			if(null != objMapper){
				objMapper = null;
			}
		}
		try {
			Callable<Boolean> callableUser = new Callable<Boolean>() {
				public Boolean call() throws Exception {

					if (null != password && !password.isEmpty()) {
						createdFedId = authenticatedUserManagerSoapServiceSync.createUIMSUserWithPassword(
								UimsConstants.CALLER_FID, identity, password, forcedFederatedId);
					} else {
						createdFedId = authenticatedUserManagerSoapServiceSync.createUIMSUser(UimsConstants.CALLER_FID,
								identity, forcedFederatedId);
					}

					if (null != createdFedId) {
						String fedID = "{" + "\"federationID\": \"" + createdFedId + "\"" + "}";
						LOGGER.info("fedID in creating UIMS user: " + fedID);
						productService.updateUser(iPlanetDirectoryKey, userName, fedID);
					}

					return true;
				}
			};

			Retryer<Boolean> retryer = RetryerBuilder.<Boolean> newBuilder()
					.retryIfResult(Predicates.<Boolean> isNull()).retryIfExceptionOfType(Exception.class)
					.retryIfRuntimeException().withStopStrategy(StopStrategies.stopAfterAttempt(3)).build();
			try {
				userCreated = retryer.call(callableUser);
				if (userCreated) {
					LOGGER.info("UIMS user created successfully::" + userCreated);
				}
			} catch (RetryException e) {
				LOGGER.error("Retry failed while calling the UIMS create user::" + e.getMessage());
				e.printStackTrace();
				
			} catch (ExecutionException e) {
				LOGGER.error("ExecutionException while calling the UIMS create user::" + e.getMessage());
				e.printStackTrace();
			}

			if((!userCreated || null == createdFedId) && (null != context && (UserConstants.USER_CONTEXT_HOME.equalsIgnoreCase(context)||UserConstants.USER_CONTEXT_HOME_1.equalsIgnoreCase(context)))) {
				LOGGER.error("CreateUser got failed -----> ::sending mail notification for userRequestjsonString::"+userRequestjsonString);
				sendEmail.emailReadyToSendEmail(supportUser, fromUserName, "UIMS CreateUser failed.", userRequestjsonString);
			}
			Callable<Boolean> callableCompany = new Callable<Boolean>() {
				public Boolean call() throws Exception {
					/**
					 * When user is creating from BFO then no need of creating
					 * company, bfo account id act as company
					 */
					if (null != userRequest.getUserRecord().getAdminCompanyFederatedId()
							&& !userRequest.getUserRecord().getAdminCompanyFederatedId().isEmpty()) {
						createdCompanyFedId = userRequest.getUserRecord().getAdminCompanyFederatedId();
					} else if (null != userRequest.getUserRecord().getBFO_ACCOUNT_ID__c()
							&& !userRequest.getUserRecord().getBFO_ACCOUNT_ID__c().isEmpty()) {
						createdCompanyFedId = userRequest.getUserRecord().getBFO_ACCOUNT_ID__c();
					} else if ((null != userRequest.getUserRecord().getAdminCompanyFederatedId()
							&& !userRequest.getUserRecord().getAdminCompanyFederatedId().isEmpty())
							&& (null != userRequest.getUserRecord().getAdminCompanyFederatedId()
									&& !userRequest.getUserRecord().getAdminCompanyFederatedId().isEmpty())) {
						createdCompanyFedId = userRequest.getUserRecord().getAdminCompanyFederatedId();
					} else if (null != userRequest.getUserRecord().getIDMS_Registration_Source__c() 
							&& UserConstants.PRM.equals(userRequest.getUserRecord().getIDMS_Registration_Source__c())){
						//if registration source is PRM then accept and force the company FederatedId from IFW/IDMS global
						createdCompanyFedId = userRequest.getUserRecord().getCompanyFederatedId();
					} else {

						createdCompanyFedId = companyManagerSoapServiceSync.createUIMSCompany(createdFedId,
								UimsConstants.VNEW, company);
					}

					if (null != createdCompanyFedId) {
						String companyFedID = "{" + "\"companyFederatedID\": \"" + createdCompanyFedId + "\"" + "}";
						LOGGER.info("companyFedID in creating UIMS Company: " + companyFedID);
						productService.updateUser(iPlanetDirectoryKey, userName, companyFedID);

						if ((null == userRequest.getUserRecord().getBFO_ACCOUNT_ID__c()
								|| userRequest.getUserRecord().getBFO_ACCOUNT_ID__c().isEmpty())
								&& (null != goDigitalValue && goDigitalValue.equalsIgnoreCase(
										userRequest.getUserRecord().getIDMS_Registration_Source__c()))) {
							userRequest.getUserRecord().setIDMS_Federated_ID__c(createdFedId);
							userRequest.getUserRecord().setCompanyFederatedId(createdCompanyFedId);

							String goDigitalRequest = buildGoDigitalRequest(userRequest);
							if (null != goDigitalRequest && !goDigitalRequest.isEmpty()) {
								goDigitalUserService.goDigitalUserRegistration(goDigitalRequest);
							}

						}
					}
					return true;
				}
			};

			Retryer<Boolean> retryerCompany = RetryerBuilder.<Boolean> newBuilder()
					.retryIfResult(Predicates.<Boolean> isNull()).retryIfExceptionOfType(Exception.class)
					.retryIfRuntimeException().withStopStrategy(StopStrategies.stopAfterAttempt(3)).build();
			try {
				if (null != context && (UserConstants.USER_CONTEXT_WORK.equalsIgnoreCase(context)|| UserConstants.USER_CONTEXT_WORK_1.equalsIgnoreCase(context))) {

					companyCreated = retryerCompany.call(callableCompany);
					if (userCreated && companyCreated) {
						// after successful creation of user and company, we
						// need to update the v_old
						String version = "{" + "\"V_Old\": \"" + v_new + "\"" + "}";
						productService.updateUser(iPlanetDirectoryKey, userName, version);
						// productService.sessionLogout(iPlanetDirectoryKey,
						// "logout");
					} 
				}

			} catch (RetryException e) {
				// productService.sessionLogout(iPlanetDirectoryKey, "logout");
				LOGGER.error("Retry failed while calling the UIMS create company::" + e.getMessage());
				e.printStackTrace();
			} catch (ExecutionException e) {
				// productService.sessionLogout(iPlanetDirectoryKey, "logout");
				LOGGER.error("ExecutionException while calling the UIMS create company::" + e.getMessage());
				e.printStackTrace();
			}
			if((!(userCreated && companyCreated) || (null == createdCompanyFedId && null == createdFedId)) && 
					(null != context && (UserConstants.USER_CONTEXT_WORK.equalsIgnoreCase(context)|| UserConstants.USER_CONTEXT_WORK_1.equalsIgnoreCase(context)))){
				LOGGER.error("UIMS CreateUser and CreateCompany got failed -----> ::sending mail notification::"+userRequestjsonString);
				sendEmail.emailReadyToSendEmail(supportUser, fromUserName,
						"UIMS CreateUser and CreateCompany failed.", userRequestjsonString);
			}
		} catch (Exception e) {
			// productService.sessionLogout(iPlanetDirectoryKey, "logout");
			LOGGER.error("Exception in UIMSUserManagerSoapService.createUIMSUserAndCompany::" + e.getMessage());
			e.printStackTrace();
		}
		createdFedId = null;
		// productService.sessionLogout(iPlanetDirectoryKey, "logout");
		LOGGER.info("Completed the createUIMSUserAndCompany Async method!!");
		return null;

	}
	
	private String buildGoDigitalRequest(CreateUserRequest userRequest) {
		LOGGER.info("Entered buildGoDigitalRequest() -> Start");
		
		ObjectMapper objMapper = new ObjectMapper();
		UserRegistrationInfoRequest userRegistrationInfoRequest = mapper.map(userRequest,
				UserRegistrationInfoRequest.class);

		Authentication authentication = new Authentication();
		authentication.setToken(goDitalToken);
		userRegistrationInfoRequest.getUserRegistrationInfoRequest().setAuthentication(authentication);

		String jsonString = "";
		try {
			LOGGER.info("Parameter userRequest -> " + objMapper.writeValueAsString(userRequest));
			jsonString = objMapper.writeValueAsString(userRegistrationInfoRequest);
			jsonString = jsonString.replace("\"\"", "[]");
		} catch (JsonProcessingException e) {
			LOGGER.error("JsonProcessingException while converting the digitalRequest to Json" + e.getMessage());
			e.printStackTrace();
		}
		objMapper = null;
		return jsonString;
	}
	
}

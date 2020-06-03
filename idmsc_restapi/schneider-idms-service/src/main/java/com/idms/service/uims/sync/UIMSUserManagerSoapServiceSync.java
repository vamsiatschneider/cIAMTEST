package com.idms.service.uims.sync;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;

import javax.inject.Inject;
import javax.xml.namespace.QName;
import javax.xml.ws.Service;

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
import com.idms.service.util.ChinaIdmsUtil;
import com.schneider.ims.service.uimsv2.CompanyV3;
import com.se.idms.util.SamlAssertionTokenGenerator;
import com.se.idms.util.UimsConstants;
import com.se.idms.util.UserConstants;
import com.se.uims.usermanager.IMSServiceSecurityCallNotAllowedException_Exception;
import com.se.uims.usermanager.InactiveUserImsException_Exception;
import com.se.uims.usermanager.InvalidImsServiceMethodArgumentException_Exception;
import com.se.uims.usermanager.LdapTemplateNotReadyException_Exception;
import com.se.uims.usermanager.RequestedEntryNotExistsException_Exception;
import com.se.uims.usermanager.SecuredImsException_Exception;
import com.se.uims.usermanager.UnexpectedLdapResponseException_Exception;
import com.se.uims.usermanager.UnexpectedRuntimeImsException_Exception;
import com.se.uims.usermanager.UserManagerUIMSV22;
import com.se.uims.usermanager.UserV6;
import com.uims.authenticatedUsermanager.AccessElement;
import com.uims.authenticatedUsermanager.Type;

/**
 * The Soap Service interface layer to call the UIMS user manager stubs.
 * 
 * @author Aravindh Kumar
 *
 */
@org.springframework.stereotype.Service("uimsUserManagerSoapServiceSync")
public class UIMSUserManagerSoapServiceSync {
	private static final Logger LOGGER = LoggerFactory.getLogger(UIMSUserManagerSoapServiceSync.class);
	private static final Logger UIMSSYNCLOGGER = LoggerFactory.getLogger("uimsSyncErrorLogger");
	
	@Autowired
	private UIMSAuthenticatedUserManagerSoapServiceSync authenticatedUserManagerSoapServiceSync;
	
	@Inject
	private SamlAssertionTokenGenerator samlTokenService;
	
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
	
	@Value("${userManagerUIMSWsdl}")
	private String userManagerUIMSWsdl;
	
	@Value("${userManagerUIMSWsdlQname}")
	private String userManagerUIMSWsdlQname;
	
	@Value("${userManagerUIMSWsdlPortName}")
	private String userManagerUIMSWsdlPortName;
	
	@Value("${goDitalToken}")
	private String goDitalToken;
	
	//CODE-RE-STRUCTURING
	@Value("${caller.fid}")
	private String CALLER_FID;
	
	@Autowired
	private GoDigitalUserService goDigitalUserService;
	
	private String applicationName = "Uims";
	private String createdFedId = null;
	private SendEmail sendEmail;
	String createdCompanyFedId = null;
	
	public UserManagerUIMSV22 getUserManager(){
		LOGGER.info("Entered getUserManager() of UIMS -> Start");
		UserManagerUIMSV22 userManagerUIMSV22 = null;
		URL url;
		try {
			url = new URL(userManagerUIMSWsdl);

			QName qname = new QName(userManagerUIMSWsdlQname, userManagerUIMSWsdlPortName);
			Service service = Service.create(url, qname);

			LOGGER.info("Start: getPort() ");
			userManagerUIMSV22 = service.getPort(UserManagerUIMSV22.class);
			LOGGER.info("End: getPort()");

		}catch (MalformedURLException e) {
			LOGGER.error("MalformedURLException in getUserManager()::" + e.getMessage(),e);
		} catch (Exception e) {
			LOGGER.error("Exception in getUserManager()::" + e.getMessage(),e);
		}
		return userManagerUIMSV22;
	}

	
	public String createUIMSUserAndCompany(String callerFid, com.uims.authenticatedUsermanager.UserV6 identity,
			String context, CompanyV3 company, String userName,
			String iPlanetDirectoryKey, String v_new, String password, String forcedFederatedId,
			CreateUserRequest userRequest,int companyCreatedCount) {
		LOGGER.info("Entered SYNC createUIMSUserAndCompany() -> Start");
		LOGGER.info("Parameter callerFid -> " + callerFid);
		LOGGER.info("Parameter context -> " + context+" ,companyCreatedCount="+companyCreatedCount);
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
			LOGGER.info("Parameter userRequest -> " + ChinaIdmsUtil.printData(objMapper.writeValueAsString(userRequest)));
			LOGGER.info("Parameter identity -> " + objMapper.writeValueAsString(identity));
			LOGGER.info("Parameter company -> " + objMapper.writeValueAsString(company));
			
			userRequestjsonString = objMapper.writeValueAsString(userRequest);
			userRequestjsonString = userRequestjsonString.replace("\"\"", "[]");
			
		} catch (JsonProcessingException e) {
			LOGGER.error("Error while converting the userRequest to Json" + e.getMessage());
			LOGGER.error("Exception >"+e);
		}/*finally{
			if(null != objMapper){
				objMapper = null;
			}
		}*/
		try {
			Callable<Boolean> callableUser = new Callable<Boolean>() {
				public Boolean call() throws Exception {

					if(null != userRequest.getUserRecord().getIDMSCompanyFederationIdentifier__c() && !userRequest.getUserRecord().getIDMSCompanyFederationIdentifier__c().isEmpty()){
						
						if(companyCreatedCount > 1){
							identity.setCompanyId(userRequest.getUserRecord().getIDMSCompanyFederationIdentifier__c());
						}
					}
					
					
					if (null != password && !password.isEmpty()) {
						LOGGER.info("calling createUIMSUserWithPassword() of UIMS");
						createdFedId = authenticatedUserManagerSoapServiceSync.createUIMSUserWithPassword(
								CALLER_FID, identity, password, forcedFederatedId);
					} else {
						LOGGER.info("calling createUIMSUser() of UIMS");
						createdFedId = authenticatedUserManagerSoapServiceSync.createUIMSUser(CALLER_FID,
								identity, forcedFederatedId);
					}

					if (null != createdFedId && !createdFedId.isEmpty()) {
						String fedID = "{" + "\"federationID\": \"" + createdFedId + "\"" + "}";
						LOGGER.info("fedID in creating UIMS user: " + fedID);
						productService.updateUser(iPlanetDirectoryKey, userName, fedID);
					} else {
						//TODO put into csv file
						LOGGER.error("User not created in UIMS having fedID "+forcedFederatedId);
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
				/*Attempt<?> lastFailedAttempt = e.getLastFailedAttempt();
				Throwable originalException = lastFailedAttempt.getExceptionCause();
				originalException.printStackTrace();
				LOGGER.error("Exception from UIMS while create user::" + originalException.getMessage());*/
				LOGGER.error("RetryException while UIMS create user::" + e.getMessage());
				LOGGER.error("Exception >"+e);
				UIMSSYNCLOGGER.error("User creation failed in UIMS, UserForceFederationId = "+forcedFederatedId);
				UIMSSYNCLOGGER.error("User info = "+objMapper.writeValueAsString(identity));
			} catch (ExecutionException e) {
				LOGGER.error("ExecutionException while UIMS create user::" + e.getMessage());
				LOGGER.error("Exception >"+e);
			}

			if((!userCreated || null == createdFedId) && (null != context && (UserConstants.USER_CONTEXT_HOME.equalsIgnoreCase(context)||UserConstants.USER_CONTEXT_HOME_1.equalsIgnoreCase(context)))) {
				LOGGER.error("UIMS CreateUser failed -----> ::sending mail notification for userRequestjsonString::"+userRequestjsonString);
				sendEmail.emailReadyToSendEmail(supportUser, fromUserName, "UIMS CreateUser failed.", userRequestjsonString);
			}
			Callable<Boolean> callableCompany = new Callable<Boolean>() {
				public Boolean call() throws Exception {
					/**
					 * When user is creating from BFO then no need of creating
					 * company, bfo account id act as company
					 */
					/*if (null != userRequest.getUserRecord().getAdminCompanyFederatedId()
							&& !userRequest.getUserRecord().getAdminCompanyFederatedId().isEmpty()) {
						createdCompanyFedId = userRequest.getUserRecord().getAdminCompanyFederatedId();
					}else if ((null != userRequest.getUserRecord().getAdminCompanyFederatedId()
							&& !userRequest.getUserRecord().getAdminCompanyFederatedId().isEmpty())
							&& (null != userRequest.getUserRecord().getAdminCompanyFederatedId()
									&& !userRequest.getUserRecord().getAdminCompanyFederatedId().isEmpty())) {
						createdCompanyFedId = userRequest.getUserRecord().getAdminCompanyFederatedId();
					} else if (null != userRequest.getUserRecord().getIDMS_Registration_Source__c() 
							&& UserConstants.PRM.equals(userRequest.getUserRecord().getIDMS_Registration_Source__c())){
						//if registration source is PRM then accept and force the company FederatedId from IFW/IDMS global
						createdCompanyFedId = userRequest.getUserRecord().getCompanyFederatedId();
					} else {

						// createdCompanyFedId = companyManagerSoapServiceSync.createUIMSCompany(createdFedId,UimsConstants.VNEW, company);
						createdCompanyFedId = companyManagerSoapServiceSync.createUIMSCompanyWithCompanyForceIdmsId(identity.getCompanyId(), UimsConstants.VNEW, company);
						
					}*/
					
					if (null != userRequest.getUserRecord().getIDMSCompanyFederationIdentifier__c()
							&& !userRequest.getUserRecord().getIDMSCompanyFederationIdentifier__c().isEmpty()) {
						if (companyCreatedCount == 1) {
							LOGGER.info("calling createUIMSCompanyWithCompanyForceIdmsId() of UIMS");
							createdCompanyFedId = companyManagerSoapServiceSync.createUIMSCompanyWithCompanyForceIdmsId(createdFedId,
									userRequest.getUserRecord().getIDMSCompanyFederationIdentifier__c(), UimsConstants.VNEW, company);
						} 
					} 
					
					
					if (null != createdCompanyFedId && !createdCompanyFedId.isEmpty()) {
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
					} else {
						LOGGER.error("Company not created in UIMS for fedID "+forcedFederatedId);
					}
					return true;
				}
			};

			Retryer<Boolean> retryerCompany = RetryerBuilder.<Boolean> newBuilder()
					.retryIfResult(Predicates.<Boolean> isNull()).retryIfExceptionOfType(Exception.class)
					.retryIfRuntimeException().withStopStrategy(StopStrategies.stopAfterAttempt(3)).build();
			try {
				if (null != context && (UserConstants.USER_CONTEXT_WORK.equalsIgnoreCase(context)|| UserConstants.USER_CONTEXT_WORK_1.equalsIgnoreCase(context))) {
					if (null != userRequest.getUserRecord().getIDMSCompanyFederationIdentifier__c()
							&& !userRequest.getUserRecord().getIDMSCompanyFederationIdentifier__c().isEmpty()
							&& null != userRequest.getUserRecord().getCompanyName()
							&& !userRequest.getUserRecord().getCompanyName().isEmpty()
							&& null != createdFedId && !createdFedId.isEmpty()) {
						LOGGER.info("User has company info and now creating company for user::"+forcedFederatedId);
						companyCreated = retryerCompany.call(callableCompany);
						if (companyCreated) {
							LOGGER.info("UIMS company created successfully::" + companyCreated);
						}
						if (userCreated && companyCreated) {
							// after successful creation of user and company, we
							// need to update the v_old
							String version = "{" + "\"V_Old\": \"" + v_new + "\"" + "}";
							productService.updateUser(iPlanetDirectoryKey, userName, version);
							// productService.sessionLogout(iPlanetDirectoryKey,
							// "logout");
						} 
					}
				}
			} catch (RetryException e) {
				// productService.sessionLogout(iPlanetDirectoryKey, "logout");
				LOGGER.error("RetryException while UIMS create company::" + e.getMessage());
				LOGGER.error("Exception >"+e);
				UIMSSYNCLOGGER.error("Company creation failed in UIMS, companyForceFederationId = "+userRequest.getUserRecord().getIDMSCompanyFederationIdentifier__c());
				UIMSSYNCLOGGER.error("Company creation failed in UIMS, company info = "+objMapper.writeValueAsString(company));
			} catch (ExecutionException e) {
				// productService.sessionLogout(iPlanetDirectoryKey, "logout");
				LOGGER.error("ExecutionException while UIMS create company::" + e.getMessage());
				LOGGER.error("Exception >"+e);
			}
			/*if((!(userCreated && companyCreated) || (null == createdCompanyFedId && null == createdFedId)) && 
					(null != context && (UserConstants.USER_CONTEXT_WORK.equalsIgnoreCase(context)|| UserConstants.USER_CONTEXT_WORK_1.equalsIgnoreCase(context)))){
				LOGGER.error("UIMS CreateUser and CreateCompany failed -----> ::sending mail notification::"+userRequestjsonString);
				sendEmail.emailReadyToSendEmail(supportUser, fromUserName,
						"UIMS CreateUser and CreateCompany failed.", userRequestjsonString);
			}*/
		} catch (Exception e) {
			// productService.sessionLogout(iPlanetDirectoryKey, "logout");
			LOGGER.error("Exception in createUIMSUserAndCompany ::" + e.getMessage());
			LOGGER.error("Exception >"+e);
		}
		createdFedId = null;
		// productService.sessionLogout(iPlanetDirectoryKey, "logout");
		LOGGER.info("Completed Sync createUIMSUserAndCompany() method!!");
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
			LOGGER.info("Parameter userRequest -> " + ChinaIdmsUtil.printData(objMapper.writeValueAsString(userRequest)));
			jsonString = objMapper.writeValueAsString(userRegistrationInfoRequest);
			jsonString = jsonString.replace("\"\"", "[]");
		} catch (JsonProcessingException e) {
			LOGGER.error("JsonProcessingException while converting the digitalRequest to Json" + e.getMessage());
			LOGGER.error("Exception >"+e);
		}
		objMapper = null;
		return jsonString;
	}
	
	public boolean updateUIMSUser(String fedId, UserV6 user, String vnew) throws MalformedURLException {
		LOGGER.info("Entered updateUIMSUser() -> Start");
		LOGGER.info("Parameter fedId -> " + fedId + " ,vnew -> " + vnew);

		boolean status = false;
		UserManagerUIMSV22 userManagerUIMSV22 = getUserManager();
		String samlAssertion = null;
		ObjectMapper objMapper = new ObjectMapper();
		try {
			LOGGER.info("Parameter user -> " + objMapper.writeValueAsString(user));

			samlAssertion = samlTokenService.getSamlAssertionToken(fedId, vnew);
			LOGGER.info("samlAssertion="+samlAssertion);

			LOGGER.info("Start: updateUser() of UIMS for user:" + user.getFirstName());
			status = userManagerUIMSV22.updateUser(CALLER_FID, samlAssertion, user);
			LOGGER.info("End: updateUser() of UIMS finished for user:" + user.getFirstName());

			if(status){
				LOGGER.info("UIMS updateUIMSUser() successful, status:" + status);
			}else{
				LOGGER.error("UIMS updateUIMSUser() failed, status:" + status);
				UIMSSYNCLOGGER.error("updateUIMSUser failed in UIMS, fedId = "+fedId);
				UIMSSYNCLOGGER.error("updateUIMSUser failed in UIMS, user info = "+objMapper.writeValueAsString(user));
			}
		} catch (IMSServiceSecurityCallNotAllowedException_Exception | InactiveUserImsException_Exception
				| InvalidImsServiceMethodArgumentException_Exception | LdapTemplateNotReadyException_Exception
				| RequestedEntryNotExistsException_Exception | SecuredImsException_Exception
				| UnexpectedLdapResponseException_Exception | UnexpectedRuntimeImsException_Exception e) {
			UIMSSYNCLOGGER.error("UIMS updateUIMSUser() failed, Exception ::" + e.getMessage(),e);
			UIMSSYNCLOGGER.error("updateUIMSUser failed in UIMS, fedId = "+fedId);
			try {
				UIMSSYNCLOGGER.error("updateUIMSUser failed in UIMS, user info = "+objMapper.writeValueAsString(user));
			} catch (JsonProcessingException e1) {
				LOGGER.error("JsonProcessingException1 in updateUIMSUser()::" + e1.getMessage(),e1);
			}
		} catch (Exception e) {
			LOGGER.error("Exception executing while getting samlAssertion::" + e.getMessage(),e);
			UIMSSYNCLOGGER.error("updateUIMSUser failed in UIMS, fedId = "+fedId);
			LOGGER.error("ECODE-SOAP-USERMGR-UPDATE-UIMSUSER-GEN-ERR : updateUIMSUser failed in UIMS, fedId = "+fedId);
			try {
				UIMSSYNCLOGGER.error("updateUIMSUser failed in UIMS, user info = "+objMapper.writeValueAsString(user));
			} catch (JsonProcessingException e1) {
				LOGGER.error("JsonProcessingException2 in updateUIMSUser()::" + e1.getMessage(),e);
			}
		}
		return status;
	}
	
}

package com.idms.service;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.UUID;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;

import javax.inject.Inject;
import javax.xml.namespace.QName;
import javax.xml.ws.Service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.rholder.retry.RetryException;
import com.github.rholder.retry.Retryer;
import com.github.rholder.retry.RetryerBuilder;
import com.github.rholder.retry.StopStrategies;
import com.google.common.base.Predicates;
import com.idms.mapper.IdmsMapper;
import com.idms.model.ConfirmPinRequest;
import com.idms.model.CreateUserRequest;
import com.idms.model.UserRegistrationInfoRequest;
import com.idms.model.digital.Authentication;
import com.idms.product.client.OpenAMService;
import com.idms.service.digital.GoDigitalUserService;
import com.se.idms.util.SamlAssertionTokenGenerator;
import com.se.idms.util.UimsConstants;
import com.se.idms.util.UserConstants;
import com.se.uims.usermanager.IMSServiceSecurityCallNotAllowedException_Exception;
import com.se.uims.usermanager.InactiveUserImsException_Exception;
import com.se.uims.usermanager.InvalidImsServiceMethodArgumentException_Exception;
import com.se.uims.usermanager.LdapTemplateNotReadyException_Exception;
import com.se.uims.usermanager.RequestedEntryNotExistsException_Exception;
import com.se.uims.usermanager.RequestedInternalUserException_Exception;
import com.se.uims.usermanager.SecuredImsException_Exception;
import com.se.uims.usermanager.UnexpectedLdapResponseException_Exception;
import com.se.uims.usermanager.UnexpectedRuntimeImsException_Exception;
import com.se.uims.usermanager.UserManagerUIMSV22;
import com.se.uims.usermanager.UserV6;
import com.uims.authenticatedUsermanager.AccessElement;
import com.uims.authenticatedUsermanager.Type;
import com.uims.companymanager.CompanyV3;

/**
 * The Soap Service interface layer to call the UIMS user manager stubs.
 * 
 * @author Aravindh Kumar
 *
 */
@org.springframework.stereotype.Service("uimsUserManagSoapService")
@EnableAsync
public class UIMSUserManagerSoapService {

	/**
	 * Logger instance.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(UIMSUserManagerSoapService.class);

	private static final Logger uimsLog = LoggerFactory.getLogger("uimsLogger");

	@Inject
	private IdmsMapper mapper;
	
	@Inject
	private OpenAMService productService;
	
	private SendEmail sendEmail;
	
	@Autowired
	@Lazy
	public void setSendEmail(SendEmail sendEmail) {
		this.sendEmail = sendEmail;
	}
	
	@Value("${fromUserName}")
	private String fromUserName;
	
	@Value("${supportUser}")
	private String supportUser;

	@Autowired
	private GoDigitalUserService goDigitalUserService;
	

	@Autowired
	private UIMSAuthenticatedUserManagerSoapService authenticatedUserManagerSoapService;

	@Autowired
	private UIMSCompanyManagerSoapService companyManagerSoapService;

	@Value("${goDitalToken}")
	private String goDitalToken;

	@Value("${goDigitalValue}")
	private String goDigitalValue;

	@Value("${userManagerUIMSWsdl}")
	private String userManagerUIMSWsdl;

	@Value("${userManagerUIMSWsdlQname}")
	private String userManagerUIMSWsdlQname;

	@Value("${userManagerUIMSWsdlPortName}")
	private String userManagerUIMSWsdlPortName;

	// private T uimsIdentity;

	// @Value("${applicationName}")
	private String applicationName = "Uims";

	private boolean updateUIMSUser = false;

	private boolean updateUIMSCompany = false;

	private String samlAssertion = null;

	private boolean isNoPwdactivated = false;

	private boolean setPasswordStatus = false;

	private boolean ispasswordupdated = false;
	
	private String createdFedId = null;
	
	String createdCompanyFedId = null;

	/**
	 * Service to fetch information about {@link Product}s.
	 */

	public UserManagerUIMSV22 getUserManager() throws MalformedURLException {
		URL url = new URL(userManagerUIMSWsdl);
		QName qname = new QName(userManagerUIMSWsdlQname, userManagerUIMSWsdlPortName);
		Service service = Service.create(url, qname);

		UserManagerUIMSV22 userManagerUIMSV2 = service.getPort(UserManagerUIMSV22.class);
		return userManagerUIMSV2;
	}

	@Async
	public void getUIMSUser(String callerFid, String vnew) throws MalformedURLException {

		String samlAssertionOrToken = null;
		try {
			samlAssertionOrToken = SamlAssertionTokenGenerator.getSamlAssertionToken(callerFid, vnew);
		} catch (Exception e1) {
			LOGGER.error("Error executing while getUIMSUser::" + e1.getMessage());
			e1.printStackTrace();
		}
		try {
			UserManagerUIMSV22 userManagerUIMSV22 = getUserManager();
			userManagerUIMSV22.getUser(callerFid, samlAssertionOrToken);
		} catch (IMSServiceSecurityCallNotAllowedException_Exception
				| InvalidImsServiceMethodArgumentException_Exception | LdapTemplateNotReadyException_Exception
				| RequestedEntryNotExistsException_Exception | SecuredImsException_Exception
				| UnexpectedLdapResponseException_Exception | UnexpectedRuntimeImsException_Exception e) {
			LOGGER.error("Error executing while getUIMSUser::" + e.getMessage());
			e.printStackTrace();
		}
	}

	@Async
	public void setUIMSPassword(String iPlanetDirectoryKey, String userId,
			String callerFid, String password, String openamVnew, String email) throws MalformedURLException {
		LOGGER.info("inside setUIMSPassword Async Method");
		try {
			samlAssertion = SamlAssertionTokenGenerator.getSamlAssertionToken(userId, openamVnew);
		} catch (Exception e) {
			LOGGER.error("Error executing while setUIMSPassword::" + e.getMessage());
			e.printStackTrace();
		}
		try {
			Callable<Boolean> callableUIMSPassword = new Callable<Boolean>() {
				public Boolean call() throws Exception {
					UserManagerUIMSV22 userManagerUIMSV22 = getUserManager();
					setPasswordStatus = userManagerUIMSV22.setPassword(UimsConstants.CALLER_FID, samlAssertion, password);
					LOGGER.info("setPasswordStatus: " + setPasswordStatus);
					return true;
				}
			};

			Retryer<Boolean> retryer = RetryerBuilder.<Boolean> newBuilder()
					.retryIfResult(Predicates.<Boolean> isNull()).retryIfExceptionOfType(Exception.class)
					.retryIfRuntimeException().withStopStrategy(StopStrategies.stopAfterAttempt(3)).build();
			try {
				retryer.call(callableUIMSPassword);
				// after successful setUIMSPassword , we need to update the
				// v_old
				if (setPasswordStatus) {
					String version = "{" + "\"V_Old\": \"" + openamVnew + "\"" + "}";
					productService.updateUser(iPlanetDirectoryKey, userId, version);
				}
			} catch (RetryException e) {
				e.printStackTrace();
				uimsLog.error("Retry failed while calling the setUIMSPassword::" + e.getMessage());
			} catch (ExecutionException e) {
				e.printStackTrace();
			}
			if(!setPasswordStatus) {
				LOGGER.info("UIMS UserpinConfirmation setPassword got failed -----> ::sending mail notification::");
				sendEmail.emailReadyToSendEmail(supportUser, fromUserName,
						"UIMS UserpinConfirmation setPassword failed.", userId);
			}
		} catch (Exception e) {
			// productService.sessionLogout(iPlanetDirectoryKey, "logout");
			LOGGER.error("Error executing while setUIMSPassword::" + e.getMessage());
			e.printStackTrace();
		}
		// productService.sessionLogout(iPlanetDirectoryKey, "logout");
		LOGGER.info("setUIMSPassword Async Method completed!");
	}

	@Async
	public void updateUIMSPassword(String callerFid, String userId, String oldPassword, String newPassword,
			String openamVnew, String iPlanetDirectoryKey) throws MalformedURLException {
		try {
			samlAssertion = SamlAssertionTokenGenerator.getSamlAssertionToken(callerFid, openamVnew);
		} catch (Exception e1) {
			LOGGER.error("Error executing while updateUIMSPassword::" + e1.getMessage());
			e1.printStackTrace();
		}
		try {
			Callable<Boolean> callableUpdateUIMSPassword = new Callable<Boolean>() {
				public Boolean call() throws Exception {
					UserManagerUIMSV22 userManagerUIMSV22 = getUserManager();
					ispasswordupdated = userManagerUIMSV22.updatePassword(UimsConstants.CALLER_FID, samlAssertion,
							oldPassword, newPassword);
					uimsLog.info("Update password status is::" + ispasswordupdated);
					return true;
				}
			};

			Retryer<Boolean> retryer = RetryerBuilder.<Boolean> newBuilder()
					.retryIfResult(Predicates.<Boolean> isNull()).retryIfExceptionOfType(Exception.class)
					.retryIfRuntimeException().withStopStrategy(StopStrategies.stopAfterAttempt(3)).build();
			try {
				retryer.call(callableUpdateUIMSPassword);
				// after successful activateIdentityNoPassword, we need to
				// update the v_old
				if (ispasswordupdated) {
					String version = "{" + "\"V_Old\": \"" + openamVnew + "\"" + "}";
					productService.updateUser(iPlanetDirectoryKey, userId, version);
				}
			} catch (RetryException e) {
				e.printStackTrace();
				uimsLog.error("Retry failed while calling the UIMS update user::" + e.getMessage());
			} catch (ExecutionException e) {
				e.printStackTrace();
			}

		} catch (Exception e) {
			LOGGER.error("Error executing while updateUIMSPassword::" + e.getMessage());
			e.printStackTrace();
		}
	}

	@Async
	public boolean updateUIMSUser(String fedId, UserV6 user, String vnew) throws MalformedURLException {
		boolean status = false;
		UserManagerUIMSV22 userManagerUIMSV22 = getUserManager();
		String samlAssertion = null;
		try {
			samlAssertion = SamlAssertionTokenGenerator.getSamlAssertionToken(fedId, vnew);
		} catch (Exception e) {
			LOGGER.error("Error executing while updateUIMSUser::" + e.getMessage());
			e.printStackTrace();
		}
		try {
			status = userManagerUIMSV22.updateUser(UimsConstants.CALLER_FID, samlAssertion, user);
		} catch (IMSServiceSecurityCallNotAllowedException_Exception | InactiveUserImsException_Exception
				| InvalidImsServiceMethodArgumentException_Exception | LdapTemplateNotReadyException_Exception
				| RequestedEntryNotExistsException_Exception | SecuredImsException_Exception
				| UnexpectedLdapResponseException_Exception | UnexpectedRuntimeImsException_Exception e) {
			LOGGER.error("Error executing while updateUIMSUser::" + e.getMessage());
			e.printStackTrace();
		}
		return status;
	}

	@Async
	public void activateUIMSIdentity(String callerFid, String password, String vnew) throws MalformedURLException {
		UserManagerUIMSV22 userManagerUIMSV22 = getUserManager();

		String authentificationToken = null;
		try {
			authentificationToken = SamlAssertionTokenGenerator.getSamlAssertionToken(callerFid, vnew);
		} catch (Exception e) {
			LOGGER.error("Error executing while activateUIMSIdentity::" + e.getMessage());
			e.printStackTrace();
		}
		try {
			userManagerUIMSV22.activateIdentity(UimsConstants.CALLER_FID, password, authentificationToken);
		} catch (IMSServiceSecurityCallNotAllowedException_Exception
				| InvalidImsServiceMethodArgumentException_Exception | LdapTemplateNotReadyException_Exception
				| RequestedEntryNotExistsException_Exception | RequestedInternalUserException_Exception
				| SecuredImsException_Exception | UnexpectedLdapResponseException_Exception
				| UnexpectedRuntimeImsException_Exception e) {
			LOGGER.error("Error executing while activateUIMSIdentity::" + e.getMessage());
			e.printStackTrace();
		}
	}

	@Async
	public void activateIdentityNoPassword(String userId, String callerFid,
			String openamVnew, String iPlanetDirectoryKey, String email) throws MalformedURLException {
		LOGGER.info("inside activateIdentityNoPassword UIMS Async Method");
		try {
			samlAssertion = SamlAssertionTokenGenerator.getSamlAssertionToken(userId, openamVnew);
		} catch (Exception e) {
			// productService.sessionLogout(iPlanetDirectoryKey, "logout");
			LOGGER.error("Error executing while activateIdentityNoPassword::" + e.getMessage());
			e.printStackTrace();
		}
		try {
			Callable<Boolean> callableActivateIdentityNoPassword = new Callable<Boolean>() {
				public Boolean call() throws Exception {
					UserManagerUIMSV22 userManagerUIMSV22 = getUserManager();
					isNoPwdactivated = userManagerUIMSV22.activateIdentityNoPassword(UimsConstants.CALLER_FID,
							samlAssertion);
					LOGGER.info("UIMS user activateIdentityNoPassword isactivated:" + isNoPwdactivated);
					return true;
				}
			};
			Retryer<Boolean> retryer = RetryerBuilder.<Boolean> newBuilder()
					.retryIfResult(Predicates.<Boolean> isNull()).retryIfExceptionOfType(Exception.class)
					.retryIfRuntimeException().withStopStrategy(StopStrategies.stopAfterAttempt(3)).build();
			try {
				retryer.call(callableActivateIdentityNoPassword);
				// after successful activateIdentityNoPassword, we need to
				// update the v_old
				if (isNoPwdactivated) {
					String version = "{" + "\"V_Old\": \"" + openamVnew + "\"" + "}";
					productService.updateUser(iPlanetDirectoryKey, userId, version);
				}
			} catch (RetryException e) {
				// productService.sessionLogout(iPlanetDirectoryKey, "logout");
				e.printStackTrace();
				uimsLog.error("Retry failed while calling the activateIdentityNoPassword::" + e.getMessage());
			} catch (ExecutionException e) {
				// productService.sessionLogout(iPlanetDirectoryKey, "logout");
				e.printStackTrace();
			}
			if(!isNoPwdactivated) {
				LOGGER.info("UIMS UserpinConfirmation activateIdentityNoPassword got failed -----> ::sending mail notification::");
				sendEmail.emailReadyToSendEmail(supportUser, fromUserName,
						"UIMS UserpinConfirmation activateIdentityNoPassword failed.", userId);
			}
		} catch (Exception e) {
			// productService.sessionLogout(iPlanetDirectoryKey, "logout");
			LOGGER.error("Error executing while activateIdentityNoPassword::" + e.getMessage());
			e.printStackTrace();
		}
		// productService.sessionLogout(iPlanetDirectoryKey, "logout");
		LOGGER.info("Completed UIMS activateIdentityNoPassword UIMS Async method!");

	}

	@Async
	public String createUIMSUserAndCompany(String callerFid, com.uims.authenticatedUsermanager.UserV6 identity,
			String context, CompanyV3 company, String userName,
			String iPlanetDirectoryKey, String v_new, String password, String forcedFederatedId,
			CreateUserRequest userRequest) {
		LOGGER.info("Inside the createUIMSUserAndCompany UIMS Async method!!");
		Boolean companyCreated = false;
		Boolean userCreated = false;
		AccessElement application = new AccessElement();
		application.setId(applicationName);
		application.setType(Type.APPLICATION);
		
		ObjectMapper objMapper = new ObjectMapper();
		String userRequestjsonString = "";
		try {
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
						createdFedId = authenticatedUserManagerSoapService.createUIMSUserWithPassword(
								UimsConstants.CALLER_FID, identity, password, forcedFederatedId);
					} else {
						createdFedId = authenticatedUserManagerSoapService.createUIMSUser(UimsConstants.CALLER_FID,
								identity, forcedFederatedId);
					}

					if (null != createdFedId) {
						String fedID = "{" + "\"federationID\": \"" + createdFedId + "\"" + "}";
						LOGGER.info("fedID in creating UIMS user: " + fedID);
						uimsLog.info("fedID in creating UIMS user: " + fedID);
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
					uimsLog.info("UIMS user created successfully::" + userCreated);
				}
			} catch (RetryException e) {
				e.printStackTrace();
				uimsLog.error("Retry failed while calling the UIMS create user::" + e.getMessage());
			} catch (ExecutionException e) {
				e.printStackTrace();
			}

			if((!userCreated || null == createdFedId) && (null != context && UserConstants.USER_CONTEXT_HOME.equalsIgnoreCase(context))) {
					LOGGER.info("CreateUser got failed -----> ::sending mail notification::");
					sendEmail.emailReadyToSendEmail(supportUser, fromUserName,
							"UIMS CreateUser failed.", userRequestjsonString);
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

						createdCompanyFedId = companyManagerSoapService.createUIMSCompany(createdFedId,
								UimsConstants.VNEW, company);
					}

					if (null != createdCompanyFedId) {
						String companyFedID = "{" + "\"companyFederatedID\": \"" + createdCompanyFedId + "\"" + "}";
						LOGGER.info("companyFedID in creating UIMS Company: " + companyFedID);
						uimsLog.info("companyFedID in creating UIMS Company: " + companyFedID);
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
				if (null != context && UserConstants.USER_CONTEXT_WORK.equalsIgnoreCase(context)) {

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
				e.printStackTrace();
				uimsLog.error("Retry failed while calling the UIMS create company::" + e.getMessage());
			} catch (ExecutionException e) {
				// productService.sessionLogout(iPlanetDirectoryKey, "logout");
				e.printStackTrace();
			}
			if((!(userCreated && companyCreated) || (null == createdCompanyFedId && null == createdFedId)) && 
					(null != context && UserConstants.USER_CONTEXT_WORK.equalsIgnoreCase(context))){
				LOGGER.info("UIMS CreateUser and CreateCompany got failed -----> ::sending mail notification::");
				sendEmail.emailReadyToSendEmail(supportUser, fromUserName,
						"UIMS CreateUser and CreateCompany failed.", userRequestjsonString);
			}
		} catch (Exception e) {
			// productService.sessionLogout(iPlanetDirectoryKey, "logout");
			uimsLog.error("Error in UIMSUserManagerSoapService.createUIMSUserAndCompany::" + e.getMessage());
			e.printStackTrace();
		}
		createdFedId = null;
		// productService.sessionLogout(iPlanetDirectoryKey, "logout");
		LOGGER.info("Completed the createUIMSUserAndCompany Async method!!");
		return null;

	}

	@Async
	public String updateUIMSUserAndCompany(String fedId, UserV6 identity, String context, CompanyV3 company,
			String vnew, OpenAMService productService, String iPlanetDirectoryKey, String userName, String email) {

		LOGGER.info("Inside the updateUIMSUserAndCompany Async method!!");
		AccessElement application = new AccessElement();
		application.setId(applicationName);
		application.setType(Type.APPLICATION);

		try {
			UIMSCompanyManagerSoapService companyManagerSoapService = new UIMSCompanyManagerSoapService();
			Callable<Boolean> callableUpdateUIMSUserAndComapany = new Callable<Boolean>() {
				public Boolean call() throws Exception {

					// TODO check in global if we pass a single attribute, what
					// should be the behaviour for other fields User object.

					// Answer from Subrat: Remaining fields should not be
					// updated.
					updateUIMSUser = updateUIMSUser(fedId, identity, vnew);

					return true;
				}
			};

			Retryer<Boolean> retryer = RetryerBuilder.<Boolean> newBuilder()
					.retryIfResult(Predicates.<Boolean> isNull()).retryIfExceptionOfType(Exception.class)
					.retryIfRuntimeException().withStopStrategy(StopStrategies.stopAfterAttempt(3)).build();
			try {
				retryer.call(callableUpdateUIMSUserAndComapany);
				
				if(updateUIMSUser){
					LOGGER.info("UIMS User updated Successfully::"+updateUIMSUser);
				}
			} catch (RetryException e) {
				e.printStackTrace();
				uimsLog.error("Retry failed while calling the UIMS update user::" + e.getMessage());
			} catch (ExecutionException e) {
				e.printStackTrace();
			}
			
			if(!updateUIMSUser && (null != context && UserConstants.USER_CONTEXT_HOME.equalsIgnoreCase(context))) {
				LOGGER.info("UIMS User updated got failed -----> ::sending mail notification::");
				sendEmail.emailReadyToSendEmail(supportUser, fromUserName,
						"UIMS Update user failed.", userName);
			}
			Callable<Boolean> callableUpdateCompany = new Callable<Boolean>() {
				public Boolean call() throws Exception {

					// TODO logic to get the federatedId
					String federatedId = "";

					updateUIMSCompany = companyManagerSoapService.updateUIMSCompany(fedId, vnew, company);
					return updateUIMSCompany;
				}
			};

			Retryer<Boolean> retryerCompany = RetryerBuilder.<Boolean> newBuilder()
					.retryIfResult(Predicates.<Boolean> isNull()).retryIfExceptionOfType(Exception.class)
					.retryIfRuntimeException().withStopStrategy(StopStrategies.stopAfterAttempt(3)).build();
			try {

				if (null != context && UserConstants.USER_CONTEXT_WORK.equalsIgnoreCase(context)) {

					retryerCompany.call(callableUpdateCompany);
					if (updateUIMSUser && updateUIMSCompany) {
						// after successful creation of user and company, we
						// need to update the v_old
						String version = "{" + "\"V_Old\": \"" + vnew + "\"" + "}";
						productService.updateUser(iPlanetDirectoryKey, userName, version);
					}
				}

			} catch (RetryException e) {
				e.printStackTrace();
				uimsLog.error("Retry failed while calling the UIMS create company::" + e.getMessage());
			} catch (ExecutionException e) {
				e.printStackTrace();
			}
			if(!(updateUIMSUser && updateUIMSCompany) && (null != context && UserConstants.USER_CONTEXT_WORK.equalsIgnoreCase(context))) {
				LOGGER.info("UIMS User and Company updated got failed -----> ::sending mail notification::");
				sendEmail.emailReadyToSendEmail(supportUser, fromUserName,
						"UIMS Update user and company failed.", userName);
			}

		} catch (Exception e) {
			// productService.sessionLogout(iPlanetDirectoryKey, "logout");
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		// productService.sessionLogout(iPlanetDirectoryKey, "logout");
		LOGGER.info("UIMS updateUIMSUserAndCompany Async method completed!!");
		return null;

	}

	/**
	 * This method is for UIMS confirm pin
	 * 
	 * @param confirmRequest
	 * @param openamVnew
	 */
	@Async
	public void activateUIMSUserConfirmPIN(ConfirmPinRequest confirmRequest,
			String openamVnew, String iPlanetDirectoryKey, String email) {
		LOGGER.info(
				"In UserServiceImpl.userPinConfirmation().activateUIMSUserConfirmPIN():--> Calling Async UIMS UserManager methods of setUIMSPassword/activateIdentityNoPassword");
		try {
			if (null != confirmRequest.getPassword() && !confirmRequest.getPassword().isEmpty()) {
				setUIMSPassword(iPlanetDirectoryKey, confirmRequest.getId(),
						confirmRequest.getIDMS_Federated_ID__c(), confirmRequest.getPassword().trim(), openamVnew, email);
			} else if (null == confirmRequest.getPassword() || "".equals(confirmRequest.getPassword())) {
				activateIdentityNoPassword(confirmRequest.getId(), confirmRequest.getIDMS_Federated_ID__c(),
						openamVnew, iPlanetDirectoryKey,email);
			}
		} catch (Exception e) {
			LOGGER.error(
					"Exception while calling UIMS UserManager API of setUIMSPassword/activateIdentityNoPassword:: -> "
							+ e.getMessage());
			e.printStackTrace();
		}
	}

	public static void main(String[] args) {

		UIMSUserManagerSoapService service = new UIMSUserManagerSoapService();
		com.uims.authenticatedUsermanager.UserV6 user = new com.uims.authenticatedUsermanager.UserV6();
		user.setFederatedID("123457");
		String email = UUID.randomUUID().toString() + "@mailinator.com";
		user.setEmail(email);
		user.setFirstName("Arvind");
		user.setLastName("kumar");
		user.setLanguageCode("zh");
		user.setCountryCode("CN");
		CompanyV3 company = new CompanyV3();
		// service.createUIMSUserAndCompany(UimsConstants.CALLER_FID, user,
		// "@Home", company, null, null, "12345678", null, "Welcome123@");
	}

	private String buildGoDigitalRequest(CreateUserRequest userRequest) {
		ObjectMapper objMapper = new ObjectMapper();
		UserRegistrationInfoRequest userRegistrationInfoRequest = mapper.map(userRequest,
				UserRegistrationInfoRequest.class);

		Authentication authentication = new Authentication();
		authentication.setToken(goDitalToken);
		userRegistrationInfoRequest.getUserRegistrationInfoRequest().setAuthentication(authentication);
		;

		String jsonString = "";
		try {
			jsonString = objMapper.writeValueAsString(userRegistrationInfoRequest);
			jsonString = jsonString.replace("\"\"", "[]");
		} catch (JsonProcessingException e) {
			LOGGER.error("Error while converting the digitalRequest to Json" + e.getMessage());
			e.printStackTrace();
		}
		objMapper = null;

		return jsonString;

	}

}

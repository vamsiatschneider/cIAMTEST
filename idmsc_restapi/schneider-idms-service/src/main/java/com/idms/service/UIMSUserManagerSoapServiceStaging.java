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
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.context.annotation.Profile;
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
import com.idms.service.uims.sync.UIMSCompanyManagerSoapServiceSync;
import com.idms.service.util.ChinaIdmsUtil;
import com.schneider.ims.service.uimsv2.CompanyV3;
import com.se.idms.cache.validate.IValidator;
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
import com.se.uims.usermanager.UserManagerUIMSV2;
import com.se.uims.usermanager.UserV5;
import com.uims.authenticatedUsermanager.AccessElement;
import com.uims.authenticatedUsermanager.Type;
import com.uims.user22.UserManagerUIMSV22;

/**
 * The Soap Service interface layer to call the UIMS user manager stubs.
 * 
 * @author Aravindh Kumar
 *
 */

@Profile("Staging")
@org.springframework.stereotype.Service("uimsUserManagSoapService")
@EnableAsync
public class UIMSUserManagerSoapServiceStaging implements UIMSUserManagerSoapService<UserManagerUIMSV2, UserV5, UserManagerUIMSV22> {

	/**
	 * Logger instance.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(UIMSUserManagerSoapService.class);

	//private static final Logger UIMSLOGGER = LoggerFactory.getLogger("uimsLogger");

	@Inject
	private SamlAssertionTokenGenerator samlTokenService;
	
	@Inject
	private IdmsMapper mapper;
	
	@Inject
	private OpenAMService productService;
	
	@Inject
	@Qualifier("pickListValidator")
	private IValidator pickListValidator;
	
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
	
	@Autowired
	private UIMSCompanyManagerSoapServiceSync companyManagerSoapServiceSync;

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
	
	@Value("${userManagerUIMSV22Wsdl}")
	private String userManagerUIMSV22Wsdl;

	@Value("${userManagerUIMSV22WsdlQname}")
	private String userManagerUIMSV22WsdlQname;

	@Value("${userManagerUIMSV22WsdlPortName}")
	private String userManagerUIMSV22WsdlPortName;

	// private T uimsIdentity;

	// @Value("${applicationName}")
	private String applicationName = "Uims";

	private boolean updateUIMSUser = false;

	private boolean updateUIMSCompany = false;

	private String samlAssertion = null;

	private boolean isNoPwdactivated = false;

	private boolean setPasswordStatus = false;

	private boolean ispasswordupdated = false;
	
	private boolean changeEmailUpdated = false;
	
	private String createdFedId = null;
	
	private String createdCompanyFedId = null;
	
	private boolean isIdentityActvated = false;

	/**
	 * Service to fetch information about {@link Product}s.
	 */


	public UserManagerUIMSV2 getUserManager() {
		LOGGER.info("Entered getUserManager() of UIMS -> Start");
		UserManagerUIMSV2 userManagerUIMSV2 = null;
		URL url;
		try {
			url = new URL(userManagerUIMSWsdl);
			QName qname = new QName(userManagerUIMSWsdlQname, userManagerUIMSWsdlPortName);
			Service service = Service.create(url, qname);
			userManagerUIMSV2 = service.getPort(UserManagerUIMSV2.class);
			LOGGER.info("getUserManager() of UIMS -> End");

		} catch (MalformedURLException e) {
			LOGGER.error("Exception while UIMSUserManagerSoapService :: getUserManager()::" + e.getMessage());
			e.printStackTrace();
		} catch (Exception e) {
			LOGGER.error("Exception while UIMSUserManagerSoapService :: getUserManager()::" + e.getMessage());
			e.printStackTrace();
		}
		return userManagerUIMSV2;
	}
	
	
	public UserManagerUIMSV22 getUIMSV22UserManager() {
		LOGGER.info("Entered getUIMSV22UserManager() -> Start");
		URL url;
		UserManagerUIMSV22 userManagerUIMSV22 = null;
		try {
			url = new URL(userManagerUIMSV22Wsdl);

			QName qname = new QName(userManagerUIMSV22WsdlQname, userManagerUIMSV22WsdlPortName);
			Service service = Service.create(url, qname);

			LOGGER.info("Start: getPort() of UIMS");
			userManagerUIMSV22 = service.getPort(UserManagerUIMSV22.class);
			LOGGER.info("End: getPort() of UIMS");

		}catch (MalformedURLException e) {
			LOGGER.error("MalformedURLException in getUIMSV22UserManager()::" + e.getMessage());
			e.printStackTrace();
		}
		catch (Exception e) {			
			LOGGER.error("Exception in getUIMSV22UserManager()::" + e.getMessage());
			e.printStackTrace();
		}
		return userManagerUIMSV22;
	}


	@Async
	public void getUIMSUser(String callerFid, String vnew) throws MalformedURLException {
		LOGGER.info("Entered getUIMSUser() -> Start");
		LOGGER.info("Parameter callerFid -> " + callerFid + " ,vnew -> " + vnew);

		String samlAssertionOrToken = null;
		try {
			samlAssertionOrToken = samlTokenService.getSamlAssertionToken(callerFid, vnew);
			LOGGER.info("samlAssertionOrToken"+samlAssertionOrToken);
			UserManagerUIMSV2 userManagerUIMSV22 = getUserManager();
		
			LOGGER.info("Start: UIMS getUser() for callerFid:" + callerFid);
			userManagerUIMSV22.getUser(callerFid, samlAssertionOrToken);
			LOGGER.info("End: UIMS getUser() finished for callerFid:" + callerFid);
		} catch (IMSServiceSecurityCallNotAllowedException_Exception
				| InvalidImsServiceMethodArgumentException_Exception | LdapTemplateNotReadyException_Exception
				| RequestedEntryNotExistsException_Exception | SecuredImsException_Exception
				| UnexpectedLdapResponseException_Exception | UnexpectedRuntimeImsException_Exception e) {
			LOGGER.error("Exception in UIMS getUser()::" + e.getMessage());
			e.printStackTrace();
		}catch (Exception e1) {
			LOGGER.error("Exception in getUIMSUser()::" + e1.getMessage());
			e1.printStackTrace();
		}
	}

	@Async
	public void activateIdentity(String iPlanetDirectoryKey, String userId, String callerFid, String password,
			String openamVnew, String loginIdentifierType, String emailOrMobile) throws MalformedURLException {
		LOGGER.info("Entered activateIdentity() -> Start");
		LOGGER.info("Parameter iPlanetDirectoryKey -> " + iPlanetDirectoryKey + " ,userId -> " + userId);
		LOGGER.info("Parameter callerFid -> " + callerFid);
		LOGGER.info("Parameter openamVnew -> " + openamVnew + " ,loginIdentifierType -> " + loginIdentifierType);
		LOGGER.info("Parameter emailOrMobile -> " + emailOrMobile);

		try {
			if (UserConstants.EMAIL.equalsIgnoreCase(loginIdentifierType)) {
				samlAssertion = samlTokenService.getSamlAssertionToken(userId, openamVnew);
			} else {
				samlAssertion = samlTokenService.getSamlAssertionToken(callerFid, openamVnew);
			}

			LOGGER.info("samlAssertion="+samlAssertion);
			Callable<Boolean> callableActivateIdentity = new Callable<Boolean>() {
				public Boolean call() throws Exception {
					UserManagerUIMSV22 userManagerUIMSV22 = getUIMSV22UserManager();
					if (UserConstants.EMAIL.equalsIgnoreCase(loginIdentifierType)) {
						LOGGER.info("Start: activateIdentity() of UIMS for EMAIL.. userId:" + userId);						
						isIdentityActvated =userManagerUIMSV22.activateIdentity(UimsConstants.CALLER_FID,password, samlAssertion);						
						LOGGER.info("End: activateIdentity() of UIMS finished for EMAIL.. userId:" + userId);
					} else {
						LOGGER.info("Start: setPasswordWithSms() of UIMS for non-EMAIL.. userId:" + userId);
						isIdentityActvated = userManagerUIMSV22.setPasswordWithSms(UimsConstants.CALLER_FID,
								emailOrMobile, samlAssertion, UserConstants.TOKEN_TYPE, password);
						LOGGER.info("End: setPasswordWithSms() of UIMS finished for non-EMAIL.. userId:" + userId);
					}
					LOGGER.info("isIdentityActvated: " + isIdentityActvated);
					return true;
				}
			};

			Retryer<Boolean> retryer = RetryerBuilder.<Boolean> newBuilder()
					.retryIfResult(Predicates.<Boolean> isNull()).retryIfExceptionOfType(Exception.class)
					.retryIfRuntimeException().withStopStrategy(StopStrategies.stopAfterAttempt(3)).build();

			retryer.call(callableActivateIdentity);
			// after successful setUIMSPassword , we need to update the
			// v_old
			if (isIdentityActvated) {
				String version = "{" + "\"V_Old\": \"" + openamVnew + "\"" + "}";
				LOGGER.info("Start: updateUser() of openamservice to update version for userId:" + userId);
				productService.updateUser(iPlanetDirectoryKey, userId, version);
				LOGGER.info("End: updateUser() of openamservice to update version finished for userId:" + userId);
			}
			if (!isIdentityActvated) {
				LOGGER.info("UIMS activateIdentity() failed -> ::sending mail notification, userid::"+ userId);

				LOGGER.info("Start: emailReadyToSendEmail() for userId:" + userId);
				sendEmail.emailReadyToSendEmail(supportUser, fromUserName,
						"UIMS UserpinConfirmation activateIdentity failed.", userId);
				LOGGER.info("End: emailReadyToSendEmail() finished for userId:" + userId);
			}
		} catch (RetryException e) {
			LOGGER.error("RetryException in activateIdentity() of UIMS::" + e.getMessage());
			e.printStackTrace();

		} catch (ExecutionException e) {
			LOGGER.error("ExecutionException in activateIdentity() of UIMS::" + e.getMessage());
			e.printStackTrace();
		} catch (Exception e) {
			LOGGER.error("Exception in activateIdentity() of UIMS::" + e.getMessage());
			e.printStackTrace();
		}
		LOGGER.info("activateIdentity() Async Method -> End");
	}
	
	@Async
	public void setUIMSPassword(String iPlanetDirectoryKey, String userId, String callerFid, String password,
			String openamVnew, String loginIdentifierType, String emailOrMobile) throws MalformedURLException {
		LOGGER.info("Entered setUIMSPassword() -> Start");
		LOGGER.info("Parameter iPlanetDirectoryKey -> " + iPlanetDirectoryKey + " ,userId -> " + userId);
		LOGGER.info("Parameter callerFid -> " + callerFid);
		LOGGER.info("Parameter openamVnew -> " + openamVnew + " ,loginIdentifierType -> " + loginIdentifierType);
		LOGGER.info("Parameter emailOrMobile -> " + emailOrMobile);

		try {
			if (UserConstants.EMAIL.equalsIgnoreCase(loginIdentifierType)) {
				samlAssertion = samlTokenService.getSamlAssertionToken(userId, openamVnew);
			} else {
				samlAssertion = samlTokenService.getSamlAssertionToken(callerFid, openamVnew);
			}
			LOGGER.info("samlAssertion="+samlAssertion);

			Callable<Boolean> callableSetUIMSPassword = new Callable<Boolean>() {
				public Boolean call() throws Exception {

					UserManagerUIMSV22 userManagerUIMSV22 = getUIMSV22UserManager();
					if (UserConstants.EMAIL.equalsIgnoreCase(loginIdentifierType)) {
						LOGGER.info("Start: setPassword() of UIMS for EMAIL.. userId:" + userId);
						setPasswordStatus = userManagerUIMSV22.setPassword(UimsConstants.CALLER_FID, samlAssertion,
								password);
						LOGGER.info("End: setPassword() of UIMS finished for EMAIL.. userId:" + userId);
					} else {
						LOGGER.info("Start: setPasswordWithSms() of UIMS for non-EMAIL.. userId:" + userId);
						setPasswordStatus = userManagerUIMSV22.setPasswordWithSms(UimsConstants.CALLER_FID,
								emailOrMobile, samlAssertion, UserConstants.TOKEN_TYPE, password);
						LOGGER.info("End: setPasswordWithSms() of UIMS finished for non-EMAIL.. userId:" + userId);
					}
					LOGGER.info("setPasswordStatus: " + setPasswordStatus);
					return true;
				}
			};

			Retryer<Boolean> retryer = RetryerBuilder.<Boolean> newBuilder()
					.retryIfResult(Predicates.<Boolean> isNull()).retryIfExceptionOfType(Exception.class)
					.retryIfRuntimeException().withStopStrategy(StopStrategies.stopAfterAttempt(3)).build();
			retryer.call(callableSetUIMSPassword);
			// after successful setUIMSPassword , we need to update the
			// v_old
			if (setPasswordStatus) {
				String version = "{" + "\"V_Old\": \"" + openamVnew + "\"" + "}";
				LOGGER.info("Start: updateUser() of openamservice to update version for userId:" + userId);
				productService.updateUser(iPlanetDirectoryKey, userId, version);
				LOGGER.info("End: updateUser() call of openamservice to update version finished for userId:" + userId);
			}
			if (!setPasswordStatus) {
				LOGGER.info(
						"UIMS UserpinConfirmation setPassword failed -> ::sending mail notification for userid::"
								+ userId);
				LOGGER.info("Start: emailReadyToSendEmail() for userId:" + userId);
				sendEmail.emailReadyToSendEmail(supportUser, fromUserName,
						"UIMS UserpinConfirmation setPassword failed.", userId);
				LOGGER.info("End: emailReadyToSendEmail() finished for userId:" + userId);
			}
		}catch (RetryException e) {
			LOGGER.error("RetryException in setUIMSPassword() of UIMS::" + e.getMessage());
			e.printStackTrace();

		} catch (ExecutionException e) {
			LOGGER.error("ExecutionException in setUIMSPassword() of UIMS::" + e.getMessage());
			e.printStackTrace();
		} catch (Exception e) {
			LOGGER.error("Exception while setUIMSPassword::" + e.getMessage());
			e.printStackTrace();
		}
		LOGGER.info("setUIMSPassword() Async Method -> End");
	}

	@Async
	public void updateUIMSPassword(String callerFid, String userId, String oldPassword, String newPassword,
			String openamVnew, String iPlanetDirectoryKey) throws MalformedURLException {
		LOGGER.info("Entered updateUIMSPassword() -> Start");
		LOGGER.info("Parameter iPlanetDirectoryKey -> " + iPlanetDirectoryKey + " ,userId -> " + userId);
		LOGGER.info("Parameter callerFid -> " + callerFid);
		LOGGER.info("Parameter openamVnew -> " + openamVnew);

		try {
			samlAssertion = samlTokenService.getSamlAssertionToken(callerFid, openamVnew);
			LOGGER.info("samlAssertion="+samlAssertion);

			Callable<Boolean> callableUpdateUIMSPassword = new Callable<Boolean>() {
				public Boolean call() throws Exception {
					UserManagerUIMSV22 userManagerUIMSV22 = getUIMSV22UserManager();					
					LOGGER.info("Start: updatePassword() of UIMS for callerFid:" + callerFid);
					ispasswordupdated = userManagerUIMSV22.updatePassword(UimsConstants.CALLER_FID, samlAssertion,
							oldPassword, newPassword);
					LOGGER.info("End: updatePassword() of UIMS finished for callerFid:" + callerFid);
					LOGGER.info("Update password status is::" + ispasswordupdated);
					return true;
				}
			};

			Retryer<Boolean> retryer = RetryerBuilder.<Boolean> newBuilder()
					.retryIfResult(Predicates.<Boolean> isNull()).retryIfExceptionOfType(Exception.class)
					.retryIfRuntimeException().withStopStrategy(StopStrategies.stopAfterAttempt(3)).build();

			retryer.call(callableUpdateUIMSPassword);
			// after successful activateIdentityNoPassword, we need to
			// update the v_old
			if (ispasswordupdated) {
				String version = "{" + "\"V_Old\": \"" + openamVnew + "\"" + "}";
				LOGGER.info("Start: updateUser() of openamservice to update version for userId:" + userId);
				productService.updateUser(iPlanetDirectoryKey, userId, version);
				LOGGER.info("End: updateUser() call of openamservice to update version finished for userId:" + userId);
			}
		} catch (RetryException e) {
			LOGGER.error("RetryException in UIMS updateUIMSPassword()::" + e.getMessage());
			e.printStackTrace();

		} catch (ExecutionException e) {
			LOGGER.error("ExecutionException in UIMS updateUIMSPassword()::" + e.getMessage());
			e.printStackTrace();
		} catch (Exception e) {
			LOGGER.error("Exception in updateUIMSPassword()::" + e.getMessage());
			e.printStackTrace();
		}
	}

	@Async
	public boolean updateUIMSUser(String fedId, UserV5 user, String vnew) throws MalformedURLException {
		LOGGER.info("Entered updateUIMSUser() -> Start");
		LOGGER.info("Parameter fedId -> " + fedId + " ,vnew -> " + vnew);

		boolean status = false;
		UserManagerUIMSV2 userManagerUIMSV2 = getUserManager();
		String samlAssertion = null;
		ObjectMapper objMapper = new ObjectMapper();
		try {
			LOGGER.info("Parameter user -> " + objMapper.writeValueAsString(user));
			
			samlAssertion = samlTokenService.getSamlAssertionToken(fedId, vnew);
			LOGGER.info("samlAssertion="+samlAssertion);
			
			LOGGER.info("Start: updateUser() of UIMS for user:" + user.getFederatedID());
			status = userManagerUIMSV2.updateUser(UimsConstants.CALLER_FID, samlAssertion, user);
			LOGGER.info("End: updateUser() of UIMS finished with status =>"+status+" for user:" + user.getFederatedID());

			if(status){
				LOGGER.info("UIMS updateUIMSUser() successful, status:" + status);
			}else{
				LOGGER.info("UIMS updateUIMSUser() failed, status:" + status);
			}
		} catch (IMSServiceSecurityCallNotAllowedException_Exception | InactiveUserImsException_Exception
				| InvalidImsServiceMethodArgumentException_Exception | LdapTemplateNotReadyException_Exception
				| RequestedEntryNotExistsException_Exception | SecuredImsException_Exception
				| UnexpectedLdapResponseException_Exception | UnexpectedRuntimeImsException_Exception e) {
			LOGGER.error("Error executing while getting status in updateUIMSUser()::" + e.getMessage());
			e.printStackTrace();
		} catch (Exception e) {
			LOGGER.error("Error executing while getting samlAssertion::" + e.getMessage());
			e.printStackTrace();
		}	
		LOGGER.info("updateUser() of UIMS finished for fedId:" + fedId);
		return status;
	}

	@Async
	public void activateUIMSIdentity(String callerFid, String password, String vnew) throws MalformedURLException {
		UserManagerUIMSV2 userManagerUIMSV2 = getUserManager();
		boolean status = false;
		LOGGER.info("Entered activateUIMSIdentity() -> Start");
		LOGGER.info("Parameter callerFid -> " + callerFid + " ,vnew -> " + vnew);

		String authentificationToken = null;
		try {
			authentificationToken = samlTokenService.getSamlAssertionToken(callerFid, vnew);
			LOGGER.info("authentificationToken="+authentificationToken);

			LOGGER.info("Start: activateIdentity() of UIMS for callerFid:" + callerFid);			
			status = userManagerUIMSV2.activateIdentity(UimsConstants.CALLER_FID, password, authentificationToken);			
			LOGGER.info("End: activateIdentity() of UIMS finished with status =>"+status+" for callerFid:" + callerFid);
			
		} catch (IMSServiceSecurityCallNotAllowedException_Exception
				| InvalidImsServiceMethodArgumentException_Exception | LdapTemplateNotReadyException_Exception
				| RequestedEntryNotExistsException_Exception | RequestedInternalUserException_Exception
				| SecuredImsException_Exception | UnexpectedLdapResponseException_Exception
				| UnexpectedRuntimeImsException_Exception e) {
			LOGGER.error("Error in activateUIMSIdentity()::" + e.getMessage());
			e.printStackTrace();
		} catch (Exception e) {
			LOGGER.error("Exception in activateUIMSIdentity()::" + e.getMessage());
			e.printStackTrace();
		}
	}

	@Async
	public void activateIdentityNoPassword(String userId, String callerFid, String openamVnew,
			String iPlanetDirectoryKey, String loginIdentifierType, String emailOrMobile) throws MalformedURLException {
		LOGGER.info("Entered activateIdentityNoPassword() -> Start");
		LOGGER.info("Parameter userId -> " + userId + " ,callerFid -> " + callerFid);
		LOGGER.info("Parameter openamVnew -> " + openamVnew + " ,iPlanetDirectoryKey -> " + iPlanetDirectoryKey);
		LOGGER.info("Parameter loginIdentifierType -> " + loginIdentifierType + " ,emailOrMobile -> " + emailOrMobile);

		try {
			// samlAssertion =
			// SamlAssertionTokenGenerator.getSamlAssertionToken(userId,openamVnew);

			if (UserConstants.EMAIL.equalsIgnoreCase(loginIdentifierType)) {
				samlAssertion = samlTokenService.getSamlAssertionToken(userId, openamVnew);
			} else {
				samlAssertion = samlTokenService.getSamlAssertionToken(callerFid, openamVnew);
			}
			LOGGER.info("samlAssertion="+samlAssertion);

			Callable<Boolean> callableActivateIdentityNoPassword = new Callable<Boolean>() {
				public Boolean call() throws Exception {

					UserManagerUIMSV22 userManagerUIMSV22 = getUIMSV22UserManager();
					if (UserConstants.EMAIL.equalsIgnoreCase(loginIdentifierType)) {
						LOGGER.info("Start: activateIdentityNoPassword() of UIMS for EMAIL.. userId:" + userId);
						isNoPwdactivated = userManagerUIMSV22.activateIdentityNoPassword(UimsConstants.CALLER_FID,
								samlAssertion);
						LOGGER.info("End: activateIdentityNoPassword() of UIMS finished with status =>"+isNoPwdactivated+" for EMAIL.. userId:" + userId);

					} else {
						LOGGER.info("Start: activateIdentityWithMobileNoPassword() of UIMS.. emailOrMobile:"
								+ emailOrMobile);
						isNoPwdactivated = userManagerUIMSV22.activateIdentityWithMobileNoPassword(
								UimsConstants.CALLER_FID, emailOrMobile, samlAssertion);
						LOGGER.info("End: activateIdentityWithMobileNoPassword() of UIMS finished with status =>"+isNoPwdactivated+" for emailOrMobile:"
								+ emailOrMobile);
					}
					LOGGER.info("UIMS activateIdentityNoPassword isactivated status:" + isNoPwdactivated);
					return true;
				}
			};
			Retryer<Boolean> retryer = RetryerBuilder.<Boolean> newBuilder()
					.retryIfResult(Predicates.<Boolean> isNull()).retryIfExceptionOfType(Exception.class)
					.retryIfRuntimeException().withStopStrategy(StopStrategies.stopAfterAttempt(3)).build();

			retryer.call(callableActivateIdentityNoPassword);
			// after successful activateIdentityNoPassword, we need to
			// update the v_old
			if (isNoPwdactivated) {
				String version = "{" + "\"V_Old\": \"" + openamVnew + "\"" + "}";
				LOGGER.info("Start: updateUser() of openamservice to update version for userID:" + userId);
				productService.updateUser(iPlanetDirectoryKey, userId, version);
				LOGGER.info("End: updateUser() of openamservice finished to update version for userID:" + userId);
			}
			if (!isNoPwdactivated) {
				LOGGER.error(
						"UIMS UserpinConfirmation activateIdentityNoPassword got failed -> ::sending mail notification for userId::"
								+ userId);
				sendEmail.emailReadyToSendEmail(supportUser, fromUserName,
						"UIMS UserpinConfirmation activateIdentityNoPassword failed.", userId);
			}
		} catch (RetryException e) {
			LOGGER.error("RetryException in activateIdentityNoPassword()::" + e.getMessage());
			e.printStackTrace();
		} catch (ExecutionException e) {
			LOGGER.error("ExecutionException in activateIdentityNoPassword()::" + e.getMessage());
			e.printStackTrace();
		} catch (Exception e) {
			LOGGER.error("Exception in activateIdentityNoPassword()::" + e.getMessage());
			e.printStackTrace();
		}
		LOGGER.info("Completed UIMS activateIdentityNoPassword() Async method!");

	}

	@Async
	public String createUIMSUserAndCompany(String callerFid, com.uims.authenticatedUsermanager.UserV6 identity,
			String context, CompanyV3 company, String userName, String iPlanetDirectoryKey, String v_new,
			String password, String forcedFederatedId, CreateUserRequest userRequest, int companyCreatedCount) {
		LOGGER.info("Entered createUIMSUserAndCompany() -> Start");
		LOGGER.info("Parameter callerFid -> " + callerFid + " ,identity -> " + identity);
		LOGGER.info("Parameter context -> " + context + " ,company -> " + company);
		LOGGER.info("Parameter userName -> " + userName + " ,iPlanetDirectoryKey -> " + iPlanetDirectoryKey);
		LOGGER.info("Parameter v_new -> " + v_new + " ,forcedFederatedId -> " + forcedFederatedId);
		//LOGGER.info("Parameter userRequest -> " + userRequest);

		Boolean companyCreated = false;
		Boolean userCreated = false;
		AccessElement application = new AccessElement();
		application.setId(applicationName);
		application.setType(Type.APPLICATION);

		ObjectMapper objMapper = new ObjectMapper();
		String userRequestjsonString = "";
		try {
			LOGGER.info("Parameter userRequest -> " + ChinaIdmsUtil.printData(objMapper.writeValueAsString(userRequest)));
			userRequestjsonString = objMapper.writeValueAsString(userRequest);
			userRequestjsonString = userRequestjsonString.replace("\"\"", "[]");
		} catch (JsonProcessingException e) {
			LOGGER.error("Error while converting the userRequest to Json" + e.getMessage());
			e.printStackTrace();
		} finally {
			if (null != objMapper) {
				objMapper = null;
			}
		}
		try {
			Callable<Boolean> callableUser = new Callable<Boolean>() {
				public Boolean call() throws Exception {
					if (null != userRequest.getUserRecord().getIDMSCompanyFederationIdentifier__c()
							&& !userRequest.getUserRecord().getIDMSCompanyFederationIdentifier__c().isEmpty()) {

						if (companyCreatedCount > 1) {
							identity.setCompanyId(userRequest.getUserRecord().getIDMSCompanyFederationIdentifier__c());
						}
					}

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
						productService.updateUser(iPlanetDirectoryKey, userName, fedID);
					}

					return true;
				}
			};

			Retryer<Boolean> retryer = RetryerBuilder.<Boolean> newBuilder()
					.retryIfResult(Predicates.<Boolean> isNull()).retryIfExceptionOfType(Exception.class)
					.retryIfRuntimeException().withStopStrategy(StopStrategies.stopAfterAttempt(3)).build();
			userCreated = retryer.call(callableUser);
			if (userCreated) {
				LOGGER.info("UIMS user created successfully::" + userCreated);
			}

			if ((!userCreated || null == createdFedId)
					&& (null != context && (UserConstants.USER_CONTEXT_HOME.equalsIgnoreCase(context)
							|| UserConstants.USER_CONTEXT_HOME_1.equalsIgnoreCase(context)))) {

				LOGGER.error("CreateUser got failed -> ::sending mail notification for userRequestjsonString::"
						+ userRequestjsonString);
				sendEmail.emailReadyToSendEmail(supportUser, fromUserName, "UIMS CreateUser failed.",
						userRequestjsonString);
			}
			Callable<Boolean> callableCompany = new Callable<Boolean>() {
				public Boolean call() throws Exception {
					/**
					 * When user is creating from BFO then no need of creating
					 * company, bfo account id act as company
					 */
					/*
					 * if (null !=
					 * userRequest.getUserRecord().getBFO_ACCOUNT_ID__c() &&
					 * !userRequest.getUserRecord().getBFO_ACCOUNT_ID__c().
					 * isEmpty()) { createdCompanyFedId =
					 * userRequest.getUserRecord().getBFO_ACCOUNT_ID__c(); }
					 * else if ((null !=
					 * userRequest.getUserRecord().getAdminCompanyFederatedId()
					 * &&
					 * !userRequest.getUserRecord().getAdminCompanyFederatedId()
					 * .isEmpty()) && (null !=
					 * userRequest.getUserRecord().getAdminCompanyFederatedId()
					 * &&
					 * !userRequest.getUserRecord().getAdminCompanyFederatedId()
					 * .isEmpty())) { createdCompanyFedId =
					 * userRequest.getUserRecord().getAdminCompanyFederatedId();
					 * } else if ((null != userRequest.getUserRecord().
					 * getIDMS_Registration_Source__c() &&
					 * !userRequest.getUserRecord().
					 * getIDMS_Registration_Source__c().isEmpty()) &&
					 * pickListValidator.validate(UserConstants.IDMS_BFO_profile
					 * ,userRequest.getUserRecord().
					 * getIDMS_Registration_Source__c())){ //if registration
					 * source is PRM then accept and force the company
					 * FederatedId from IFW/IDMS global createdCompanyFedId =
					 * userRequest.getUserRecord().getCompanyFederatedId();
					 * companyManagerSoapService.
					 * createUIMSCompanyWithCompanyForceIdmsId(createdFedId,
					 * UimsConstants.VNEW, company); } else {
					 * 
					 * //createdCompanyFedId =
					 * companyManagerSoapService.createUIMSCompany(createdFedId,
					 * UimsConstants.VNEW, company);
					 * companyManagerSoapService.createUIMSCompany(createdFedId,
					 * UimsConstants.VNEW, company); }
					 */

					if (null != userRequest.getUserRecord().getIDMSCompanyFederationIdentifier__c()
							&& !userRequest.getUserRecord().getIDMSCompanyFederationIdentifier__c().isEmpty()) {
						if (companyCreatedCount == 1) {
							createdCompanyFedId = companyManagerSoapService.createUIMSCompanyWithCompanyForceIdmsId(
									createdFedId, userRequest.getUserRecord().getIDMSCompanyFederationIdentifier__c(),
									UimsConstants.VNEW, company);
						}
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

			if (null != context && (UserConstants.USER_CONTEXT_WORK.equalsIgnoreCase(context)
					|| UserConstants.USER_CONTEXT_WORK_1.equalsIgnoreCase(context))) {

				companyCreated = retryerCompany.call(callableCompany);
				if (companyCreated) {
					LOGGER.info("UIMS company created successfully::" + companyCreated);
				}
				if (userCreated && companyCreated) {
					// after successful creation of user and company, we
					// need to update the v_old
					String version = "{" + "\"V_Old\": \"" + v_new + "\"" + "}";
					productService.updateUser(iPlanetDirectoryKey, userName, version);
				}
			}

			if ((!(userCreated && companyCreated) || (null == createdCompanyFedId && null == createdFedId))
					&& (null != context && (UserConstants.USER_CONTEXT_WORK.equalsIgnoreCase(context)
							|| UserConstants.USER_CONTEXT_WORK_1.equalsIgnoreCase(context)))) {

				LOGGER.error("UIMS CreateUser and CreateCompany got failed --> ::sending mail notification::"
						+ userRequestjsonString);
				sendEmail.emailReadyToSendEmail(supportUser, fromUserName, "UIMS CreateUser and CreateCompany failed.",
						userRequestjsonString);
			}
		}catch (RetryException e) {
			LOGGER.error("RetryException in createUIMSUserAndCompany()::" + e.getMessage());
			e.printStackTrace();

		} catch (ExecutionException e) {
			LOGGER.error("ExecutionException in createUIMSUserAndCompany()::" + e.getMessage());
			e.printStackTrace();
		} catch (Exception e) {
			LOGGER.error("Exception in createUIMSUserAndCompany()::" + e.getMessage());
			e.printStackTrace();
		}
		createdFedId = null;
		LOGGER.info("Completed the createUIMSUserAndCompany() Async method!!");
		return null;
	}

	@Async
	public String updateUIMSUserAndCompany(String fedId, UserV5 identity, String context, CompanyV3 company,
			String vnew, OpenAMService productService, String iPlanetDirectoryKey, String userName,String companyFedId,String email) {

		LOGGER.info("Inside the updateUIMSUserAndCompany Async method!!");
		LOGGER.info("Entered updateUIMSUserAndCompany() -> Start");
		LOGGER.info("Parameter fedId -> " + fedId);
		LOGGER.info("Parameter context -> " + context);
		LOGGER.info("Parameter userName -> " + userName + " ,email -> " + email);

		AccessElement application = new AccessElement();
		application.setId(applicationName);
		application.setType(Type.APPLICATION);
		ObjectMapper objectMapper = new ObjectMapper();

		try {

			LOGGER.info("Parameter company -> " + objectMapper.writeValueAsString(company));
			LOGGER.info("Parameter identity -> " + objectMapper.writeValueAsString(identity));

			//UIMSCompanyManagerSoapService companyManagerSoapService = new UIMSCompanyManagerSoapService();
			Callable<Boolean> callableUpdateUIMSUserAndComapany = new Callable<Boolean>() {
				public Boolean call() throws Exception {

					// TODO check in global if we pass a single attribute, what
					// should be the behaviour for other fields User object.

					// Answer from Subrat: Remaining fields should not be
					// updated.
					LOGGER.info("Start: updateUIMSUser() for fedId:" + fedId);
					updateUIMSUser = updateUIMSUser(fedId, identity, vnew);
					LOGGER.info("End: updateUIMSUser() finished for fedId:" + fedId + " with status:" + updateUIMSUser);

					return true;
				}
			};

			Retryer<Boolean> retryer = RetryerBuilder.<Boolean> newBuilder()
					.retryIfResult(Predicates.<Boolean> isNull()).retryIfExceptionOfType(Exception.class)
					.retryIfRuntimeException().withStopStrategy(StopStrategies.stopAfterAttempt(3)).build();

			retryer.call(callableUpdateUIMSUserAndComapany);

			if (updateUIMSUser) {
				LOGGER.info("UIMS User updated Successfully::" + updateUIMSUser);
			}
			if (!updateUIMSUser && (null != context && (UserConstants.USER_CONTEXT_HOME.equalsIgnoreCase(context)
					|| UserConstants.USER_CONTEXT_HOME_1.equalsIgnoreCase(context)))) {

				LOGGER.error("updateUIMSUser() got failed -> ::sending mail notification for userName::"
						+ userName);

				sendEmail.emailReadyToSendEmail(supportUser, fromUserName, "UIMS Update user failed.", userName);
			}



			Callable<Boolean> callableUpdateCompany = new Callable<Boolean>() {
				public Boolean call() throws Exception {

					// TODO logic to get the federatedId
					//String federatedId = ""; 
					//company.getFederatedId() is compFedIdInOpenAM, companyFedId is from request  
					if(null == company.getFederatedId() && null != companyFedId){						
						String uimsUserResponse = companyManagerSoapServiceSync.createUIMSCompanyWithCompanyForceIdmsId(fedId, companyFedId, vnew, company);
						LOGGER.info("uimsUserResponse = "+uimsUserResponse);
					} else if(null == company.getFederatedId() && null == companyFedId){
						String newcompanyFedId = ChinaIdmsUtil.generateFedId();
						
						String companyFederatedIDQuery = "{" + "\"companyFederatedID\": \"" + newcompanyFedId + "\"" + "}";
						LOGGER.info("companyFederatedIDQuery = "+companyFederatedIDQuery);
						LOGGER.info("Start: updateUser() of openam to update companyFedId for username : "+userName);
						productService.updateUser(iPlanetDirectoryKey, userName, companyFederatedIDQuery);
						LOGGER.info("End: updateUser() of openam to update companyFedId finished for username : "+userName);
						String uimsUserResponse = companyManagerSoapServiceSync.createUIMSCompanyWithCompanyForceIdmsId(fedId, newcompanyFedId, vnew, company);
						LOGGER.info("uimsUserResponse = "+uimsUserResponse);
					} else{
						updateUIMSCompany = companyManagerSoapService.updateUIMSCompany(fedId, vnew, company,company.getFederatedId());
					}
					return updateUIMSCompany;
				}
			};

			Retryer<Boolean> retryerCompany = RetryerBuilder.<Boolean> newBuilder()
					.retryIfResult(Predicates.<Boolean> isNull()).retryIfExceptionOfType(Exception.class)
					.retryIfRuntimeException().withStopStrategy(StopStrategies.stopAfterAttempt(3)).build();


			/*if (null != context && (UserConstants.USER_CONTEXT_WORK.equalsIgnoreCase(context)
					* || UserConstants.USER_CONTEXT_WORK_1.equalsIgnoreCase(context))) {
					
					* removing the check to fire uims update company even in null context
					* as in some scenario we are getting company info even context is null
					* company object's organization name attribute is sometime empty
					* so not checking on organization name also 					
					* */

				retryerCompany.call(callableUpdateCompany);
				if(updateUIMSCompany){
					LOGGER.info("UIMS company updated Successfully:"+updateUIMSCompany);
				}
				if (updateUIMSUser && updateUIMSCompany) {
					// after successful creation of user and company, we
					// need to update the v_old
					String version = "{" + "\"V_Old\": \"" + vnew + "\"" + "}";
					productService.updateUser(iPlanetDirectoryKey, userName, version);
				}
				if (!(updateUIMSUser && updateUIMSCompany)
						&& (null != context && (UserConstants.USER_CONTEXT_WORK.equalsIgnoreCase(context)
								|| UserConstants.USER_CONTEXT_WORK_1.equalsIgnoreCase(context)))) {
					LOGGER.error(
							"UIMS User and Company updated got failed -----> ::sending mail notification for userName::"
									+ userName);
					sendEmail.emailReadyToSendEmail(supportUser, fromUserName, "UIMS Update user and company failed.",
							userName);
				}
			//}

		} catch (RetryException e) {
			LOGGER.error("RetryException in updateUIMSUserAndCompany()::" + e.getMessage());
			e.printStackTrace();

		} catch (ExecutionException e) {
			LOGGER.error("ExecutionException in updateUIMSUserAndCompany()::" + e.getMessage());
			e.printStackTrace();
		} catch (Exception e) {
			LOGGER.error("Exception in updateUIMSUserAndCompany()::" + e.getMessage());
			// LOGGER.error("UIMS User and Company updated got failed ----->
			// ::sending mail notification for userName::"+userName);
			e.printStackTrace();
		}

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
	public void activateUIMSUserConfirmPIN(ConfirmPinRequest confirmRequest, String openamVnew,
			String iPlanetDirectoryKey, String loginIdentifierType, String emailOrMobile) {
		LOGGER.info("Entered activateUIMSUserConfirmPIN() -> Start");
		//LOGGER.info("Parameter confirmRequest -> " + confirmRequest);
		LOGGER.info("Parameter loginIdentifierType -> " + loginIdentifierType + " ,emailOrMobile -> " + emailOrMobile);
		ObjectMapper objMapper=new ObjectMapper();

		try {
			LOGGER.info("Parameter confirmRequest -> "+ ChinaIdmsUtil.printInfo(ChinaIdmsUtil.printData(objMapper.writeValueAsString(confirmRequest))));
			
			if ((null != confirmRequest.getPassword() && !confirmRequest.getPassword().isEmpty())) {
				activateIdentity(iPlanetDirectoryKey, confirmRequest.getId(), confirmRequest.getIDMS_Federated_ID__c(),
						confirmRequest.getPassword().trim(), openamVnew, loginIdentifierType, emailOrMobile);
			} else if (null == confirmRequest.getPassword() || "".equals(confirmRequest.getPassword())) {
				activateIdentityNoPassword(confirmRequest.getId(), confirmRequest.getIDMS_Federated_ID__c(), openamVnew,
						iPlanetDirectoryKey, loginIdentifierType, emailOrMobile);
			}
		} catch (Exception e) {
			LOGGER.error("Exception in activateUIMSUserConfirmPIN():: -> "+ e.getMessage());
			e.printStackTrace();
		}
	}

	@Async
	public void updateChangeEmailOrMobile(String iPlanetDirectoryKey, String userId, String callerFid,
			String openamVnew, String loginIdentifierType, String newEmailOrMobile) throws MalformedURLException {
		LOGGER.info("Entered updateChangeEmailOrMobile() -> Start");
		LOGGER.info("Parameter iPlanetDirectoryKey -> " + iPlanetDirectoryKey + " ,userId -> " + userId);
		LOGGER.info("Parameter callerFid -> " + callerFid);
		LOGGER.info("Parameter openamVnew -> " + openamVnew + " ,loginIdentifierType -> " + loginIdentifierType);
		LOGGER.info("Parameter emailOrMobile -> " + newEmailOrMobile);

		com.uims.user22.AccessElement application = new com.uims.user22.AccessElement();
		application.setId(applicationName);
		application.setType(com.uims.user22.Type.APPLICATION);

		try {
			if (UserConstants.EMAIL.equalsIgnoreCase(loginIdentifierType)) {
				samlAssertion = samlTokenService.getSamlAssertionToken(userId, openamVnew);
			} else {
				samlAssertion = samlTokenService.getSamlAssertionToken(callerFid, openamVnew);
			}
			LOGGER.info("samlAssertion="+samlAssertion);

			Callable<Boolean> callableUIMSPassword = new Callable<Boolean>() {
				public Boolean call() throws Exception {
					UserManagerUIMSV22 userManagerUIMSV22 = getUIMSV22UserManager();
					if (UserConstants.EMAIL.equalsIgnoreCase(loginIdentifierType)) {						
						LOGGER.info("Start: requestEmailChange() of UIMS for EMAIL.. userId:" + userId);
						changeEmailUpdated = userManagerUIMSV22.requestEmailChange(UimsConstants.CALLER_FID,
								samlAssertion, application, newEmailOrMobile);

						LOGGER.info("End: requestEmailChange() of UIMS finished for EMAIL.. userId:" + userId);
					} else {
						LOGGER.info("Start: requestPhoneIdChange() of UIMS for non-EMAIL.. userId:" + userId);

						changeEmailUpdated = userManagerUIMSV22.requestPhoneIdChange(UimsConstants.CALLER_FID,
								samlAssertion, application, newEmailOrMobile);

						LOGGER.info("End: requestPhoneIdChange() of UIMS finished for non-EMAIL.. userId:" + userId);
					}
					LOGGER.info("request Email/PhoneId Change status: " + changeEmailUpdated);

					if (changeEmailUpdated) {
						changeEmailUpdated = userManagerUIMSV22.updateEmail(UimsConstants.CALLER_FID, samlAssertion);
					}

					return true;
				}
			};

			Retryer<Boolean> retryer = RetryerBuilder.<Boolean> newBuilder()
					.retryIfResult(Predicates.<Boolean> isNull()).retryIfExceptionOfType(Exception.class)
					.retryIfRuntimeException().withStopStrategy(StopStrategies.stopAfterAttempt(3)).build();

			retryer.call(callableUIMSPassword);
			// after successful setUIMSPassword , we need to update the
			// v_old
			if (changeEmailUpdated) {

				String version = "{" + "\"V_Old\": \"" + openamVnew + "\"" + "}";
				LOGGER.info("Start: updateUser() of openamservice to update version for userId:" + userId);
				// productService.updateUser(iPlanetDirectoryKey, userId,
				// version);
				LOGGER.info("End: updateUser() call of openamservice to update version finished for userId:" + userId);
			}
			if (!changeEmailUpdated) {
				LOGGER.info(
						"UIMS requestEmailChange got failed -----> ::sending mail notification for userid::"
								+ userId);
				LOGGER.info("Start: emailReadyToSendEmail() for userId:" + userId);
				sendEmail.emailReadyToSendEmail(supportUser, fromUserName, "UIMS updateChangeEmailOrMobile failed.",
						userId);
				LOGGER.info("End: emailReadyToSendEmail() finished for userId:" + userId);
			}
		} catch (RetryException e) {
			LOGGER.error("RetryException in updateChangeEmailOrMobile() of UIMS::" + e.getMessage());
			e.printStackTrace();
		} catch (ExecutionException e) {
			LOGGER.error("ExecutionException in updateChangeEmailOrMobile() of UIMS::" + e.getMessage());
			e.printStackTrace();
		} catch (Exception e) {
			LOGGER.error("Exception in updateChangeEmailOrMobile()::" + e.getMessage());
			e.printStackTrace();
		}
		LOGGER.info("updateChangeEmailOrMobile() Async Method -> End");
	}

	public static void main(String[] args) {

		UIMSUserManagerSoapService service = new UIMSUserManagerSoapServiceStaging();
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
		LOGGER.info("Entered buildGoDigitalRequest() -> Start");

		ObjectMapper objMapper = new ObjectMapper();
		String jsonString = "";
		try {
			LOGGER.info("Parameter userRequest -> " + ChinaIdmsUtil.printData(objMapper.writeValueAsString(userRequest)));
			UserRegistrationInfoRequest userRegistrationInfoRequest = mapper.map(userRequest,
					UserRegistrationInfoRequest.class);

			Authentication authentication = new Authentication();
			authentication.setToken(goDitalToken);
			userRegistrationInfoRequest.getUserRegistrationInfoRequest().setAuthentication(authentication);

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
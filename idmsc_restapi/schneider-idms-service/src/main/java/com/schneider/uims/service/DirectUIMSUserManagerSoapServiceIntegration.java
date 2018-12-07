package com.schneider.uims.service;

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
import com.idms.model.AILRequest;
import com.idms.model.UserRegistrationInfoRequest;
import com.idms.model.digital.Authentication;
import com.idms.product.client.OpenAMService;
import com.idms.service.SendEmail;
import com.idms.service.UIMSAuthenticatedUserManagerSoapService;
import com.idms.service.UIMSCompanyManagerSoapService;
import com.idms.service.digital.GoDigitalUserService;
import com.schneider.idms.model.IdmsUserAilRequest;
import com.schneider.idms.model.IdmsUserConfirmRequest;
import com.schneider.idms.model.IdmsUserRequest;
import com.schneider.ims.service.uimsv2.CompanyV3;
import com.se.idms.dto.IDMSUserAIL;
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
import com.uims.accessmanager.UserAccessManagerUIMSV2;
import com.uims.authenticatedUsermanager.AccessElement;
import com.uims.authenticatedUsermanager.Type;

/**
 * 
 * @author SESA453215
 *
 */

@Profile("Integration")
@org.springframework.stereotype.Service("directUIMSUserManagerSoapService")
@EnableAsync
public class DirectUIMSUserManagerSoapServiceIntegration implements DirectUIMSUserManagerSoapService<UserV6, UserManagerUIMSV22> {

	/**
	 * Logger instance.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(DirectUIMSUserManagerSoapServiceIntegration.class);

	private static final Logger UIMSLOGGER = LoggerFactory.getLogger("uimsLogger");

	@Inject
	private IdmsMapper mapper;
	
	@Inject
	private SamlAssertionTokenGenerator samlTokenService;

	@Inject
	private OpenAMService productService;
	
	//CODE-RE-STRUCTURING
	@Value("${caller.fid}")
	private String CALLER_FID;

	private SendEmail sendEmail;

	/* (non-Javadoc)
	 * @see com.schneider.uims.service.DirectUIMSUserManagerSoapService#setSendEmail(com.idms.service.SendEmail)
	 */
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
	
	@Value("${userAccessManagerUIMSVWsdl}")
	private String userAccessManagerUIMSVWsdl;
	
	@Value("${userAccessManagerUIMSQname}")
	private String userAccessManagerUIMSQname;
	
	@Value("${userAccessManagerUIMSVPortName}")
	private String userAccessManagerUIMSVPortName;

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
	
	private boolean isrevokeresult = false;

	private boolean isgrantresult = false;

	/* (non-Javadoc)
	 * @see com.schneider.uims.service.DirectUIMSUserManagerSoapService#getUserManager()
	 */

	public UserManagerUIMSV22 getUserManager(){
		LOGGER.info("Entered getUserManager() of UIMS -> Start");
		UIMSLOGGER.info("Entered getUserManager() of UIMS -> Start");

		URL url;
		UserManagerUIMSV22 userManagerUIMSV2 = null;
		try {
			url = new URL(userManagerUIMSWsdl);

			QName qname = new QName(userManagerUIMSWsdlQname, userManagerUIMSWsdlPortName);
			Service service = Service.create(url, qname);

			userManagerUIMSV2 = service.getPort(UserManagerUIMSV22.class);
			LOGGER.info("getUserManager() of UIMS -> End");
		}catch (MalformedURLException e) {
			LOGGER.error("Exception while getUserManager()::" + e.getMessage());
			e.printStackTrace();
		}
		catch (Exception e) {
			LOGGER.error("Exception while getUserManager()::" + e.getMessage());
			e.printStackTrace();
		}
		return userManagerUIMSV2;
	}
	
	
	/* (non-Javadoc)
	 * @see com.schneider.uims.service.DirectUIMSUserManagerSoapService#getAccessManager()
	 */
	public UserAccessManagerUIMSV2 getAccessManager(){
		LOGGER.info("Entered getAccessManager(): -. Start ");
		URL url;
		UserAccessManagerUIMSV2 accessManagerUIMSV2 = null;
		try {
			url = new URL(userAccessManagerUIMSVWsdl);

			QName qname = new QName(userAccessManagerUIMSQname,userAccessManagerUIMSVPortName);
			Service service = Service.create(url, qname);
			LOGGER.info("Start: getPort() of UIMS");
			accessManagerUIMSV2 = service.getPort(UserAccessManagerUIMSV2.class);
			LOGGER.info("End: getPort() of UIMS");

		} catch (MalformedURLException e) {
			LOGGER.error("MalformedURLException in getAccessManager()::" + e.getMessage());
			e.printStackTrace();
		}
		catch (Exception e) {
			LOGGER.error("Exception in getAccessManager()::" + e.getMessage());
			e.printStackTrace();
		}
		return accessManagerUIMSV2;
	}

	/* (non-Javadoc)
	 * @see com.schneider.uims.service.DirectUIMSUserManagerSoapService#getUIMSUser(java.lang.String, java.lang.String)
	 */
	@Async
	public void getUIMSUser(String callerFid, String vnew) throws MalformedURLException {
		LOGGER.info("Entered getUIMSUser() -> Start");
		LOGGER.info("Parameter callerFid -> " + callerFid + " ,vnew -> " + vnew);
		UIMSLOGGER.info("Entered getUIMSUser() -> Start");
		UIMSLOGGER.info("Parameter callerFid -> " + callerFid + " ,vnew -> " + vnew);

		String samlAssertionOrToken = null;
		try {
			LOGGER.info("Going to call getSamlAssertionToken() of UIMS for callerFid:" + callerFid);
			samlAssertionOrToken = samlTokenService.getSamlAssertionToken(callerFid, vnew);
			LOGGER.info("getSamlAssertionToken() of UIMS finished.. samlAssertionOrToken:" + samlAssertionOrToken);
		} catch (Exception e1) {
			LOGGER.error("Exception while getting samlAssertionOrToken in getUIMSUser()::" + e1.getMessage());
			UIMSLOGGER.error("Exception while getting samlAssertionOrToken in getUIMSUser()::" + e1.getMessage());
			e1.printStackTrace();
		}
		try {
			UserManagerUIMSV22 userManagerUIMSV22 = getUserManager();
			LOGGER.info("Going to call getUser() of UIMS for callerFid:" + callerFid);
			userManagerUIMSV22.getUser(callerFid, samlAssertionOrToken);
			LOGGER.info("getUser() of UIMS finished for callerFid:" + callerFid);
		} catch (IMSServiceSecurityCallNotAllowedException_Exception
				| InvalidImsServiceMethodArgumentException_Exception | LdapTemplateNotReadyException_Exception
				| RequestedEntryNotExistsException_Exception | SecuredImsException_Exception
				| UnexpectedLdapResponseException_Exception | UnexpectedRuntimeImsException_Exception e) {
			LOGGER.error("Exception while getUser() of UIMS::" + e.getMessage());
			UIMSLOGGER.error("Exception while getUser() of UIMS::" + e.getMessage());
			e.printStackTrace();
		}
	}

	/* (non-Javadoc)
	 * @see com.schneider.uims.service.DirectUIMSUserManagerSoapService#setUIMSPassword(java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String)
	 */
	@Async
	public void setUIMSPassword(String iPlanetDirectoryKey, String userId, String callerFid, String password,
			String openamVnew, String loginIdentifierType, String emailOrMobile) throws MalformedURLException {
		LOGGER.info("Entered setUIMSPassword() -> Start");
		LOGGER.info("Parameter iPlanetDirectoryKey -> " + iPlanetDirectoryKey + " ,userId -> " + userId);
		LOGGER.info("Parameter callerFid -> " + callerFid);
		LOGGER.info("Parameter openamVnew -> " + openamVnew + " ,loginIdentifierType -> " + loginIdentifierType);
		LOGGER.info("Parameter emailOrMobile -> " + emailOrMobile);
		UIMSLOGGER.info("Entered setUIMSPassword() -> Start");
		UIMSLOGGER.info("Parameter iPlanetDirectoryKey -> " + iPlanetDirectoryKey + " ,userId -> " + userId);
		UIMSLOGGER.info("Parameter callerFid -> " + callerFid);
		UIMSLOGGER.info("Parameter openamVnew -> " + openamVnew + " ,loginIdentifierType -> " + loginIdentifierType);
		UIMSLOGGER.info("Parameter emailOrMobile -> " + emailOrMobile);

		try {
			if (UserConstants.EMAIL.equalsIgnoreCase(loginIdentifierType)) {
				LOGGER.info("Going to call getSamlAssertionToken() of UIMS for EMAIL.. userId:" + userId);
				samlAssertion = samlTokenService.getSamlAssertionToken(userId, openamVnew);
				LOGGER.info("getSamlAssertionToken() of UIMS finished for EMAIL.. userId:" + userId);
			} else {
				LOGGER.info("Going to call getSamlAssertionToken() of UIMS.. callerFid:" + callerFid);
				samlAssertion = samlTokenService.getSamlAssertionToken(callerFid, openamVnew);
				LOGGER.info("getSamlAssertionToken() of UIMS finished.. callerFid:" + callerFid);
			}
		} catch (Exception e) {
			LOGGER.error("Exception while getting getSamlAssertionToken() of UIMS::" + e.getMessage());
			UIMSLOGGER.error("Exception while getting getSamlAssertionToken() of UIMS::" + e.getMessage());
			e.printStackTrace();
		}
		try {
			Callable<Boolean> callableUIMSPassword = new Callable<Boolean>() {
				public Boolean call() throws Exception {
					UserManagerUIMSV22 userManagerUIMSV22 = getUserManager();
					if (UserConstants.EMAIL.equalsIgnoreCase(loginIdentifierType)) {
						LOGGER.info("Going to call setPassword() of UIMS for EMAIL.. userId:" + userId);
						setPasswordStatus = userManagerUIMSV22.setPassword(CALLER_FID, samlAssertion,
								password);
						LOGGER.info("setPassword() of UIMS finished for EMAIL.. userId:" + userId);
					} else {
						LOGGER.info("Going to call setPasswordWithSms() of UIMS for non-EMAIL.. userId:" + userId);
						setPasswordStatus = userManagerUIMSV22.setPasswordWithSms(CALLER_FID,
								emailOrMobile, samlAssertion, UserConstants.TOKEN_TYPE, password);
						LOGGER.info("setPasswordWithSms() of UIMS finished for non-EMAIL.. userId:" + userId);
					}
					LOGGER.info("setPasswordStatus: " + setPasswordStatus);
					UIMSLOGGER.info("setPasswordStatus: " + setPasswordStatus);
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
					LOGGER.info("Going to call updateUser() of openamservice to update version for userId:" + userId);
					productService.updateUser(iPlanetDirectoryKey, userId, version);
					LOGGER.info("updateUser() call of openamservice finished for userId:" + userId);
				}
			} catch (RetryException e) {
				LOGGER.error("Retry failed while calling setUIMSPassword() of UIMS::" + e.getMessage());
				UIMSLOGGER.error("Retry failed while calling setUIMSPassword() of UIMS::" + e.getMessage());
				e.printStackTrace();

			} catch (ExecutionException e) {
				LOGGER.error("ExecutionException while calling setUIMSPassword() of UIMS::" + e.getMessage());
				UIMSLOGGER.error("ExecutionException while calling setUIMSPassword() of UIMS::" + e.getMessage());
				e.printStackTrace();
			}
			if (!setPasswordStatus) {
				LOGGER.info(
						"UIMS UserpinConfirmation setPassword got failed -----> ::sending mail notification for userid::"
								+ userId);
				UIMSLOGGER
						.info("UIMS UserpinConfirmation setPassword got failed -----> ::sending mail notification for userid::"
								+ userId);
				LOGGER.info("Going to call emailReadyToSendEmail() for userId:" + userId);
				sendEmail.emailReadyToSendEmail(supportUser, fromUserName,
						"UIMS UserpinConfirmation setPassword failed.", userId);
				LOGGER.info("emailReadyToSendEmail() finished for userId:" + userId);
			}
		} catch (Exception e) {
			// productService.sessionLogout(iPlanetDirectoryKey, "logout");
			LOGGER.error("Exception while setUIMSPassword::" + e.getMessage());
			UIMSLOGGER.error("Exception while setUIMSPassword::" + e.getMessage());
			e.printStackTrace();
		}
		// productService.sessionLogout(iPlanetDirectoryKey, "logout");
		LOGGER.info("setUIMSPassword() Async Method -> End");
		UIMSLOGGER.info("setUIMSPassword Async Method -> End");
	}

	/* (non-Javadoc)
	 * @see com.schneider.uims.service.DirectUIMSUserManagerSoapService#updateUIMSPassword(java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String)
	 */
	@Async
	public void updateUIMSPassword(String callerFid, String userId, String oldPassword, String newPassword,
			String openamVnew, String iPlanetDirectoryKey) throws MalformedURLException {
		LOGGER.info("Entered updateUIMSPassword() -> Start");
		LOGGER.info("Parameter iPlanetDirectoryKey -> " + iPlanetDirectoryKey + " ,userId -> " + userId);
		LOGGER.info("Parameter callerFid -> " + callerFid);
		LOGGER.info("Parameter openamVnew -> " + openamVnew);
		UIMSLOGGER.info("Entered updateUIMSPassword() -> Start");
		UIMSLOGGER.info("Parameter iPlanetDirectoryKey -> " + iPlanetDirectoryKey + " ,userId -> " + userId);
		UIMSLOGGER.info("Parameter callerFid -> " + callerFid);
		UIMSLOGGER.info("Parameter openamVnew -> " + openamVnew);
		try {
			LOGGER.info("Going to call getSamlAssertionToken() of UIMS for callerFid:" + callerFid);
			samlAssertion = samlTokenService.getSamlAssertionToken(callerFid, openamVnew);
			LOGGER.info("getSamlAssertionToken() of UIMS finished for callerFid:" + callerFid);
		} catch (Exception e1) {
			LOGGER.error("Exception while getting samlAssertion ::" + e1.getMessage());
			UIMSLOGGER.error("Exception while getting samlAssertion::" + e1.getMessage());
			e1.printStackTrace();
		}
		try {
			Callable<Boolean> callableUpdateUIMSPassword = new Callable<Boolean>() {
				public Boolean call() throws Exception {
					UserManagerUIMSV22 userManagerUIMSV22 = getUserManager();
					LOGGER.info("Going to call updatePassword() of UIMS for callerFid:" + callerFid);
					ispasswordupdated = userManagerUIMSV22.updatePassword(CALLER_FID, samlAssertion,
							oldPassword, newPassword);
					LOGGER.info("updatePassword() of UIMS finished for callerFid:" + callerFid);
					LOGGER.info("Update password status is::" + ispasswordupdated);
					UIMSLOGGER.info("Update password status is::" + ispasswordupdated);
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
					LOGGER.info("Going to call updateUser() of openamservice to update version for userId:" + userId);
					productService.updateUser(iPlanetDirectoryKey, userId, version);
					LOGGER.info("updateUser() call of openamservice finished for userId:" + userId);
				}
			} catch (RetryException e) {
				LOGGER.error("Retry failed while calling UIMS update user::" + e.getMessage());
				UIMSLOGGER.error("Retry failed while calling UIMS update user::" + e.getMessage());
				e.printStackTrace();

			} catch (ExecutionException e) {
				LOGGER.error("ExecutionException while calling UIMS update user::" + e.getMessage());
				UIMSLOGGER.error("ExecutionException while calling UIMS update user::" + e.getMessage());
				e.printStackTrace();
			}

		} catch (Exception e) {
			LOGGER.error("Exception while updateUIMSPassword()::" + e.getMessage());
			UIMSLOGGER.error("Exception while updateUIMSPassword()::" + e.getMessage());
			e.printStackTrace();
		}
	}

	/* (non-Javadoc)
	 * @see com.schneider.uims.service.DirectUIMSUserManagerSoapService#updateUIMSUser(java.lang.String, com.se.uims.usermanager.UserV6, java.lang.String)
	 */
	@Async
	public boolean updateUIMSUser(String fedId, UserV6 user, String vnew) throws MalformedURLException {
		LOGGER.info("Entered updateUIMSUser() -> Start");
		LOGGER.info("Parameter fedId -> " + fedId + " ,user -> " + user + " ,vnew -> " + vnew);
		UIMSLOGGER.info("Entered updateUIMSUser() -> Start");
		UIMSLOGGER.info("Parameter fedId -> " + fedId + " ,user -> " + user + " ,vnew -> " + vnew);
		boolean status = false;
		UserManagerUIMSV22 userManagerUIMSV22 = getUserManager();
		String samlAssertion = null;
		try {
			LOGGER.info("Going to call getSamlAssertionToken() of UIMS for fedId:" + fedId);
			samlAssertion = samlTokenService.getSamlAssertionToken(fedId, vnew);
			LOGGER.info("getSamlAssertionToken() of UIMS finished for fedId:" + fedId);
		} catch (Exception e) {
			LOGGER.error("Error executing while getting samlAssertion::" + e.getMessage());
			UIMSLOGGER.error("Error executing while getting samlAssertion::" + e.getMessage());
			e.printStackTrace();
		}
		try {
			LOGGER.info("Going to call updateUser() of UIMS for user:" + user.getFirstName());
			status = userManagerUIMSV22.updateUser(CALLER_FID, samlAssertion, user);
			LOGGER.info("updateUser() of UIMS finished for user:" + user.getFirstName());
		} catch (IMSServiceSecurityCallNotAllowedException_Exception | InactiveUserImsException_Exception
				| InvalidImsServiceMethodArgumentException_Exception | LdapTemplateNotReadyException_Exception
				| RequestedEntryNotExistsException_Exception | SecuredImsException_Exception
				| UnexpectedLdapResponseException_Exception | UnexpectedRuntimeImsException_Exception e) {
			LOGGER.error("Error executing while getting status in updateUIMSUser()::" + e.getMessage());
			UIMSLOGGER.error("Error executing while getting status in updateUIMSUser::" + e.getMessage());
			e.printStackTrace();
		}
		LOGGER.info("Status got from updateUIMSUser() in UIMS is:" + status);
		return status;
	}

	/* (non-Javadoc)
	 * @see com.schneider.uims.service.DirectUIMSUserManagerSoapService#activateUIMSIdentity(java.lang.String, java.lang.String, java.lang.String)
	 */
	@Async
	public void activateUIMSIdentity(String callerFid, String password, String vnew) throws MalformedURLException {
		LOGGER.info("Entered activateUIMSIdentity() -> Start");
		LOGGER.info("Parameter callerFid -> " + callerFid + " ,vnew -> " + vnew);
		UIMSLOGGER.info("Entered activateUIMSIdentity() -> Start");
		UIMSLOGGER.info("Parameter callerFid -> " + callerFid + " ,vnew -> " + vnew);
		UserManagerUIMSV22 userManagerUIMSV22 = getUserManager();

		String authentificationToken = null;
		try {
			LOGGER.info("Going to call getSamlAssertionToken() of UIMS for callerFid:" + callerFid);
			authentificationToken = samlTokenService.getSamlAssertionToken(callerFid, vnew);
			LOGGER.info("getSamlAssertionToken() of UIMS finished for callerFid:" + callerFid);
		} catch (Exception e) {
			UIMSLOGGER.error("Error while getting authentificationToken in UIMS::" + e.getMessage());
			LOGGER.error("Error while getting authentificationToken in UIMS::" + e.getMessage());
			e.printStackTrace();
		}
		try {
			LOGGER.info("Going to call activateIdentity() of UIMS for callerFid:" + callerFid);
			userManagerUIMSV22.activateIdentity(CALLER_FID, password, authentificationToken);
			LOGGER.info("activateIdentity() of UIMS finished for callerFid:" + callerFid);
		} catch (IMSServiceSecurityCallNotAllowedException_Exception
				| InvalidImsServiceMethodArgumentException_Exception | LdapTemplateNotReadyException_Exception
				| RequestedEntryNotExistsException_Exception | RequestedInternalUserException_Exception
				| SecuredImsException_Exception | UnexpectedLdapResponseException_Exception
				| UnexpectedRuntimeImsException_Exception e) {
			UIMSLOGGER.error("Error executing while activateUIMSIdentity()::" + e.getMessage());
			LOGGER.error("Error executing while activateUIMSIdentity()::" + e.getMessage());
			e.printStackTrace();
		}
	}

	/* (non-Javadoc)
	 * @see com.schneider.uims.service.DirectUIMSUserManagerSoapService#activateIdentityNoPassword(java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String, java.lang.String)
	 */
	@Async
	public void activateIdentityNoPassword(String userId, String callerFid, String openamVnew,
			String iPlanetDirectoryKey, String loginIdentifierType, String emailOrMobile) throws MalformedURLException {
		LOGGER.info("Entered activateIdentityNoPassword() -> Start");
		LOGGER.info("Parameter userId -> " + userId + " ,callerFid -> " + callerFid);
		LOGGER.info("Parameter openamVnew -> " + openamVnew + " ,iPlanetDirectoryKey -> " + iPlanetDirectoryKey);
		LOGGER.info("Parameter loginIdentifierType -> " + loginIdentifierType + " ,emailOrMobile -> " + emailOrMobile);
		UIMSLOGGER.info("Entered activateIdentityNoPassword() -> Start");
		UIMSLOGGER.info("Parameter userId -> " + userId + " ,callerFid -> " + callerFid);
		UIMSLOGGER.info("Parameter openamVnew -> " + openamVnew + " ,iPlanetDirectoryKey -> " + iPlanetDirectoryKey);
		UIMSLOGGER.info(
				"Parameter loginIdentifierType -> " + loginIdentifierType + " ,emailOrMobile -> " + emailOrMobile);

		try {
			// samlAssertion =
			// SamlAssertionTokenGenerator.getSamlAssertionToken(userId,openamVnew);

			if (UserConstants.EMAIL.equalsIgnoreCase(loginIdentifierType)) {
				LOGGER.info("Going to call getSamlAssertionToken() of UIMS for EMAIL.. userId:" + userId);
				samlAssertion = samlTokenService.getSamlAssertionToken(userId, openamVnew);
				LOGGER.info("getSamlAssertionToken() of UIMS finished for EMAIL.. userId:" + userId);
			} else {
				LOGGER.info("Going to call getSamlAssertionToken() of UIMS for non-EMAIL.. callerFid:" + callerFid);
				samlAssertion = samlTokenService.getSamlAssertionToken(callerFid, openamVnew);
				LOGGER.info("getSamlAssertionToken() of UIMS finished for non-EMAIL.. callerFid:" + callerFid);
			}

		} catch (Exception e) {
			// productService.sessionLogout(iPlanetDirectoryKey, "logout");
			LOGGER.error("Error executing while getting samlAssertion::" + e.getMessage());
			UIMSLOGGER.error("Error executing while getting samlAssertion::" + e.getMessage());
			e.printStackTrace();
		}
		try {
			Callable<Boolean> callableActivateIdentityNoPassword = new Callable<Boolean>() {
				public Boolean call() throws Exception {
					UserManagerUIMSV22 userManagerUIMSV22 = getUserManager();
					if (UserConstants.EMAIL.equalsIgnoreCase(loginIdentifierType)) {
						LOGGER.info("Going to call activateIdentityNoPassword() of UIMS for EMAIL.. userId:" + userId);
						isNoPwdactivated = userManagerUIMSV22.activateIdentityNoPassword(CALLER_FID,
								samlAssertion);
						LOGGER.info("activateIdentityNoPassword() of UIMS finished for EMAIL.. userId:" + userId);

					} else {
						LOGGER.info("Going to call activateIdentityWithMobileNoPassword() of UIMS.. emailOrMobile:"
								+ emailOrMobile);
						isNoPwdactivated = userManagerUIMSV22.activateIdentityWithMobileNoPassword(
								CALLER_FID, emailOrMobile, samlAssertion);
						LOGGER.info("activateIdentityWithMobileNoPassword() of UIMS finished.. emailOrMobile:"
								+ emailOrMobile);
					}
					LOGGER.info("UIMS user activateIdentityNoPassword isactivated status:" + isNoPwdactivated);
					UIMSLOGGER.info("UIMS user activateIdentityNoPassword isactivated status:" + isNoPwdactivated);
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
					LOGGER.info("Going to call updateUser() of openamservice to update version for userID:" + userId);
					productService.updateUser(iPlanetDirectoryKey, userId, version);
					LOGGER.info("updateUser() of openamservice finsihed to update version for userID:" + userId);
				}
			} catch (RetryException e) {
				// productService.sessionLogout(iPlanetDirectoryKey, "logout");
				UIMSLOGGER.error("RetryException while calling activateIdentityNoPassword::" + e.getMessage());
				LOGGER.error("RetryException while calling activateIdentityNoPassword::" + e.getMessage());
				e.printStackTrace();
			} catch (ExecutionException e) {
				// productService.sessionLogout(iPlanetDirectoryKey, "logout");
				UIMSLOGGER.error("ExecutionException while calling activateIdentityNoPassword::" + e.getMessage());
				LOGGER.error("ExecutionException while calling activateIdentityNoPassword::" + e.getMessage());
				e.printStackTrace();
			}
			if (!isNoPwdactivated) {
				UIMSLOGGER
						.error("UIMS UserpinConfirmation activateIdentityNoPassword got failed -----> ::sending mail notification for userId::"
								+ userId);
				LOGGER.error(
						"UIMS UserpinConfirmation activateIdentityNoPassword got failed -----> ::sending mail notification for userId::"
								+ userId);
				sendEmail.emailReadyToSendEmail(supportUser, fromUserName,
						"UIMS UserpinConfirmation activateIdentityNoPassword failed.", userId);
			}
		} catch (Exception e) {
			// productService.sessionLogout(iPlanetDirectoryKey, "logout");
			UIMSLOGGER.error("Exception while activateIdentityNoPassword::" + e.getMessage());
			LOGGER.error("Exception while activateIdentityNoPassword::" + e.getMessage());
			e.printStackTrace();
		}
		// productService.sessionLogout(iPlanetDirectoryKey, "logout");
		UIMSLOGGER.info("Completed UIMS activateIdentityNoPassword UIMS Async method!");
		LOGGER.info("Completed UIMS activateIdentityNoPassword UIMS Async method!");

	}

	/* (non-Javadoc)
	 * @see com.schneider.uims.service.DirectUIMSUserManagerSoapService#createUIMSUserAndCompany(java.lang.String, com.uims.authenticatedUsermanager.UserV6, java.lang.String, com.schneider.ims.service.uimsv2.CompanyV3, java.lang.String, java.lang.String, java.lang.String, java.lang.String, com.schneider.idms.model.IdmsUserRequest, int)
	 */
	@Async
	public String createUIMSUserAndCompany(String callerFid, com.uims.authenticatedUsermanager.UserV6 identity,
			String context, CompanyV3 company, String userName, String iPlanetDirectoryKey, String v_new,
			String forcedFederatedId, IdmsUserRequest userRequest,int companyCreatedCount) {
		LOGGER.info("Entered createUIMSUserAndCompany() -> Start");
		LOGGER.info("Parameter callerFid -> " + callerFid + " ,identity -> " + identity);
		LOGGER.info("Parameter context -> " + context + " ,company -> " + company);
		LOGGER.info("Parameter userName -> " + userName + " ,iPlanetDirectoryKey -> " + iPlanetDirectoryKey);
		LOGGER.info("Parameter v_new -> " + v_new + " ,forcedFederatedId -> " + forcedFederatedId);
		LOGGER.info("Parameter userRequest -> " + userRequest);
		UIMSLOGGER.info("Entered createUIMSUserAndCompany() -> Start");
		UIMSLOGGER.info("Parameter callerFid -> " + callerFid + " ,identity -> " + identity);
		UIMSLOGGER.info("Parameter context -> " + context + " ,company -> " + company);
		UIMSLOGGER.info("Parameter userName -> " + userName + " ,iPlanetDirectoryKey -> " + iPlanetDirectoryKey);
		UIMSLOGGER.info("Parameter v_new -> " + v_new + " ,forcedFederatedId -> " + forcedFederatedId);
		UIMSLOGGER.info("Parameter userRequest -> " + userRequest);

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
			UIMSLOGGER.error("Error while converting the userRequest to Json" + e.getMessage());
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
					
					if(null != userRequest.getCompanyFederatedId() && !userRequest.getCompanyFederatedId().isEmpty()){

						if(companyCreatedCount > 1){
							identity.setCompanyId(userRequest.getCompanyFederatedId());
						}
					}
					

					createdFedId = authenticatedUserManagerSoapService.createUIMSUser(CALLER_FID,
							identity, forcedFederatedId);

					if (null != createdFedId) {
						String fedID = "{" + "\"federationID\": \"" + createdFedId + "\"" + "}";
						LOGGER.info("fedID in creating UIMS user: " + fedID);
						UIMSLOGGER.info("fedID in creating UIMS user: " + fedID);
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
					UIMSLOGGER.info("UIMS user created successfully::" + userCreated);
					LOGGER.info("UIMS user created successfully::" + userCreated);
				}
			} catch (RetryException e) {
				UIMSLOGGER.error("Retry failed while calling the UIMS create user::" + e.getMessage());
				LOGGER.error("Retry failed while calling the UIMS create user::" + e.getMessage());
				e.printStackTrace();

			} catch (ExecutionException e) {
				UIMSLOGGER.error("ExecutionException while calling the UIMS create user::" + e.getMessage());
				LOGGER.error("ExecutionException while calling the UIMS create user::" + e.getMessage());
				e.printStackTrace();
			}

			if ((!userCreated || null == createdFedId)
					&& (null != context && UserConstants.USER_CONTEXT_HOME.equalsIgnoreCase(context))) {
				UIMSLOGGER.error("CreateUser got failed -----> ::sending mail notification for userRequestjsonString::"
						+ userRequestjsonString);
				LOGGER.error("CreateUser got failed -----> ::sending mail notification for userRequestjsonString::"
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
					/*if (null != userRequest.getAdminCompanyFederatedId()
							&& !userRequest.getAdminCompanyFederatedId().isEmpty()) {
						createdCompanyFedId = userRequest.getAdminCompanyFederatedId();
					} else if (null != userRequest.getAccountId() && !userRequest.getAccountId().isEmpty()) {
						createdCompanyFedId = userRequest.getAccountId();
					} else if ((null != userRequest.getAdminCompanyFederatedId()
							&& !userRequest.getAdminCompanyFederatedId().isEmpty())
							&& (null != userRequest.getAdminCompanyFederatedId()
									&& !userRequest.getAdminCompanyFederatedId().isEmpty())) {
						createdCompanyFedId = userRequest.getAdminCompanyFederatedId();
					} else if (null != userRequest.getRegistrationSource()
							&& UserConstants.PRM.equals(userRequest.getRegistrationSource())) {
						// if registration source is PRM then accept and force
						// the company FederatedId from IFW/IDMS global
						createdCompanyFedId = userRequest.getCompanyFederatedId();
					} else {

						createdCompanyFedId = companyManagerSoapService.createUIMSCompany(createdFedId,
								UimsConstants.VNEW, company);
					}*/

					if (null != userRequest.getCompanyFederatedId()
							&& !userRequest.getCompanyFederatedId().isEmpty()) {
						if (companyCreatedCount == 1) {
							createdCompanyFedId = companyManagerSoapService.createUIMSCompanyWithCompanyForceIdmsId(createdFedId,
									userRequest.getCompanyFederatedId(), UimsConstants.VNEW, company);
						} 
					} 
					
					
					if (null != createdCompanyFedId) {
						String companyFedID = "{" + "\"companyFederatedID\": \"" + createdCompanyFedId + "\"" + "}";
						LOGGER.info("companyFedID in creating UIMS Company: " + companyFedID);
						UIMSLOGGER.info("companyFedID in creating UIMS Company: " + companyFedID);
						productService.updateUser(iPlanetDirectoryKey, userName, companyFedID);

						if ((null == userRequest.getAccountId() || userRequest.getAccountId().isEmpty())
								&& (null != goDigitalValue
										&& goDigitalValue.equalsIgnoreCase(userRequest.getRegistrationSource()))) {
							userRequest.setFederationId(createdFedId);
							userRequest.setCompanyFederatedId(createdCompanyFedId);

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
				UIMSLOGGER.error("Retry failed while calling the UIMS create company::" + e.getMessage());
				LOGGER.error("Retry failed while calling the UIMS create company::" + e.getMessage());
				e.printStackTrace();
			} catch (ExecutionException e) {
				// productService.sessionLogout(iPlanetDirectoryKey, "logout");
				UIMSLOGGER.error("ExecutionException while calling the UIMS create company::" + e.getMessage());
				LOGGER.error("ExecutionException while calling the UIMS create company::" + e.getMessage());
				e.printStackTrace();
			}
			if ((!(userCreated && companyCreated) || (null == createdCompanyFedId && null == createdFedId))
					&& (null != context && UserConstants.USER_CONTEXT_WORK.equalsIgnoreCase(context))) {
				UIMSLOGGER.error("UIMS CreateUser and CreateCompany got failed -----> ::sending mail notification::"
						+ userRequestjsonString);
				LOGGER.error("UIMS CreateUser and CreateCompany got failed -----> ::sending mail notification::"
						+ userRequestjsonString);
				sendEmail.emailReadyToSendEmail(supportUser, fromUserName, "UIMS CreateUser and CreateCompany failed.",
						userRequestjsonString);
			}
		} catch (Exception e) {
			// productService.sessionLogout(iPlanetDirectoryKey, "logout");
			UIMSLOGGER.error("Exception in UIMSUserManagerSoapService.createUIMSUserAndCompany::" + e.getMessage());
			LOGGER.error("Exception in UIMSUserManagerSoapService.createUIMSUserAndCompany::" + e.getMessage());
			e.printStackTrace();
		}
		createdFedId = null;
		// productService.sessionLogout(iPlanetDirectoryKey, "logout");
		LOGGER.info("Completed the createUIMSUserAndCompany Async method!!");
		UIMSLOGGER.info("Completed the createUIMSUserAndCompany Async method!!");
		return null;

	}

	/* (non-Javadoc)
	 * @see com.schneider.uims.service.DirectUIMSUserManagerSoapService#updateUIMSUserAndCompany(java.lang.String, com.se.uims.usermanager.UserV6, java.lang.String, com.schneider.ims.service.uimsv2.CompanyV3, java.lang.String, com.idms.product.client.OpenAMService, java.lang.String, java.lang.String, java.lang.String, java.lang.String)
	 */
	@Async
	public String updateUIMSUserAndCompany(String fedId, UserV6 identity, String context, CompanyV3 company,
			String vnew, OpenAMService productService, String iPlanetDirectoryKey, String userName, String companyFedId, String email) {
		LOGGER.info("Entered updateUIMSUserAndCompany() -> Start");
		LOGGER.info("Parameter fedId -> " + fedId + " ,identity -> " + identity);
		LOGGER.info("Parameter context -> " + context + " ,company -> " + company);
		LOGGER.info("Parameter userName -> " + userName + " ,email -> " + email);
		UIMSLOGGER.info("Entered updateUIMSUserAndCompany() -> Start");
		UIMSLOGGER.info("Parameter fedId -> " + fedId + " ,identity -> " + identity);
		UIMSLOGGER.info("Parameter context -> " + context + " ,company -> " + company);
		UIMSLOGGER.info("Parameter userName -> " + userName + " ,email -> " + email);

		AccessElement application = new AccessElement();
		application.setId(applicationName);
		application.setType(Type.APPLICATION);

		try {
			//CODE-RE-STRUCTURING: Replaced explicit creation with Spring injected instance
			//UIMSCompanyManagerSoapService companyManagerSoapService = new UIMSCompanyManagerSoapService();
			Callable<Boolean> callableUpdateUIMSUserAndComapany = new Callable<Boolean>() {
				public Boolean call() throws Exception {

					// TODO check in global if we pass a single attribute, what
					// should be the behaviour for other fields User object.

					// Answer from Subrat: Remaining fields should not be
					// updated.
					LOGGER.info("Going to call updateUIMSUser() of UIMS for fedId:" + fedId);
					updateUIMSUser = updateUIMSUser(fedId, identity, vnew);
					LOGGER.info(
							"updateUIMSUser() of UIMS finished for fedId:" + fedId + " with status:" + updateUIMSUser);

					return true;
				}
			};

			Retryer<Boolean> retryer = RetryerBuilder.<Boolean> newBuilder()
					.retryIfResult(Predicates.<Boolean> isNull()).retryIfExceptionOfType(Exception.class)
					.retryIfRuntimeException().withStopStrategy(StopStrategies.stopAfterAttempt(3)).build();
			try {
				retryer.call(callableUpdateUIMSUserAndComapany);

				if (updateUIMSUser) {
					LOGGER.info("UIMS User updated Successfully::" + updateUIMSUser);
					UIMSLOGGER.info("UIMS User updated Successfully::" + updateUIMSUser);
				}
			} catch (RetryException e) {
				UIMSLOGGER.error("Retry failed while calling updateUIMSUser() of UIMS for fedId :" + fedId + "->"
						+ e.getMessage());
				LOGGER.error("Retry failed while calling updateUIMSUser() of UIMS for fedId :" + fedId + "->"
						+ e.getMessage());
				e.printStackTrace();
			} catch (ExecutionException e) {
				UIMSLOGGER.error("ExecutionException while calling updateUIMSUser() of UIMS for fedId :" + fedId + "->"
						+ e.getMessage());
				LOGGER.error("ExecutionException while calling updateUIMSUser() of UIMS for fedId :" + fedId + "->"
						+ e.getMessage());
				e.printStackTrace();
			}

			if (!updateUIMSUser && (null != context && UserConstants.USER_CONTEXT_HOME.equalsIgnoreCase(context))) {
				UIMSLOGGER.error("updateUIMSUser() of UIMS got failed -----> ::sending mail notification for userName::"
						+ userName);
				LOGGER.error("updateUIMSUser() of UIMS got failed -----> ::sending mail notification for userName::"
						+ userName);

				sendEmail.emailReadyToSendEmail(supportUser, fromUserName, "UIMS Update user failed.", userName);
			}
			Callable<Boolean> callableUpdateCompany = new Callable<Boolean>() {
				public Boolean call() throws Exception {

					// TODO logic to get the federatedId
					String federatedId = "";

					updateUIMSCompany = companyManagerSoapService.updateUIMSCompany(fedId, vnew, company, companyFedId);
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
				UIMSLOGGER.error("Retry failed while calling the UIMS create company::" + e.getMessage());
				LOGGER.error("Retry failed while calling the UIMS create company::" + e.getMessage());
				e.printStackTrace();

			} catch (ExecutionException e) {
				UIMSLOGGER.error("ExecutionException while calling the UIMS create company::" + e.getMessage());
				LOGGER.error("ExecutionException while calling the UIMS create company::" + e.getMessage());
				e.printStackTrace();
			}
			if (!(updateUIMSUser && updateUIMSCompany)
					&& (null != context && UserConstants.USER_CONTEXT_WORK.equalsIgnoreCase(context))) {
				UIMSLOGGER
						.error("UIMS User and Company updated got failed -----> ::sending mail notification for userName::"
								+ userName);
				LOGGER.error(
						"UIMS User and Company updated got failed -----> ::sending mail notification for userName::"
								+ userName);
				sendEmail.emailReadyToSendEmail(supportUser, fromUserName, "UIMS Update user and company failed.",
						userName);
			}

		} catch (Exception e) {
			// productService.sessionLogout(iPlanetDirectoryKey, "logout");
			// TODO Auto-generated catch block
			UIMSLOGGER.error("Exception for userName::" + userName);
			LOGGER.error("Exception for userName::" + userName);
			// LOGGER.error("UIMS User and Company updated got failed ----->
			// ::sending mail notification for userName::"+userName);
			e.printStackTrace();
		}

		// productService.sessionLogout(iPlanetDirectoryKey, "logout");
		UIMSLOGGER.info("UIMS updateUIMSUserAndCompany Async method completed!!");
		LOGGER.info("UIMS updateUIMSUserAndCompany Async method completed!!");
		return null;

	}

	/* (non-Javadoc)
	 * @see com.schneider.uims.service.DirectUIMSUserManagerSoapService#activateUIMSUserConfirmPIN(com.schneider.idms.model.IdmsUserConfirmRequest, java.lang.String, java.lang.String, java.lang.String, java.lang.String)
	 */
	@Async
	public void activateUIMSUserConfirmPIN(IdmsUserConfirmRequest confirmRequest, String openamVnew,
			String iPlanetDirectoryKey, String loginIdentifierType, String emailOrMobile) {
		LOGGER.info("Entered activateUIMSUserConfirmPIN() -> Start");
		LOGGER.info("Parameter confirmRequest -> " + confirmRequest);
		LOGGER.info("Parameter loginIdentifierType -> " + loginIdentifierType + " ,emailOrMobile -> " + emailOrMobile);
		UIMSLOGGER.info("Entered activateUIMSUserConfirmPIN() -> Start");
		UIMSLOGGER.info("Parameter confirmRequest -> " + confirmRequest);
		UIMSLOGGER.info(
				"Parameter loginIdentifierType -> " + loginIdentifierType + " ,emailOrMobile -> " + emailOrMobile);

		try {

			activateIdentityNoPassword(confirmRequest.getFederatedId(), confirmRequest.getFederatedId(), openamVnew,
					iPlanetDirectoryKey, loginIdentifierType, emailOrMobile);
		} catch (Exception e) {
			UIMSLOGGER
					.error("Exception while calling UIMS UserManager API of setUIMSPassword/activateIdentityNoPassword:: -> "
							+ e.getMessage());
			LOGGER.error(
					"Exception while calling UIMS UserManager API of setUIMSPassword/activateIdentityNoPassword:: -> "
							+ e.getMessage());
			e.printStackTrace();
		}
	}
	
	
	/* (non-Javadoc)
	 * @see com.schneider.uims.service.DirectUIMSUserManagerSoapService#updateUIMSUserAIL(com.schneider.idms.model.IdmsUserAilRequest, com.se.idms.dto.IDMSUserAIL, java.lang.String, com.idms.product.client.OpenAMService, java.lang.String, java.lang.String)
	 */
	@Async
	public void updateUIMSUserAIL(IdmsUserAilRequest userAilRequest, IDMSUserAIL idmsUserAIL, String vNewCntValue,
			OpenAMService productService, String iPlanetDirectoryKey, String usermail) {
		LOGGER.info("inside updateUIMSUserAIL Async Method");
		try {
			//LOGGER.info("In UserServiceImpl.updateAIL():-->Caling Async UIMS method of grant/revoke access manager control method");
			com.uims.accessmanager.AccessElement access = new com.uims.accessmanager.AccessElement();
			if ("APPLICATION".equalsIgnoreCase(userAilRequest.getAclType())) {
				access.setType(com.uims.accessmanager.Type.APPLICATION);
				access.setId(userAilRequest.getAcl());
			} else if ("FEATURE".equalsIgnoreCase(userAilRequest.getAclType())) {
				access.setType(com.uims.accessmanager.Type.FEATURE);
				access.setId(userAilRequest.getAcl());
			} else if ("PROGRAM".equalsIgnoreCase(userAilRequest.getAclType())) {
				access.setType(com.uims.accessmanager.Type.PROGRAM);
				access.setId(userAilRequest.getAcl());
			} else {
				access.setType(com.uims.accessmanager.Type.PROGRAM_LEVEL);
				access.setId(userAilRequest.getAcl());
			}
			
			if (userAilRequest.getOperation().equalsIgnoreCase("GRANT") && !(idmsUserAIL.isIdmsisRevokedOperation__c())) {
				grantAccessControlToUser(CALLER_FID,
						userAilRequest.getFederatedId(),
						userAilRequest.getFederatedId(), access, vNewCntValue, productService,
						iPlanetDirectoryKey, usermail);
			} else if (userAilRequest.getOperation().equalsIgnoreCase("REVOKE") && idmsUserAIL.isIdmsisRevokedOperation__c()) {
				revokeAccessControlToUser(CALLER_FID,
						userAilRequest.getFederatedId(),
						userAilRequest.getFederatedId(), access, vNewCntValue, productService,
						iPlanetDirectoryKey, usermail);
			}
		} catch (Exception e) {
			//productService.sessionLogout(iPlanetDirectoryKey, "logout");
			LOGGER.error("Exception in updateUIMSUserAIL():"+ e.getMessage());
			e.printStackTrace();
		}
		//productService.sessionLogout(iPlanetDirectoryKey, "logout");
		LOGGER.info("UIMS updateAIL Async Method completed!");
	}
	
	/* (non-Javadoc)
	 * @see com.schneider.uims.service.DirectUIMSUserManagerSoapService#grantAccessControlToUser(java.lang.String, java.lang.String, java.lang.String, com.uims.accessmanager.AccessElement, java.lang.String, com.idms.product.client.OpenAMService, java.lang.String, java.lang.String)
	 */
	@Async
	public void grantAccessControlToUser(String callerFid, String federatedId, String userId, com.uims.accessmanager.AccessElement access,
			String openamVnew, OpenAMService productService, String iPlanetDirectoryKey, String email) throws MalformedURLException {
		LOGGER.info("inside grantAccessControToUser Async method");
		try {
			Callable<Boolean> callableGrantAccess = new Callable<Boolean>() {
				public Boolean call() throws Exception {
					UserAccessManagerUIMSV2 accessManagerUIMSV2 = getAccessManager();
					isgrantresult = accessManagerUIMSV2.grantAccessControlToUser(callerFid, federatedId, access);
					LOGGER.info("grantAccessControlToUser result:" + isgrantresult);
					return true;
				}
			};
			Retryer<Boolean> retryer = RetryerBuilder.<Boolean> newBuilder()
					.retryIfResult(Predicates.<Boolean> isNull()).retryIfExceptionOfType(Exception.class)
					.retryIfRuntimeException().withStopStrategy(StopStrategies.stopAfterAttempt(3)).build();
			try {
				retryer.call(callableGrantAccess);
				// after successful grantAccessControlToUser, we need to update
				// the v_old
				if (isgrantresult) {
					String version = "{" + "\"V_Old\": \"" + openamVnew + "\"" + "}";
					productService.updateUser(iPlanetDirectoryKey, userId, version);
				}
			} catch (RetryException e) {
				e.printStackTrace();
				LOGGER.error("Retry failed while calling the grantAccessControlToUser::" + e.getMessage());
			} catch (ExecutionException e) {
				e.printStackTrace();
			}
			/*if(!isgrantresult) {
				LOGGER.info("UIMS UpdateAIL Grant Access got failed -----> ::sending mail notification::");
				sendEmail.emailReadyToSendEmail(supportUser, fromUserName,
						"UIMS UpdateAIL Grant Access failed.", userId);
			}*/
		} catch (Exception e) {
			LOGGER.error("Remote Soap Exception while consuming grantAccessControlToUser :-->" + e.getMessage());
			e.printStackTrace();
		}
		LOGGER.info("Completed grantAccessControToUser Async method!");
	}

	/* (non-Javadoc)
	 * @see com.schneider.uims.service.DirectUIMSUserManagerSoapService#revokeAccessControlToUser(java.lang.String, java.lang.String, java.lang.String, com.uims.accessmanager.AccessElement, java.lang.String, com.idms.product.client.OpenAMService, java.lang.String, java.lang.String)
	 */
	@Async
	public void revokeAccessControlToUser(String callerFid, String federatedId, String userId, com.uims.accessmanager.AccessElement access,
			String openamVnew, OpenAMService productService, String iPlanetDirectoryKey, String email) throws MalformedURLException {
		LOGGER.info("inside revokeAccessControlToUser Async method");
		try {
			Callable<Boolean> callableRevokeAccess = new Callable<Boolean>() {
				public Boolean call() throws Exception {
					UserAccessManagerUIMSV2 accessManagerUIMSV2 = getAccessManager();
					isrevokeresult = accessManagerUIMSV2.revokeAccessControlToUser(callerFid, federatedId, access);
					LOGGER.info("revokeAccessControlToUser::" + isrevokeresult);
					return true;
				}
			};
			Retryer<Boolean> retryer = RetryerBuilder.<Boolean> newBuilder()
					.retryIfResult(Predicates.<Boolean> isNull()).retryIfExceptionOfType(Exception.class)
					.retryIfRuntimeException().withStopStrategy(StopStrategies.stopAfterAttempt(3)).build();
			try {
				retryer.call(callableRevokeAccess);
				// after successful revokeAccessControlToUser, we need to update
				// the v_old
				if (isrevokeresult) {
					String version = "{" + "\"V_Old\": \"" + openamVnew + "\"" + "}";
					productService.updateUser(iPlanetDirectoryKey, userId, version);
				}
			} catch (RetryException e) {
				e.printStackTrace();
				LOGGER.error("Retry failed while calling the revokeAccessControlToUser::" + e.getMessage());
			} catch (ExecutionException e) {
				e.printStackTrace();
			}
			/*if(!isrevokeresult) {
				LOGGER.info("UIMS UpdateAIL revoke access got failed -----> ::sending mail notification::");
				sendEmail.emailReadyToSendEmail(supportUser, fromUserName,
						"UIMS UpdateAIL revoke failed.", userId);
			}*/
		} catch (Exception e) {
			LOGGER.error("Remote Soap Exception while consuming revokeAccessControlToUser:-->" + e.getMessage());
			e.printStackTrace();
		}
		LOGGER.info("inside revokeAccessControlToUser Async method!");
	}

	public static void main(String[] args) {

		DirectUIMSUserManagerSoapServiceIntegration service = new DirectUIMSUserManagerSoapServiceIntegration();
		com.uims.authenticatedUsermanager.UserV6 user = new com.uims.authenticatedUsermanager.UserV6();
		user.setFederatedID("123457");
		String email = UUID.randomUUID().toString() + "@mailinator.com";
		user.setEmail(email);
		user.setFirstName("Arvind");
		user.setLastName("kumar");
		user.setLanguageCode("zh");
		user.setCountryCode("CN");
		CompanyV3 company = new CompanyV3();
		// service.createUIMSUserAndCompany(CALLER_FID, user,
		// "@Home", company, null, null, "12345678", null, "Welcome123@");
	}

	private String buildGoDigitalRequest(IdmsUserRequest userRequest) {
		LOGGER.info("Entered buildGoDigitalRequest() -> Start");
		LOGGER.info("Parameter userRequest -> " + userRequest);
		UIMSLOGGER.info("Entered buildGoDigitalRequest() -> Start");
		LOGGER.info("Parameter userRequest -> " + userRequest);

		ObjectMapper objMapper = new ObjectMapper();
		UserRegistrationInfoRequest userRegistrationInfoRequest = mapper.map(userRequest,
				UserRegistrationInfoRequest.class);

		Authentication authentication = new Authentication();
		authentication.setToken(goDitalToken);
		userRegistrationInfoRequest.getUserRegistrationInfoRequest().setAuthentication(authentication);

		String jsonString = "";
		try {
			jsonString = objMapper.writeValueAsString(userRegistrationInfoRequest);
			jsonString = jsonString.replace("\"\"", "[]");
		} catch (JsonProcessingException e) {
			UIMSLOGGER.error("JsonProcessingException while converting the digitalRequest to Json" + e.getMessage());
			LOGGER.error("JsonProcessingException while converting the digitalRequest to Json" + e.getMessage());
			e.printStackTrace();
		}
		objMapper = null;
		return jsonString;
	}

}

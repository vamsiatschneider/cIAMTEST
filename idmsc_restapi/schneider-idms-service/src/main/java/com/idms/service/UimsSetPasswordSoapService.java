package com.idms.service;

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

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.rholder.retry.RetryException;
import com.github.rholder.retry.Retryer;
import com.github.rholder.retry.RetryerBuilder;
import com.github.rholder.retry.StopStrategies;
import com.google.common.base.Predicates;
import com.idms.model.ConfirmPinRequest;
import com.idms.product.client.OpenAMService;
import com.se.idms.util.SamlAssertionTokenGenerator;
import com.se.idms.util.UimsConstants;
import com.se.idms.util.UserConstants;
import com.se.uims.usermanager.UserManagerUIMSV22;

@org.springframework.stereotype.Service("uimsSetPasswordSoapService")
public class UimsSetPasswordSoapService {
	
	/**
	 * Logger instance.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(UimsSetPasswordSoapService.class);

	private static final Logger UIMSLOGGER = LoggerFactory.getLogger("uimsLogger");
	
	@Autowired
	private UIMSUserManagerSoapService userManagerSoapService;
	
	@Inject
	private OpenAMService productService;
	
	private SendEmail sendEmail;
	
	@Value("${fromUserName}")
	private String fromUserName;
	
	@Value("${supportUser}")
	private String supportUser;
	
	@Value("${userManagerUIMSWsdl}")
	private String userManagerUIMSWsdl;
	
	@Value("${userManagerUIMSWsdlQname}")
	private String userManagerUIMSWsdlQname;
	
	@Value("${userManagerUIMSWsdlPortName}")
	private String userManagerUIMSWsdlPortName;

	private String samlAssertion = null;
	
	private boolean setPasswordStatus = false;
	
	private boolean isNoPwdactivated = false;
	
	private boolean ispasswordupdated = false;
	
	private boolean isIdentityActvated = false;
	
	/**
	 * 
	 * @return
	 * @throws MalformedURLException
	 */
	public UserManagerUIMSV22 getUserManager() {
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
			LOGGER.error("Exception while UimsSetPasswordSoapService :: getUserManager()::" + e.getMessage());
			e.printStackTrace();
		}
		catch (Exception e) {
			LOGGER.error("Exception while UimsSetPasswordSoapService:: getUserManager()::" + e.getMessage());
			e.printStackTrace();
		}
		return userManagerUIMSV2;
	}
	
	
	/**
	 * 
	 * @param iPlanetDirectoryKey
	 * @param userId
	 * @param callerFid
	 * @param password
	 * @param openamVnew
	 * @param loginIdentifierType
	 * @param emailOrMobile
	 * @throws MalformedURLException
	 */
	public void activateIdentity(String iPlanetDirectoryKey, String userId,
			String callerFid, String password, String openamVnew, String loginIdentifierType,String emailOrMobile) throws MalformedURLException {
		LOGGER.info("Entered activateIdentity() -> Start");
		LOGGER.info("Parameter iPlanetDirectoryKey -> " + iPlanetDirectoryKey+" ,userId -> "+userId);
		LOGGER.info("Parameter callerFid -> " + callerFid);
		LOGGER.info("Parameter openamVnew -> " + openamVnew+" ,loginIdentifierType -> "+loginIdentifierType);
		LOGGER.info("Parameter emailOrMobile -> " + emailOrMobile);
		UIMSLOGGER.info("Entered activateIdentity() -> Start");
		UIMSLOGGER.info("Parameter iPlanetDirectoryKey -> " + iPlanetDirectoryKey+" ,userId -> "+userId);
		UIMSLOGGER.info("Parameter callerFid -> " + callerFid);
		UIMSLOGGER.info("Parameter openamVnew -> " + openamVnew+" ,loginIdentifierType -> "+loginIdentifierType);
		UIMSLOGGER.info("Parameter emailOrMobile -> " + emailOrMobile);
		
		try {
			if (UserConstants.EMAIL.equalsIgnoreCase(loginIdentifierType)) {
				samlAssertion = SamlAssertionTokenGenerator.getSamlAssertionToken(userId, openamVnew);
			}else{
				samlAssertion = SamlAssertionTokenGenerator.getSamlAssertionToken(callerFid, openamVnew);
			}
		} catch (Exception e) {
			LOGGER.error("Exception while getting samlAssertion::" + e.getMessage());
			UIMSLOGGER.error("Exception while getting samlAssertion::" + e.getMessage());
			e.printStackTrace();
		}
		try {
			Callable<Boolean> callableUIMSPassword = new Callable<Boolean>() {
				public Boolean call() throws Exception {
					LOGGER.info("Going to call getUserManager() of sync UserManagerUIMSV22");
					UserManagerUIMSV22 userManagerUIMSV22 = userManagerSoapService.getUserManager();
					LOGGER.info("getUserManager() of sync UserManagerUIMSV22 finished");
					if (UserConstants.EMAIL.equalsIgnoreCase(loginIdentifierType)) {
						LOGGER.info("Going to call activateIdentity() of sync UserManagerUIMSV22 of loginIdentifierType:"+loginIdentifierType);
						
						isIdentityActvated = userManagerUIMSV22.activateIdentity(UimsConstants.CALLER_FID,password, samlAssertion);
						
						LOGGER.info("activateIdentity() of sync UserManagerUIMSV22 finished of loginIdentifierType:"+loginIdentifierType);
					} else {
						LOGGER.info("Going to call setPasswordWithSms() of sync UserManagerUIMSV22 of emailOrMobile:"+emailOrMobile);
						
						isIdentityActvated = userManagerUIMSV22.setPasswordWithSms(UimsConstants.CALLER_FID, emailOrMobile, samlAssertion, UserConstants.TOKEN_TYPE, password);
						LOGGER.info("setPasswordWithSms() of sync UserManagerUIMSV22 finished of emailOrMobile:"+emailOrMobile);
					}
					LOGGER.info("isIdentityActvated: " + isIdentityActvated);
					UIMSLOGGER.info("isIdentityActvated: " + isIdentityActvated);
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
				if (isIdentityActvated) {
					String version = "{" + "\"V_Old\": \"" + openamVnew + "\"" + "}";
					LOGGER.info("Going to call updateUser() of OpenAMService for userId:"+userId+" ,version:"+version);
					productService.updateUser(iPlanetDirectoryKey, userId, version);
					LOGGER.info("updateUser() of OpenAMService finished for userId:"+userId+" ,version:"+version);
				}
			} catch (RetryException e) {
				LOGGER.error("Retry failed while calling activateIdentity::" + e.getMessage());
				UIMSLOGGER.error("Retry failed while calling activateIdentity::" + e.getMessage());
				e.printStackTrace();
				
			} catch (ExecutionException e) {
				LOGGER.error("ExecutionException while calling activateIdentity::" + e.getMessage());
				UIMSLOGGER.error("ExecutionException while calling activateIdentity::" + e.getMessage());
				e.printStackTrace();
			}
			if(!isIdentityActvated) {
				LOGGER.info("UIMS UserpinConfirmation activateIdentity got failed -----> ::sending mail notification for userid::"+userId);
				UIMSLOGGER.info("UIMS UserpinConfirmation activateIdentity got failed -----> ::sending mail notification for userid::"+userId);
				sendEmail.emailReadyToSendEmail(supportUser, fromUserName,
						"UIMS UserpinConfirmation activateIdentity failed.", userId);
				LOGGER.info("sending mail notification finished for userid::"+userId);
				UIMSLOGGER.info("sending mail notification finished for userid::"+userId);
			}
		} catch (Exception e) {
			LOGGER.error("Error executing while activateIdentity::" + e.getMessage());
			UIMSLOGGER.error("Error executing while activateIdentity::" + e.getMessage());
			e.printStackTrace();
		}
		LOGGER.info("activateIdentity() finished!");
		UIMSLOGGER.info("activateIdentity() finished!");
	}
	
	/**
	 * 
	 * @param iPlanetDirectoryKey
	 * @param userId
	 * @param callerFid
	 * @param password
	 * @param openamVnew
	 * @param loginIdentifierType
	 * @param emailOrMobile
	 * @throws MalformedURLException
	 */
	public boolean setUIMSPassword(String iPlanetDirectoryKey, String userId,
			String callerFid, String password, String openamVnew, String loginIdentifierType, String emailOrMobile)
					throws MalformedURLException {
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
				samlAssertion = SamlAssertionTokenGenerator.getSamlAssertionToken(userId, openamVnew);
			} else {
				samlAssertion = SamlAssertionTokenGenerator.getSamlAssertionToken(callerFid, openamVnew);
			}

			Callable<Boolean> callableUIMSPassword = new Callable<Boolean>() {
				public Boolean call() throws Exception {
					LOGGER.info("Start: calling getUserManager() of SYNC UserManagerUIMSV22");
					UserManagerUIMSV22 userManagerUIMSV22 = userManagerSoapService.getUserManager();
					LOGGER.info("End: getUserManager() of SYNC UserManagerUIMSV22 finished");
					if (UserConstants.EMAIL.equalsIgnoreCase(loginIdentifierType)) {
						LOGGER.info("Start: calling setPassword() of SYNC UserManagerUIMSV22 for loginIdentifierType:"
								+ loginIdentifierType);
						setPasswordStatus = userManagerUIMSV22.setPassword(UimsConstants.CALLER_FID, samlAssertion,
								password);
						LOGGER.info("End: setPassword() of SYNC UserManagerUIMSV22 finished for loginIdentifierType:"
								+ loginIdentifierType);
					} else {
						LOGGER.info("Start: calling setPasswordWithSms() of SYNC UserManagerUIMSV22 of emailOrMobile:"
								+ emailOrMobile);
						setPasswordStatus = userManagerUIMSV22.setPasswordWithSms(UimsConstants.CALLER_FID,
								emailOrMobile, samlAssertion, UserConstants.TOKEN_TYPE, password);
						LOGGER.info("End: setPasswordWithSms() of SYNC UserManagerUIMSV22 finished of emailOrMobile:"
								+ emailOrMobile);
					}
					LOGGER.info("setPasswordStatus from UIMS: " + setPasswordStatus);
					UIMSLOGGER.info("setPasswordStatus from UIMS: " + setPasswordStatus);
					return true;
				}
			};

			Retryer<Boolean> retryer = RetryerBuilder.<Boolean> newBuilder()
					.retryIfResult(Predicates.<Boolean> isNull()).retryIfExceptionOfType(Exception.class)
					.retryIfRuntimeException().withStopStrategy(StopStrategies.stopAfterAttempt(3)).build();

			retryer.call(callableUIMSPassword);
			// after successful setUIMSPassword , we need to update the
			// v_old
			if (setPasswordStatus) {
				String version = "{" + "\"V_Old\": \"" + openamVnew + "\"" + "}";
				LOGGER.info(
						"Start: calling updateUser() of OpenAMService to update version for userId:" + userId + " ,version:" + version);
				productService.updateUser(iPlanetDirectoryKey, userId, version);
				LOGGER.info("End: updateUser() of OpenAMService finished to update version for userId:" + userId + " ,version:" + version);
			}
			if (!setPasswordStatus) {
				LOGGER.info("UIMS UserpinConfirmation setPassword got failed --> ::sending mail notification for userid::"+ userId);
				UIMSLOGGER.info("UIMS UserpinConfirmation setPassword got failed --> ::sending mail notification for userid::"+ userId);
				
				sendEmail.emailReadyToSendEmail(supportUser, fromUserName,"UIMS UserpinConfirmation setPassword failed.", userId);
				
				LOGGER.info("sending mail notification finished for userid::" + userId);
				UIMSLOGGER.info("sending mail notification finished for userid::" + userId);
			}
		} catch (RetryException e) {
			LOGGER.error("Retry failed while calling UIMS setUIMSPassword::" + e.getMessage());
			UIMSLOGGER.error("Retry failed while calling setUIMSPassword::" + e.getMessage());
			e.printStackTrace();

		} catch (ExecutionException e) {
			LOGGER.error("ExecutionException while calling UIMS setUIMSPassword::" + e.getMessage());
			UIMSLOGGER.error("ExecutionException while calling UIMS setUIMSPassword::" + e.getMessage());
			e.printStackTrace();
		} catch (Exception e) {
			LOGGER.error("Error executing while UIMS setUIMSPassword::" + e.getMessage());
			UIMSLOGGER.error("Error executing while UIMS setUIMSPassword::" + e.getMessage());
			e.printStackTrace();
		}
		LOGGER.info("UIMS setUIMSPassword() finished!");
		UIMSLOGGER.info("UIMS setUIMSPassword() finished!");
		return setPasswordStatus;
	}
	
	/**
	 * 
	 * @param confirmRequest
	 * @param openamVnew
	 * @param iPlanetDirectoryKey
	 * @param loginIdentifierType
	 * @param emailOrMobile
	 */
	public void activateUIMSUserConfirmPIN(ConfirmPinRequest confirmRequest,
			String openamVnew, String iPlanetDirectoryKey,String loginIdentifierType,String emailOrMobile) {
		LOGGER.info("Entered activateUIMSUserConfirmPIN() -> Start");		
		LOGGER.info("Parameter loginIdentifierType -> " + loginIdentifierType+" ,emailOrMobile -> "+emailOrMobile);
		UIMSLOGGER.info("Entered activateUIMSUserConfirmPIN() -> Start");
		UIMSLOGGER.info("Parameter loginIdentifierType -> " + loginIdentifierType+" ,emailOrMobile -> "+emailOrMobile);
		ObjectMapper objMapper = new ObjectMapper();
		
		try {
			LOGGER.info("Parameter confirmRequest -> " + objMapper.writeValueAsString(confirmRequest));
			LOGGER.info("Parameter confirmRequest -> " + objMapper.writeValueAsString(confirmRequest));
			if ((null != confirmRequest.getPassword() && !confirmRequest.getPassword().isEmpty())) {
				activateIdentity(iPlanetDirectoryKey, confirmRequest.getId(),
						confirmRequest.getIDMS_Federated_ID__c(), confirmRequest.getPassword().trim(), openamVnew, loginIdentifierType,emailOrMobile);
			} else if (null == confirmRequest.getPassword() || "".equals(confirmRequest.getPassword())) {
				activateIdentityNoPassword(confirmRequest.getId(), confirmRequest.getIDMS_Federated_ID__c(),
						openamVnew, iPlanetDirectoryKey,loginIdentifierType,emailOrMobile);
			}
		} catch (Exception e) {
			UIMSLOGGER.error(
					"Exception while calling UIMS UserManager API of setUIMSPassword/activateIdentityNoPassword:: -> "
							+ e.getMessage());
			LOGGER.error(
					"Exception while calling UIMS UserManager API of setUIMSPassword/activateIdentityNoPassword:: -> "
							+ e.getMessage());
			e.printStackTrace();
		}
	}
	/**
	 * 
	 * @param userId
	 * @param callerFid
	 * @param openamVnew
	 * @param iPlanetDirectoryKey
	 * @param loginIdentifierType
	 * @param emailOrMobile
	 * @throws MalformedURLException
	 */
	public void activateIdentityNoPassword(String userId, String callerFid,
			String openamVnew, String iPlanetDirectoryKey, String loginIdentifierType,String emailOrMobile) throws MalformedURLException {
		LOGGER.info("Entered activateIdentityNoPassword() -> Start");
		LOGGER.info("Parameter userId -> " + userId+" ,callerFid -> "+callerFid);
		LOGGER.info("Parameter openamVnew -> " + openamVnew+" ,iPlanetDirectoryKey -> "+iPlanetDirectoryKey);
		LOGGER.info("Parameter loginIdentifierType -> " + loginIdentifierType+" ,emailOrMobile -> "+emailOrMobile);
		UIMSLOGGER.info("Entered activateIdentityNoPassword() -> Start");
		UIMSLOGGER.info("Parameter userId -> " + userId+" ,callerFid -> "+callerFid);
		UIMSLOGGER.info("Parameter openamVnew -> " + openamVnew+" ,iPlanetDirectoryKey -> "+iPlanetDirectoryKey);
		UIMSLOGGER.info("Parameter loginIdentifierType -> " + loginIdentifierType+" ,emailOrMobile -> "+emailOrMobile);
		
		try {
			if (UserConstants.EMAIL.equalsIgnoreCase(loginIdentifierType)) {
				samlAssertion = SamlAssertionTokenGenerator.getSamlAssertionToken(userId, openamVnew);
			} else {
				samlAssertion = SamlAssertionTokenGenerator.getSamlAssertionToken(callerFid, openamVnew);
			}

		} catch (Exception e) {
			LOGGER.error("Error executing while getting samlAssertion::" + e.getMessage());
			UIMSLOGGER.error("Error executing while getting samlAssertion::" + e.getMessage());
			e.printStackTrace();
		}
		try {
			Callable<Boolean> callableActivateIdentityNoPassword = new Callable<Boolean>() {
				public Boolean call() throws Exception {
					UserManagerUIMSV22 userManagerUIMSV22 = getUserManager();
					if (UserConstants.EMAIL.equalsIgnoreCase(loginIdentifierType)) {
						LOGGER.info("Going to call activateIdentityNoPassword() of UIMS for EMAIL.. userId:"+userId);
						isNoPwdactivated = userManagerUIMSV22.activateIdentityNoPassword(UimsConstants.CALLER_FID,
							samlAssertion);
						LOGGER.info("activateIdentityNoPassword() of UIMS finished for EMAIL.. userId:"+userId);
					
					}else{
						LOGGER.info("Going to call activateIdentityWithMobileNoPassword() of UIMS.. emailOrMobile:"+emailOrMobile);
						isNoPwdactivated = userManagerUIMSV22.activateIdentityWithMobileNoPassword(UimsConstants.CALLER_FID, emailOrMobile, samlAssertion);
						LOGGER.info("activateIdentityWithMobileNoPassword() of UIMS finished.. emailOrMobile:"+emailOrMobile);
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
					LOGGER.info("Going to call updateUser() of openamservice to update version for userID:"+userId);
					productService.updateUser(iPlanetDirectoryKey, userId, version);
					LOGGER.info("updateUser() of openamservice finsihed to update version for userID:"+userId);
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
			if(!isNoPwdactivated) {
				UIMSLOGGER.error("UIMS UserpinConfirmation activateIdentityNoPassword got failed -----> ::sending mail notification for userId::"+userId);
				LOGGER.error("UIMS UserpinConfirmation activateIdentityNoPassword got failed -----> ::sending mail notification for userId::"+userId);
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
		UIMSLOGGER.info("Completed UIMS activateIdentityNoPassword UIMS Sync method!");
		LOGGER.info("Completed UIMS activateIdentityNoPassword UIMS Sync method!");
	}
	
	/**
	 * 
	 * @param callerFid
	 * @param userId
	 * @param oldPassword
	 * @param newPassword
	 * @param openamVnew
	 * @param iPlanetDirectoryKey
	 * @throws MalformedURLException
	 */
	public boolean updateUIMSPassword(String callerFid, String userId, String oldPassword, String newPassword,
			String openamVnew, String iPlanetDirectoryKey) throws MalformedURLException {
		LOGGER.info("Entered Sync updateUIMSPassword() -> Start");
		LOGGER.info("Parameter iPlanetDirectoryKey -> " + iPlanetDirectoryKey+" ,userId -> "+userId);
		LOGGER.info("Parameter callerFid -> " + callerFid);
		LOGGER.info("Parameter openamVnew -> " + openamVnew);
		UIMSLOGGER.info("Entered updateUIMSPassword() -> Start");
		UIMSLOGGER.info("Parameter iPlanetDirectoryKey -> " + iPlanetDirectoryKey+" ,userId -> "+userId);
		UIMSLOGGER.info("Parameter callerFid -> " + callerFid);
		UIMSLOGGER.info("Parameter openamVnew -> " + openamVnew);
		try {
			samlAssertion = SamlAssertionTokenGenerator.getSamlAssertionToken(callerFid, openamVnew);

			Callable<Boolean> callableUpdateUIMSPassword = new Callable<Boolean>() {
				public Boolean call() throws Exception {
					UserManagerUIMSV22 userManagerUIMSV22 = getUserManager();
					LOGGER.info("Start: calling updatePassword() of UIMS for callerFid:" + callerFid);
					ispasswordupdated = userManagerUIMSV22.updatePassword(UimsConstants.CALLER_FID, samlAssertion,
							oldPassword, newPassword);
					LOGGER.info("End: updatePassword() of UIMS finished for callerFid:" + callerFid);
					LOGGER.info("Update password status in UIMS is::" + ispasswordupdated);
					UIMSLOGGER.info("Update password status in UIMS is::" + ispasswordupdated);
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
				LOGGER.info("Start: calling updateUser() of openamservice to update version for userId:" + userId);
				productService.updateUser(iPlanetDirectoryKey, userId, version);
				LOGGER.info("End: updateUser() of openamservice to update version  finished for userId:" + userId);
			}
		} catch (RetryException e) {
			LOGGER.error("Retry failed while calling UIMS updatepassword() 3 times for userId::"+ userId);
			LOGGER.error(e.getMessage());
			UIMSLOGGER.error("Retry failed while calling UIMS updatepassword() 3 times for userId::"+ userId);
			UIMSLOGGER.error(e.getMessage());
			e.printStackTrace();

		} catch (ExecutionException e) {
			LOGGER.error("ExecutionException while calling UIMS updatepassword for userId::" +userId+" is ->" + e.getMessage());
			UIMSLOGGER.error("ExecutionException while calling UIMS updatepassword for userId::" + userId+" is ->" + e.getMessage());
			e.printStackTrace();
		} catch (Exception e) {
			LOGGER.error("Exception while updateUIMSPassword() for userId::" +userId +" is ->" + e.getMessage());
			UIMSLOGGER.error("Exception while updateUIMSPassword() for userId::" +userId +" is ->" + e.getMessage());
			e.printStackTrace();
		}
		return ispasswordupdated;
	}

}

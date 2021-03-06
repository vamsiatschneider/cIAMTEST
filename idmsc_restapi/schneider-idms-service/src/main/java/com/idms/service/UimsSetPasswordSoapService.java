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
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.github.rholder.retry.RetryException;
import com.github.rholder.retry.Retryer;
import com.github.rholder.retry.RetryerBuilder;
import com.github.rholder.retry.StopStrategies;
import com.google.common.base.Predicates;
import com.idms.model.ConfirmPinRequest;
import com.idms.product.client.OpenAMService;
import com.idms.service.util.ChinaIdmsUtil;
import com.idms.service.util.UserServiceUtil;
import com.se.idms.util.SamlAssertionTokenGenerator;
import com.se.idms.util.UserConstants;
import com.se.uims.usermanager.UserManagerUIMSV22;

@org.springframework.stereotype.Service("uimsSetPasswordSoapService")
public class UimsSetPasswordSoapService {
	
	/**
	 * Logger instance.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(UimsSetPasswordSoapService.class);
	private static final Logger UIMSSYNCLOGGER = LoggerFactory.getLogger("uimsSyncErrorLogger");

	//private static final Logger UIMSLOGGER = LoggerFactory.getLogger("uimsLogger");
	
	@Inject
	private SamlAssertionTokenGenerator samlTokenService;
	//PMD:UnusedPrivateField
	//@Autowired
	//private UIMSUserManagerSoapService userManagerSoapService;
	
	@Inject
	private OpenAMService productService;
	
	@Inject
	@Qualifier("emailService")
	@Lazy	
	private SendEmail sendEmail;
	
	@Value("${frVersion}")
	private String frVersion;

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
	
	//CODE-RE-STRUCTURING
	@Value("${caller.fid}")
	private String CALLER_FID;

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

		URL url;
		UserManagerUIMSV22 userManagerUIMSV22 = null;
		try {
			url = new URL(userManagerUIMSWsdl);

			QName qname = new QName(userManagerUIMSWsdlQname, userManagerUIMSWsdlPortName);
			Service service = Service.create(url, qname);

			LOGGER.info("Start: getPort() of UIMS");
			userManagerUIMSV22 = service.getPort(UserManagerUIMSV22.class);
			LOGGER.info("End: getPort() of UIMS");

		}catch (MalformedURLException e) {
			LOGGER.error("MalformedURLException in getUserManager()::" + e.getMessage(),e);
			LOGGER.error("ECODE-SOAP-GETUSRMGR-MFURL-ERR : Malformed URL exception when invoking SOAP service");
		}
		catch (Exception e) {
			LOGGER.error("Exception in getUserManager()::" + e.getMessage(),e);
			LOGGER.error("ECODE-SOAP-GETUSRMGR-PROC-ERR : Generic exception when invoking SOAP service");
		}
		return userManagerUIMSV22;
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

		try {
			if (UserConstants.EMAIL.equalsIgnoreCase(loginIdentifierType)) {
				samlAssertion = samlTokenService.getSamlAssertionToken(userId, openamVnew);
			}else{
				samlAssertion = samlTokenService.getSamlAssertionToken(callerFid, openamVnew);
			}
			LOGGER.info("samlAssertion="+samlAssertion);

			Callable<Boolean> callableUIMSPassword = new Callable<Boolean>() {
				public Boolean call() throws Exception {
					UserManagerUIMSV22 userManagerUIMSV22 = getUserManager();
					if (UserConstants.EMAIL.equalsIgnoreCase(loginIdentifierType)) {
						LOGGER.info("Start: activateIdentity() of sync UserManagerUIMSV22 of loginIdentifierType:"+loginIdentifierType);
						isIdentityActvated = userManagerUIMSV22.activateIdentity(CALLER_FID,password, samlAssertion);
						LOGGER.info("End: activateIdentity() of sync UserManagerUIMSV22 finished of loginIdentifierType:"+loginIdentifierType);
					} else {
						LOGGER.info("Start: setPasswordWithSms() of sync UserManagerUIMSV22 of emailOrMobile:"+emailOrMobile);
						isIdentityActvated = userManagerUIMSV22.setPasswordWithSms(CALLER_FID, emailOrMobile, samlAssertion, UserConstants.TOKEN_TYPE, password);
						LOGGER.info("End: setPasswordWithSms() of sync UserManagerUIMSV22 finished of emailOrMobile:"+emailOrMobile);
					}
					LOGGER.info("isIdentityActvated: " + isIdentityActvated);
					
					if(!isIdentityActvated){
						UIMSSYNCLOGGER.error("activateIdentity failed in UIMS for emailOrMobile = "+emailOrMobile);
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
			if (isIdentityActvated) {
				String version = "{" + "\"V_Old\": \"" + openamVnew + "\"" + "}";
				LOGGER.info("Start: updateUser() of OpenAMService for userId:"+userId+" ,version:"+version);
				UserServiceUtil.updateUserBasedOnFRVersion(productService, frVersion, iPlanetDirectoryKey, userId, version);
				LOGGER.info("End: updateUser() of OpenAMService finished for userId:"+userId+" ,version:"+version);
			}
			if(!isIdentityActvated) {
				LOGGER.info("UIMS UserpinConfirmation activateIdentity got failed -> ::sending mail notification for userid::"+userId);
				sendEmail.emailReadyToSendEmail(supportUser, fromUserName,
						"UIMS UserpinConfirmation activateIdentity failed.", userId);
				LOGGER.info("sending mail notification finished for userid::"+userId);
			}
		} catch (RetryException e) {
			LOGGER.error("RetryException in activateIdentity()::" + e.getMessage(),e);
			LOGGER.error("ECODE-SOAP-SETPWD-ACTIVEID-RETRY-ERR : Retry error during activate identity");
			UIMSSYNCLOGGER.error("activateIdentity failed in UIMS for emailOrMobile = "+emailOrMobile);
		} catch (ExecutionException e) {
			LOGGER.error("ExecutionException in activateIdentity()::" + e.getMessage(),e);
		} catch (Exception e) {
			LOGGER.error("Exception in activateIdentity()::" + e.getMessage(),e);
		}
		LOGGER.info("activateIdentity() finished!");
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

		try {
			if (UserConstants.EMAIL.equalsIgnoreCase(loginIdentifierType)) {
				samlAssertion = samlTokenService.getSamlAssertionToken(userId, openamVnew);
			} else {
				samlAssertion = samlTokenService.getSamlAssertionToken(callerFid, openamVnew);
			}

			LOGGER.info("samlAssertion="+samlAssertion);
			
			Callable<Boolean> callableUIMSPassword = new Callable<Boolean>() {
				public Boolean call() throws Exception {
					UserManagerUIMSV22 userManagerUIMSV22 = getUserManager();
					if (UserConstants.EMAIL.equalsIgnoreCase(loginIdentifierType)) {
						LOGGER.info("Start: setPassword() of SYNC UserManagerUIMSV22 for loginIdentifierType:"
								+ loginIdentifierType);
						setPasswordStatus = userManagerUIMSV22.setPassword(CALLER_FID, samlAssertion,
								password);
						LOGGER.info("End: setPassword() of SYNC UserManagerUIMSV22 finished for loginIdentifierType:"
								+ loginIdentifierType);
					} else {
						LOGGER.info("Start: setPasswordWithSms() of SYNC UserManagerUIMSV22 of emailOrMobile:"
								+ emailOrMobile);
						setPasswordStatus = userManagerUIMSV22.setPasswordWithSms(CALLER_FID,
								emailOrMobile, samlAssertion, UserConstants.TOKEN_TYPE, password);
						LOGGER.info("End: setPasswordWithSms() of SYNC UserManagerUIMSV22 finished of emailOrMobile:"
								+ emailOrMobile);
					}
					LOGGER.info("setPasswordStatus from UIMS: " + setPasswordStatus);
					if(!setPasswordStatus){
						UIMSSYNCLOGGER.error("setUIMSPassword failed in UIMS for emailOrMobile = "+emailOrMobile);
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
			if (setPasswordStatus) {
				String version = "{" + "\"V_Old\": \"" + openamVnew + "\"" + "}";
				LOGGER.info(
						"Start: updateUser() of OpenAMService to update version for userId:" + userId + " ,version:" + version);
				UserServiceUtil.updateUserBasedOnFRVersion(productService, frVersion, iPlanetDirectoryKey, userId, version);
				LOGGER.info("End: updateUser() of OpenAMService finished to update version for userId:" + userId + " ,version:" + version);
			}
			if (!setPasswordStatus) {
				LOGGER.info("UIMS UserpinConfirmation setPassword got failed --> ::sending mail notification for userid::"+ userId);
				
				sendEmail.emailReadyToSendEmail(supportUser, fromUserName,"UIMS UserpinConfirmation setPassword failed.", userId);
				
				UIMSSYNCLOGGER.error("UIMS UserpinConfirmation setPassword got failed --> ::sending mail notification for userid::"+ userId);
				
				LOGGER.info("sending mail notification finished for userid::" + userId);
			}
		} catch (RetryException e) {
			LOGGER.error("RetryException in setUIMSPassword()::" + e.getMessage(),e);
			UIMSSYNCLOGGER.error("setUIMSPassword failed in UIMS for emailOrMobile = "+emailOrMobile,e);
			LOGGER.error("ECODE-SOAP-SETPWD-SETPWD-RETRY-ERR : Retry error");
		} catch (ExecutionException e) {
			LOGGER.error("ExecutionException in setUIMSPassword()::" + e.getMessage(),e);
			UIMSSYNCLOGGER.error("setUIMSPassword failed in UIMS for emailOrMobile = "+emailOrMobile,e);
			LOGGER.error("ECODE-SOAP-SETPWD-SETPWD-EXEC-ERR : Execution error");
		} catch (Exception e) {
			LOGGER.error("Exception in setUIMSPassword()::" + e.getMessage(),e);
			UIMSSYNCLOGGER.error("setUIMSPassword failed in UIMS for emailOrMobile = "+emailOrMobile,e);
			LOGGER.error("ECODE-SOAP-SETPWD-SETPWD-GEN-ERR : Generic error");
		}
		LOGGER.info("UIMS setUIMSPassword() finished!");
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

		ObjectMapper objMapper = new ObjectMapper();

		try {
			LOGGER.info("Parameter confirmRequest -> " + ChinaIdmsUtil.printInfo(ChinaIdmsUtil.printData(objMapper.writeValueAsString(confirmRequest))));

			if ((null != confirmRequest.getPassword() && !confirmRequest.getPassword().isEmpty())) {
				activateIdentity(iPlanetDirectoryKey, confirmRequest.getId(),
						confirmRequest.getIDMS_Federated_ID__c(), confirmRequest.getPassword().trim(), openamVnew, loginIdentifierType,emailOrMobile);
			} else if (null == confirmRequest.getPassword() || "".equals(confirmRequest.getPassword())) {
				activateIdentityNoPassword(confirmRequest.getId(), confirmRequest.getIDMS_Federated_ID__c(),
						openamVnew, iPlanetDirectoryKey,loginIdentifierType,emailOrMobile);
			}
		} catch (Exception e) {			
			LOGGER.error("Exception in activateUIMSUserConfirmPIN():: -> "+ e.getMessage(),e);
			LOGGER.error("ECODE-SOAP-SETPWD-USER-CONFIRM-PIN-ERR : Error confirming PIN with UIMS");
			UIMSSYNCLOGGER.error("Exception in UIMS activateUIMSUserConfirmPIN()::" + e.getMessage(),e);
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

		try {
			if (UserConstants.EMAIL.equalsIgnoreCase(loginIdentifierType)) {
				samlAssertion = samlTokenService.getSamlAssertionToken(userId, openamVnew);
			} else {
				samlAssertion = samlTokenService.getSamlAssertionToken(callerFid, openamVnew);
			}
			LOGGER.info("samlAssertion="+samlAssertion);

			Callable<Boolean> callableActivateIdentityNoPassword = new Callable<Boolean>() {
				public Boolean call() throws Exception {
					UserManagerUIMSV22 userManagerUIMSV22 = getUserManager();
					if (UserConstants.EMAIL.equalsIgnoreCase(loginIdentifierType)) {
						LOGGER.info("Start: activateIdentityNoPassword() of UIMS for EMAIL.. userId:"+userId);
						isNoPwdactivated = userManagerUIMSV22.activateIdentityNoPassword(CALLER_FID,
								samlAssertion);
						LOGGER.info("End: activateIdentityNoPassword() of UIMS finished for EMAIL.. userId:"+userId);

					}else{
						LOGGER.info("Start: activateIdentityWithMobileNoPassword() of UIMS.. emailOrMobile:"+emailOrMobile);
						isNoPwdactivated = userManagerUIMSV22.activateIdentityWithMobileNoPassword(CALLER_FID, emailOrMobile, samlAssertion);
						LOGGER.info("End: activateIdentityWithMobileNoPassword() of UIMS finished.. emailOrMobile:"+emailOrMobile);
					}
					LOGGER.info("UIMS user activateIdentityNoPassword() isactivated status:" + isNoPwdactivated);
					
					if(!isNoPwdactivated){
						UIMSSYNCLOGGER.error("activateIdentityNoPassword failed in UIMS for emailOrMobile = "+emailOrMobile);
					}
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
				LOGGER.info("Start: updateUser() of openamservice to update version for userID:"+userId);
				UserServiceUtil.updateUserBasedOnFRVersion(productService, frVersion, iPlanetDirectoryKey, userId, version);
				LOGGER.info("End: updateUser() of openamservice finsihed to update version for userID:"+userId);
			}
			if(!isNoPwdactivated) {
				LOGGER.error("UIMS UserpinConfirmation activateIdentityNoPassword() got failed -> ::sending mail notification for userId::"+userId);
				sendEmail.emailReadyToSendEmail(supportUser, fromUserName,
						"UIMS UserpinConfirmation activateIdentityNoPassword failed.", userId);
			}
		} catch (RetryException e) {
			LOGGER.error("RetryException in activateIdentityNoPassword()::" + e.getMessage(),e);
			UIMSSYNCLOGGER.error("activateIdentityNoPassword failed in UIMS for emailOrMobile = "+emailOrMobile);
			LOGGER.error("ECODE-SOAP-ACTIVEID-NOPWD-RETRY-ERR : Retry error during activate identity");
		} catch (ExecutionException e) {
			LOGGER.error("ExecutionException in activateIdentityNoPassword()::" + e.getMessage(),e);
			LOGGER.error("ECODE-SOAP-ACTIVEID-NOPWD-EXEC-ERR : Execution error during activate identity");
		} catch (Exception e) {
			LOGGER.error("Exception in activateIdentityNoPassword()::" + e.getMessage(),e);
			LOGGER.error("ECODE-SOAP-ACTIVEID-NOPWD-GEN-ERR : Generic error during activate identity");
		}
		LOGGER.info("Completed UIMS activateIdentityNoPassword() Sync method!");
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
		
		try {
			samlAssertion = samlTokenService.getSamlAssertionToken(callerFid, openamVnew);
			LOGGER.info("samlAssertion="+samlAssertion);

			Callable<Boolean> callableUpdateUIMSPassword = new Callable<Boolean>() {
				public Boolean call() throws Exception {
					UserManagerUIMSV22 userManagerUIMSV22 = getUserManager();
					LOGGER.info("Start: updatePassword() of UIMS for callerFid:" + callerFid);
					ispasswordupdated = userManagerUIMSV22.updatePassword(CALLER_FID, samlAssertion,
							oldPassword, newPassword);
					LOGGER.info("End: updatePassword() of UIMS finished for callerFid:" + callerFid);
					LOGGER.info("Update password status in UIMS is::" + ispasswordupdated);
					
					if(!ispasswordupdated){
						UIMSSYNCLOGGER.error("updateUIMSPassword failed in UIMS for userId = "+userId);
					}
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
				UserServiceUtil.updateUserBasedOnFRVersion(productService, frVersion, iPlanetDirectoryKey, userId, version);
				LOGGER.info("End: updateUser() of openamservice to update version  finished for userId:" + userId);
			}
		} catch (RetryException e) {
			LOGGER.error("RetryException in UIMS updatepassword() for userId::"+ userId);
			LOGGER.error(e.getMessage(),e);
			UIMSSYNCLOGGER.error("updateUIMSPassword failed in UIMS for userId = "+userId,e);
			LOGGER.error("ECODE-SOAP-UPDATE-UIMSPASSWORD-RETRY-ERR : Retry error");
		} catch (ExecutionException e) {
			LOGGER.error("ExecutionException in UIMS updatepassword() for userId::" +userId+" is ->" + e.getMessage(),e);
			UIMSSYNCLOGGER.error("updateUIMSPassword failed in UIMS for userId = "+userId,e);
			LOGGER.error("ECODE-SOAP-UPDATE-UIMSPASSWORD-EXEC-ERR : Execution error");
		} catch (Exception e) {
			LOGGER.error("Exception in updateUIMSPassword() for userId::" +userId +" is ->" + e.getMessage(),e);
			UIMSSYNCLOGGER.error("updateUIMSPassword failed in UIMS for userId = "+userId,e);
			LOGGER.error("ECODE-SOAP-UPDATE-UIMSPASSWORD-GEN-ERR : Generic error");
			
		}
		return ispasswordupdated;
	}
	
	public void setFromUserName(String fromUserName) {
		this.fromUserName = fromUserName;
	}

	public void setSupportUser(String supportUser) {
		this.supportUser = supportUser;
	}

	public String getCALLER_FID() {
		return CALLER_FID;
	}

	public void setCALLER_FID(String cALLER_FID) {
		CALLER_FID = cALLER_FID;
	}


	public String getFromUserName() {
		return fromUserName;
	}


	public String getSupportUser() {
		return supportUser;
	}

	public String getFrVersion() {
		return frVersion;
	}

	public void setFrVersion(String frVersion) {
		this.frVersion = frVersion;
	}

}

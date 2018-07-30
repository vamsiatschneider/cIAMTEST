package com.idms.service;

import java.net.MalformedURLException;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;

import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

import com.github.rholder.retry.RetryException;
import com.github.rholder.retry.Retryer;
import com.github.rholder.retry.RetryerBuilder;
import com.github.rholder.retry.StopStrategies;
import com.google.common.base.Predicates;
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
	private static final Logger LOGGER = LoggerFactory.getLogger(UIMSUserManagerSoapService.class);

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

	private String samlAssertion = null;
	
	private boolean setPasswordStatus = false;
	
	public void setUIMSPassword(String iPlanetDirectoryKey, String userId,
			String callerFid, String password, String openamVnew, String loginIdentifierType,String emailOrMobile) throws MalformedURLException {
		LOGGER.info("Entered setUIMSPassword() -> Start");
		LOGGER.info("Parameter iPlanetDirectoryKey -> " + iPlanetDirectoryKey+" ,userId -> "+userId);
		LOGGER.info("Parameter callerFid -> " + callerFid);
		LOGGER.info("Parameter openamVnew -> " + openamVnew+" ,loginIdentifierType -> "+loginIdentifierType);
		LOGGER.info("Parameter emailOrMobile -> " + emailOrMobile);
		UIMSLOGGER.info("Entered setUIMSPassword() -> Start");
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
			LOGGER.error("Error executing while getting samlAssertion::" + e.getMessage());
			UIMSLOGGER.error("Error executing while getting samlAssertion::" + e.getMessage());
			e.printStackTrace();
		}
		try {
			Callable<Boolean> callableUIMSPassword = new Callable<Boolean>() {
				public Boolean call() throws Exception {
					UserManagerUIMSV22 userManagerUIMSV22 = userManagerSoapService.getUserManager();
					if (UserConstants.EMAIL.equalsIgnoreCase(loginIdentifierType)) {
						setPasswordStatus = userManagerUIMSV22.setPassword(UimsConstants.CALLER_FID, samlAssertion,
								password);
					} else {
						setPasswordStatus = userManagerUIMSV22.setPasswordWithSms(UimsConstants.CALLER_FID, emailOrMobile, samlAssertion, UserConstants.TOKEN_TYPE, password);
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
					productService.updateUser(iPlanetDirectoryKey, userId, version);
				}
			} catch (RetryException e) {
				LOGGER.error("Retry failed while calling setUIMSPassword::" + e.getMessage());
				UIMSLOGGER.error("Retry failed while calling setUIMSPassword::" + e.getMessage());
				e.printStackTrace();
				
			} catch (ExecutionException e) {
				LOGGER.error("ExecutionException while calling setUIMSPassword::" + e.getMessage());
				UIMSLOGGER.error("ExecutionException while calling setUIMSPassword::" + e.getMessage());
				e.printStackTrace();
			}
			if(!setPasswordStatus) {
				LOGGER.info("UIMS UserpinConfirmation setPassword got failed -----> ::sending mail notification for userid::"+userId);
				UIMSLOGGER.info("UIMS UserpinConfirmation setPassword got failed -----> ::sending mail notification for userid::"+userId);
				sendEmail.emailReadyToSendEmail(supportUser, fromUserName,
						"UIMS UserpinConfirmation setPassword failed.", userId);
			}
		} catch (Exception e) {
			// productService.sessionLogout(iPlanetDirectoryKey, "logout");
			LOGGER.error("Error executing while setUIMSPassword::" + e.getMessage());
			UIMSLOGGER.error("Error executing while setUIMSPassword::" + e.getMessage());
			e.printStackTrace();
		}
		// productService.sessionLogout(iPlanetDirectoryKey, "logout");
		LOGGER.info("setUIMSPassword Async Method completed!");
		UIMSLOGGER.info("setUIMSPassword Async Method completed!");
	}

}

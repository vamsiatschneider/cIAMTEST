package com.idms.service;

/**
 * The Soap Service interface layer to call the UIMS Access manager
 * stubs.
 * @author Subbarao Maniam(SESA468450)
 */

import java.net.MalformedURLException;
import java.net.URL;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;

import javax.xml.namespace.QName;
import javax.xml.ws.Service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.stereotype.Component;

import com.github.rholder.retry.RetryException;
import com.github.rholder.retry.Retryer;
import com.github.rholder.retry.RetryerBuilder;
import com.github.rholder.retry.StopStrategies;
import com.google.common.base.Predicates;
import com.idms.model.AILRequest;
import com.idms.product.client.OpenAMService;
import com.se.idms.dto.IDMSUserAIL;
import com.se.idms.util.UimsConstants;
import com.se.idms.util.UserConstants;
import com.uims.accessmanager.AccessElement;
import com.uims.accessmanager.Type;
import com.uims.accessmanager.UserAccessManagerUIMSV2;

@org.springframework.stereotype.Service("uimsAccessManagerSoapService")
@EnableAsync
public class UIMSAccessManagerSoapService {

	/**
	 * Logger instance.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(UIMSAccessManagerSoapService.class);

	private static final Logger uimsLog = LoggerFactory.getLogger("uimsLogger");
	
	private SendEmail sendEmail;
	
	//CODE-RE-STRUCTURING
	@Value("${caller.fid}")
	private String CALLER_FID;
	
	@Autowired
	@Lazy
	public void setSendEmail(SendEmail sendEmail) {
		this.sendEmail = sendEmail;
	}
	
	@Value("${fromUserName}")
	private String fromUserName;
	
	@Value("${supportUser}")
	private String supportUser;

	private boolean isrevokeresult = false;

	private boolean isgrantresult = false;
	
	@Value("${userAccessManagerUIMSVWsdl}")
	private String userAccessManagerUIMSVWsdl;
	
	@Value("${userAccessManagerUIMSQname}")
	private String userAccessManagerUIMSQname;
	
	@Value("${userAccessManagerUIMSVPortName}")
	private String userAccessManagerUIMSVPortName;

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
			LOGGER.error("MalformedURLException in getAccessManager()::" + e.getMessage(),e);
		}
		catch (Exception e) {
			LOGGER.error("Exception in getAccessManager()::" + e.getMessage(),e);
		}
		return accessManagerUIMSV2;
	}

	@Async
	public void grantAccessControlToUser(String callerFid, String federatedId, String userId, AccessElement access,
			String openamVnew, OpenAMService productService, String iPlanetDirectoryKey, String email) throws MalformedURLException {
		uimsLog.info("inside grantAccessControToUser Async method");
		try {
			Callable<Boolean> callableGrantAccess = new Callable<Boolean>() {
				public Boolean call() throws Exception {
					UserAccessManagerUIMSV2 accessManagerUIMSV2 = getAccessManager();
					isgrantresult = accessManagerUIMSV2.grantAccessControlToUser(callerFid, federatedId, access);
					uimsLog.info("grantAccessControlToUser result:" + isgrantresult);
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
				uimsLog.error("Retry failed while calling the grantAccessControlToUser::" + e.getMessage());
			} catch (ExecutionException e) {
			}
			if(!isgrantresult) {
				LOGGER.info("UIMS UpdateAIL Grant Access got failed -----> ::sending mail notification::");
				sendEmail.emailReadyToSendEmail(supportUser, fromUserName,
						"UIMS UpdateAIL Grant Access failed.", userId);
			}
		} catch (Exception e) {
			uimsLog.error("Remote Soap Exception while consuming grantAccessControlToUser :-->" + e.getMessage());
		}
		uimsLog.info("Completed grantAccessControToUser Async method!");
	}

	@Async
	public void revokeAccessControlToUser(String callerFid, String federatedId, String userId, AccessElement access,
			String openamVnew, OpenAMService productService, String iPlanetDirectoryKey, String email) throws MalformedURLException {
		uimsLog.info("inside revokeAccessControlToUser Async method");
		try {
			Callable<Boolean> callableRevokeAccess = new Callable<Boolean>() {
				public Boolean call() throws Exception {
					UserAccessManagerUIMSV2 accessManagerUIMSV2 = getAccessManager();
					isrevokeresult = accessManagerUIMSV2.revokeAccessControlToUser(callerFid, federatedId, access);
					uimsLog.info("revokeAccessControlToUser::" + isrevokeresult);
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
				uimsLog.error("Retry failed while calling the revokeAccessControlToUser::" + e.getMessage());
			} catch (ExecutionException e) {
				LOGGER.error("An error occured."+e.getMessage(),e);
			}
			if(!isrevokeresult) {
				LOGGER.info("UIMS UpdateAIL revoke access got failed -----> ::sending mail notification::");
				sendEmail.emailReadyToSendEmail(supportUser, fromUserName,
						"UIMS UpdateAIL revoke failed.", userId);
			}
		} catch (Exception e) {
			uimsLog.error("Remote Soap Exception while consuming revokeAccessControlToUser:-->" + e.getMessage());
		}
		uimsLog.info("inside revokeAccessControlToUser Async method!");
	}
	
	@Async
	public void updateUIMSUserAIL(AILRequest ailRequest, IDMSUserAIL idmsUserAIL, String vNewCntValue,
			OpenAMService productService, String iPlanetDirectoryKey, String usermail) {
		uimsLog.info("inside updateUIMSUserAIL Async Method");
		try {
			//LOGGER.info("In UserServiceImpl.updateAIL():-->Caling Async UIMS method of grant/revoke access manager control method");
			AccessElement access = new AccessElement();
			if ("APPLICATION".equalsIgnoreCase(ailRequest.getUserAILRecord().getIDMSAclType__c())) {
				access.setType(Type.APPLICATION);
				access.setId(ailRequest.getUserAILRecord().getIDMSAcl__c());
			} else if ("FEATURE".equalsIgnoreCase(ailRequest.getUserAILRecord().getIDMSAclType__c())) {
				access.setType(Type.FEATURE);
				access.setId(ailRequest.getUserAILRecord().getIDMSAcl__c());
			} else if ("PROGRAM".equalsIgnoreCase(ailRequest.getUserAILRecord().getIDMSAclType__c())) {
				access.setType(Type.PROGRAM);
				access.setId(ailRequest.getUserAILRecord().getIDMSAcl__c());
			} else {
				access.setType(Type.PROGRAM_LEVEL);
				access.setId(ailRequest.getUserAILRecord().getIDMSAcl__c());
			}
			//TODO:FedId and ID both are same
			if(null == ailRequest.getUserAILRecord().getIDMS_Federated_ID__c()){
				ailRequest.getUserAILRecord().setIDMS_Federated_ID__c(ailRequest.getUserAILRecord().getIDMSUser__c());
			}else if(null == ailRequest.getUserAILRecord().getIDMSUser__c()){
				ailRequest.getUserAILRecord().setIDMSUser__c(ailRequest.getUserAILRecord().getIDMS_Federated_ID__c());
			}
			
			if (idmsUserAIL.getIdmsoperation__c().equalsIgnoreCase("GRANT") && !(idmsUserAIL.isIdmsisRevokedOperation__c())) {
				grantAccessControlToUser(CALLER_FID,
						ailRequest.getUserAILRecord().getIDMSUser__c(),
						ailRequest.getUserAILRecord().getIDMSUser__c(), access, vNewCntValue, productService,
						iPlanetDirectoryKey, usermail);
			} else if (idmsUserAIL.getIdmsoperation__c().equalsIgnoreCase("REVOKE") && idmsUserAIL.isIdmsisRevokedOperation__c()) {
				revokeAccessControlToUser(CALLER_FID,
						ailRequest.getUserAILRecord().getIDMSUser__c(),
						ailRequest.getUserAILRecord().getIDMSUser__c(), access, vNewCntValue, productService,
						iPlanetDirectoryKey, usermail);
			}
		} catch (Exception e) {
			//productService.sessionLogout(iPlanetDirectoryKey, "logout");
			uimsLog.error("Exception in updateUIMSUserAIL():"+ e.getMessage());
		}
		//productService.sessionLogout(iPlanetDirectoryKey, "logout");
		uimsLog.info("UIMS updateAIL Async Method completed!");
	}
}

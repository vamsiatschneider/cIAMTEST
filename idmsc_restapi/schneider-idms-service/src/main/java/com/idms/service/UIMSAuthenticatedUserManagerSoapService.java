package com.idms.service;

import java.net.MalformedURLException;
import java.net.URL;

import javax.xml.namespace.QName;
import javax.xml.ws.Service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.stereotype.Component;

import com.uims.authenticatedUsermanager.AccessElement;
import com.uims.authenticatedUsermanager.AuthenticatedUserManagerUIMSV22;
import com.uims.authenticatedUsermanager.CreatedIdentityReport;
import com.uims.authenticatedUsermanager.ForcedFidAlreadyExistException_Exception;
import com.uims.authenticatedUsermanager.IMSServiceSecurityCallNotAllowedException_Exception;
import com.uims.authenticatedUsermanager.ImsMailerException_Exception;
import com.uims.authenticatedUsermanager.InactiveUserImsException_Exception;
import com.uims.authenticatedUsermanager.InvalidImsPropertiesFileException_Exception;
import com.uims.authenticatedUsermanager.InvalidImsServiceMethodArgumentException_Exception;
import com.uims.authenticatedUsermanager.LdapTemplateNotReadyException_Exception;
import com.uims.authenticatedUsermanager.RequestedEntryNotExistsException_Exception;
import com.uims.authenticatedUsermanager.RequestedInternalUserException_Exception;
import com.uims.authenticatedUsermanager.SecuredImsException_Exception;
import com.uims.authenticatedUsermanager.UnexpectedLdapResponseException_Exception;
import com.uims.authenticatedUsermanager.UnexpectedRuntimeImsException_Exception;
import com.uims.authenticatedUsermanager.UserV6;

/**
 * The Soap Service interface layer to call the UIMS authenticated user manager
 * stubs.
 * 
 * @author Aravindh Kumar
 *
 */
@org.springframework.stereotype.Service("uimsAuthUserManagSoapService")
@EnableAsync
public class UIMSAuthenticatedUserManagerSoapService {

	/**
	 * Logger instance.
	 */
	private static final Logger uimsLog = LoggerFactory.getLogger(UIMSAuthenticatedUserManagerSoapService.class);
	
	@Value("${authUserManaUIMSVWsdl}")
	private String authUserManaUIMSVWsdl;
	
	@Value("${authUserManaUIMSVQname}")
	private String authUserManaUIMSVQname;
	
	@Value("${authUserManaUIMSVPName}")
	private String authUserManaUIMSVPName;

	public AuthenticatedUserManagerUIMSV22 getAuthenticatedUserManager() throws MalformedURLException {
		URL url = new URL(authUserManaUIMSVWsdl);
		QName qname = new QName(authUserManaUIMSVQname,authUserManaUIMSVPName);
		Service service = Service.create(url, qname);

		AuthenticatedUserManagerUIMSV22 authenticatedUserManagerUIMSV2 = service
				.getPort(AuthenticatedUserManagerUIMSV22.class);
		return authenticatedUserManagerUIMSV2;
	}

	public String createUIMSUser(String callerFid,UserV6 identity,String forcedFederatedId)
			throws MalformedURLException, ForcedFidAlreadyExistException_Exception {
		
		uimsLog.info("inside createUIMSUser Async method");
		CreatedIdentityReport uimsUserResponse = null;
		try {
			AuthenticatedUserManagerUIMSV22 authenticatedUserManagerUIMSV2 = getAuthenticatedUserManager();
			//uimsUserResponse = authenticatedUserManagerUIMSV2.createIdentity(callerFid, identity, application);
			uimsUserResponse =authenticatedUserManagerUIMSV2.createIdentityForceIdmsId(callerFid, identity, forcedFederatedId);
		} catch (IMSServiceSecurityCallNotAllowedException_Exception | ImsMailerException_Exception
				| InvalidImsServiceMethodArgumentException_Exception | LdapTemplateNotReadyException_Exception
				| RequestedEntryNotExistsException_Exception | RequestedInternalUserException_Exception
				| SecuredImsException_Exception | UnexpectedLdapResponseException_Exception
				| UnexpectedRuntimeImsException_Exception e) {
			uimsLog.error("Error executing while createUIMSUser::" + e.getMessage());
			e.printStackTrace();
		}
		uimsLog.info("Completed createUIMSUser Async method!");
		return uimsUserResponse.getFederatedID();
	}
	
	public String createUIMSUserWithPassword(String callerFid,UserV6 identity,String password,String forcedFederatedId)
			throws MalformedURLException, ForcedFidAlreadyExistException_Exception {

		uimsLog.info("inside createUIMSUserWithPassword UIMS Async method");
		CreatedIdentityReport uimsUserResponse = null;
		try {
			AuthenticatedUserManagerUIMSV22 authenticatedUserManagerUIMSV2 = getAuthenticatedUserManager();
			//uimsUserResponse = authenticatedUserManagerUIMSV2.createIdentityWithPassword(callerFid, identity, application,password);
			uimsUserResponse=authenticatedUserManagerUIMSV2.createIdentityWithPasswordForceIdmsId(callerFid, identity, password, forcedFederatedId);
		} catch (IMSServiceSecurityCallNotAllowedException_Exception | ImsMailerException_Exception
				| InvalidImsServiceMethodArgumentException_Exception | LdapTemplateNotReadyException_Exception
				| RequestedEntryNotExistsException_Exception | RequestedInternalUserException_Exception
				| SecuredImsException_Exception | UnexpectedLdapResponseException_Exception
				| UnexpectedRuntimeImsException_Exception e) {
			uimsLog.error("Error executing while createUIMSUser::" + e.getMessage());
			e.printStackTrace();
		}
		uimsLog.info("Completed createUIMSUserWithPassword UIMS Async method");
		return uimsUserResponse.getFederatedID();
	}

	@Async
	public void resetUIMSPassword(String callerFid, String federatedId, AccessElement application)
			throws MalformedURLException {

		uimsLog.info("inside resetUIMSPassword UIMS Async method");
		try {
			AuthenticatedUserManagerUIMSV22 authenticatedUserManagerUIMSV2 = getAuthenticatedUserManager();
			authenticatedUserManagerUIMSV2.resetPassword(callerFid, federatedId, application);
		} catch (IMSServiceSecurityCallNotAllowedException_Exception | ImsMailerException_Exception
				| InactiveUserImsException_Exception | InvalidImsPropertiesFileException_Exception
				| InvalidImsServiceMethodArgumentException_Exception | LdapTemplateNotReadyException_Exception
				| RequestedEntryNotExistsException_Exception | SecuredImsException_Exception
				| UnexpectedLdapResponseException_Exception | UnexpectedRuntimeImsException_Exception e) {
			uimsLog.error("Error executing while resetUIMSPassword::" + e.getMessage());
			e.printStackTrace();
		}
		uimsLog.info("Completed resetUIMSPassword UIMS Async method");
	}

}

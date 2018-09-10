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
import com.uims.authenticatedUsermanager.Type;
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
	private static final Logger LOGGER = LoggerFactory.getLogger(UIMSAuthenticatedUserManagerSoapService.class);
	//private static final Logger UIMSLOGGER = LoggerFactory.getLogger("uimsLogger");
	
	@Value("${authUserManaUIMSVWsdl}")
	private String authUserManaUIMSVWsdl;
	
	@Value("${authUserManaUIMSVQname}")
	private String authUserManaUIMSVQname;
	
	@Value("${authUserManaUIMSVPName}")
	private String authUserManaUIMSVPName;

	public AuthenticatedUserManagerUIMSV22 getAuthenticatedUserManager() {
		LOGGER.info("Entered getAuthenticatedUserManager() method -> Start");
		URL url;
		AuthenticatedUserManagerUIMSV22 authenticatedUserManagerUIMSV2 = null;
		try {
			url = new URL(authUserManaUIMSVWsdl);

			QName qname = new QName(authUserManaUIMSVQname,authUserManaUIMSVPName);
			Service service = Service.create(url, qname);

			LOGGER.info("Going to call getPort() of UIMS");
			authenticatedUserManagerUIMSV2 = service
					.getPort(AuthenticatedUserManagerUIMSV22.class);
			LOGGER.info("getPort() of UIMS -> End, response is:"+authenticatedUserManagerUIMSV2);
		}catch (MalformedURLException e) {
			LOGGER.error("Exception while UIMSAuthenticatedUserManagerSoapService :: getAuthenticatedUserManager()::" + e.getMessage());
			e.printStackTrace();
		}
		catch (Exception e) {
			LOGGER.error("Exception while UIMSAuthenticatedUserManagerSoapService ::getAuthenticatedUserManager()::" + e.getMessage());
			e.printStackTrace();
		}
		return authenticatedUserManagerUIMSV2;
	}

	public String createUIMSUser(String callerFid,UserV6 identity,String forcedFederatedId)
			throws MalformedURLException, ForcedFidAlreadyExistException_Exception {
		
		LOGGER.info("Entered createUIMSUser() method - > Start");
		LOGGER.info("Parameter callerFid -> " + callerFid+" ,identity -> "+identity);
		LOGGER.info("Parameter forcedFederatedId -> " + forcedFederatedId);
		CreatedIdentityReport uimsUserResponse = null;
		try {
			AuthenticatedUserManagerUIMSV22 authenticatedUserManagerUIMSV2 = getAuthenticatedUserManager();
			//uimsUserResponse = authenticatedUserManagerUIMSV2.createIdentity(callerFid, identity, application);
			if ((null == identity.getEmail() || identity.getEmail().isEmpty()) &&(null != identity.getPhoneId() && !identity.getPhoneId().isEmpty())) {

				AccessElement application = new AccessElement();
				application.setId("Uims");
				application.setType(Type.APPLICATION);
				LOGGER.info("Going to call createIdentityWithPhoneId() of UIMS for phone:"+identity.getPhoneId());
				uimsUserResponse = authenticatedUserManagerUIMSV2.createIdentityWithPhoneId(callerFid, identity, application);
				LOGGER.info("createIdentityWithPhoneId() of UIMS finished, response:"+uimsUserResponse);
			}else{
				LOGGER.info("Going to call createIdentityForceIdmsId() of UIMS for phone:"+identity.getPhoneId());
			uimsUserResponse =authenticatedUserManagerUIMSV2.createIdentityForceIdmsId(callerFid, identity, forcedFederatedId);
			LOGGER.info("createIdentityForceIdmsId() of UIMS finished, response:"+uimsUserResponse);
			}
		} catch (IMSServiceSecurityCallNotAllowedException_Exception | ImsMailerException_Exception
				| InvalidImsServiceMethodArgumentException_Exception | LdapTemplateNotReadyException_Exception
				| RequestedEntryNotExistsException_Exception | RequestedInternalUserException_Exception
				| SecuredImsException_Exception | UnexpectedLdapResponseException_Exception
				| UnexpectedRuntimeImsException_Exception e) {
			LOGGER.error("Exception while createUIMSUser() of UIMS::" + e.getMessage());
			e.printStackTrace();
		}
		LOGGER.info("createUIMSUser() Async method -> End..FederatedID="+uimsUserResponse.getFederatedID());
		return uimsUserResponse.getFederatedID();
	}
	
	public String createUIMSUserWithPassword(String callerFid,UserV6 identity,String password,String forcedFederatedId)
			throws MalformedURLException, ForcedFidAlreadyExistException_Exception {

		LOGGER.info("Entered createUIMSUserWithPassword() Async -> Start");
		LOGGER.info("Parameter callerFid -> " + callerFid+" ,identity -> "+identity);
		LOGGER.info("Parameter password -> " + password+" ,forcedFederatedId -> "+forcedFederatedId);
		CreatedIdentityReport uimsUserResponse = null;
		try {
			AuthenticatedUserManagerUIMSV22 authenticatedUserManagerUIMSV2 = getAuthenticatedUserManager();
			//uimsUserResponse = authenticatedUserManagerUIMSV2.createIdentityWithPassword(callerFid, identity, application,password);

			if ((null == identity.getEmail() || identity.getEmail().isEmpty()) &&(null != identity.getPhoneId() && !identity.getPhoneId().isEmpty())) {

				AccessElement application = new AccessElement();
				application.setId("Uims");
				application.setType(Type.APPLICATION);
				LOGGER.info("Going to call createIdentityWithMobileWithPassword() of UIMS for phone:"+identity.getPhoneId());
				uimsUserResponse = authenticatedUserManagerUIMSV2.createIdentityWithMobileWithPassword(callerFid,
						identity,application,password);
				LOGGER.info("createIdentityWithMobileWithPassword() of UIMS finished, response:"+uimsUserResponse);
			} else {
				LOGGER.info("Going to call createIdentityWithPasswordForceIdmsId() of UIMS for phone:"+identity.getPhoneId());
				uimsUserResponse = authenticatedUserManagerUIMSV2.createIdentityWithPasswordForceIdmsId(callerFid,
						identity, password, forcedFederatedId);
				LOGGER.info("createIdentityWithPasswordForceIdmsId() of UIMS finished, response:"+uimsUserResponse);
			}
		} catch (IMSServiceSecurityCallNotAllowedException_Exception | ImsMailerException_Exception
				| InvalidImsServiceMethodArgumentException_Exception | LdapTemplateNotReadyException_Exception
				| RequestedEntryNotExistsException_Exception | RequestedInternalUserException_Exception
				| SecuredImsException_Exception | UnexpectedLdapResponseException_Exception
				| UnexpectedRuntimeImsException_Exception e) {
			LOGGER.error("Exception while createUIMSUserWithPassword()::" + e.getMessage());
			e.printStackTrace();
		}
		LOGGER.info("createUIMSUserWithPassword() UIMS Async method -> End.. with FederatedID:"+uimsUserResponse.getFederatedID());
		return uimsUserResponse.getFederatedID();
	}

	@Async
	public void resetUIMSPassword(String callerFid, String federatedId, AccessElement application)
			throws MalformedURLException {
		LOGGER.info("Entered resetUIMSPassword() UIMS Async method -> Start");
		try {
			AuthenticatedUserManagerUIMSV22 authenticatedUserManagerUIMSV2 = getAuthenticatedUserManager();
			LOGGER.info("Going to call resetPassword() of UIMS for federatedId:"+federatedId);
			authenticatedUserManagerUIMSV2.resetPassword(callerFid, federatedId, application);
			LOGGER.info("resetPassword() of UIMS finishedfor federatedId:"+federatedId);
		} catch (IMSServiceSecurityCallNotAllowedException_Exception | ImsMailerException_Exception
				| InactiveUserImsException_Exception | InvalidImsPropertiesFileException_Exception
				| InvalidImsServiceMethodArgumentException_Exception | LdapTemplateNotReadyException_Exception
				| RequestedEntryNotExistsException_Exception | SecuredImsException_Exception
				| UnexpectedLdapResponseException_Exception | UnexpectedRuntimeImsException_Exception e) {
			LOGGER.error("Exception while resetPassword() of UIMS::" + e.getMessage());
			e.printStackTrace();
		}
		LOGGER.info("resetUIMSPassword() UIMS Async method -> End");
	}

}

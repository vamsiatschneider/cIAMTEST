package com.idms.service.uims.sync;

import java.net.MalformedURLException;
import java.net.URL;

import javax.xml.namespace.QName;
import javax.xml.ws.Service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.uims.authenticatedUsermanager.AccessElement;
import com.uims.authenticatedUsermanager.AuthenticatedUserManagerUIMSV22;
import com.uims.authenticatedUsermanager.CreatedIdentityReport;
import com.uims.authenticatedUsermanager.ForcedFidAlreadyExistException_Exception;
import com.uims.authenticatedUsermanager.IMSServiceSecurityCallNotAllowedException_Exception;
import com.uims.authenticatedUsermanager.ImsMailerException_Exception;
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
@org.springframework.stereotype.Service("uimsAuthUserManagSoapServiceSync")
public class UIMSAuthenticatedUserManagerSoapServiceSync {
	
	private static final Logger LOGGER = LoggerFactory.getLogger(UIMSAuthenticatedUserManagerSoapServiceSync.class);
	
	@Value("${authUserManaUIMSVWsdl}")
	private String authUserManaUIMSVWsdl;
	
	@Value("${authUserManaUIMSVQname}")
	private String authUserManaUIMSVQname;
	
	@Value("${authUserManaUIMSVPName}")
	private String authUserManaUIMSVPName;
	
	public AuthenticatedUserManagerUIMSV22 getAuthenticatedUserManager() {
		LOGGER.info("Entered getAuthenticatedUserManager() method -> Start");
		URL url;
		AuthenticatedUserManagerUIMSV22 authenticatedUserManagerUIMSV22 = null;
		try {
			url = new URL(authUserManaUIMSVWsdl);

			QName qname = new QName(authUserManaUIMSVQname, authUserManaUIMSVPName);
			Service service = Service.create(url, qname);

			LOGGER.info("Start: getPort() of UIMS");
			authenticatedUserManagerUIMSV22 = service.getPort(AuthenticatedUserManagerUIMSV22.class);
			LOGGER.info("End: getPort() of UIMS -> End, response is:" + authenticatedUserManagerUIMSV22);
		} catch (MalformedURLException e) {
			LOGGER.error("MalformedURLException in getAuthenticatedUserManager()::" + e.getMessage());
		}
		catch (Exception e) {
			LOGGER.error("Exception in getAuthenticatedUserManager()::" + e.getMessage());
		}
		return authenticatedUserManagerUIMSV22;
	}
	
	public String createUIMSUserWithPassword(String callerFid,UserV6 identity,String password,String forcedFederatedId)
			throws MalformedURLException, ForcedFidAlreadyExistException_Exception {

		LOGGER.info("Entered createUIMSUserWithPassword() Sync -> Start");
		LOGGER.info("Parameter callerFid -> " + callerFid);
		LOGGER.info("Parameter forcedFederatedId -> "+forcedFederatedId);
		
		ObjectMapper objMapper = new ObjectMapper();
		CreatedIdentityReport uimsUserResponse = null;
		
		try {
			LOGGER.info("Parameter identity -> " + objMapper.writeValueAsString(identity));
			AuthenticatedUserManagerUIMSV22 authenticatedUserManagerUIMSV22 = getAuthenticatedUserManager();

			if ((null == identity.getEmail() || identity.getEmail().isEmpty()) &&(null != identity.getPhoneId() && !identity.getPhoneId().isEmpty())) {

				AccessElement application = new AccessElement();
				application.setId("Uims");
				application.setType(Type.APPLICATION);

				//CODE-RE-STRUCTURING - Comment differs across Integration, Staging and Preprod
				LOGGER.info("Start: UIMS createIdentityWithMobileWithPassword() for phone:"+identity.getPhoneId());
				/*uimsUserResponse = authenticatedUserManagerUIMSV2.createIdentityWithMobileWithPassword(callerFid,
						identity,application,password);*/
				uimsUserResponse = authenticatedUserManagerUIMSV22.createIdentityWithMobileWithPasswordForceIdmsId(callerFid, identity, password, forcedFederatedId);
				//CODE-RE-STRUCTURING - Comment differs across Integration, Staging and Preprod
				LOGGER.info("End: UIMS createIdentityWithMobileWithPassword() finished, response:"+objMapper.writeValueAsString(uimsUserResponse));
			} else {
				//CODE-RE-STRUCTURING - Comment differs across Integration, Staging and Preprod
				LOGGER.info("Start: UIMS createIdentityWithPasswordForceIdmsId() for phone:"+identity.getPhoneId());
				uimsUserResponse = authenticatedUserManagerUIMSV22.createIdentityWithPasswordForceIdmsId(callerFid,
						identity, password, forcedFederatedId);				
				LOGGER.info("End: UIMS createIdentityWithPasswordForceIdmsId() finished, response:"+objMapper.writeValueAsString(uimsUserResponse));
			}
		} catch (IMSServiceSecurityCallNotAllowedException_Exception | ImsMailerException_Exception
				| InvalidImsServiceMethodArgumentException_Exception | LdapTemplateNotReadyException_Exception
				| RequestedEntryNotExistsException_Exception | RequestedInternalUserException_Exception
				| SecuredImsException_Exception | UnexpectedLdapResponseException_Exception
				| UnexpectedRuntimeImsException_Exception e) {
			LOGGER.error("Exception in createUIMSUserWithPassword()::" + e.getMessage());
		} catch (JsonProcessingException e) {
			LOGGER.error("JsonProcessingException in createUIMSUserWithPassword()::" + e.getMessage());
		}
		LOGGER.info("createUIMSUserWithPassword() UIMS Sync method -> End.. with FederatedID:"+uimsUserResponse.getFederatedID());
		return uimsUserResponse.getFederatedID();
	}
	
	public String createUIMSUser(String callerFid,UserV6 identity,String forcedFederatedId)
			throws MalformedURLException, ForcedFidAlreadyExistException_Exception {
		
		LOGGER.info("Entered createUIMSUser() method - > Start");
		LOGGER.info("Parameter callerFid -> " + callerFid);
		LOGGER.info("Parameter forcedFederatedId -> " + forcedFederatedId);
		CreatedIdentityReport uimsUserResponse = null;
		ObjectMapper objMapper = new ObjectMapper();
		
		try {
			LOGGER.info("Parameter identity -> " + objMapper.writeValueAsString(identity));
			
			AuthenticatedUserManagerUIMSV22 authenticatedUserManagerUIMSV22 = getAuthenticatedUserManager();
			if ((null == identity.getEmail() || identity.getEmail().isEmpty()) &&(null != identity.getPhoneId() && !identity.getPhoneId().isEmpty())) {

				AccessElement application = new AccessElement();
				application.setId("Uims");
				application.setType(Type.APPLICATION);
				//CODE-RE-STRUCTURING - Comment differs across Integration, Staging and Preprod
				LOGGER.info("Start: UIMS createIdentityWithPhoneId() for phone:"+identity.getPhoneId());
				//uimsUserResponse = authenticatedUserManagerUIMSV2.createIdentityWithPhoneId(callerFid, identity, application);
				uimsUserResponse = authenticatedUserManagerUIMSV22.createIdentityWithPhoneIdForceIdmsId(callerFid, identity, forcedFederatedId);
				//CODE-RE-STRUCTURING - Comment differs across Integration, Staging and Preprod
				LOGGER.info("End: UIMS createIdentityWithPhoneId() finished, response:"+objMapper.writeValueAsString(uimsUserResponse));
			}else{
				//CODE-RE-STRUCTURING - Comment differs across Integration, Staging and Preprod
				LOGGER.info("Start: UIMS createIdentityForceIdmsId() for phone:"+identity.getPhoneId());
			uimsUserResponse =authenticatedUserManagerUIMSV22.createIdentityForceIdmsId(callerFid, identity, forcedFederatedId);
			LOGGER.info("End: UIMS createIdentityForceIdmsId() finished, response:"+objMapper.writeValueAsString(uimsUserResponse));
			}
		} catch (IMSServiceSecurityCallNotAllowedException_Exception | ImsMailerException_Exception
				| InvalidImsServiceMethodArgumentException_Exception | LdapTemplateNotReadyException_Exception
				| RequestedEntryNotExistsException_Exception | RequestedInternalUserException_Exception
				| SecuredImsException_Exception | UnexpectedLdapResponseException_Exception
				| UnexpectedRuntimeImsException_Exception e) {
			LOGGER.error("Exception in createUIMSUser() of UIMS::" + e.getMessage());
		} catch (JsonProcessingException e) {
			LOGGER.error("JsonProcessingException in createUIMSUser()::" + e.getMessage());
		}
		LOGGER.info("createUIMSUser() Sync method -> End..FederatedID="+uimsUserResponse.getFederatedID());
		return uimsUserResponse.getFederatedID();
	}
}

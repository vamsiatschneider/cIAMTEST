package com.idms.service;

import java.net.MalformedURLException;
import java.net.URL;

import javax.inject.Inject;
import javax.xml.namespace.QName;
import javax.xml.ws.Service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.EnableAsync;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.schneider.ims.service.company.impl.uimsv2.AuthenticatedCompanyManagerUIMSV2;
import com.schneider.ims.service.company.impl.uimsv2.ForcedFidAlreadyExistException;
import com.schneider.ims.service.company.impl.uimsv2.IMSServiceSecurityCallNotAllowedException;
import com.schneider.ims.service.company.impl.uimsv2.InvalidImsServiceMethodArgumentException;
import com.schneider.ims.service.company.impl.uimsv2.LdapTemplateNotReadyException;
import com.schneider.ims.service.company.impl.uimsv2.RequestedEntryNotExistsException;
import com.schneider.ims.service.company.impl.uimsv2.RequestedInternalUserException;
import com.schneider.ims.service.company.impl.uimsv2.SecuredImsException;
import com.schneider.ims.service.company.impl.uimsv2.UnexpectedLdapResponseException;
import com.schneider.ims.service.company.impl.uimsv2.UnexpectedRuntimeImsException;
import com.schneider.ims.service.uimsv2.CompanyV3;
import com.se.idms.util.SamlAssertionTokenGenerator;
import com.se.idms.util.UimsConstants;
import com.uims.companymanager.CompanyManagerUIMSV2;

/**
 * The Soap Service interface layer to call the UIMS company manager stubs.
 * 
 * @author Aravindh Kumar
 *
 */
@org.springframework.stereotype.Service("uimsCompManagSoapService")
@EnableAsync
public class UIMSCompanyManagerSoapService {

	/**
	 * Logger instance.
	 */
	private static final Logger uimsLog = LoggerFactory.getLogger(UIMSCompanyManagerSoapService.class);
	
	/**
	 * Logger instance.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(UIMSUserManagerSoapService.class);
	
	@Inject
	private SamlAssertionTokenGenerator samlTokenService;

	@Value("${uimsCompanyManagerWsdl}")
	private String uimsCompanyManagerWsdl;
	
	@Value("${uimsCompanyManagerQname}")
	private String uimsCompanyManagerQname;
	
	@Value("${uimsCompanyManagerPortName}")
	private String uimsCompanyManagerPortName;

	public CompanyManagerUIMSV2 getCompanyManager(){
		URL url;
		CompanyManagerUIMSV2 userManagerUIMSV2 = null;
		try {
			url = new URL(uimsCompanyManagerWsdl);

			QName qname = new QName(uimsCompanyManagerQname,uimsCompanyManagerPortName);
			Service service = Service.create(url, qname);

			userManagerUIMSV2 = service.getPort(CompanyManagerUIMSV2.class);

		}catch (MalformedURLException e) {
			LOGGER.error("Exception while UIMSCompanyManagerSoapService :: getAuthenticatedUserManager()::" + e.getMessage());
			e.printStackTrace();
		}
		catch (Exception e) {
			LOGGER.error("Exception while UIMSCompanyManagerSoapService :: getAuthenticatedUserManager()::" + e.getMessage());
			e.printStackTrace();
		}
		return userManagerUIMSV2;
	}

	/**
	 * This is implementing to user forceCompanyFederatedId
	 * @return
	 * @throws MalformedURLException
	 */
	public AuthenticatedCompanyManagerUIMSV2 getAuthenitcatedCompanyManager(){
		URL url;
		AuthenticatedCompanyManagerUIMSV2 userManagerUIMSV2 = null;
		try {
			url = new URL(uimsCompanyManagerWsdl);

			QName qname = new QName(uimsCompanyManagerQname,uimsCompanyManagerPortName);
			Service service = Service.create(url, qname);

			userManagerUIMSV2 = service.getPort(AuthenticatedCompanyManagerUIMSV2.class);
		}catch (MalformedURLException e) {
				LOGGER.error("Exception while getAuthenticatedUserManager()::" + e.getMessage());
				e.printStackTrace();
			}
			catch (Exception e) {
				LOGGER.error("Exception while getAuthenticatedUserManager()::" + e.getMessage());
				e.printStackTrace();
			}
			return userManagerUIMSV2;
		}
	
	public String createUIMSCompany(String fedId, String vnew, CompanyV3 company) {
		String uimsUserResponse = "";
		//String samlAssertion = null;
		AuthenticatedCompanyManagerUIMSV2 companyManagerUIMSV2 = null;
		try {
			companyManagerUIMSV2 =  getAuthenitcatedCompanyManager();
			//samlAssertion = samlTokenService.getSamlAssertionToken(fedId, vnew);
		} catch (Exception e) {
			uimsLog.error("Error executing while createUIMSCompany::" + e.getMessage());
			e.printStackTrace();
		}
			try {
				uimsUserResponse = companyManagerUIMSV2.createCompany(UimsConstants.CALLER_FID, fedId, company);
			} catch (IMSServiceSecurityCallNotAllowedException | InvalidImsServiceMethodArgumentException
					| LdapTemplateNotReadyException | RequestedEntryNotExistsException | RequestedInternalUserException
					| UnexpectedLdapResponseException | UnexpectedRuntimeImsException e) {
				uimsLog.error("Error executing while createUIMSCompany::" + e.getMessage());
				e.printStackTrace();
			}
		
		return uimsUserResponse;
	}
	
	public String createUIMSCompanyWithCompanyForceIdmsId(String idmsFederationId, String companyForceFederationId, String vnew, CompanyV3 company) {
		String uimsUserResponse = "";
		//String samlAssertion = null;
		AuthenticatedCompanyManagerUIMSV2 authenticatedCompanyManagerUIMSV2 = null;
		ObjectMapper objMapper = new ObjectMapper();
		try {
			uimsLog.info("Parameter fedId -> " + idmsFederationId +" ,vnew="+vnew);
			uimsLog.info("Parameter company -> " + objMapper.writeValueAsString(company));
			authenticatedCompanyManagerUIMSV2 = getAuthenitcatedCompanyManager();
			//samlAssertion = SamlAssertionTokenGenerator.getSamlAssertionToken(idmsFederationId, vnew);
		} catch (Exception e) {
			uimsLog.error("Error executing while createUIMSCompany::" + e.getMessage());
			e.printStackTrace();
		}
		try {
			uimsUserResponse = authenticatedCompanyManagerUIMSV2.createCompanyForceIdmsId(UimsConstants.CALLER_FID, idmsFederationId, company, companyForceFederationId);
		} catch (ForcedFidAlreadyExistException | IMSServiceSecurityCallNotAllowedException
				| InvalidImsServiceMethodArgumentException | LdapTemplateNotReadyException
				| RequestedEntryNotExistsException | RequestedInternalUserException | SecuredImsException
				| UnexpectedLdapResponseException | UnexpectedRuntimeImsException e) {
			e.printStackTrace();
			uimsLog.error("Error executing while createUIMSCompany::" + e.getMessage());
		}
		return uimsUserResponse;
	}

	public boolean updateUIMSCompany(String fedId, String vnew, CompanyV3 company, String companyFedId)
			throws MalformedURLException {
		boolean uimsUserResponse = false;
		/*String samlAssertion = null;
		try {
			samlAssertion = samlTokenService.getSamlAssertionToken(fedId, vnew);
		} catch (Exception e) {
			uimsLog.error("Error executing while updateUIMSCompany::" + e.getMessage());
			e.printStackTrace();
		}*/
		//CompanyManagerUIMSV2 companyManagerUIMSV2 = getCompanyManager();
		AuthenticatedCompanyManagerUIMSV2 companyManagerUIMSV2 = getAuthenitcatedCompanyManager();
		//TODO check with Prasenjit what to pass as fedId
		try {
			uimsUserResponse = companyManagerUIMSV2.updateCompany(UimsConstants.CALLER_FID, fedId, companyFedId, company);
		} catch (IMSServiceSecurityCallNotAllowedException | InvalidImsServiceMethodArgumentException
				| LdapTemplateNotReadyException | RequestedEntryNotExistsException | SecuredImsException
				| UnexpectedLdapResponseException | UnexpectedRuntimeImsException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return uimsUserResponse;
	}

	public CompanyV3 getUIMSCompany(String callerFid, String federatedId, String vnew, CompanyV3 company, String companyFedId)
			throws MalformedURLException {
		AuthenticatedCompanyManagerUIMSV2 companyManagerUIMSV2 = getAuthenitcatedCompanyManager();
		CompanyV3 uimsUserResponse = null;
		//String samlAssertionOrToken = null;
		/*try {
			samlAssertionOrToken = samlTokenService.getSamlAssertionToken(callerFid, vnew);
		} catch (Exception e) {
			uimsLog.error("Error executing while getUIMSCompany::" + e.getMessage());
			e.printStackTrace();
		}*/
		try {
			uimsUserResponse = companyManagerUIMSV2.getCompany(callerFid, federatedId, companyFedId);
		} catch (IMSServiceSecurityCallNotAllowedException | InvalidImsServiceMethodArgumentException
				| LdapTemplateNotReadyException | RequestedEntryNotExistsException | UnexpectedLdapResponseException
				| UnexpectedRuntimeImsException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return uimsUserResponse;

	}
}

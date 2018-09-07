package com.idms.service.uims.sync;

import java.net.MalformedURLException;
import java.net.URL;

import javax.xml.namespace.QName;
import javax.xml.ws.Service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;

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
@org.springframework.stereotype.Service("uimsCompManagSoapServiceSync")
public class UIMSCompanyManagerSoapServiceSync {
	private static final Logger LOGGER = LoggerFactory.getLogger(UIMSCompanyManagerSoapServiceSync.class);
	
	@Value("${uimsCompanyManagerWsdl}")
	private String uimsCompanyManagerWsdl;
	
	@Value("${uimsCompanyManagerQname}")
	private String uimsCompanyManagerQname;
	
	@Value("${uimsCompanyManagerPortName}")
	private String uimsCompanyManagerPortName;
	
	public CompanyManagerUIMSV2 getCompanyManager() throws MalformedURLException {
		URL url = new URL(uimsCompanyManagerWsdl);
		QName qname = new QName(uimsCompanyManagerQname,uimsCompanyManagerPortName);
		Service service = Service.create(url, qname);

		CompanyManagerUIMSV2 userManagerUIMSV2 = service.getPort(CompanyManagerUIMSV2.class);
		return userManagerUIMSV2;
	}
	/**
	 * This is implementing to user forceCompanyFederatedId
	 * @return
	 * @throws MalformedURLException
	 */
	public AuthenticatedCompanyManagerUIMSV2 getAuthenitcatedCompanyManager() throws MalformedURLException {
		URL url = new URL(uimsCompanyManagerWsdl);
		QName qname = new QName(uimsCompanyManagerQname,uimsCompanyManagerPortName);
		Service service = Service.create(url, qname);

		AuthenticatedCompanyManagerUIMSV2 userManagerUIMSV2 = service.getPort(AuthenticatedCompanyManagerUIMSV2.class);
		
		return userManagerUIMSV2;
	}
	
	public String createUIMSCompany(String fedId, String vnew, CompanyV3 company) {
		String uimsUserResponse = "";
		String samlAssertion = null;
		AuthenticatedCompanyManagerUIMSV2 companyManagerUIMSV2 = null;
		ObjectMapper objMapper = new ObjectMapper();
		try {
			LOGGER.info("Parameter fedId -> " + fedId +" ,vnew="+vnew);
			LOGGER.info("Parameter company -> " + objMapper.writeValueAsString(company));
			companyManagerUIMSV2 = getAuthenitcatedCompanyManager();
			samlAssertion = SamlAssertionTokenGenerator.getSamlAssertionToken(fedId, vnew);
		} catch (Exception e) {
			LOGGER.error("Error executing while createUIMSCompany::" + e.getMessage());
			e.printStackTrace();
		}
			try {
				uimsUserResponse = companyManagerUIMSV2.createCompany(UimsConstants.CALLER_FID, fedId, company);
			} catch (IMSServiceSecurityCallNotAllowedException | InvalidImsServiceMethodArgumentException
					| LdapTemplateNotReadyException | RequestedEntryNotExistsException | RequestedInternalUserException
					| UnexpectedLdapResponseException | UnexpectedRuntimeImsException e) {
				LOGGER.error("Error executing while createUIMSCompany::" + e.getMessage());
				e.printStackTrace();
			}
		
		return uimsUserResponse;
	}
	
	public String createUIMSCompanyWithCompanyForceIdmsId(String idmsFederationId, String companyForceFederationId,String vnew, CompanyV3 company) {
		String uimsUserResponse = "";
		String samlAssertion = null;
		AuthenticatedCompanyManagerUIMSV2 authenticatedCompanyManagerUIMSV2 = null;
		ObjectMapper objMapper = new ObjectMapper();
		try {
			LOGGER.info("Parameter fedId -> " + idmsFederationId +" ,vnew="+vnew);
			LOGGER.info("Parameter company -> " + objMapper.writeValueAsString(company));
			authenticatedCompanyManagerUIMSV2 = getAuthenitcatedCompanyManager();
			//samlAssertion = SamlAssertionTokenGenerator.getSamlAssertionToken(idmsFederationId, vnew);
		} catch (Exception e) {
			LOGGER.error("Error executing while createUIMSCompany::" + e.getMessage());
			e.printStackTrace();
		}
		try {
			uimsUserResponse = authenticatedCompanyManagerUIMSV2.createCompanyForceIdmsId(UimsConstants.CALLER_FID, idmsFederationId, company, companyForceFederationId);
		} catch (ForcedFidAlreadyExistException | IMSServiceSecurityCallNotAllowedException
				| InvalidImsServiceMethodArgumentException | LdapTemplateNotReadyException
				| RequestedEntryNotExistsException | RequestedInternalUserException | SecuredImsException
				| UnexpectedLdapResponseException | UnexpectedRuntimeImsException e) {
			LOGGER.error("Error executing while createUIMSCompany::" + e.getMessage());
			e.printStackTrace();
		}
		return uimsUserResponse;
	}
}

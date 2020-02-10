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
	private static final Logger UIMSSYNCLOGGER = LoggerFactory.getLogger("uimsSyncErrorLogger");
	
	@Value("${uimsCompanyManagerWsdl}")
	private String uimsCompanyManagerWsdl;
	
	@Value("${uimsCompanyManagerQname}")
	private String uimsCompanyManagerQname;
	
	@Value("${uimsCompanyManagerPortName}")
	private String uimsCompanyManagerPortName;
	
	//CODE-RE-STRUCTURING
	@Value("${caller.fid}")
	private String CALLER_FID;
	
	/* (non-Javadoc)
	 * @see com.idms.service.uims.sync.UIMSCompanyManagerSoapServiceSync#getCompanyManager()
	 */
	public CompanyManagerUIMSV2 getCompanyManager() {
		LOGGER.info("Entered getCompanyManager() method -> Start");
		
		URL url;
		CompanyManagerUIMSV2 userManagerUIMSV2 = null;
		
		try {
			url = new URL(uimsCompanyManagerWsdl);
			QName qname = new QName(uimsCompanyManagerQname,uimsCompanyManagerPortName);
			Service service = Service.create(url, qname);

			LOGGER.info("Start: getPort() of UIMS");
			userManagerUIMSV2 = service.getPort(CompanyManagerUIMSV2.class);
			LOGGER.info("End: getPort() of UIMS");

		}catch (MalformedURLException e) {
			LOGGER.error("MalformedURLException in getCompanyManager()::" + e.getMessage());
			LOGGER.error("Exception >"+e);
		}
		catch (Exception e) {
			LOGGER.error("Exception in getCompanyManager()::" + e.getMessage());
			LOGGER.error("Exception >"+e);
		}
		return userManagerUIMSV2;
	}
	/**
	 * This is implementing to user forceCompanyFederatedId
	 * @return
	 * @throws MalformedURLException
	 */
	public AuthenticatedCompanyManagerUIMSV2 getAuthenitcatedCompanyManager() {
		LOGGER.info("Entered getAuthenitcatedCompanyManager() method -> Start");
		
		URL url;
		AuthenticatedCompanyManagerUIMSV2 userManagerUIMSV2 = null;
		
		try {
			url = new URL(uimsCompanyManagerWsdl);		
		QName qname = new QName(uimsCompanyManagerQname,uimsCompanyManagerPortName);
		Service service = Service.create(url, qname);
		
		LOGGER.info("Start: getPort() of UIMS");
		userManagerUIMSV2 = service.getPort(AuthenticatedCompanyManagerUIMSV2.class);
		LOGGER.info("End: getPort() of UIMS -> End, response is:" + userManagerUIMSV2);
		
		} catch (MalformedURLException e) {
			LOGGER.error("Exception in getAuthenitcatedCompanyManager()::" + e.getMessage());
			LOGGER.error("Exception >"+e);
		}
		catch (Exception e) {
			LOGGER.error("Exception in getAuthenitcatedCompanyManager()::" + e.getMessage());
			LOGGER.error("Exception >"+e);
		}
		return userManagerUIMSV2;
	}
	
	/* (non-Javadoc)
	 * @see com.idms.service.uims.sync.UIMSCompanyManagerSoapServiceSync#createUIMSCompany(java.lang.String, java.lang.String, com.schneider.ims.service.uimsv2.CompanyV3)
	 */
	public String createUIMSCompany(String fedId, String vnew, CompanyV3 company) {
		LOGGER.info("Entered createUIMSCompany() method -> Start");
		
		String uimsUserResponse = "";
		//String samlAssertion = null;
		AuthenticatedCompanyManagerUIMSV2 companyManagerUIMSV2 = null;
		ObjectMapper objMapper = new ObjectMapper();
		
		try {
			LOGGER.info("Parameter fedId -> " + fedId +" ,vnew="+vnew);
			LOGGER.info("Parameter company -> " + objMapper.writeValueAsString(company));
			
			companyManagerUIMSV2 = getAuthenitcatedCompanyManager();
			//samlAssertion = SamlAssertionTokenGenerator.getSamlAssertionToken(fedId, vnew);
			
			LOGGER.info("Start: UIMS createCompany()");
			uimsUserResponse = companyManagerUIMSV2.createCompany(CALLER_FID, fedId, company);
			LOGGER.info("End: UIMS createCompany()");
			} catch (IMSServiceSecurityCallNotAllowedException | InvalidImsServiceMethodArgumentException
					| LdapTemplateNotReadyException | RequestedEntryNotExistsException | RequestedInternalUserException
					| UnexpectedLdapResponseException | UnexpectedRuntimeImsException e) {
				LOGGER.error("Error executing while createUIMSCompany()::" + e.getMessage());
				LOGGER.error("Exception >"+e);
			}catch (Exception e) {
				LOGGER.error("Error executing while createUIMSCompany::" + e.getMessage());
				LOGGER.error("Exception >"+e);
			}
		
		return uimsUserResponse;
	}
	
	/* (non-Javadoc)
	 * @see com.idms.service.uims.sync.UIMSCompanyManagerSoapServiceSync#createUIMSCompanyWithCompanyForceIdmsId(java.lang.String, java.lang.String, java.lang.String, com.schneider.ims.service.uimsv2.CompanyV3)
	 */
	public String createUIMSCompanyWithCompanyForceIdmsId(String idmsFederationId, String companyForceFederationId,String vnew, CompanyV3 company) {
		LOGGER.info("Entered createUIMSCompanyWithCompanyForceIdmsId() method -> Start");
		
		String uimsUserResponse = "";
		//String samlAssertion = null;
		AuthenticatedCompanyManagerUIMSV2 authenticatedCompanyManagerUIMSV2 = null;
		ObjectMapper objMapper = new ObjectMapper();
		
		try {
			LOGGER.info("Parameter fedId -> " + idmsFederationId +" ,vnew="+vnew);
			LOGGER.info("Parameter companyForceFederationId -> " + companyForceFederationId);
			LOGGER.info("Parameter company -> " + objMapper.writeValueAsString(company));
			
			authenticatedCompanyManagerUIMSV2 = getAuthenitcatedCompanyManager();
			//samlAssertion = SamlAssertionTokenGenerator.getSamlAssertionToken(idmsFederationId, vnew);
			
			LOGGER.info("Start: UIMS createCompanyForceIdmsId()");
			uimsUserResponse = authenticatedCompanyManagerUIMSV2.createCompanyForceIdmsId(CALLER_FID, idmsFederationId, company, companyForceFederationId);
			LOGGER.info("End: UIMS createCompanyForceIdmsId(), response is "+uimsUserResponse);
			
			if(null == uimsUserResponse || uimsUserResponse.isEmpty()){
				UIMSSYNCLOGGER.error("Company creation failed in UIMS, companyForceFederationId = "+companyForceFederationId);
				UIMSSYNCLOGGER.error("Company creation failed in UIMS, company info = "+objMapper.writeValueAsString(company));
			}
		} catch (ForcedFidAlreadyExistException | IMSServiceSecurityCallNotAllowedException
				| InvalidImsServiceMethodArgumentException | LdapTemplateNotReadyException
				| RequestedEntryNotExistsException | RequestedInternalUserException | SecuredImsException
				| UnexpectedLdapResponseException | UnexpectedRuntimeImsException e) {
			UIMSSYNCLOGGER.error("Error in createUIMSCompanyWithCompanyForceIdmsId()::" + e.getMessage());
			UIMSSYNCLOGGER.error("Company creation failed in UIMS, companyForceFederationId = "+companyForceFederationId);
			
			try {
				UIMSSYNCLOGGER.error("Company creation failed in UIMS, company info = "+objMapper.writeValueAsString(company));
			} catch (JsonProcessingException e1) {
				LOGGER.error("JsonProcessingException1 in createUIMSCompanyWithCompanyForceIdmsId()::" + e1.getMessage());
				LOGGER.error("Exception >"+e1);
			}
		}catch (Exception e) {
			UIMSSYNCLOGGER.error("Exception in createUIMSCompanyWithCompanyForceIdmsId()::" + e.getMessage());
			UIMSSYNCLOGGER.error("Company creation failed in UIMS, companyForceFederationId = "+companyForceFederationId);
			
			try {
				UIMSSYNCLOGGER.error("Company creation failed in UIMS, company info = "+objMapper.writeValueAsString(company));
			} catch (JsonProcessingException e1) {
				LOGGER.error("JsonProcessingException2 in createUIMSCompanyWithCompanyForceIdmsId()::" + e1.getMessage());
				LOGGER.error("Exception >"+e1);
			}
		}
		return uimsUserResponse;
	}
	
	public CompanyV3 getUIMSCompany(String federatedId, String companyFedId) {
		LOGGER.info("Entered getUIMSCompany() method -> Start");
		LOGGER.info("Parameter federatedId -> " + federatedId);
		LOGGER.info("Parameter companyFedId -> " + companyFedId);
		
		AuthenticatedCompanyManagerUIMSV2 companyManagerUIMSV2 = getAuthenitcatedCompanyManager();
		CompanyV3 uimsCompanyResponse = null;
		
		try {
			LOGGER.info("Start: UIMS getCompany() for federatedId::"+federatedId);
			uimsCompanyResponse = companyManagerUIMSV2.getCompany(CALLER_FID, federatedId, companyFedId);
			LOGGER.info("End: UIMS getCompany() finished for federatedId::"+federatedId);
		} catch (IMSServiceSecurityCallNotAllowedException | InvalidImsServiceMethodArgumentException
				| LdapTemplateNotReadyException | RequestedEntryNotExistsException | UnexpectedLdapResponseException
				| UnexpectedRuntimeImsException e) {
			LOGGER.error("Error in getUIMSCompany()::" + e.getMessage(),e);
		}
		return uimsCompanyResponse;
	}
}

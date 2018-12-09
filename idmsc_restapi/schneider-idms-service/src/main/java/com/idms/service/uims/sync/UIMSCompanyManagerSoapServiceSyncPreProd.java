package com.idms.service.uims.sync;


import java.net.MalformedURLException;
import java.net.URL;

import javax.inject.Inject;
import javax.ws.rs.HEAD;
import javax.xml.namespace.QName;
import javax.xml.ws.Service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Profile;

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


/**
 * The Soap Service interface layer to call the UIMS company manager stubs.
 * 
 * @author Aravindh Kumar
 *
 */

@Profile("PPRD")
@org.springframework.stereotype.Service("uimsCompManagSoapServiceSync")
public class UIMSCompanyManagerSoapServiceSyncPreProd implements UIMSCompanyManagerSoapServiceSync<AuthenticatedCompanyManagerUIMSV2> {
	private static final Logger LOGGER = LoggerFactory.getLogger(UIMSCompanyManagerSoapServiceSyncPreProd.class);
	
	@Inject
	private SamlAssertionTokenGenerator samlTokenService;
	
	@Value("${uimsCompanyManagerWsdl}")
	private String uimsCompanyManagerWsdl;
	
	@Value("${uimsCompanyManagerQname}")
	private String uimsCompanyManagerQname;
	
	@Value("${uimsCompanyManagerPortName}")
	private String uimsCompanyManagerPortName;
	
	//CODE-RE-STRUCTURING
	@Value("${caller.fid}")
	private String CALLER_FID;
	
	//CODE-RE-STRUCTURING - Removed the malformed exception throws clause, and instead
	//catching the error in the method and logging it
	public AuthenticatedCompanyManagerUIMSV2 getCompanyManager() {
		URL url;
		AuthenticatedCompanyManagerUIMSV2 userManagerUIMSV2 = null;
			try {
				LOGGER.info("Entered getCompanyManager() method -> Start");
				url = new URL(uimsCompanyManagerWsdl);
				QName qname = new QName(uimsCompanyManagerQname,uimsCompanyManagerPortName);
				Service service = Service.create(url, qname);
				LOGGER.info("Start: getPort() of UIMS");
				userManagerUIMSV2 = service.getPort(AuthenticatedCompanyManagerUIMSV2.class);
				LOGGER.info("End: getPort() of UIMS -> End, response is:" + userManagerUIMSV2);
			} catch (MalformedURLException e) {
				LOGGER.error("MalformedURLException in getCompanyManager()::" + e.getMessage());
				e.printStackTrace();
			}
			catch (Exception e) {
				LOGGER.error("Exception in getCompanyManager()::" + e.getMessage());
				e.printStackTrace();
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
			e.printStackTrace();
		}
		catch (Exception e) {
			LOGGER.error("Exception in getAuthenitcatedCompanyManager()::" + e.getMessage());
			e.printStackTrace();
		}
		return userManagerUIMSV2;
	}
	
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
			
			LOGGER.info("Start: UIMS createCompany() for fedId:"+fedId);
			uimsUserResponse = companyManagerUIMSV2.createCompany(CALLER_FID, fedId, company);
			LOGGER.info("End: UIMS createCompany() and status is =>"+uimsUserResponse);
			} catch (IMSServiceSecurityCallNotAllowedException | InvalidImsServiceMethodArgumentException
					| LdapTemplateNotReadyException | RequestedEntryNotExistsException | RequestedInternalUserException
					| UnexpectedLdapResponseException | UnexpectedRuntimeImsException e) {
				LOGGER.error("Error executing while createUIMSCompany()::" + e.getMessage());
				e.printStackTrace();
			}catch (Exception e) {
				LOGGER.error("Exception in createUIMSCompany::" + e.getMessage());
				e.printStackTrace();
			}
		
		return uimsUserResponse;
	}
	
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
		} catch (ForcedFidAlreadyExistException | IMSServiceSecurityCallNotAllowedException
				| InvalidImsServiceMethodArgumentException | LdapTemplateNotReadyException
				| RequestedEntryNotExistsException | RequestedInternalUserException | SecuredImsException
				| UnexpectedLdapResponseException | UnexpectedRuntimeImsException e) {
			LOGGER.error("Error in createUIMSCompanyWithCompanyForceIdmsId()::" + e.getMessage());
			e.printStackTrace();
		}catch (Exception e) {
			LOGGER.error("Error in createUIMSCompanyWithCompanyForceIdmsId()::" + e.getMessage());
			e.printStackTrace();
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
			LOGGER.error("Error in getUIMSCompany()::" + e.getMessage());
			e.printStackTrace();
		}
		return uimsCompanyResponse;
	}
}

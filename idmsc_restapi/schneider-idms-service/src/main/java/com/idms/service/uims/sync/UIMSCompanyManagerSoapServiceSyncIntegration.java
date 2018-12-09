package com.idms.service.uims.sync;

import java.net.MalformedURLException;
import java.net.URL;

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
import com.se.idms.util.UimsConstants;
import com.uims.companymanager.CompanyManagerUIMSV2;

/**
 * The Soap Service interface layer to call the UIMS company manager stubs.
 * 
 * @author Aravindh Kumar
 *
 */

@Profile("INTG")
@org.springframework.stereotype.Service("uimsCompManagSoapServiceSync")
public class UIMSCompanyManagerSoapServiceSyncIntegration implements UIMSCompanyManagerSoapServiceSync<CompanyManagerUIMSV2> {
	private static final Logger LOGGER = LoggerFactory.getLogger(UIMSCompanyManagerSoapServiceSyncIntegration.class);
	
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
			LOGGER.info("End: getPort() of UIMS -> End, response is:" + userManagerUIMSV2);

		}catch (MalformedURLException e) {
			LOGGER.error("MalformedURLException in getCompanyManager()::" + e.getMessage());
			e.printStackTrace();
		}
		catch (Exception e) {
			LOGGER.error("Exception in getCompanyManager()::" + e.getMessage());
			e.printStackTrace();
		}
		return userManagerUIMSV2;
	}
	/* (non-Javadoc)
	 * @see com.idms.service.uims.sync.UIMSCompanyManagerSoapServiceSync#getAuthenitcatedCompanyManager()
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
				e.printStackTrace();
			}catch (Exception e) {
				LOGGER.error("Error executing while createUIMSCompany::" + e.getMessage());
				e.printStackTrace();
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
	
	/**
	 * CODE-RE-STRUCTURING
	 * Method present in Pre-prod add for Interface compliance
	 */
	
	@Override
	public CompanyV3 getUIMSCompany(String federatedId, String companyFedId) {
		throw new UnsupportedOperationException();
	}
}

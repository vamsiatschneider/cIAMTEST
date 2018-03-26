package com.idms.service;

import java.net.MalformedURLException;
import java.net.URL;

import javax.xml.namespace.QName;
import javax.xml.ws.Service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.stereotype.Component;

import com.se.idms.util.SamlAssertionTokenGenerator;
import com.se.idms.util.UimsConstants;
import com.uims.companymanager.CompanyManagerUIMSV2;
import com.uims.companymanager.CompanyV3;
import com.uims.companymanager.IMSServiceSecurityCallNotAllowedException_Exception;
import com.uims.companymanager.InvalidImsServiceMethodArgumentException_Exception;
import com.uims.companymanager.LdapTemplateNotReadyException_Exception;
import com.uims.companymanager.RequestedEntryNotExistsException_Exception;
import com.uims.companymanager.RequestedInternalUserException_Exception;
import com.uims.companymanager.SecuredImsException_Exception;
import com.uims.companymanager.UnexpectedLdapResponseException_Exception;
import com.uims.companymanager.UnexpectedRuntimeImsException_Exception;

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

	public String createUIMSCompany(String fedId, String vnew, CompanyV3 company) {
		String uimsUserResponse = "";
		String samlAssertion = null;
		CompanyManagerUIMSV2 companyManagerUIMSV2 = null;
		try {
			companyManagerUIMSV2 = getCompanyManager();
			samlAssertion = SamlAssertionTokenGenerator.getSamlAssertionToken(fedId, vnew);
		} catch (Exception e) {
			uimsLog.error("Error executing while createUIMSCompany::" + e.getMessage());
			e.printStackTrace();
		}
		try {
			uimsUserResponse = companyManagerUIMSV2.createCompany(UimsConstants.CALLER_FID, samlAssertion, company);
		} catch (IMSServiceSecurityCallNotAllowedException_Exception
				| InvalidImsServiceMethodArgumentException_Exception | LdapTemplateNotReadyException_Exception
				| RequestedEntryNotExistsException_Exception | RequestedInternalUserException_Exception
				| SecuredImsException_Exception | UnexpectedLdapResponseException_Exception
				| UnexpectedRuntimeImsException_Exception e) {
			uimsLog.error("Error executing while createUIMSCompany::" + e.getMessage());
			e.printStackTrace();
		}
		return uimsUserResponse;
	}

	public boolean updateUIMSCompany(String fedId, String vnew, CompanyV3 company)
			throws MalformedURLException {
		boolean uimsUserResponse = false;
		String samlAssertion = null;
		try {
			samlAssertion = SamlAssertionTokenGenerator.getSamlAssertionToken(fedId, vnew);
		} catch (Exception e) {
			uimsLog.error("Error executing while updateUIMSCompany::" + e.getMessage());
			e.printStackTrace();
		}
		try {
			CompanyManagerUIMSV2 companyManagerUIMSV2 = getCompanyManager();
			//TODO check with Prasenjit what to pass as fedId
			uimsUserResponse = companyManagerUIMSV2.updateCompany(UimsConstants.CALLER_FID, samlAssertion, fedId, company);
		} catch (IMSServiceSecurityCallNotAllowedException_Exception
				| InvalidImsServiceMethodArgumentException_Exception | LdapTemplateNotReadyException_Exception
				| RequestedEntryNotExistsException_Exception | SecuredImsException_Exception
				| UnexpectedLdapResponseException_Exception | UnexpectedRuntimeImsException_Exception e) {
			uimsLog.error("Error executing while updateUIMSCompany::" + e.getMessage());
			e.printStackTrace();
		}
		return uimsUserResponse;
	}

	public CompanyV3 getUIMSCompany(String callerFid, String federatedId, String vnew, CompanyV3 company)
			throws MalformedURLException {
		CompanyManagerUIMSV2 companyManagerUIMSV2 = getCompanyManager();
		CompanyV3 uimsUserResponse = null;
		String samlAssertionOrToken = null;
		try {
			samlAssertionOrToken = SamlAssertionTokenGenerator.getSamlAssertionToken(callerFid, vnew);
		} catch (Exception e) {
			uimsLog.error("Error executing while getUIMSCompany::" + e.getMessage());
			e.printStackTrace();
		}
		try {
			companyManagerUIMSV2 = getCompanyManager();
			uimsUserResponse = companyManagerUIMSV2.getCompany(callerFid, samlAssertionOrToken, federatedId);
		} catch (IMSServiceSecurityCallNotAllowedException_Exception
				| InvalidImsServiceMethodArgumentException_Exception | LdapTemplateNotReadyException_Exception
				| RequestedEntryNotExistsException_Exception | SecuredImsException_Exception
				| UnexpectedLdapResponseException_Exception | UnexpectedRuntimeImsException_Exception e) {
			uimsLog.error("Error executing while getUIMSCompany::" + e.getMessage());
			e.printStackTrace();
		}
		return uimsUserResponse;

	}
}

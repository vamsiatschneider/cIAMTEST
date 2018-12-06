package com.idms.service;

import java.net.MalformedURLException;

import com.schneider.ims.service.company.impl.uimsv2.AuthenticatedCompanyManagerUIMSV2;
import com.schneider.ims.service.uimsv2.CompanyV3;

public interface UIMSCompanyManagerSoapService <T> {
	
	public T getCompanyManager();
	
	public AuthenticatedCompanyManagerUIMSV2 getAuthenitcatedCompanyManager();
	
	public String createUIMSCompany(String fedId, String vnew, CompanyV3 company);
	
	public String createUIMSCompanyWithCompanyForceIdmsId(String idmsFederationId, String companyForceFederationId, String vnew, CompanyV3 company);
	
	public boolean updateUIMSCompany(String fedId, String vnew, CompanyV3 company, String companyFedId) throws MalformedURLException;
	
	public CompanyV3 getUIMSCompany(String callerFid, String federatedId, String vnew, CompanyV3 company, String companyFedId) throws MalformedURLException;
	
	public CompanyV3 getUIMSCompany(String federatedId, String companyFedId) throws MalformedURLException;

}

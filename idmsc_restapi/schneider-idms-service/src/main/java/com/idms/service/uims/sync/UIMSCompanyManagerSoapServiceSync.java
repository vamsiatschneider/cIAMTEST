package com.idms.service.uims.sync;

import com.schneider.ims.service.company.impl.uimsv2.AuthenticatedCompanyManagerUIMSV2;
import com.schneider.ims.service.uimsv2.CompanyV3;

public interface UIMSCompanyManagerSoapServiceSync<T> {

	T getCompanyManager();

	AuthenticatedCompanyManagerUIMSV2 getAuthenitcatedCompanyManager();

	String createUIMSCompany(String fedId, String vnew, CompanyV3 company);

	String createUIMSCompanyWithCompanyForceIdmsId(String idmsFederationId, String companyForceFederationId,
			String vnew, CompanyV3 company);
	
	//CODE-RE-STRUCTURING - Method specific to Pre-prod
	CompanyV3 getUIMSCompany(String federatedId, String companyFedId);

}
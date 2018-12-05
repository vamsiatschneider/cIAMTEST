package com.idms.service;

import java.net.MalformedURLException;

import com.idms.model.ConfirmPinRequest;
import com.idms.model.CreateUserRequest;
import com.idms.product.client.OpenAMService;
import com.schneider.ims.service.uimsv2.CompanyV3;

public interface UIMSUserManagerSoapService<T, U, V> {

	void setSendEmail(SendEmail sendEmail);

	T getUserManager();
	
	V getUIMSV22UserManager();
	
	void getUIMSUser(String callerFid, String vnew) throws MalformedURLException;

	void activateIdentity(String iPlanetDirectoryKey, String userId, String callerFid, String password,
			String openamVnew, String loginIdentifierType, String emailOrMobile) throws MalformedURLException;

	void setUIMSPassword(String iPlanetDirectoryKey, String userId, String callerFid, String password,
			String openamVnew, String loginIdentifierType, String emailOrMobile) throws MalformedURLException;

	void updateUIMSPassword(String callerFid, String userId, String oldPassword, String newPassword, String openamVnew,
			String iPlanetDirectoryKey) throws MalformedURLException;

	boolean updateUIMSUser(String fedId, U user, String vnew) throws MalformedURLException;

	void activateUIMSIdentity(String callerFid, String password, String vnew) throws MalformedURLException;

	void activateIdentityNoPassword(String userId, String callerFid, String openamVnew, String iPlanetDirectoryKey,
			String loginIdentifierType, String emailOrMobile) throws MalformedURLException;

	String createUIMSUserAndCompany(String callerFid, com.uims.authenticatedUsermanager.UserV6 identity, String context,
			CompanyV3 company, String userName, String iPlanetDirectoryKey, String v_new, String password,
			String forcedFederatedId, CreateUserRequest userRequest, int companyCreatedCount);

	String updateUIMSUserAndCompany(String fedId, U identity, String context, CompanyV3 company,
			String vnew, OpenAMService productService, String iPlanetDirectoryKey, String userName,String companyFedId,String email);

	void activateUIMSUserConfirmPIN(ConfirmPinRequest confirmRequest, String openamVnew, String iPlanetDirectoryKey,
			String loginIdentifierType, String emailOrMobile);

	void updateChangeEmailOrMobile(String iPlanetDirectoryKey, String userId, String callerFid, String openamVnew,
			String loginIdentifierType, String newEmailOrMobile) throws MalformedURLException;
	
}

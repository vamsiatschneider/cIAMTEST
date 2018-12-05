package com.schneider.uims.service;

import java.net.MalformedURLException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.Async;

import com.idms.product.client.OpenAMService;
import com.idms.service.SendEmail;
import com.schneider.idms.model.IdmsUserAilRequest;
import com.schneider.idms.model.IdmsUserConfirmRequest;
import com.schneider.idms.model.IdmsUserRequest;
import com.schneider.ims.service.uimsv2.CompanyV3;
import com.se.idms.dto.IDMSUserAIL;
import com.se.uims.usermanager.UserManagerUIMSV22;
import com.se.uims.usermanager.UserV6;
import com.uims.accessmanager.UserAccessManagerUIMSV2;

public interface DirectUIMSUserManagerSoapService<T> {

	void setSendEmail(SendEmail sendEmail);

	UserManagerUIMSV22 getUserManager();

	UserAccessManagerUIMSV2 getAccessManager();

	void getUIMSUser(String callerFid, String vnew) throws MalformedURLException;

	void setUIMSPassword(String iPlanetDirectoryKey, String userId, String callerFid, String password,
			String openamVnew, String loginIdentifierType, String emailOrMobile) throws MalformedURLException;

	void updateUIMSPassword(String callerFid, String userId, String oldPassword, String newPassword, String openamVnew,
			String iPlanetDirectoryKey) throws MalformedURLException;

	boolean updateUIMSUser(String fedId, T user, String vnew) throws MalformedURLException;

	void activateUIMSIdentity(String callerFid, String password, String vnew) throws MalformedURLException;

	void activateIdentityNoPassword(String userId, String callerFid, String openamVnew, String iPlanetDirectoryKey,
			String loginIdentifierType, String emailOrMobile) throws MalformedURLException;

	String createUIMSUserAndCompany(String callerFid, com.uims.authenticatedUsermanager.UserV6 identity, String context,
			CompanyV3 company, String userName, String iPlanetDirectoryKey, String v_new, String forcedFederatedId,
			IdmsUserRequest userRequest, int companyCreatedCount);

	String updateUIMSUserAndCompany(String fedId, T identity, String context, CompanyV3 company, String vnew,
			OpenAMService productService, String iPlanetDirectoryKey, String userName, String companyFedId,
			String email);

	void activateUIMSUserConfirmPIN(IdmsUserConfirmRequest confirmRequest, String openamVnew,
			String iPlanetDirectoryKey, String loginIdentifierType, String emailOrMobile);

	void updateUIMSUserAIL(IdmsUserAilRequest userAilRequest, IDMSUserAIL idmsUserAIL, String vNewCntValue,
			OpenAMService productService, String iPlanetDirectoryKey, String usermail);

	void grantAccessControlToUser(String callerFid, String federatedId, String userId,
			com.uims.accessmanager.AccessElement access, String openamVnew, OpenAMService productService,
			String iPlanetDirectoryKey, String email) throws MalformedURLException;

	void revokeAccessControlToUser(String callerFid, String federatedId, String userId,
			com.uims.accessmanager.AccessElement access, String openamVnew, OpenAMService productService,
			String iPlanetDirectoryKey, String email) throws MalformedURLException;

}
package com.idms.service;

import java.net.MalformedURLException;

import com.idms.model.ConfirmPinRequest;

public interface UimsSetPasswordSoapService<T> {
	
	public T getUserManager();

	public void activateIdentity(String iPlanetDirectoryKey, String userId,
			String callerFid, String password, String openamVnew, String loginIdentifierType,String emailOrMobile) 
					throws MalformedURLException;
	
	public boolean setUIMSPassword(String iPlanetDirectoryKey, String userId,
			String callerFid, String password, String openamVnew, String loginIdentifierType, String emailOrMobile)
					throws MalformedURLException;
	
	public void activateUIMSUserConfirmPIN(ConfirmPinRequest confirmRequest,
			String openamVnew, String iPlanetDirectoryKey,String loginIdentifierType,String emailOrMobile);
	
	public void activateIdentityNoPassword(String userId, String callerFid,
			String openamVnew, String iPlanetDirectoryKey, String loginIdentifierType,String emailOrMobile) throws MalformedURLException;
	
	public boolean updateUIMSPassword(String callerFid, String userId, String oldPassword, String newPassword,
			String openamVnew, String iPlanetDirectoryKey) throws MalformedURLException;
}

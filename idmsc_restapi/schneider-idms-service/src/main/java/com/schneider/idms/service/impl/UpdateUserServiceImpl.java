package com.schneider.idms.service.impl;

import javax.ws.rs.core.Response;

import com.schneider.idms.model.IdmsUpdateUserResponse;
import com.schneider.idms.model.IdmsUserRequest;
import com.schneider.idms.service.IUpdateUserService;

public abstract class UpdateUserServiceImpl extends IdmsCommonServiceImpl implements IUpdateUserService {

	public abstract Response updateUser(String authorizedToken, String accept, String region, IdmsUserRequest userRequest);
	
	public abstract IdmsUpdateUserResponse convertUserDataToUserResponse(String userData);
}

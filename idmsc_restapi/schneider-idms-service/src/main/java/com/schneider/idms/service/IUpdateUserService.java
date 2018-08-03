package com.schneider.idms.service;

import javax.ws.rs.Consumes;
import javax.ws.rs.HeaderParam;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;

import com.idms.model.UpdateUserRequest;

@Path("/identity/services/apexrest")
@Produces("application/json")
public interface IUpdateUserService {

	@PUT
	@Path("/IDMSuser/1.0/IDMSUser")
	@Consumes("application/json")
	Response updateUser(@HeaderParam("Authorization")String authorizedToken,@HeaderParam("client_id")String clientId,
			@HeaderParam("client_secret")String clientSecret,UpdateUserRequest userRequest);
	
}

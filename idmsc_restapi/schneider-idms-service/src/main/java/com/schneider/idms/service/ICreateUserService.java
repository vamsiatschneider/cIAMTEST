package com.schneider.idms.service;

import javax.validation.Valid;
import javax.ws.rs.Consumes;
import javax.ws.rs.HeaderParam;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;

import com.idms.model.CreateUserRequest;

@Path("/identity/services/apexrest")
@Produces("application/json")
interface ICreateUserService {

	@POST
	@Path("/IDMSsvcs/1.0/IDMSUser")
	@Consumes("application/json")
	Response userRegistration(@HeaderParam("client_id")String clientId,
			@HeaderParam("client_secret")String clientSecret,@Valid CreateUserRequest userRequest);
}

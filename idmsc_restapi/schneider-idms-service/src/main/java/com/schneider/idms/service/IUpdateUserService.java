package com.schneider.idms.service;

import javax.ws.rs.Consumes;
import javax.ws.rs.HeaderParam;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;

import com.schneider.idms.model.IdmsUserRequest;

@Produces("application/json")
@Path("/services/apexrest/IDMSUser/1.0")
public interface IUpdateUserService {

	@PUT
	@Consumes("application/json")
	Response updateUser(@HeaderParam("IDMS-Authorization")String authorization,
			@HeaderParam("Accept")String accept,
			@HeaderParam("IDMS-Region")String region,IdmsUserRequest userRequest);
	
}

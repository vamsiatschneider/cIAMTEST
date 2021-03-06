package com.schneider.idms.service;

import javax.validation.Valid;
import javax.ws.rs.Consumes;
import javax.ws.rs.HeaderParam;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;

import com.schneider.idms.model.IdmsUserRequest;

@Path("/services/apexrest/IDMSServices")
@Produces("application/json")
public interface ICreateUserService {

	@POST
	@Path("/Registration/1.0")
	@Consumes("application/json")
	Response userRegistration(@HeaderParam("Authorization")String authorization,
			@HeaderParam("IDMS-Authorization")String secretToken,
			@HeaderParam("Accept")String accept,
			@HeaderParam("IDMS-Region")String region,@Valid IdmsUserRequest userRequest);
}

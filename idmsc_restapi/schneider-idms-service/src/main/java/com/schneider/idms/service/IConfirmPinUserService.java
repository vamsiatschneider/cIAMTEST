package com.schneider.idms.service;

import javax.validation.Valid;
import javax.ws.rs.Consumes;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;

import com.idms.model.ConfirmPinRequest;

@Path("/identity/services/apexrest")
@Produces("application/json")
public interface IConfirmPinUserService {

	@POST
	@Path("/IDMSsvcs/1.0/ConfirmPIN")
	@Consumes("application/json")
	Response userPinConfirmation(@Valid ConfirmPinRequest confirmPIN);
}

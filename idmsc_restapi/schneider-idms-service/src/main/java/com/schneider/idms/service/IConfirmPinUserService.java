package com.schneider.idms.service;

import javax.validation.Valid;
import javax.ws.rs.Consumes;
import javax.ws.rs.HeaderParam;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;

import com.idms.model.ConfirmPinRequest;
import com.idms.model.ResendPinRequest;

@Path("/identity/services/apexrest")
@Produces("application/json")
public interface IConfirmPinUserService {

	@POST
	@Path("/IDMSsvcs/1.0/ConfirmPIN")
	@Consumes("application/json")
	Response userPinConfirmation(@Valid ConfirmPinRequest confirmPIN);
	
	@PUT
	@Path("/IDMSsvcs/1.0/ResendPinCode")
	@Consumes("application/json")
	Response resendPIN(@HeaderParam("Authorization") String token,@Valid ResendPinRequest resendPinRequest);
}

package com.schneider.idms.service;

import javax.validation.Valid;
import javax.ws.rs.Consumes;
import javax.ws.rs.HeaderParam;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;

import com.idms.model.ResendPinRequest;

@Path("/services/apexrest/IDMSServices")
@Produces("application/json")
public interface IResendPinService {

	@PUT
	@Path("/ResendPinCode/1.0")
	@Consumes("application/json")
	Response resendPIN(@HeaderParam("Authorization") String token,@Valid ResendPinRequest resendPinRequest);
}

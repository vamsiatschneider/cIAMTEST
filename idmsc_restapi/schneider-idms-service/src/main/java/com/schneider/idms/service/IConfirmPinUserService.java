package com.schneider.idms.service;

import javax.validation.Valid;
import javax.ws.rs.Consumes;
import javax.ws.rs.HeaderParam;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;

import com.idms.model.ResendPinRequest;
import com.schneider.idms.model.IdmsUserConfirmRequest;

@Path("/identity/services/apexrest/IDMSServices")
@Produces("application/json")
public interface IConfirmPinUserService {

	@POST
	@Path("/ConfirmPin/1.0")
	@Consumes("application/json")
	Response userPinConfirmation(@HeaderParam("Authorization")String authorization,
			@HeaderParam("Accept")String accept,
			@HeaderParam("IDMS-Region")String region,@Valid IdmsUserConfirmRequest confirmPinRequest);
	
	@PUT
	@Path("/ResendPinCode/1.0")
	@Consumes("application/json")
	Response resendPIN(@HeaderParam("Authorization") String token,@Valid ResendPinRequest resendPinRequest);
}

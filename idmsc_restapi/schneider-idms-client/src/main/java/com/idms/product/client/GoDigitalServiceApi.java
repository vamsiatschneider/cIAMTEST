package com.idms.product.client;

import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;

@Produces("application/json")
@Path("rest/model/smp/rest/actors")
public interface GoDigitalServiceApi {

	@POST
	@Path("/SMPRegisterActor/userRegistrationInfo")
	String userRegistrationInfo(String userRegistrationInfoRequest);
}

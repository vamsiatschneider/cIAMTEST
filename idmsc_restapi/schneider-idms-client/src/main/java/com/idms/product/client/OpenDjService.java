package com.idms.product.client;

import javax.ws.rs.GET;
import javax.ws.rs.HeaderParam;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;

@Produces("application/json")
@Path("/opendj")
public interface OpenDjService {

	@GET
	@Path("/Apps/{applicationName}")
	Response getUser(@HeaderParam("X-OpenIDM-Username") String userName,@HeaderParam("X-OpenIDM-Password") String password,@PathParam("applicationName") String applicationName);
}

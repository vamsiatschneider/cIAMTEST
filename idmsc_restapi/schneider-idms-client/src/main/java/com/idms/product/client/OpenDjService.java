package com.idms.product.client;

import javax.ws.rs.GET;
import javax.ws.rs.HeaderParam;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;

@Produces("application/json")
public interface OpenDjService {

	@GET
	@Path("/opendj/Apps/{applicationName}")
	Response getUser(@HeaderParam("X-OpenIDM-Username") String userName,@HeaderParam("X-OpenIDM-Password") String password,@PathParam("applicationName") String applicationName);
	
	@GET
	@Path("accessmanager/oauth2/se/userinfo")
	String getUserDetails(@HeaderParam("Authorization") String authorization);
}

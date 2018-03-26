package com.idms.product.client;

import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.HeaderParam;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Response;

@Produces("application/json")
@Path("/accessmanager/json")
public interface OpenAMProvisionalService {

	@POST
	@Path("/ProvisionalRealm/selfservice/userRegistration")
	@Consumes("application/json")
	String userRegistration(@HeaderParam("Cookie") String iPlanetDirectoryKey,@QueryParam("_action") String action,String userRequest);
	
	@POST
	@Path("/ProvisionalRealm/authenticate")
	@Consumes("application/json")
	Response otpAuthentication(@HeaderParam("Cookie") String cookieValue,@QueryParam("service") String service,@QueryParam("authIndexType") String authIndexType, @QueryParam("authIndexValue") String authIndexValue,String request);
	
	@PUT
	@Path("/ProvisionalRealm/users/{userId}")
	@Consumes("application/json")
	String updateUser(@HeaderParam("Cookie") String iPlanetDirectoryKey,@PathParam("userId") String userId,String authId);
	
	@GET
	@Path("/ProvisionalRealm/users/{userId}")
	String getUser(@HeaderParam("iPlanetDirectoryPro") String iPlanetDirectoryKey,@PathParam("userId") String userId);
	
	@DELETE
	@Path("/ProvisionalRealm/users/{userId}")
	@Consumes("application/json")
	String deleteUser(@HeaderParam("Cookie") String iPlanetDirectoryKey,@PathParam("userId") String userId);
}

/**
 * 
 */
package com.schneider.idms.service;

import javax.ws.rs.GET;
import javax.ws.rs.HeaderParam;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;

/**
 * @author SESA508936
 *
 */
@Path("/identity/services/apexrest")
@Produces("application/json")
public interface GetUserService {
	
	@GET
	@Path("/IDMSuser/1.0/users/{userId}")
	Response getUser(@PathParam("userId") String userId);
	
	@GET
	@Path("/IDMSuser/1.0/oauth2/userinfo")
	Response getUserByOauth(@HeaderParam("Authorization") String token);
	
	@GET
	@Path("/IDMSuser/1.0/users")
	Response getUserbyToken(@HeaderParam("Authorization") String token);
	
	@GET
	@Path("/IDMSuser/1.0/GetIDMSUser/{loginIdentifier}")
	Response getUserByLoginIdentifier(@PathParam("loginIdentifier") String loginIdentifier);
	
	@GET
	@Path("/IDMSuser/1.0/oauth2/userinfo/ui")
	Response getUserByOauthFromUI(@HeaderParam("Authorization") String token);

}

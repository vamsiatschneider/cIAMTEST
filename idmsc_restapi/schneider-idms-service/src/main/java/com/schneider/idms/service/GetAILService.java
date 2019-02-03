/**
 * 
 */
package com.schneider.idms.service;

import javax.ws.rs.GET;
import javax.ws.rs.HeaderParam;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Response;

/**
 * @author SESA508936
 *
 */
@Produces("application/json")
//@Path("/services/apexrest/IDMSUserAIL/{federationId}")
@Path("/services")
public interface GetAILService {
	
	@GET
	@Path("/apexrest/IDMSUserAIL/{federationId}")
	Response getUserAIL(@HeaderParam("Authorization") String authorization, @HeaderParam("Accept") String accept,
			@HeaderParam("Region") String region,@PathParam("federationId") String federationId);
	
	@GET
	@Path("/apexrest/IDMSUserSearch")
	Response getUserBySearch(@HeaderParam("Authorization") String authorization,
			@HeaderParam("Region") String region, @QueryParam("federationID") String federationId,  @QueryParam("userID") String userId,
			 @QueryParam("email") String email, @QueryParam("mobile") String mobile);

}

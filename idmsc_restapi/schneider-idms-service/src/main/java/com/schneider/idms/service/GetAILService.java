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
@Produces("application/json")
@Path("/services/apexrest/IDMSUserAIL/{federationId}")
public interface GetAILService {
	
	@GET
	Response getUserAIL(@HeaderParam("Authorization") String authorization, @HeaderParam("Accept") String accept,
			@HeaderParam("Region") String region,@PathParam("federationId") String federationId);

}

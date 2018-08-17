/**
 * 
 */
package com.schneider.idms.service;

import javax.ws.rs.GET;
import javax.ws.rs.HeaderParam;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;

/**
 * @author SESA508936
 *
 */
@Path("/services/apexrest")
@Produces("application/json")
public interface GetUserService {

	@GET
	@Path("/IDMSUser/1.0")
	Response getUser(@HeaderParam("IDMS-Authorization") String authorization, @HeaderParam("Accept") String accept,
			@HeaderParam("IDMS-Region") String region);

}

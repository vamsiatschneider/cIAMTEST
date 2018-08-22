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
@Produces("application/json")
@Path("/services/apexrest/IDMSUser/1.0")
public interface GetUserService {

	@GET
	Response getUser(@HeaderParam("IDMS-Authorization") String authorization, @HeaderParam("Accept") String accept,
			@HeaderParam("IDMS-Region") String region);

}

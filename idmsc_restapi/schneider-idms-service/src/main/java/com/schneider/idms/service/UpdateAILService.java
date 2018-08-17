package com.schneider.idms.service;

import javax.ws.rs.Consumes;
import javax.ws.rs.HeaderParam;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;

import com.schneider.idms.model.IdmsUserAilRequest;

/**
 * @author SESA508936
 *
 */
@Path("/services/apexrest/IDMSServices")
@Produces("application/json")
public interface UpdateAILService {

	@PUT
	@Path("/UserAIL/1.0/{federatedId}")
	@Consumes("application/json")
	Response updateAIL(@PathParam("federatedId") String federatedId, @HeaderParam("Authorization") String authorization,
			@HeaderParam("Accept") String accept, @HeaderParam("IDMS-Region") String region, IdmsUserAilRequest userAilRequest);

}

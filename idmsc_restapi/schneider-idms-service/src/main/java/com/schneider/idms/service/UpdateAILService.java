package com.schneider.idms.service;

import javax.ws.rs.Consumes;
import javax.ws.rs.HeaderParam;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;

import com.schneider.idms.model.IdmsUserAilRequest;

/**
 * @author SESA508936
 *
 */
@Path("/services/apexrest/IDMSServices/UserAIL/1.0")
@Produces("application/json")
public interface UpdateAILService {

	@PUT
	@Consumes("application/json")
	Response updateAIL(@HeaderParam("Authorization") String authorization,
			@HeaderParam("Accept") String accept, @HeaderParam("IDMS-Region") String region, IdmsUserAilRequest userAilRequest);

}

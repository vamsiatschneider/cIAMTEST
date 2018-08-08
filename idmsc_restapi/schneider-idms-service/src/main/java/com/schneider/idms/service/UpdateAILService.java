/**
 * 
 */
package com.schneider.idms.service;

import javax.validation.Valid;
import javax.ws.rs.Consumes;
import javax.ws.rs.HeaderParam;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;

import com.idms.model.AILRequest;

/**
 * @author SESA508936
 *
 */
@Path("/identity/services/apexrest")
@Produces("application/json")
public interface UpdateAILService {
	
	@PUT
	@Path("/IDMSsvcs/1.0/IDMSUpdateUserAIL")
	@Consumes("application/json")
	Response updateAIL(@HeaderParam("client_id")String clientId,
			@HeaderParam("client_secret")String clientSecret,@Valid AILRequest aRequest);

}

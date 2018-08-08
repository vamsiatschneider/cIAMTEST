/**
 * 
 */
package com.schneider.idms.service;

import javax.validation.Valid;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Response;

import com.idms.model.CheckUserExistsRequest;

/**
 * @author SESA508936
 *
 */
@Path("/identity/services/apexrest")
@Produces("application/json")
public interface CheckUserExistService {
	
	@GET
	@Path("/IDMSsvcs/1.0/{loginIdentifier}")
	Response checkUserExists(@PathParam("loginIdentifier") String loginIdentifier,@QueryParam("WithGlobalUsers") String withGlobalUsers);
	
	@POST
	@Path("/IDMSsvcs/1.0/IDMSCheckUser")
	Response idmsCheckUserExists(@Valid CheckUserExistsRequest request);
	

}

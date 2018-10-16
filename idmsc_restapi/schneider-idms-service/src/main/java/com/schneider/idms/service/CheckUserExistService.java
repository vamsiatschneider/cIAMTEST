/**
 * 
 */
package com.schneider.idms.service;

import javax.ws.rs.HeaderParam;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;

import com.idms.model.CheckUserExistsRequest;

/**
 * @author SESA508936
 * For Direct API Call
 */
@Path("/services/apexrest/IDMSServices")
@Produces("application/json")
public interface CheckUserExistService {
	
	@POST
	@Path("/CheckUser/1.0")
	Response idmsCheckUserExists(@HeaderParam("Authorization") String authorization,@HeaderParam("IDMS-Authorization")String secretToken, @HeaderParam("Accept") String accept,
			@HeaderParam("IDMS-Region") String region, CheckUserExistsRequest checkUserExistsRequest);
}

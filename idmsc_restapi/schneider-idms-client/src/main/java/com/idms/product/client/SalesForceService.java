package com.idms.product.client;

import javax.ws.rs.FormParam;
import javax.ws.rs.HeaderParam;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;

@Produces("application/json")
public interface SalesForceService {

	@POST
	@Path("/services/oauth2/token")
	String getSalesForceToken(@PathParam("grant_type") String grantType,@PathParam("client_id") String clientId, @PathParam("client_secret") String clientSecret,@PathParam("username") String username, @PathParam("password") String password);
	
	@POST
	@Path("/services/oauth2/token")
	String getSalesForceToken(@HeaderParam("Content-Type") String contetType,@FormParam("grant_type") String grantType,@FormParam("client_id") String clientId, @FormParam("client_secret") String clientSecret,@FormParam("username") String username, @FormParam("password") String password);
	
	@POST
	@Path("/services/apexrest/IDMSSetActivationDate/")
	Response populateActivationDate(@HeaderParam("Content-Type") String contetType,@HeaderParam("Authorization") String salesForceToken,String request);
}

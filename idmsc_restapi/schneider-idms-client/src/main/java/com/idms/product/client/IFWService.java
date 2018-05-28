package com.idms.product.client;

import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.HEAD;
import javax.ws.rs.HeaderParam;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Response;

@Produces("application/json")
public interface IFWService {
	@POST
	@Path("/token")
	@Consumes("application/json")
	String getIFWToken(@HeaderParam("Content-Type") String contetType, @QueryParam("grant_type") String grantType,
			@QueryParam("client_id") String clientId, @QueryParam("client_secret") String clientSecret);

	@HEAD
	@Path("/rest/idms/user/2.0/users")
	Response checkUserExistsWithEmail(@HeaderParam("X-BFO-Authorization") String bfoAuthorization,
			@HeaderParam("X-SE-IFW-ApplicationName") String applicationName,
			@HeaderParam("X-SE-IFW-CountryCode") String countryCode,
			@HeaderParam("X-SE-IFW-LanguageCode") String languageCode,
			@HeaderParam("X-SE-IFW-RequestId") String requestId, 
			@HeaderParam("Authorization") String authorization,
			@QueryParam("email") String email,
			@QueryParam("withGlobalUsers") String withGlobalUsers);
	
	@HEAD
	@Path("/rest/idms/user/2.0/users")
	Response checkUserExistsWithMobile(@HeaderParam("X-BFO-Authorization") String bfoAuthorization,
			@HeaderParam("X-SE-IFW-ApplicationName") String applicationName,
			@HeaderParam("X-SE-IFW-CountryCode") String countryCode,
			@HeaderParam("X-SE-IFW-LanguageCode") String languageCode,
			@HeaderParam("X-SE-IFW-RequestId") String requestId, @HeaderParam("Authorization") String authorization,
			@QueryParam("mobile") String email,
			@QueryParam("withGlobalUsers") String withGlobalUsers);
	
	
	@POST
	@Path("/rest/idms/user/2.0/password/resetrequest")
	String initiatePasswordRecovery(@HeaderParam("Content-Type") String contetType,
			@HeaderParam("X-BFO-Authorization") String bfoAuthorization,
			@HeaderParam("X-SE-IFW-ApplicationName") String applicationName,
			@HeaderParam("X-SE-IFW-CountryCode") String countryCode,
			@HeaderParam("X-SE-IFW-LanguageCode") String languageCode,
			@HeaderParam("X-SE-IFW-RequestId") String requestId, 
			@HeaderParam("Authorization") String authorization,
			@HeaderParam("Accept") String accept,
			@QueryParam("withGlobalUsers") String withGlobalUsers,
			String userRequest);
	
	@GET
	@Path("/rest/idms/user/3.0/users")
	Response getUser(
			@HeaderParam("Authorization") String authorization,
			@HeaderParam("X-BFO-Authorization") String bfoAuthorization,
			@HeaderParam("Accept") String accept,
			@HeaderParam("X-SE-IFW-ApplicationName") String applicationName,
			@HeaderParam("X-SE-IFW-RequestId") String requestId, 
			@HeaderParam("X-SE-IFW-CountryCode") String countryCode,
			@HeaderParam("X-SE-IFW-LanguageCode") String languageCode,
			@QueryParam("UIMSFederatedId") String iDMSFederatedId);
	
}

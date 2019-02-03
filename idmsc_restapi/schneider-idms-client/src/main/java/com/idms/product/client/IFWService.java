package com.idms.product.client;

import javax.ws.rs.Consumes;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.HeaderParam;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
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

	//CODE-RE-STRUCTURING - 3-Feb-19 merge (API upgrade from 2.0 to 4.0)
	@POST
	@Path("/rest/idms/user/4.0/users/exist")
	Response checkUserExistsWithEmail(@HeaderParam("X-BFO-Authorization") String bfoAuthorization,
			@HeaderParam("X-SE-IFW-ApplicationName") String applicationName,
			@HeaderParam("X-SE-IFW-CountryCode") String countryCode,
			@HeaderParam("X-SE-IFW-LanguageCode") String languageCode,
			@HeaderParam("X-SE-IFW-RequestId") String requestId, 
			@HeaderParam("Authorization") String authorization,
			@FormParam("email") String email, @FormParam("withGlobalUsers") Boolean withGlobalUsers);
	
	//CODE-RE-STRUCTURING - 3-Feb-19 merge (API upgrade from 2.0 to 4.0)
	@POST
	@Path("/rest/idms/user/4.0/users/exist")
	Response checkUserExistsWithMobile(@HeaderParam("X-BFO-Authorization") String bfoAuthorization,
			@HeaderParam("X-SE-IFW-ApplicationName") String applicationName,
			@HeaderParam("X-SE-IFW-CountryCode") String countryCode,
			@HeaderParam("X-SE-IFW-LanguageCode") String languageCode,
			@HeaderParam("X-SE-IFW-RequestId") String requestId, @HeaderParam("Authorization") String authorization,
			@FormParam("mobile") String email, @FormParam("withGlobalUsers") Boolean withGlobalUsers);
	
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

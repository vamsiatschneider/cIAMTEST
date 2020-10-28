package com.idms.product.client;

import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.GET;
import javax.ws.rs.HeaderParam;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Response;

import org.springframework.cache.annotation.Cacheable;


@Produces("application/json")
@Path("/accessmanager/json")
public interface OpenAMService {

	// OpenAM 6.5 User REST APIs START here
	@GET
	@Path("/se/users/{userId}")
	String getUser(@HeaderParam("Accept-API-Version") String acceptAPIVersion,
			@HeaderParam("iPlanetDirectoryPro") String iPlanetDirectoryKey, @PathParam("userId") String userId);

	@GET
	@Path("/se/users")
	@Consumes("application/json")
	String checkUserExistsWithEmailMobile(@HeaderParam("Accept-API-Version") String acceptAPIVersion,
			@HeaderParam("Cookie") String iPlanetDirectoryKey, @QueryParam("_queryFilter") String emailOrMobile);

	@POST
	@Path("/se/selfservice/userRegistration")
	@Consumes("application/json")
	Response userRegistration(@HeaderParam("Accept-API-Version") String acceptAPIVersion,
			@HeaderParam("Cookie") String iPlanetDirectoryKey, @QueryParam("_action") String action,
			String userRequest);

	@PUT
	@Path("/se/users/{userId}")
	@Consumes("application/json")
	String updateUser(@HeaderParam("Accept-API-Version") String acceptAPIVersion,
			@HeaderParam("Cookie") String iPlanetDirectoryKey, @PathParam("userId") String userId, String authId);

	@PUT
	@Path("/se/users/{userId}")
	@Consumes("application/json")
	Response updateUserForPassword(@HeaderParam("Accept-API-Version") String acceptAPIVersion,
			@HeaderParam("Cookie") String iPlanetDirectoryKey, @PathParam("userId") String userId, String authId);

	@DELETE
	@Path("/se/users/{userId}")
	@Consumes("application/json")
	Response deleteUser(@HeaderParam("Accept-API-Version") String acceptAPIVersion,
			@HeaderParam("Cookie") String iPlanetDirectoryKey, @PathParam("userId") String userId);

	@PUT
	@Path("/se/users/{userId}")
	@Consumes("application/json")
	String updateCounter(@HeaderParam("Accept-API-Version") String acceptAPIVersion,
			@HeaderParam("Cookie") String iPlanetDirectoryPro, @PathParam("userId") String userId, String request);

	// Changing to HTTP PUT since PATCH is not supported by Java 1.8 HTTP URL connection.
	//@PATCH
	@PUT
	@Path("/se/users/{userId}")
	@Consumes("application/json")
	Response updateSocialProfile(@HeaderParam("Accept-API-Version") String acceptAPIVersion,
			@HeaderParam("Cookie") String iPlanetDirectoryPro, @PathParam("userId") String userId, String patchRequest);

	// OpenAM 6.5 User REST APIs END here

	// OpenAM 6.5 Authenticate REST APIs START here
	@POST
	@Path("/authenticate")
	@Cacheable(value = "iPlanetToken", key = "#p0")
	String authenticateUser(@HeaderParam("Accept-API-Version") String acceptAPIVersion,
			@HeaderParam("X-OpenAM-Username") String userName, @HeaderParam("X-OpenAM-Password") String password,
			@QueryParam("realm") String realm);
	
	@POST
	@Path("/authenticate")
	@Consumes("application/json")
	Response otpAuthentication(@HeaderParam("Accept-API-Version") String acceptAPIVersion,
			@QueryParam("realm") String realm, @QueryParam("authIndexType") String authIndexType,
			@QueryParam("authIndexValue") String authIndexValue);

	@POST
	@Path("/authenticate")
	String authenticateIdmsChinaUser(@HeaderParam("Accept-API-Version") String acceptAPIVersion,
			@HeaderParam("X-OpenAM-Username") String userName, @HeaderParam("X-OpenAM-Password") String password,
			@QueryParam("realm") String realm);

	// OpenAM 6.5 Authenticate REST APIs END here

	// OpenAM 6.5 Session REST APIs START here
	@POST
	@Path("/se/sessions/")
	@Consumes("application/json")
	String activeToken(@HeaderParam("Accept-API-Version") String acceptAPIVersion,
			@HeaderParam("Cookie") String iPlanetDirectoryKey, @QueryParam("_action") String isActive,
			@QueryParam("tokenId") String tokenId);

	@POST
	@Path("/sessions/")
	@Consumes("application/json")
	String sessionLogout(@HeaderParam("Accept-API-Version") String acceptAPIVersion,
			@HeaderParam("Content-Type") String contentType, String token, @QueryParam("_action") String logout);
	// OpenAM 6.5 Session REST APIs END here

	@GET
	// @Path("/se/users")
	String getUserString();

	@POST
	@Path("/authenticate")
	@Cacheable(value = "iPlanetToken", key = "#p0")
	String authenticateUser(@HeaderParam("X-OpenAM-Username") String userName,
			@HeaderParam("X-OpenAM-Password") String password, @QueryParam("realm") String realm);

	@PUT
	@Path("/se/users/{userId}")
	@Consumes("application/json")
	String updateCounter(@HeaderParam("Cookie") String iPlanetDirectoryPro, @PathParam("userId") String userId,
			String request);

	@GET
	@Path("/se/users/{userId}")
	String getUser(@HeaderParam("iPlanetDirectoryPro") String iPlanetDirectoryKey, @PathParam("userId") String userId);

	@POST
	@Path("/se/selfservice/userRegistration")
	@Consumes("application/json")
	Response userRegistration(@HeaderParam("Cookie") String iPlanetDirectoryKey, @QueryParam("_action") String action,
			String userRequest);

	@GET
	@Path("/se/users")
	@Consumes("application/json")
	String checkUserExistsWithEmailMobile(@HeaderParam("Cookie") String iPlanetDirectoryKey,
			@QueryParam("_queryFilter") String emailOrMobile);

	@POST
	@Path("/se/authenticate")
	@Consumes("application/json")
	Response otpAuthentication(@HeaderParam("Cookie") String cookieValue, @QueryParam("service") String service,
			@QueryParam("authIndexType") String authIndexType, @QueryParam("authIndexValue") String authIndexValue,
			String request);

	@PUT
	@Path("/se/users/{userId}")
	@Consumes("application/json")
	String updateUser(@HeaderParam("Cookie") String iPlanetDirectoryKey, @PathParam("userId") String userId,
			String authId);
	@PUT
	@Path("/se/users/{userId}")
	@Consumes("application/json")
	Response updateMFADetails(@HeaderParam("Accept-API-Version") String acceptAPIVersion,
			@HeaderParam("Cookie") String iPlanetDirectoryPro, @PathParam("userId") String userId, String request);
			
	@PUT
	@Path("/se/users/{userId}")
	@Consumes("application/json")
	Response updateUserForPassword(@HeaderParam("Cookie") String iPlanetDirectoryKey,
			@PathParam("userId") String userId, String authId);

	@POST
	@Path("/se/sessions/")
	@Consumes("application/json")
	String activeToken(@HeaderParam("Cookie") String iPlanetDirectoryKey, @QueryParam("_action") String isActive,
			@QueryParam("tokenId") String tokenId);

	@POST
	@Path("/se/sessions/")
	@Consumes("application/json")
	String sessionLogout(@HeaderParam("Cookie") String iPlanetDirectoryKey, @QueryParam("_action") String logout);

	@DELETE
	@Path("/se/users/{userId}")
	@Consumes("application/json")
	Response deleteUser(@HeaderParam("Cookie") String iPlanetDirectoryKey, @PathParam("userId") String userId);

	@POST
	@Path("/se/authenticate")
	@Consumes("application/form-data")
	Response initSocialLogin(@QueryParam("service") String service, @QueryParam("authIndexType") String authIndexType,
			@QueryParam("authIndexValue") String authIndexValue);

	@POST
	@Path("/authenticate")
	String authenticateIdmsChinaUser(@HeaderParam("X-OpenAM-Username") String userName,
			@HeaderParam("X-OpenAM-Password") String password, @QueryParam("realm") String realm);

	@POST
	@Path("/se/authenticate")
	@Consumes("application/json")
	Response oauth2iplanet(@HeaderParam("Cookie") String cookie, @HeaderParam("Cache-Control") String cacheControl,
			@HeaderParam("access_token") String accessToken, @QueryParam("service") String service,
			@QueryParam("authIndexType") String authIndexType, @QueryParam("authIndexValue") String authIndexValue,
			String auth);

}

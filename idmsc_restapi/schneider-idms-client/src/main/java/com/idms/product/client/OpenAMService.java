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

	@GET
	//@Path("/se/users")
	String getUserString();

	@POST
	@Path("/authenticate")
	@Cacheable(value="iPlanetToken", key="#p0")
	String authenticateUser(@HeaderParam("X-OpenAM-Username") String userName,@HeaderParam("X-OpenAM-Password") String password,@QueryParam("realm") String realm);
	
	@GET
	@Path("/se/users/{userId}")
	String getUser(@HeaderParam("iPlanetDirectoryPro") String iPlanetDirectoryKey,@PathParam("userId") String userId);
	
	@POST
	@Path("/se/selfservice/userRegistration")
	@Consumes("application/json")
	Response userRegistration(@HeaderParam("Cookie") String iPlanetDirectoryKey,@QueryParam("_action") String action,String userRequest);
	
	@GET
	@Path("/se/users")
	@Consumes("application/json")
	String checkUserExistsWithEmailMobile(@HeaderParam("Cookie") String iPlanetDirectoryKey,@QueryParam("_queryFilter") String emailOrMobile);
	
	@POST
	@Path("/se/authenticate")
	@Consumes("application/json")
	Response otpAuthentication(@HeaderParam("Cookie") String cookieValue, @QueryParam("service") String service,@QueryParam("authIndexType") String authIndexType, @QueryParam("authIndexValue") String authIndexValue,String request);
	
	@PUT
	@Path("/se/users/{userId}")
	@Consumes("application/json")
	String updateUser(@HeaderParam("Cookie") String iPlanetDirectoryKey,@PathParam("userId") String userId,String authId);
	
	@PUT
	@Path("/se/users/{userId}")
	@Consumes("application/json")
	Response updateUserForPassword(@HeaderParam("Cookie") String iPlanetDirectoryKey,@PathParam("userId") String userId,String authId);
	
	@POST
	@Path("/se/sessions/")
	@Consumes("application/json")
	String activeToken(@HeaderParam("Cookie") String iPlanetDirectoryKey,@QueryParam("_action") String isActive,@QueryParam("tokenId") String tokenId);
	
	@POST
	@Path("/se/sessions/")
	@Consumes("application/json")
	String sessionLogout(@HeaderParam("Cookie") String iPlanetDirectoryKey,@QueryParam("_action") String logout);
	
	@DELETE
	@Path("/se/users/{userId}")
	@Consumes("application/json")
	Response deleteUser(@HeaderParam("Cookie") String iPlanetDirectoryKey,@PathParam("userId") String userId);
	
	@POST
	@Path("/se/authenticate")
	@Consumes("application/form-data")
	Response initSocialLogin(@QueryParam("service") String service,@QueryParam("authIndexType") String authIndexType, @QueryParam("authIndexValue") String authIndexValue);
	
	
	@POST
	@Path("/authenticate")
	String authenticateIdmsChinaUser(@HeaderParam("X-OpenAM-Username") String userName,@HeaderParam("X-OpenAM-Password") String password,@QueryParam("realm") String realm);
	
	@POST
	@Path("/se/authenticate")
	@Consumes("application/json")
	Response oauth2iplanet(@HeaderParam("Cache-Control") String cacheControl,@HeaderParam("access_token") String accessToken, @QueryParam("service") String service,@QueryParam("authIndexType") String authIndexType, @QueryParam("authIndexValue") String authIndexValue,String auth);
	
}

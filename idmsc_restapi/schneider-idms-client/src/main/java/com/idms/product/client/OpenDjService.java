package com.idms.product.client;

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

@Produces("application/json")
public interface OpenDjService {

	@GET
	@Path("/applications/{applicationName}")
	Response getUser(@HeaderParam("X-OpenIDM-Username") String userName,@HeaderParam("X-OpenIDM-Password") String password,@PathParam("applicationName") String applicationName);
	
	@GET
	@Path("/applications")
	Response getAppInfo(@HeaderParam("X-OpenIDM-Username") String userName,@HeaderParam("X-OpenIDM-Password") String password,@QueryParam("_queryFilter") String appHash);
	
	/*@GET
	@Path("accessmanager/oauth2/se/userinfo")
	String getUserDetails(@HeaderParam("Authorization") String authorization);*/
	
	@GET
	@Path("/mobileotp/{mobilenumber}")
	Response getMobileOTPDetails(@HeaderParam("X-OpenIDM-Username") String userName,@HeaderParam("X-OpenIDM-Password") String password,@PathParam("mobilenumber") String mobilenumber);
	
	@POST
	@Path("/mobileotp")
	Response postMobileOTPDetails(@HeaderParam("Content-Type") String type,@HeaderParam("X-OpenIDM-Username") String userName,@HeaderParam("X-OpenIDM-Password") String password,@QueryParam("_action") String action, String mobileRecord);

	@PUT
	@Path("/mobileotp/{mobilenumber}")
	Response putMobileOTPDetails(@HeaderParam("Content-Type") String type,@HeaderParam("If-Match") String matchstr,@HeaderParam("X-OpenIDM-Username") String userName,@HeaderParam("X-OpenIDM-Password") String password,@PathParam("mobilenumber") String mobilenumber,String mobileRecord);

	@DELETE
	@Path("/mobileotp/{mobilenumber}")
	Response deleteMobileOTPDetails(@HeaderParam("Content-Type") String type,@HeaderParam("X-OpenIDM-Username") String userName,@HeaderParam("X-OpenIDM-Password") String password,@PathParam("mobilenumber") String mobilenumber);

	@GET
	@Path("/emailtemplates/{operationType}")
	Response getEmailTemplateDetails(@HeaderParam("X-OpenIDM-Username") String userName,@HeaderParam("X-OpenIDM-Password") String password,@PathParam("operationType") String operationType);
}

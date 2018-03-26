package com.idms.product.client;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Response;

@Produces("application/json")
public interface SmsService {

	@GET
	@Path("/sendSMSCode")
	Response sendSMSCode(@QueryParam("account") String account, @QueryParam("password") String password,
			@QueryParam("phone") String phone, @QueryParam("content") String content,
			@QueryParam("template") String template);

}

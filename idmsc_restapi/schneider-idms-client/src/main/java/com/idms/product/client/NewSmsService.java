package com.idms.product.client;

import javax.ws.rs.GET;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Response;

@Produces("text/xml")
public interface NewSmsService {

	@GET
	Response sendSMSCode(@QueryParam("sn") String sn, @QueryParam("pwd") String pwd,
			@QueryParam("mobile") String mobile, @QueryParam("content") String content);

}

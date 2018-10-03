package com.idms.product.client;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Response;

@Produces("text/xml")
public interface NewSmsService {

	@GET
	@Path("/mdsmssend")
	Response sendSMSCode(@QueryParam("sn") String sn, @QueryParam("pwd") String pwd,
			@QueryParam("mobile") String mobile, @QueryParam("content") String content,
			@QueryParam("ext") String ext, @QueryParam("stime") String stime,@QueryParam("rrid") String rrid,
			@QueryParam("msgfmt") String msgfmt);

}

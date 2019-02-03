package com.idms.product.client;

import javax.ws.rs.Consumes;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.HeaderParam;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;

@Produces("application/json")
@Path("/accessmanager")
public interface IdentityService {

	@GET
	@Path("/oauth2/se/.well-known/openid-configuration")
	Response getOIDCAutoDiscoveryConfig();
	
	//CODE-RE-STRUCTURING - 3-Feb-19 merge (New API added)
	@POST
	@Path("/SSOPOST/metaAlias/se/{idp}")
	@Consumes("application/x-www-form-urlencoded")
	//@Produces("text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8")
	Response buildQueryParam(@FormParam("RelayState") String relayState,@FormParam("SAMLRequest") String samlRequest,@PathParam("idp") String idp,@HeaderParam("Content-Length") int length);

}

package com.idms.product.client;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.Response;

@Produces("application/json")
@Path("/accessmanager")
public interface IdentityService {

	@GET
	@Path("/oauth2/se/.well-known/openid-configuration")
	Response getOIDCAutoDiscoveryConfig();

}

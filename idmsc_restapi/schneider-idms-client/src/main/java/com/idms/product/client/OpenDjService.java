package com.idms.product.client;

import javax.ws.rs.core.Response;

/**
 * Marker interface which is sub-classed by per-environment interfaces
 * which are used as references by the Apache CXF configuration file
 *
 */

public interface OpenDjService {

	Response getUser(String userName, String password, String applicationName);
	
	String getUserDetails(String authorization);
	
}

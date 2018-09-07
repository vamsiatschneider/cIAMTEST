package com.idms.product.client;

import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.HeaderParam;
import javax.ws.rs.POST;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Response;

@Produces("application/json")
@Path("/accessmanager")
public interface OpenAMTokenService {

	@POST
	@Path("/oauth2/userinfo")
	String getUserInfoByAccessToken(@HeaderParam("Authorization") String accessToken,@QueryParam("realm") String realm);


	@GET
	@Path("/oauth2/authorize")
	@Consumes("application/x-www-form-urlencoded")
	Response getOauthFromIPlanet(
			@HeaderParam("Cookie")String string,@HeaderParam("Cache-Control")String cache,
			@HeaderParam("Content-Type")String contentType,
			@QueryParam("response_type") String responseType,
			@QueryParam("redirect_uri")  String redirecturi,
			@QueryParam("scope") String scope,
			@QueryParam("realm") String realm,
			@QueryParam("client_id")  String clientid,
			@QueryParam("csrf")  String csrf,
			@QueryParam("decision") String decision,
			@QueryParam("save_consent") String saveconsent);

	@POST
	@Path("/oauth2/se/access_token")
	@Consumes("application/x-www-form-urlencoded")
	String getOauthTokenFromCode(
			@HeaderParam("Cache-Control")String cache,
			@HeaderParam("Content-Type")String contentType,
			@HeaderParam("Authorization")String authorization,
			@QueryParam("grant_type")  String grantType,
			@QueryParam("realm") String realm,
			@QueryParam("redirect_uri")  String redirecturi,
			@QueryParam("code")  String code);

	@GET
	@Path("/oauth2/se/.well-known/openid-configuration")
	Response getOIDCAutoDiscoveryConfig();

	//	@POST
	//	@Path("/oauth2/authorize")
	//	@Consumes("application/x-www-form-urlencoded")
	//	Response getOauthFromIPlanet(@HeaderParam("Cookie") String cookie,MultivaluedMap<String, String> params);


	//?response_type=code&scope=openid&client_id=myClientID&redirect_uri=https://identity-int.schneider-electric.com/openid/cb-basic.html&realm=/se
	/*@GET
	@Path("/oauth2/authorize")
	@Consumes("application/x-www-form-urlencoded")
	Response getOauthFromIPlanet(@HeaderParam("Cookie")String string,@HeaderParam("Cache-Control")String cache, @QueryParam("response_type") String string2, @QueryParam("scope") String string3,@QueryParam("client_id")  String string4,@QueryParam("redirect_uri")  String string5,
			@QueryParam("realm") String string6);*/

	//without cache
	//	Response getOauthFromIPlanet(@HeaderParam("Cookie")String string, @QueryParam("response_type") String string2, @QueryParam("scope") String string3,@QueryParam("client_id")  String string4,@QueryParam("redirect_uri")  String string5,
	//			@QueryParam("realm") String string6);


	//openAMTokenService.getOauthFromIPlanet("iPlanetDirectoryPro=" + token,"code", "myClientID","/se","openid profile",
	//"https://identity-int.schneider-electric.com:/openid/cb-basic.html");
}

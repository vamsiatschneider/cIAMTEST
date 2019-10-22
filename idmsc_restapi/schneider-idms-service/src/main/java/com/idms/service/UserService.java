package com.idms.service;

import java.util.List;

import javax.validation.Valid;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.FormParam;
import javax.ws.rs.GET;
import javax.ws.rs.HeaderParam;
import javax.ws.rs.POST;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.PathParam;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.Response;

import com.idms.model.AILRequest;
import com.idms.model.ActivateUserRequest;
import com.idms.model.AddEmailRequest;
import com.idms.model.AddMobileRequest;
import com.idms.model.CheckUserExistsRequest;
import com.idms.model.CheckUserIdentityRequest;
import com.idms.model.ConfirmPinRequest;
import com.idms.model.CreateUserRequest;
import com.idms.model.PasswordRecoveryRequest;
import com.idms.model.ResendEmailChangeRequest;
import com.idms.model.ResendPinRequest;
import com.idms.model.ResendRegEmailRequest;
import com.idms.model.SendInvitationRequest;
import com.idms.model.SendOTPRequest;
import com.idms.model.UpdatePasswordRequest;
import com.idms.model.UpdateUserRequest;
import com.idms.model.UserDetailByApplicationRequest;
import com.idms.model.UserMFADataRequest;
import com.idms.model.VerifyPinRequest;
import com.se.idms.dto.SetPasswordRequest;

@Path("/services")
@Produces("application/json")
public interface UserService {

	@POST
	@Path("/apexrest/authenticate")
	Response authenticateUser(@HeaderParam("X-OpenAM-Username") String userName,
			@HeaderParam("X-OpenAM-Password") String password, @QueryParam("realm") String realm);

	@GET
	@Path("/apexrest/users/IDMS_Federated_ID__c/{federationId}")
	Response getUserByFederationId(@HeaderParam("Authorization") String authorizationToken,
			@PathParam("federationId") String federationId);

	@GET
	@Path("/apexrest/users/{userId}")
	Response getUser(@HeaderParam("Authorization") String authorizationToken, @PathParam("userId") String userId);

	// Response getUser(String userId);

	@GET
	@Path("/oauth2/userinfo")
	Response getUserByOauth(@HeaderParam("Authorization") String token);

	@GET
	@Path("/apexrest/users")
	Response getUserbyToken(@HeaderParam("Authorization") String token);

	@POST
	@Path("/apexrest/IDMSUser")
	@Consumes("application/json")
	Response userRegistration(@HeaderParam("client_id") String clientId,
			@HeaderParam("client_secret") String clientSecret, @Valid CreateUserRequest userRequest);

	@POST
	@Path("/apexrest/ConfirmPIN")
	@Consumes("application/json")
	Response userPinConfirmation(@Valid ConfirmPinRequest confirmPIN);

	@GET
	@Path("/apexrest/IDMSUser/{loginIdentifier}")
	Response checkUserExists(@PathParam("loginIdentifier") String loginIdentifier,
			@QueryParam("WithGlobalUsers") String withGlobalUsers, @QueryParam("appName") String applicationName);

	@POST
	@Path("/apexrest/IDMSCheckUser")
	Response idmsCheckUserExists(@Valid CheckUserExistsRequest request);

	@POST
	@Path("/apexrest/IDMSCheckIdentity")
	Response idmsCheckIdentity(CheckUserIdentityRequest emailOrMobileReq);

	@GET
	@Path("/apexrest/oauth2")
	@Produces("application/json")
	Response getOauthFromIPlanet(@HeaderParam("Authorization") String token);

	@GET
	@Path("/apexrest/IDMSUser")
	@Produces("application/json")
	Response userExists(@QueryParam("email") String email);

	@POST
	@Path("/apexrest/IDMSPasswordRecovery")
	@Consumes("application/json")
	Response passwordRecovery(@Valid PasswordRecoveryRequest passwordRecoveryRequest);

	@PUT
	@Path("/apexrest/IDMSPassword")
	@Consumes("application/json")
	Response updatePassword(@HeaderParam("Authorization") String token,
			@Valid UpdatePasswordRequest updatePasswordRequest);

	@PUT
	@Path("/apexrest/IDMSUser")
	@Consumes("application/json")
	Response updateUser(@HeaderParam("Authorization") String authorizedToken, @HeaderParam("client_id") String clientId,
			@HeaderParam("client_secret") String clientSecret, UpdateUserRequest userRequest);

	@PUT
	@Path("/apexrest/IDMSUpdateUserAIL")
	@Consumes("application/json")
	Response updateAIL(@HeaderParam("Authorization") String authorizedToken, @HeaderParam("client_id") String clientId,
			@HeaderParam("client_secret") String clientSecret, @Valid AILRequest aRequest);

	@POST
	@Path("/apexrest/IDMSPassword")
	@Consumes("application/json")
	Response setPassword(@HeaderParam("Authorization") String authorizedToken,
			@HeaderParam("client_id") String clientId, @HeaderParam("client_secret") String clientSecret,
			SetPasswordRequest setPasswordRequest);

	@PUT
	@Path("/apexrest/ResendPinCode")
	@Consumes("application/json")
	Response resendPIN(@HeaderParam("Authorization") String token, @Valid ResendPinRequest resendPinRequest);

	@PUT
	@Path("/apexrest/ActivateUser")
	@Consumes("application/json")
	Response activateUser(@HeaderParam("Authorization") String token, @HeaderParam("client_id") String clientId,
			@HeaderParam("client_secret") String clientSecret, @Valid ActivateUserRequest activateUserRequest);

	@POST
	@Path("/apexrest/ActivateToken")
	@Consumes("application/json")
	Response activateToken(@HeaderParam("iPlanetDirectoryPro") String userTokenId);

	@GET
	@Path("/apexrest/GetIDMSUser/{loginIdentifier}")
	Response getUserByLoginIdentifier(@PathParam("loginIdentifier") String loginIdentifier);

	@GET
	@Path("/oauth2/userinfo/ui")
	Response getUserByOauthFromUI(@HeaderParam("Authorization") String token, @HeaderParam("Appname") String appName);

	@GET
	@Path("/apexrest/ActivateBulkUser")
	@Consumes("application/json")
	Response activateBulkUser();

	@PUT
	@Path("/apexrest/SendInvitation")
	@Consumes("application/json")
	Response sendInvitation(@HeaderParam("Authorization") String token,
			@Valid SendInvitationRequest sendInvitaionRequest);

	@POST
	@Path("/apexrest/resendRegEmail")
	@Consumes("application/json")
	Response resendRegEmail(@Valid ResendRegEmailRequest resendRegEmail);

	@POST
	@Path("/apexrest/IdmsIdpChaining")
	@Consumes("application/x-www-form-urlencoded")
	Response idmsIdpChaning(@FormParam("IDToken1") String idToken1, @FormParam("IDToken2") String idToken2,
			@FormParam("IDButton") String idButton, @FormParam("goto") String gotoUrl,
			@FormParam("gotoOnFail") String gotoOnFail, @FormParam("SunQueryParamsString") String sunQueryParamsString,
			@FormParam("encoded") String encoded, @FormParam("errorMessage") String errorMessage,
			@FormParam("gx_charset") String gxCharset);

	@POST
	@Path("/apexrest/resendChangeEmail")
	@Consumes("application/json")
	Response resendChangeEmail(@Valid ResendEmailChangeRequest request);

	@POST
	@Path("/apexrest/initSocialLogin")
	@Consumes("application/json")
	Response initSocialLogin(@QueryParam("service") String service);

	@POST
	@Path("/apexrest/transliterator")
	@Consumes("application/json")
	@Produces("application/json")
	Response transliterator(String jsonAsString);

	@POST
	@Path("/apexrest/IdmsDirectLogin")
	@Consumes("application/x-www-form-urlencoded")
	Response idmsDirectLogin(@FormParam("startUrl") String startUrl, @FormParam("IDToken1") String idToken1,
			@FormParam("IDToken2") String idToken2, @FormParam("submitted") String submitted,
			@FormParam("loginbutton") String loginbutton);

	@POST
	@Path("/apexrest/sendRemainderEmail")
	@Consumes("application/json")
	Response sendRemainderEmail(List<String> remainderUsersForActivation);

	@POST
	@Path("/apexrest/transliterator/2.0")
	@Consumes("application/json")
	@Produces("application/json;**charset=UTF-8**")
	Response transliteratorConversion(String jsonAsString);

	@POST
	@Path("/apexrest/oauth2iplanet")
	@Consumes("application/json")
	Response oauthToIplanet(@HeaderParam("Authorization") String token);

	@POST
	@Path("/apexrest/IDMSUserService")
	@Consumes("application/json")
	Response userRegistration_4_1(@HeaderParam("client_id") String clientId,
			@HeaderParam("client_secret") String clientSecret, @Valid CreateUserRequest userRequest);

	@PUT
	@Path("/apexrest/IDMSUserService")
	@Consumes("application/json")
	Response updateIDMSUserService(@HeaderParam("Authorization") String authorizedToken,
			@HeaderParam("client_id") String clientId, @HeaderParam("client_secret") String clientSecret,
			UpdateUserRequest userRequest);

	@GET
	@Path("/apexrest/oidcautodiscovery")
	@Consumes("application/json")
	Response getOIDCAutoDiscoveryConfig();

	@POST
	@Path("/apexrest/securedlogin")
	Response securedLogin(@HeaderParam("X-OpenAM-Username") String userName,
			@HeaderParam("X-OpenAM-Password") String password, @QueryParam("realm") String realm,
			@QueryParam("app") String app);

	@POST
	@Path("/apexrest/verifyPIN")
	Response verifyPIN(VerifyPinRequest pinRequest);

	@POST
	@Path("/apexrest/ssopost")
	@Consumes("application/x-www-form-urlencoded")
	Response buildQueryParam(@FormParam("RelayState") String relayState, @FormParam("SAMLRequest") String SAMLRequest,
			@HeaderParam("Content-Length") int length);

	@POST
	@Path("/apexrest/sendOTP")
	Response sendOTP(SendOTPRequest pinRequest) throws Exception;

	@POST
	@Path("/apexrest/addMobile")
	Response addMobile(AddMobileRequest addMobileRequest);

	@POST
	@Path("/apexrest/addEmail")
	Response addEmail(AddEmailRequest addEmailRequest);

	@PUT
	@Path("/apexrest/addEmail")
	Response addEmailToUser(AddEmailRequest addEmailRequest);

	@DELETE
	@Path("/apexrest/deleteMobile")
	Response deleteMobile(SendOTPRequest deleteRequest);

	@POST
	@Path("/apexrest/GetUserDetailByApplication")
	Response getUserDetailByApplication(@HeaderParam("Authorization") String authorizationToken,
			@HeaderParam("Content-Type") String type, UserDetailByApplicationRequest userDetailByApplicationRequest);
	
	@POST
	@Path("/apexrest/sendDemoMail")
	Response sendDemoMail(CheckUserIdentityRequest mailIds);
	
	@POST
	@Path("/apexrest/securedLoginNext")
	Response securedLoginNext(UserMFADataRequest userMFADataRequest);
}

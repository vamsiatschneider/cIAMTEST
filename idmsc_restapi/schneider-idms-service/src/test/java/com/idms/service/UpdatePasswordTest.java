package com.idms.service;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.when;

import javax.ws.rs.BadRequestException;
import javax.ws.rs.NotAuthorizedException;
import javax.ws.rs.NotFoundException;
import javax.ws.rs.core.Response;

import org.json.simple.JSONObject;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.cache.ehcache.EhCacheCache;

import com.idms.mapper.IdmsMapper;
import com.idms.model.CreateUserResponse;
import com.idms.model.IDMSUserResponse;
import com.idms.model.UpdatePasswordRequest;
import com.idms.product.client.OpenAMService;
import com.idms.product.client.OpenAMTokenService;
import com.idms.product.model.Attributes;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.Option;
import com.se.idms.dto.ErrorResponse;
import com.se.idms.dto.IDMSUserRecordUpdatePassword;
import com.se.idms.dto.PasswordRecoveryResponse;
import com.se.idms.dto.UpdatePasswordResponse;
import com.se.idms.dto.UserServiceResponse;
import com.se.idms.util.UserConstants;
import com.jayway.jsonpath.JsonPath;
import com.se.idms.util.ValidatingInvocationHandler;

/**
 * Test class for UpdatePassword
 * 
 * @author Pujarani Panda
 *
 */
public class UpdatePasswordTest {

	final String ROOT_URL = "http://localhost:8080/IDMS/services/apexrest/IDMSPassword";

	/**
	 * Class under test.
	 */
	@InjectMocks
	private UserService userService = new UserServiceImpl();

	@Rule
	public ExpectedException thrown = ExpectedException.none();

	@Mock
	private OpenAMService productService;
	
	@Mock
	private OpenAMTokenService openAMTokenService;

	@Mock
	private org.springframework.cache.ehcache.EhCacheCacheManager cacheManager;

	@Mock
	private EhCacheCache cache;
	
	@Mock
	private IdmsMapper mapper;
	
	@Mock
	private IDMSUserRecordUpdatePassword idmsUserRecord;
	
	@Mock
	private Attributes attributes;
	
	@Mock
	private ErrorResponse errorResponse;

	@Mock
	private UIMSUserManagerSoapService uimsUserManagerSoapService;
	
	@Mock
	private PasswordRecoveryResponse passwordEmptyResponse;
	
	@Mock
	private UserServiceResponse userResponse;

	/**
	 * Initialize mocks.
	 */
	@Before
	public void before() {
		MockitoAnnotations.initMocks(this);
		userService = ValidatingInvocationHandler.createValidatingServiceProxy(userService, UserService.class);
	}

	@Test
	public void testUpdatePasswordSuccess() {
		
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		DocumentContext productDocCtx = JsonPath.using(conf).parse(DomainMockData.EMAIL_OTP);
		UpdatePasswordRequest updatePasswordRequest = DtoMockData.buildUpdatePasswordRequest();
		updatePasswordRequest.setIDMS_Profile_update_source("IDMSWork");
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);

		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		
		when(openAMTokenService.getUserInfoByAccessToken(anyString(), anyString())).thenReturn(DomainMockData.USER_INFO);
		
		when(productService.updateUser(anyString(), anyString(), anyString())).thenReturn("");
		
		when(mapper.map(updatePasswordRequest, IDMSUserRecordUpdatePassword.class)).thenReturn(idmsUserRecord);
		
		Response response = userService.updatePassword(token, updatePasswordRequest);
		
		UpdatePasswordResponse updatePasswordResponse = (UpdatePasswordResponse)response.getEntity();
		
		assertThat("Status ", updatePasswordResponse.getStatus(), equalTo("success"));
		assertThat("Message ", updatePasswordResponse.getMessage(), equalTo("Password Updated successfully"));

	}

	@Test
	public void testUpdatePwdWhenTokenisEmpty() {
		
		String token = "";
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		DocumentContext productDocCtx = JsonPath.using(conf).parse(DomainMockData.EMAIL_OTP);
		UpdatePasswordRequest updatePasswordRequest = DtoMockData.buildUpdatePasswordRequest();
		updatePasswordRequest.setIDMS_Profile_update_source("IDMSWork");
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);

		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		
		when(openAMTokenService.getUserInfoByAccessToken(anyString(), anyString())).thenReturn(DomainMockData.USER_INFO);
		
		when(productService.updateUser(anyString(), anyString(), anyString())).thenReturn("");
		
		when(mapper.map(updatePasswordRequest, IDMSUserRecordUpdatePassword.class)).thenReturn(idmsUserRecord);
		
		Response response = userService.updatePassword(token, updatePasswordRequest);
		
		ErrorResponse errorResponse = (ErrorResponse)response.getEntity();
		
		assertThat("Status ", errorResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", errorResponse.getMessage(), equalTo("Error in Updating User Password."));

	}

	@Test
	public void testUpdatePwdWhenUpdateSourceIsEmpty() {
		
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		DocumentContext productDocCtx = JsonPath.using(conf).parse(DomainMockData.EMAIL_OTP);
		UpdatePasswordRequest updatePasswordRequest = DtoMockData.buildUpdatePasswordRequest();
		updatePasswordRequest.setIDMS_Profile_update_source("");
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);

		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		
		when(openAMTokenService.getUserInfoByAccessToken(anyString(), anyString())).thenReturn(DomainMockData.USER_INFO);
		
		when(productService.updateUser(anyString(), anyString(), anyString())).thenReturn("");
		
		when(mapper.map(updatePasswordRequest, IDMSUserRecordUpdatePassword.class)).thenReturn(idmsUserRecord);
		
		Response response = userService.updatePassword(token, updatePasswordRequest);
		
		ErrorResponse errorResponse = (ErrorResponse)response.getEntity();
		
		assertThat("Status ", errorResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", errorResponse.getMessage(), equalTo("Update source not found"));

	}

	@Test
	public void testUpdatePwdWhenNewPwdAndOldPwdIsNull() {
		
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		DocumentContext productDocCtx = JsonPath.using(conf).parse(DomainMockData.EMAIL_OTP);
		UpdatePasswordRequest updatePasswordRequest = DtoMockData.buildUpdatePasswordRequest();
		updatePasswordRequest.setIDMS_Profile_update_source("IDMSWork");
		updatePasswordRequest.setExistingPwd("");
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);

		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		
		when(openAMTokenService.getUserInfoByAccessToken(anyString(), anyString())).thenReturn(DomainMockData.USER_INFO);
		
		when(productService.updateUser(anyString(), anyString(), anyString())).thenReturn("");
		
		when(mapper.map(updatePasswordRequest, IDMSUserRecordUpdatePassword.class)).thenReturn(idmsUserRecord);
		
		Response response = userService.updatePassword(token, updatePasswordRequest);
		
		PasswordRecoveryResponse passwordEmptyResponse = (PasswordRecoveryResponse)response.getEntity();
		
		assertThat("Status ", passwordEmptyResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", passwordEmptyResponse.getMessage(), equalTo("Existing Password and  New Password are mandatory values."));

	}

	@Test
	public void testUpdatePwdWhenOldPwdAndNewPwdSame() {
		
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		DocumentContext productDocCtx = JsonPath.using(conf).parse(DomainMockData.EMAIL_OTP);
		UpdatePasswordRequest updatePasswordRequest = DtoMockData.buildUpdatePasswordRequest();
		updatePasswordRequest.setIDMS_Profile_update_source("IDMSWork");
		updatePasswordRequest.setExistingPwd("Welcome123");
		updatePasswordRequest.setNewPwd("Welcome123");
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);

		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		
		when(openAMTokenService.getUserInfoByAccessToken(anyString(), anyString())).thenReturn(DomainMockData.USER_INFO);
		
		when(productService.updateUser(anyString(), anyString(), anyString())).thenReturn("");
		
		when(mapper.map(updatePasswordRequest, IDMSUserRecordUpdatePassword.class)).thenReturn(idmsUserRecord);
		
		Response response = userService.updatePassword(token, updatePasswordRequest);
		
		ErrorResponse errorResponse = (ErrorResponse)response.getEntity();
		
		assertThat("Status ", errorResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", errorResponse.getMessage(), equalTo(UserConstants.UPDATE_PR_EQUAL));

	}

	@Test
	public void testUpdatePwdWhenNewPwdPRREGEXIsMatched() {
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		DocumentContext productDocCtx = JsonPath.using(conf).parse(DomainMockData.EMAIL_OTP);
		UpdatePasswordRequest updatePasswordRequest = DtoMockData.buildUpdatePasswordRequest();
		updatePasswordRequest.setIDMS_Profile_update_source("IDMSWork");
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);

		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		
		when(openAMTokenService.getUserInfoByAccessToken(anyString(), anyString())).thenReturn(DomainMockData.USER_INFO);
		
		when(productService.updateUser(anyString(), anyString(), anyString())).thenReturn("");
		
		when(mapper.map(updatePasswordRequest, IDMSUserRecordUpdatePassword.class)).thenReturn(idmsUserRecord);
		
		Response response = userService.updatePassword(token, updatePasswordRequest);
		
		UpdatePasswordResponse updatePasswordResponse = (UpdatePasswordResponse)response.getEntity();
		
		assertThat("Status ", updatePasswordResponse.getStatus(), equalTo("success"));
		assertThat("Message ", updatePasswordResponse.getMessage(), equalTo("Password Updated successfully"));

	}

	@Test
	public void testUpdatePwdWhenNewPwdPRREGEXIsNotMatched() {
		
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		DocumentContext productDocCtx = JsonPath.using(conf).parse(DomainMockData.EMAIL_OTP);
		UpdatePasswordRequest updatePasswordRequest = DtoMockData.buildUpdatePasswordRequest();
		updatePasswordRequest.setIDMS_Profile_update_source("IDMSWork");
		updatePasswordRequest.setNewPwd("password");
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);

		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		
		when(openAMTokenService.getUserInfoByAccessToken(anyString(), anyString())).thenReturn(DomainMockData.USER_INFO);
		
		when(productService.updateUser(anyString(), anyString(), anyString())).thenReturn("");
		
		when(mapper.map(updatePasswordRequest, IDMSUserRecordUpdatePassword.class)).thenReturn(idmsUserRecord);
		
		Response response = userService.updatePassword(token, updatePasswordRequest);
		
		ErrorResponse errorResponse = (ErrorResponse)response.getEntity();
		
		assertThat("Status ", errorResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", errorResponse.getMessage(), equalTo("Password does not match with password policy."));

	}
	
	@Test
	public void testUpdatePwdWhenWhenaThrowsNotAuthorizedExceptionException() {
		
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		DocumentContext productDocCtx = JsonPath.using(conf).parse(DomainMockData.EMAIL_OTP);
		UpdatePasswordRequest updatePasswordRequest = DtoMockData.buildUpdatePasswordRequest();
		updatePasswordRequest.setIDMS_Profile_update_source("IDMSWork");
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenThrow(new NotAuthorizedException(""));

		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		
		when(openAMTokenService.getUserInfoByAccessToken(anyString(), anyString())).thenReturn(DomainMockData.USER_INFO);
		
		when(productService.updateUser(anyString(), anyString(), anyString())).thenReturn("");
		
		when(mapper.map(updatePasswordRequest, IDMSUserRecordUpdatePassword.class)).thenReturn(idmsUserRecord);
		
		Response response = userService.updatePassword(token, updatePasswordRequest);
		
		UserServiceResponse userResponse = (UserServiceResponse)response.getEntity();
		
		assertThat("Status ", userResponse.getStatus(), equalTo("INVALID_SESSION_ID"));
		assertThat("Message ", userResponse.getMessage(), equalTo("Session expired or invalid"));

	}

	@Test
	public void testUpdatePwdWhenAuthenticateUserThrowsException() {
		
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		DocumentContext productDocCtx = JsonPath.using(conf).parse(DomainMockData.EMAIL_OTP);
		UpdatePasswordRequest updatePasswordRequest = DtoMockData.buildUpdatePasswordRequest();
		updatePasswordRequest.setIDMS_Profile_update_source("IDMSWork");
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenThrow(new BadRequestException());

		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		
		when(openAMTokenService.getUserInfoByAccessToken(anyString(), anyString())).thenReturn(DomainMockData.USER_INFO);
		
		when(productService.updateUser(anyString(), anyString(), anyString())).thenReturn("");
		
		when(mapper.map(updatePasswordRequest, IDMSUserRecordUpdatePassword.class)).thenReturn(idmsUserRecord);
		
		Response response = userService.updatePassword(token, updatePasswordRequest);
		
		ErrorResponse errorResponse = (ErrorResponse)response.getEntity();
		
		assertThat("Status ", errorResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", errorResponse.getMessage(), equalTo("Error in Updating User Password."));

	}
	
	@Test
	public void testUpdatePwdWhenaThrowsNotFoundException() {
		
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		DocumentContext productDocCtx = JsonPath.using(conf).parse(DomainMockData.EMAIL_OTP);
		UpdatePasswordRequest updatePasswordRequest = DtoMockData.buildUpdatePasswordRequest();
		updatePasswordRequest.setIDMS_Profile_update_source("IDMSWork");
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenThrow(new NotFoundException(""));

		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		
		when(openAMTokenService.getUserInfoByAccessToken(anyString(), anyString())).thenReturn(DomainMockData.USER_INFO);
		
		when(productService.updateUser(anyString(), anyString(), anyString())).thenReturn("");
		
		when(mapper.map(updatePasswordRequest, IDMSUserRecordUpdatePassword.class)).thenReturn(idmsUserRecord);
		
		Response response = userService.updatePassword(token, updatePasswordRequest);
		
		ErrorResponse errorResponse = (ErrorResponse)response.getEntity();
		
		assertThat("Status ", errorResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", errorResponse.getMessage(), equalTo("Error in Updating User Password."));
		
	}

	/*@Ignore
	@Test
	public void testUpdateUserSuccess() {
		System.out.println("Testing Update Password user");
		given().when().get(ROOT_URL).then().statusCode(200);
	}

	@Ignore
	@Test
	public void testAuthorization() {
		System.out.println("Testing the authorization of the  user");
		Response response = given().when().get(ROOT_URL);
		assertEquals(200, response.getStatusCode());
		String json = response.asString();
		JsonPath jsonPath = new JsonPath(json);
		assertEquals("Unauthorized", jsonPath.get("Status"));
	}

	@Ignore
	@Test
	public void testUpdatePasswordValidation() {
		System.out.println("Testing the password  validity  of the  user");
		Response response = given().when().get(ROOT_URL);
		assertEquals(200, response.getStatusCode());
		String json = response.asString();
		JsonPath jsonPath = new JsonPath(json);
		assertEquals("Exception", jsonPath.get("Status"));
	}

	@Ignore
	@Test
	public void testUpdatePassword() {
		System.out.println("Testing the password  update successfully");
		Response response = given().when().get(ROOT_URL);
		assertEquals(200, response.getStatusCode());
		String json = response.asString();
		JsonPath jsonPath = new JsonPath(json);
		assertEquals("success", jsonPath.get("Status"));
	}*/

	public void setCacheManager(org.springframework.cache.ehcache.EhCacheCacheManager cacheManager) {
		this.cacheManager = cacheManager;
	}

}

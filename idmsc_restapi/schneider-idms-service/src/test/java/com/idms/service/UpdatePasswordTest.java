package com.idms.service;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;

import javax.ws.rs.BadRequestException;
import javax.ws.rs.NotAuthorizedException;
import javax.ws.rs.NotFoundException;
import javax.ws.rs.core.Response;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.cache.ehcache.EhCacheCache;
import org.springframework.http.HttpStatus;

import com.idms.mapper.IdmsMapper;
import com.idms.model.UpdatePasswordRequest;
import com.idms.product.client.OpenAMService;
import com.idms.product.client.OpenAMTokenService;
import com.idms.product.model.Attributes;
import com.se.idms.cache.validate.IValidator;
import com.se.idms.cache.validate.impl.LengthValidatorImpl;
import com.se.idms.cache.validate.impl.PickListValidatorImpl;
import com.se.idms.dto.ErrorResponse;
import com.se.idms.dto.IDMSUserRecordUpdatePassword;
import com.se.idms.dto.PasswordRecoveryResponse;
import com.se.idms.dto.UserServiceResponse;
import com.se.idms.util.UserConstants;
import com.se.idms.util.ValidatingInvocationHandler;

/**
 * Test class for UpdatePassword
 * 
 * @author Pujarani Panda
 *
 */
public class UpdatePasswordTest {

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
	private IValidator pickListValidator = new PickListValidatorImpl();
	
	@Mock
	private IValidator multiPickListValidator;

	@Mock
	private IValidator legthValidator = new LengthValidatorImpl();
	
	@Mock
	private IDMSUserRecordUpdatePassword idmsUserRecord;
	
	@Mock
	private Attributes attributes;
	
	@Mock
	private ErrorResponse errorResponse;

	@Mock
	private UIMSUserManagerSoapService uimsUserManagerSoapService;
	
	@Mock
	private UimsSetPasswordSoapService uimsSetPasswordSoapService;
	
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
		legthValidator = ValidatingInvocationHandler.createValidatingServiceProxy(legthValidator, IValidator.class);
		pickListValidator = ValidatingInvocationHandler.createValidatingServiceProxy(pickListValidator,
				IValidator.class);
	}

	@Test
	public void testUpdatePwd_Success_UIMSSync() throws Exception {
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		String adminAuthToken = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		UpdatePasswordRequest updatePasswordRequest = DtoMockData.buildUpdatePasswordRequest();
		updatePasswordRequest.setIDMS_Profile_update_source("IDMSWork");
		
		when(legthValidator.validate(anyString(),anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(openAMTokenService.getUserInfoByAccessToken(anyString(), anyString())).thenReturn(DomainMockData.USER_INFO);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		when(productService.updateUser(anyString(), anyString(), anyString())).thenReturn("");
		when(productService.updateUserForPassword(anyString(), anyString(), anyString())).thenReturn(DomainMockData.getUpdatePasswordDetails());
		when(mapper.map(updatePasswordRequest, IDMSUserRecordUpdatePassword.class)).thenReturn(idmsUserRecord);
		when(uimsSetPasswordSoapService.updateUIMSPassword(anyString(), anyString(), anyString(), anyString(), anyString(), anyString())).thenReturn(true);
		
		Response response = userService.updatePassword(adminAuthToken,token, updatePasswordRequest);
		assertEquals(HttpStatus.OK, HttpStatus.valueOf(response.getStatus()));
		UserServiceResponse updatePasswordResponse = (UserServiceResponse)response.getEntity();
		assertThat("Status ", updatePasswordResponse.getStatus(), equalTo("Success"));
		assertThat("Message ", updatePasswordResponse.getMessage(), equalTo("User Password updated successfully in IDMS China and UIMS"));
	}
	
	@Test
	public void testUpdatePwd_Success_WithoutUIMSSync() throws Exception {
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		String adminAuthToken = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		UpdatePasswordRequest updatePasswordRequest = DtoMockData.buildUpdatePasswordRequest();
		updatePasswordRequest.setIDMS_Profile_update_source("IDMSWork");
		
		when(legthValidator.validate(anyString(),anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(),anyString())).thenReturn(false);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(openAMTokenService.getUserInfoByAccessToken(anyString(), anyString())).thenReturn(DomainMockData.USER_INFO);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		when(productService.updateUser(anyString(), anyString(), anyString())).thenReturn("");
		when(productService.updateUserForPassword(anyString(), anyString(), anyString())).thenReturn(DomainMockData.getUpdatePasswordDetails());
		when(mapper.map(updatePasswordRequest, IDMSUserRecordUpdatePassword.class)).thenReturn(idmsUserRecord);
		doNothing().when(uimsUserManagerSoapService).updateUIMSPassword(anyString(), anyString(), anyString(), anyString(), anyString(), anyString());
		
		Response response = userService.updatePassword(adminAuthToken,token, updatePasswordRequest);
		assertEquals(HttpStatus.OK, HttpStatus.valueOf(response.getStatus()));
		UserServiceResponse updatePasswordResponse = (UserServiceResponse)response.getEntity();
		assertThat("Status ", updatePasswordResponse.getStatus(), equalTo("Success"));
		assertThat("Message ", updatePasswordResponse.getMessage(), equalTo("User Password updated successfully in IDMS China"));
	}

	@Test
	public void testUpdatePwd_EmptyToken() {
		String token = "";
		String adminAuthToken = "";
		UpdatePasswordRequest updatePasswordRequest = DtoMockData.buildUpdatePasswordRequest();
		updatePasswordRequest.setIDMS_Profile_update_source("IDMSWork");
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		when(openAMTokenService.getUserInfoByAccessToken(anyString(), anyString())).thenReturn(DomainMockData.USER_INFO);
		when(productService.updateUser(anyString(), anyString(), anyString())).thenReturn("");
		when(mapper.map(updatePasswordRequest, IDMSUserRecordUpdatePassword.class)).thenReturn(idmsUserRecord);
		
		Response response = userService.updatePassword(adminAuthToken,token, updatePasswordRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse errorResponse = (ErrorResponse)response.getEntity();
		assertThat("Status ", errorResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", errorResponse.getMessage(), equalTo("Authorization token is null or missing"));
	}

	@Test
	public void testUpdatePwd_EmptyUpdateSource() {
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		String adminAuthToken = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		UpdatePasswordRequest updatePasswordRequest = DtoMockData.buildUpdatePasswordRequest();
		updatePasswordRequest.setIDMS_Profile_update_source("");
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		when(openAMTokenService.getUserInfoByAccessToken(anyString(), anyString())).thenReturn(DomainMockData.USER_INFO);
		when(productService.updateUser(anyString(), anyString(), anyString())).thenReturn("");
		when(mapper.map(updatePasswordRequest, IDMSUserRecordUpdatePassword.class)).thenReturn(idmsUserRecord);
		
		Response response = userService.updatePassword(adminAuthToken,token, updatePasswordRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse errorResponse = (ErrorResponse)response.getEntity();
		assertThat("Status ", errorResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", errorResponse.getMessage(), equalTo("Update source not found"));
	}

	@Test
	public void testUpdatePwd_NullOldAndNewPwd() {
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		String adminAuthToken = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		UpdatePasswordRequest updatePasswordRequest = DtoMockData.buildUpdatePasswordRequest();
		updatePasswordRequest.setIDMS_Profile_update_source("IDMSWork");
		updatePasswordRequest.setExistingPwd("");
		updatePasswordRequest.setNewPwd("");
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		when(openAMTokenService.getUserInfoByAccessToken(anyString(), anyString())).thenReturn(DomainMockData.USER_INFO);
		when(productService.updateUser(anyString(), anyString(), anyString())).thenReturn("");
		when(mapper.map(updatePasswordRequest, IDMSUserRecordUpdatePassword.class)).thenReturn(idmsUserRecord);
		
		Response response = userService.updatePassword(adminAuthToken,token, updatePasswordRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		PasswordRecoveryResponse passwordEmptyResponse = (PasswordRecoveryResponse)response.getEntity();
		assertThat("Status ", passwordEmptyResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", passwordEmptyResponse.getMessage(), equalTo("Existing Password and  New Password are mandatory values."));
	}

	@Test
	public void testUpdatePwd_SameOldAndNewPwd() {
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		String adminAuthToken = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
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
		
		Response response = userService.updatePassword(adminAuthToken,token, updatePasswordRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse errorResponse = (ErrorResponse)response.getEntity();
		assertThat("Status ", errorResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", errorResponse.getMessage(), equalTo(UserConstants.UPDATE_PR_EQUAL));
	}

	@Test
	public void testUpdatePwd_PwdComplexityNotMet() {
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		String adminAuthToken = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		UpdatePasswordRequest updatePasswordRequest = DtoMockData.buildUpdatePasswordRequest();
		updatePasswordRequest.setIDMS_Profile_update_source("IDMSWork");
		updatePasswordRequest.setNewPwd("password");
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		when(openAMTokenService.getUserInfoByAccessToken(anyString(), anyString())).thenReturn(DomainMockData.USER_INFO);
		when(productService.updateUser(anyString(), anyString(), anyString())).thenReturn("");
		when(mapper.map(updatePasswordRequest, IDMSUserRecordUpdatePassword.class)).thenReturn(idmsUserRecord);
		
		Response response = userService.updatePassword(adminAuthToken,token, updatePasswordRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse errorResponse = (ErrorResponse)response.getEntity();
		assertThat("Status ", errorResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", errorResponse.getMessage(), equalTo("Password does not match with password policy."));
	}
	
	@Test
	public void testUpdatePwd_NotAuthorizedException() {
		
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		String adminAuthToken = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		UpdatePasswordRequest updatePasswordRequest = DtoMockData.buildUpdatePasswordRequest();
		updatePasswordRequest.setIDMS_Profile_update_source("IDMSWork");
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenThrow(new NotAuthorizedException(""));
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		when(openAMTokenService.getUserInfoByAccessToken(anyString(), anyString())).thenReturn(DomainMockData.USER_INFO);
		when(productService.updateUser(anyString(), anyString(), anyString())).thenReturn("");
		when(mapper.map(updatePasswordRequest, IDMSUserRecordUpdatePassword.class)).thenReturn(idmsUserRecord);

		Response response = userService.updatePassword(adminAuthToken,token, updatePasswordRequest);
		assertEquals(HttpStatus.UNAUTHORIZED, HttpStatus.valueOf(response.getStatus()));
		UserServiceResponse userResponse = (UserServiceResponse)response.getEntity();
		assertThat("Status ", userResponse.getStatus(), equalTo("INVALID_SESSION_ID"));
		assertThat("Message ", userResponse.getMessage(), equalTo("Session expired or invalid"));
	}

	@Test
	public void testUpdatePwd_BadRequestException() {
		
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		String adminAuthToken = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		UpdatePasswordRequest updatePasswordRequest = DtoMockData.buildUpdatePasswordRequest();
		updatePasswordRequest.setIDMS_Profile_update_source("IDMSWork");
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenThrow(new BadRequestException());
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		when(openAMTokenService.getUserInfoByAccessToken(anyString(), anyString())).thenReturn(DomainMockData.USER_INFO);
		when(productService.updateUser(anyString(), anyString(), anyString())).thenReturn("");
		when(mapper.map(updatePasswordRequest, IDMSUserRecordUpdatePassword.class)).thenReturn(idmsUserRecord);
		
		Response response = userService.updatePassword(adminAuthToken,token, updatePasswordRequest);
		assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse errorResponse = (ErrorResponse)response.getEntity();
		assertThat("Status ", errorResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", errorResponse.getMessage(), equalTo("HTTP 400 Bad Request"));
	}
	
	@Test
	public void testUpdatePwd_NotFoundException() {
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		String adminAuthToken = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		UpdatePasswordRequest updatePasswordRequest = DtoMockData.buildUpdatePasswordRequest();
		updatePasswordRequest.setIDMS_Profile_update_source("IDMSWork");
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenThrow(new NotFoundException(""));
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		when(openAMTokenService.getUserInfoByAccessToken(anyString(), anyString())).thenReturn(DomainMockData.USER_INFO);
		when(productService.updateUser(anyString(), anyString(), anyString())).thenReturn("");
		when(mapper.map(updatePasswordRequest, IDMSUserRecordUpdatePassword.class)).thenReturn(idmsUserRecord);

		Response response = userService.updatePassword(adminAuthToken,token, updatePasswordRequest);
		assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse errorResponse = (ErrorResponse)response.getEntity();
		assertThat("Status ", errorResponse.getStatus(), equalTo("Error"));
	}
}
package com.idms.service;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

import javax.ws.rs.BadRequestException;
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
import org.springframework.http.HttpStatus;

import com.idms.mapper.IdmsMapper;
import com.idms.model.ResendPinRequest;
import com.idms.product.client.OpenAMService;
import com.se.idms.cache.validate.IValidator;
import com.se.idms.cache.validate.impl.LengthValidatorImpl;
import com.se.idms.cache.validate.impl.PickListValidatorImpl;
import com.se.idms.util.UserConstants;
import com.se.idms.util.ValidatingInvocationHandler;

public class ResendPinTest extends PropertyVariables{

	/**
	 * Class under test.
	 */
	@InjectMocks
	private UserService userService = new UserServiceImpl();

	@Mock
	private IdmsMapper idmsMapper;

	@Mock
	private IValidator pickListValidator = new PickListValidatorImpl();

	@Mock
	private IValidator multiPickListValidator;

	@Mock
	private IValidator legthValidator = new LengthValidatorImpl();

	@Rule
	public ExpectedException thrown = ExpectedException.none();

	@Mock
	private OpenAMService productService;
	
	@Mock
	private org.springframework.cache.ehcache.EhCacheCacheManager cacheManager;
	
	@Mock
	private EhCacheCache cache;
	
	@Mock
	private SendEmail sendEmail;
	
	/**
	 * Initialize mocks.
	 */
	@Before
	public void before() {
		MockitoAnnotations.initMocks(this);
		userService = ValidatingInvocationHandler.createValidatingServiceProxy(userService, UserService.class);
	}
	
	/**
	 * Test Submitting of new pin when new user is registered provided - when
	 * pin is not expired
	 * @throws Exception 
	 */
	@Test
	public void testUserPinConfirmation() throws Exception {
		
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		String adminAuthToken = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea2";
		ResendPinRequest confirmPinRequest = DtoMockData.buildResendPinRequest();
		
		InputStream stream = new ByteArrayInputStream(DomainMockData.ENTITY.getBytes(StandardCharsets.UTF_8));
		Response otpResponse = Response.status(Response.Status.OK).entity(stream).build();
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		when(sendEmail.generateOtp(anyString())).thenReturn("1234");
		when(productService.otpAuthentication(anyString(),anyString(),anyString(), anyString(), anyString()))
		.thenReturn(otpResponse);
		
		Response response = userService.resendPIN(adminAuthToken, token, confirmPinRequest);
		assertEquals(HttpStatus.OK, HttpStatus.valueOf(response.getStatus()));
		JSONObject actualResponse = (JSONObject)response.getEntity();
		assertThat("Status ", actualResponse.get(UserConstants.STATUS), equalTo("Success"));
		assertThat("Message ", actualResponse.get(UserConstants.MESSAGE), equalTo(UserConstants.PIN_SEND_SUCCESS));
	}
	
	/**
	 * Test Submitting of resends pin when new user is provided token - when
	 * auth token throw bad request
	 */
	@Test
	public void testUserPinConfirmationWhenAuthenticateUserThrowsBadRequest() {
		
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		String adminAuthToken = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea2";
		ResendPinRequest confirmPinRequest = DtoMockData.buildResendPinRequest();
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenThrow(new BadRequestException(""));
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);

		Response response = userService.resendPIN(adminAuthToken, token, confirmPinRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		JSONObject actualResponse = (JSONObject)response.getEntity();
		assertThat("Status ", actualResponse.get(UserConstants.STATUS), equalTo(UserConstants.STATUS_FAILD));
		assertThat("Message ", actualResponse.get(UserConstants.MESSAGE), equalTo(UserConstants.ERROR_RESEND_PIN));
		assertThat(response.getStatus(),equalTo(400));
	}
	
	/**
	 * Test Submitting of resends pin when new user is provided token - when
	 * auth token throw NotFound
	 */
	@Test
	public void testUserPinConfirmation_NotFoundException() {
		
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		String adminAuthToken = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea2";
		ResendPinRequest confirmPinRequest = DtoMockData.buildResendPinRequest();
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenThrow(new NotFoundException(""));
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		
		Response response = userService.resendPIN(adminAuthToken, token, confirmPinRequest);
		assertEquals(HttpStatus.NOT_FOUND, HttpStatus.valueOf(response.getStatus()));
		JSONObject actualResponse = (JSONObject)response.getEntity();
		assertThat("Status ", actualResponse.get(UserConstants.STATUS), equalTo(UserConstants.STATUS_FAILD));
		assertThat("Message ", actualResponse.get(UserConstants.MESSAGE), equalTo(UserConstants.ERROR_RESEND_PIN));
	}
	
	/**
	 * Test Submitting of resends pin when new user is provided token - when
	 * get User throw Bad Request
	 */
	@Test
	public void testUserPinConfirmation_BadRequestException() {
		
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		String adminAuthToken = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea2";
		ResendPinRequest confirmPinRequest = DtoMockData.buildResendPinRequest();
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(productService.getUser(anyString(), anyString())).thenThrow(new BadRequestException(""));
		
		Response response = userService.resendPIN(adminAuthToken, token, confirmPinRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		JSONObject actualResponse = (JSONObject)response.getEntity();
		assertThat("Status ", actualResponse.get(UserConstants.STATUS), equalTo(UserConstants.STATUS_FAILD));
		assertThat("Message ", actualResponse.get(UserConstants.MESSAGE), equalTo(UserConstants.ERROR_RESEND_PIN));
		assertThat(response.getStatus(),equalTo(400));
	}
	
	/**
	 * Test Submitting of resends pin when new user is provided token - when
	 * get User throw Bad Request
	 */
	@Test
	public void testUserPinConfirmation_UserNotFoundException() {
		
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		String adminAuthToken = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea2";
		ResendPinRequest confirmPinRequest = DtoMockData.buildResendPinRequest();
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(productService.getUser(anyString(), anyString())).thenThrow(new NotFoundException(""));
		
		Response response = userService.resendPIN(adminAuthToken, token, confirmPinRequest);
		JSONObject actualResponse = (JSONObject)response.getEntity();
		assertThat("Status ", actualResponse.get(UserConstants.STATUS), equalTo(UserConstants.STATUS_FAILD));
		assertThat("Message ", actualResponse.get(UserConstants.MESSAGE), equalTo(UserConstants.ERROR_RESEND_PIN));
		assertThat(response.getStatus(),equalTo(404));
	}
}

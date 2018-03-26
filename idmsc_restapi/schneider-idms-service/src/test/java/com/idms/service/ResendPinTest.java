package com.idms.service;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
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

import com.idms.mapper.IdmsMapper;
import com.idms.model.ResendPinRequest;
import com.idms.product.client.OpenAMService;
import com.se.idms.cache.validate.IValidator;
import com.se.idms.cache.validate.impl.LengthValidatorImpl;
import com.se.idms.cache.validate.impl.PickListValidatorImpl;
import com.se.idms.util.UserConstants;
import com.se.idms.util.ValidatingInvocationHandler;

public class ResendPinTest implements PropertyVariables{

	/**
	 * Class under test.
	 */
	@InjectMocks
	private UserService userService = new UserServiceImpl();

	@Mock
	private IdmsMapper idmsMapper;

	@Mock
	private IValidator pickListValidator = new PickListValidatorImpl();;

	@Mock
	private IValidator multiPickListValidator;

	@Mock
	private IValidator legthValidator = new LengthValidatorImpl();

	@Rule
	public ExpectedException thrown = ExpectedException.none();

	@Mock
	private OpenAMService productService;
	
	
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
	 */
	@Test
	public void testUserPinConfirmation() {
		
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		ResendPinRequest confirmPinRequest = DtoMockData.buildResendRequPinReuest();
		
		InputStream stream = new ByteArrayInputStream(DomainMockData.ENTITY.getBytes(StandardCharsets.UTF_8));
		Response otpResponse = Response.status(Response.Status.OK).entity(stream).build();
		
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);

		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		when(productService.otpAuthentication(anyString(),anyString(),anyString(), anyString(), anyString()))
		.thenReturn(otpResponse);
		
		Response response = userService.resendPIN(token, confirmPinRequest);
		
		
		JSONObject actualResponse = (JSONObject)response.getEntity();
		
		assertThat("Status ", actualResponse.get(UserConstants.STATUS), equalTo("Success"));
		assertThat("Message ", actualResponse.get(UserConstants.MESSAGE), equalTo(UserConstants.PIN_SEND_SUCCESS));
	}
	
	/**
	 * Test Submitting of resends pin when new user is provided token - when
	 * auth token throw bad request
	 */
	@Test
	public void testUserPinConfirmationWhenAuthenticateUserThrowBadRequset() {
		
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		ResendPinRequest confirmPinRequest = DtoMockData.buildResendRequPinReuest();
		
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenThrow(new BadRequestException(""));

		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		
		/*when(productService.otpAuthentication(anyString(), anyString(), anyString(), anyString()))
		.thenReturn(DomainMockData.EMAIL_OTP);*/
		
		Response response = userService.resendPIN(token, confirmPinRequest);
		
		
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
	public void testUserPinConfirmationWhenAuthenticateUserThrowNotFound() {
		
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		ResendPinRequest confirmPinRequest = DtoMockData.buildResendRequPinReuest();
		
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenThrow(new NotFoundException(""));

		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		
		/*when(productService.otpAuthentication(anyString(), anyString(), anyString(), anyString()))
		.thenReturn(DomainMockData.EMAIL_OTP);*/
		
		Response response = userService.resendPIN(token, confirmPinRequest);
		
		
		JSONObject actualResponse = (JSONObject)response.getEntity();
		
		assertThat("Status ", actualResponse.get(UserConstants.STATUS), equalTo(UserConstants.STATUS_FAILD));
		assertThat("Message ", actualResponse.get(UserConstants.MESSAGE), equalTo(UserConstants.ERROR_RESEND_PIN));
		assertThat(response.getStatus(),equalTo(404));
	}
	
	/**
	 * Test Submitting of resends pin when new user is provided token - when
	 * get User throw Bad Request
	 */
	@Test
	public void testUserPinConfirmationWhenGetUserThrowInternalServerException() {
		
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		ResendPinRequest confirmPinRequest = DtoMockData.buildResendRequPinReuest();
		
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);

		when(productService.getUser(anyString(), anyString())).thenThrow(new BadRequestException(""));
		
		/*when(productService.otpAuthentication(anyString(), anyString(), anyString(), anyString()))
		.thenReturn(DomainMockData.EMAIL_OTP);*/
		
		Response response = userService.resendPIN(token, confirmPinRequest);
		
		
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
	public void testUserPinConfirmationWhenGetUserThrowNotFoundException() {
		
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		ResendPinRequest confirmPinRequest = DtoMockData.buildResendRequPinReuest();
		
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);

		when(productService.getUser(anyString(), anyString())).thenThrow(new NotFoundException(""));
		
		/*when(productService.otpAuthentication(anyString(), anyString(), anyString(), anyString()))
		.thenReturn(DomainMockData.EMAIL_OTP);*/
		
		Response response = userService.resendPIN(token, confirmPinRequest);
		
		
		JSONObject actualResponse = (JSONObject)response.getEntity();
		
		assertThat("Status ", actualResponse.get(UserConstants.STATUS), equalTo(UserConstants.STATUS_FAILD));
		assertThat("Message ", actualResponse.get(UserConstants.MESSAGE), equalTo(UserConstants.ERROR_RESEND_PIN));
		assertThat(response.getStatus(),equalTo(404));
	}
}

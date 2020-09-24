package com.idms.service;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.when;

import javax.ws.rs.core.Response;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.cache.ehcache.EhCacheCache;
import org.springframework.context.annotation.Bean;
import org.springframework.context.support.PropertySourcesPlaceholderConfigurer;
import org.springframework.http.HttpStatus;
import org.springframework.test.util.ReflectionTestUtils;

import com.idms.mapper.IdmsMapper;
import com.idms.model.ResendEmailChangeRequest;
import com.idms.product.client.OpenAMService;
import com.idms.product.client.OpenDjService;
import com.se.idms.cache.validate.IValidator;
import com.se.idms.cache.validate.impl.LengthValidatorImpl;
import com.se.idms.cache.validate.impl.PickListValidatorImpl;
import com.se.idms.dto.UserServiceResponse;
import com.se.idms.dto.UserServiceResponseMailCounter;
import com.se.idms.util.UserConstants;
import com.se.idms.util.ValidatingInvocationHandler;

public class ResendChangeEmailTest {
	
	/**
	 * Class under test.
	 */
	@InjectMocks
	private UserServiceImpl userService = new UserServiceImpl();

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
	private OpenDjService openDJService;
	
	@Mock
	private org.springframework.cache.ehcache.EhCacheCacheManager cacheManager;
	
	@Mock
	private EhCacheCache cache;
	
	@Mock
	private SendEmail sendEmail;
	
	@Mock
	private UserServiceResponseMailCounter userResponseMailCounter = new UserServiceResponseMailCounter() ;
	
	@Mock
	private UIMSUserManagerSoapService uimsUserManagerSoapService;

	
	/**
	 * Initialize mocks.
	 */
	@Before
	public void before() {
		MockitoAnnotations.initMocks(this);
		legthValidator = ValidatingInvocationHandler.createValidatingServiceProxy(legthValidator, IValidator.class);
		pickListValidator = ValidatingInvocationHandler.createValidatingServiceProxy(pickListValidator,
				IValidator.class);

	}

	/**
	 * Test Resend the email for updated user when user is providing required details - when
	 * pin is not expired
	 * @throws Exception 
	 */
	@Test
	public void testResendChangeEmailTest() throws Exception {

		ResendEmailChangeRequest emailChangeRequest = DtoMockData.buildResendEmailChangeRequest();
		emailChangeRequest.setNewEmail("godtestNew@mailinator.com");
		emailChangeRequest.setOldEmail("godtest33@mailinator.com");
		emailChangeRequest.setApplicationName("godigital");
		
		when(legthValidator.validate(anyString(),anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(productService.authenticateUser(anyString(), anyString(), anyString()))
				.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS_TRUE_WITH_NEWEMAIL);
		when(openDJService.getUser(anyString(), anyString(),anyString())).thenReturn(DomainMockData.getAppDetails());
		when(sendEmail.generateOtp(anyString())).thenReturn("1234");
		when(sendEmail.validatePin(anyString(),anyString())).thenReturn(true);
		ReflectionTestUtils.setField(userService, "maxEmailLimit", "5");
		when(userResponseMailCounter.getStatus()).thenReturn("Success");
		when(userResponseMailCounter.getMessage()).thenReturn(UserConstants.RESEND_REGEMAIL_SUCCESS_MESSAGE);
		
		Response response = userService.resendChangeEmail(emailChangeRequest);
		assertEquals(HttpStatus.OK, HttpStatus.valueOf(response.getStatus()));
		UserServiceResponseMailCounter actualResponse = (UserServiceResponseMailCounter) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Success"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.RESEND_REGEMAIL_SUCCESS_MESSAGE));
	}
	
	/**
	 * Test Resend the email for updated user when user is providing required details - when
	 * pin is not expired
	 * @throws Exception 
	 */
	@Test
	public void testResendChangeEmailTestWhenAppNameIsEmpty() throws Exception {

		ResendEmailChangeRequest emailChangeRequest = DtoMockData.buildResendEmailChangeRequest();
		emailChangeRequest.setApplicationName(null);
		emailChangeRequest.setNewEmail("godtestNew@mailinator.com");
		emailChangeRequest.setOldEmail("godtest33@mailinator.com");
		
		when(legthValidator.validate(anyString(),anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(productService.authenticateUser(anyString(), anyString(), anyString()))
				.thenReturn(DomainMockData.AUTHENTICATION_JSON);

		Response response = userService.resendChangeEmail(emailChangeRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		UserServiceResponse actualResponse = (UserServiceResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		System.out.println(actualResponse.getMessage());
		assertThat("Message ", actualResponse.getMessage(), equalTo("OldEmail,NewEmail and AppSource are mandatory"));
	}

	/**
	 * Test Resend the email for updated user when user is providing required details - when
	 * pin is not expired
	 * @throws Exception 
	 */
	@Test
	public void testResendChangeEmailTestWhenNewEmailIsEmpty() throws Exception {

		ResendEmailChangeRequest emailChangeRequest = DtoMockData.buildResendEmailChangeRequest();
		emailChangeRequest.setNewEmail(null);
		emailChangeRequest.setOldEmail("godtest33@mailinator.com");
		emailChangeRequest.setApplicationName("godigital");
		
		when(legthValidator.validate(anyString(),anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(productService.authenticateUser(anyString(), anyString(), anyString()))
				.thenReturn(DomainMockData.AUTHENTICATION_JSON);

		Response response = userService.resendChangeEmail(emailChangeRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		UserServiceResponse actualResponse = (UserServiceResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo("OldEmail,NewEmail and AppSource are mandatory"));
	}
	
	/**
	 * Test Resend the email for updated user when user is providing required details - when
	 * pin is not expired
	 * @throws Exception 
	 */
	@Test
	public void testResendChangeEmailTestWhenOldEmailIsEmpty() throws Exception {

		ResendEmailChangeRequest emailChangeRequest = DtoMockData.buildResendEmailChangeRequest();
		emailChangeRequest.setNewEmail("godtestNew@mailinator.com");
		emailChangeRequest.setOldEmail(null);
		emailChangeRequest.setApplicationName("godigital");
		
		when(legthValidator.validate(anyString(),anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(productService.authenticateUser(anyString(), anyString(), anyString()))
				.thenReturn(DomainMockData.AUTHENTICATION_JSON);

		Response response = userService.resendChangeEmail(emailChangeRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		UserServiceResponse actualResponse = (UserServiceResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo("OldEmail,NewEmail and AppSource are mandatory"));
	}
	
	/**
	 * Test Resend the email for updated user when user is providing required details - when
	 * pin is not expired
	 * @throws Exception 
	 */
	@Test
	public void testResendChangeEmailTestWhenOldEmailIsNotMatchingWithOpenAm() throws Exception {

		ResendEmailChangeRequest emailChangeRequest = DtoMockData.buildResendEmailChangeRequest();
		emailChangeRequest.setNewEmail("godtestNew@mailinator.com");
		emailChangeRequest.setOldEmail("godtest331@mailinator.com");
		emailChangeRequest.setApplicationName("godigital");
		
		when(legthValidator.validate(anyString(),anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(productService.authenticateUser(anyString(), anyString(), anyString()))
				.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS_TRUE_WITH_NEWEMAIL);
		Response response = userService.resendChangeEmail(emailChangeRequest);
		assertEquals(HttpStatus.UNAUTHORIZED, HttpStatus.valueOf(response.getStatus()));
		UserServiceResponse actualResponse = (UserServiceResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo("newEmail does not matched with Email given!!"));
	}

	/**
	 * Test Resend the email for updated user when user is providing required details - when
	 * pin is not expired
	 * @throws Exception 
	 */
	@Test
	public void testResendChangeEmailTestWhenOldEmailIsAvailable() throws Exception {

		ResendEmailChangeRequest emailChangeRequest = DtoMockData.buildResendEmailChangeRequest();
		emailChangeRequest.setNewEmail("godtestNew@mailinator.com");
		emailChangeRequest.setOldEmail("godtest33@mailinator.com");
		emailChangeRequest.setApplicationName("godigital");
		
		when(legthValidator.validate(anyString(),anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(productService.authenticateUser(anyString(), anyString(), anyString()))
				.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS_TRUE);

		Response response = userService.resendChangeEmail(emailChangeRequest);
		assertEquals(HttpStatus.UNAUTHORIZED, HttpStatus.valueOf(response.getStatus()));
		UserServiceResponse actualResponse = (UserServiceResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo("newEmail does not matched with Email given!!"));
	}

}

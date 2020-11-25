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
import org.springframework.http.HttpStatus;
import org.springframework.test.util.ReflectionTestUtils;

import com.idms.mapper.IdmsMapper;
import com.idms.model.ResendRegEmailRequest;
import com.idms.product.client.OpenAMService;
import com.idms.product.client.OpenDjService;
import com.se.idms.cache.validate.IValidator;
import com.se.idms.cache.validate.impl.LengthValidatorImpl;
import com.se.idms.cache.validate.impl.PickListValidatorImpl;
import com.se.idms.dto.UserServiceResponse;
import com.se.idms.dto.UserServiceResponseMailCounter;
import com.se.idms.dto.UserServiceResponseMobCounter;
import com.se.idms.util.UserConstants;
import com.se.idms.util.ValidatingInvocationHandler;

public class ResendRegEmailTest {
	
	/**
	 * Class under test.
	 */
	@InjectMocks
	private UserServiceImpl userService = new UserServiceImpl();

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
	
	@Mock
	private OpenDjService openDJService;
	
	@Mock
	private org.springframework.cache.ehcache.EhCacheCacheManager cacheManager;// = new EhCacheCacheManager();
	
	@Mock
	private EhCacheCache cache;
	
	@Mock
	private SendEmail sendEmail;
	
	@Mock
	private UserServiceResponseMailCounter userResponseMailCounter = new UserServiceResponseMailCounter();
	
	@Mock
	private UserServiceResponseMobCounter userResponseMobCounter;
	
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
	 * Test Resend the email for user not activated when user is providing required details - when
	 * pin is not expired
	 * @throws Exception 
	 */
	@Test
	public void testResendRegEmail_EmailIdentifier() throws Exception {

		ResendRegEmailRequest resendRegEmail = DtoMockData.buildRegEmailChangeRequest();
		resendRegEmail.setEmail("godtest33@mailinator.com");
		
		when(legthValidator.validate(anyString(),anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(productService.authenticateUser(anyString(), anyString(), anyString()))
				.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS_WITHOUT_LOGINID);
		when(openDJService.getUser(anyString(), anyString(),anyString())).thenReturn(DomainMockData.getAppDetails());
		when(sendEmail.generateOtp(anyString())).thenReturn("1234");
		when(sendEmail.validatePin(anyString(),anyString())).thenReturn(true);
		ReflectionTestUtils.setField(userService, "maxEmailLimit", "5");
		when(userResponseMailCounter.getStatus()).thenReturn("Success");
		when(userResponseMailCounter.getMessage()).thenReturn(UserConstants.RESEND_REGEMAIL_SUCCESS_MESSAGE);
		
		Response response = userService.resendRegEmail(resendRegEmail);
		assertEquals(HttpStatus.OK, HttpStatus.valueOf(response.getStatus()));
		UserServiceResponseMailCounter actualResponse = (UserServiceResponseMailCounter) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Success"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.RESEND_REGEMAIL_SUCCESS_MESSAGE));
	}
	
	@Test
	public void testResendRegEmail_AlreadyRegisteredEmailUser() throws Exception {

		ResendRegEmailRequest resendRegEmail = DtoMockData.buildRegEmailChangeRequest();
		resendRegEmail.setEmail("godtest33@mailinator.com");
		
		when(legthValidator.validate(anyString(),anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(productService.authenticateUser(anyString(), anyString(), anyString()))
				.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS_TRUE);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		when(sendEmail.generateOtp(anyString())).thenReturn("1234");
		when(sendEmail.validatePin(anyString(),anyString())).thenReturn(true);

		Response response = userService.resendRegEmail(resendRegEmail);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		UserServiceResponse actualResponse = (UserServiceResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.ALREADY_REGISTERED_MESSAGE));
	}
	
	/**
	 * Test Resend the email for Create User user when user is providing required details - when
	 * pin is not expired
	 * @throws Exception 
	 */
	@Test
	public void testResendChangeEmail_EmailIdentifier_EmptyEmail() throws Exception {

		ResendRegEmailRequest resendRegEmail = DtoMockData.buildRegEmailChangeRequest();
		resendRegEmail.setEmail("");
		
		Response response = userService.resendRegEmail(resendRegEmail);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		UserServiceResponse actualResponse = (UserServiceResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo("Email or mobile is mandatory"));
	}
	
	/**
	 * Test Resend the email for user not activated when user is providing required details - when
	 * pin is not expired
	 * @throws Exception 
	 */
	@Test
	public void testResendRegEmail_MobileIdentifier() throws Exception {

		ResendRegEmailRequest resendRegEmail = DtoMockData.buildRegEmailChangeRequest();
		resendRegEmail.setEmail("");
		resendRegEmail.setMobile("13455576554");
		
		when(legthValidator.validate(anyString(),anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(productService.authenticateUser(anyString(), anyString(), anyString()))
				.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.GET_INACTIVE_MOBILE_USER);
		when(openDJService.getUser(anyString(), anyString(),anyString())).thenReturn(DomainMockData.getAppDetails());
		when(sendEmail.generateOtp(anyString())).thenReturn("1234");
		when(sendEmail.validatePin(anyString(),anyString())).thenReturn(true);
		ReflectionTestUtils.setField(userService, "maxMobLimit", "5");
		when(userResponseMobCounter.getStatus()).thenReturn("Success");
		when(userResponseMobCounter.getMessage()).thenReturn(UserConstants.RESEND_REGEMOB_SUCCESS_MESSAGE);
		
		Response response = userService.resendRegEmail(resendRegEmail);
		assertEquals(HttpStatus.OK, HttpStatus.valueOf(response.getStatus()));
		UserServiceResponseMobCounter actualResponse = (UserServiceResponseMobCounter) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Success"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.RESEND_REGEMOB_SUCCESS_MESSAGE));
	}
	
	@Test
	public void testResendRegEmail_AlreadyRegisteredMobileUser() throws Exception {

		ResendRegEmailRequest resendRegEmail = DtoMockData.buildRegEmailChangeRequest();
		resendRegEmail.setEmail("");
		resendRegEmail.setMobile("13455576554");
		
		when(legthValidator.validate(anyString(),anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(productService.authenticateUser(anyString(), anyString(), anyString()))
				.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.MOBILE_USER_EXISTS);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_MOBILE_USER);
		when(sendEmail.generateOtp(anyString())).thenReturn("1234");
		when(sendEmail.validatePin(anyString(),anyString())).thenReturn(true);

		Response response = userService.resendRegEmail(resendRegEmail);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		UserServiceResponse actualResponse = (UserServiceResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.ALREADY_REGISTERED_MESSAGE));
	}
	
	/**
	 * Test Resend the email for Create User user when user is providing required details - when
	 * pin is not expired
	 * @throws Exception 
	 */
	@Test
	public void testResendChangeEmail_EmailIdentifier_EmptyMobile() throws Exception {

		ResendRegEmailRequest resendRegEmail = DtoMockData.buildRegEmailChangeRequest();
		resendRegEmail.setEmail(null);
		resendRegEmail.setMobile("");

		Response response = userService.resendRegEmail(resendRegEmail);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		UserServiceResponse actualResponse = (UserServiceResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo("Email or mobile is mandatory"));
	}
}
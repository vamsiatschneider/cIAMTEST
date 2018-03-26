package com.idms.service;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
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

import com.idms.mapper.IdmsMapper;
import com.idms.model.ResendEmailChangeRequest;
import com.idms.model.ResendRegEmailRequest;
import com.idms.product.client.OpenAMService;
import com.se.idms.cache.validate.IValidator;
import com.se.idms.cache.validate.impl.LengthValidatorImpl;
import com.se.idms.cache.validate.impl.PickListValidatorImpl;
import com.se.idms.dto.UserServiceResponse;
import com.se.idms.util.UserConstants;
import com.se.idms.util.ValidatingInvocationHandler;

public class ResendRegEmailTest {
	
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
	
	@Mock
	private org.springframework.cache.ehcache.EhCacheCacheManager cacheManager;// = new EhCacheCacheManager();
	
	@Mock
	private EhCacheCache cache;
	
	@Mock
	private SendEmail sendEmail;
	
	@Mock
	private UIMSUserManagerSoapService uimsUserManagerSoapService;

	
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

	/**
	 * Test Resend the email for Create user user when user is providing required details - when
	 * pin is not expired
	 * @throws Exception 
	 */
	@Test
	public void testResendRegEmailTest() throws Exception {

		ResendRegEmailRequest resendRegEmail = DtoMockData.buildRegEmailChangeRequest();
		
		resendRegEmail.setFirstName("godigital");
		resendRegEmail.setLastName("china");
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

		UserServiceResponse actualResponse = (UserServiceResponse) response.getEntity();

		assertThat("Status ", actualResponse.getStatus(), equalTo("Success"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.RESEND_REGEMAIL_SUCCESS_MESSAGE));
	}
	
	/**
	 * Test Resend the email for updated user when user is providing required details - when
	 * pin is not expired
	 * @throws Exception 
	 */
	@Test
	public void testResendChangeEmailTestWhenFirstNameIsEmpty() throws Exception {

		ResendRegEmailRequest resendRegEmail = DtoMockData.buildRegEmailChangeRequest();
		
		resendRegEmail.setFirstName(null);
		resendRegEmail.setLastName("china");
		resendRegEmail.setEmail("godtest33@mailinator.com");
		
		when(legthValidator.validate(anyString(),anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(),anyString())).thenReturn(true);

		when(productService.authenticateUser(anyString(), anyString(), anyString()))
				.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS_TRUE_WITH_NEWEMAIL);

		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		
		when(sendEmail.generateOtp(anyString())).thenReturn("1234");
		
		when(sendEmail.validatePin(anyString(),anyString())).thenReturn(true);

		Response response = userService.resendRegEmail(resendRegEmail);

		UserServiceResponse actualResponse = (UserServiceResponse) response.getEntity();

		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo("Email, FirstNmae and LastName are mandatory"));
	}

	/**
	 * Test Resend the email for updated user when user is providing required details - when
	 * pin is not expired
	 * @throws Exception 
	 */
	@Test
	public void testResendChangeEmailTestWhenLastNameIsEmpty() throws Exception {

		ResendRegEmailRequest resendRegEmail = DtoMockData.buildRegEmailChangeRequest();
		
		resendRegEmail.setFirstName("godigital");
		resendRegEmail.setLastName(null);
		resendRegEmail.setEmail("godtest33@mailinator.com");
		
		when(legthValidator.validate(anyString(),anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(),anyString())).thenReturn(true);

		when(productService.authenticateUser(anyString(), anyString(), anyString()))
				.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS_TRUE_WITH_NEWEMAIL);

		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		
		when(sendEmail.generateOtp(anyString())).thenReturn("1234");
		
		when(sendEmail.validatePin(anyString(),anyString())).thenReturn(true);

		Response response = userService.resendRegEmail(resendRegEmail);

		UserServiceResponse actualResponse = (UserServiceResponse) response.getEntity();

		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo("Email, FirstNmae and LastName are mandatory"));
	}
	
	/**
	 * Test Resend the email for Create User user when user is providing required details - when
	 * pin is not expired
	 * @throws Exception 
	 */
	@Test
	public void testResendChangeEmailTestWhenEmailIsEmpty() throws Exception {

		ResendRegEmailRequest resendRegEmail = DtoMockData.buildRegEmailChangeRequest();
		
		resendRegEmail.setFirstName("godigital");
		resendRegEmail.setLastName("China");
		resendRegEmail.setEmail(null);
		
		when(legthValidator.validate(anyString(),anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(),anyString())).thenReturn(true);

		when(productService.authenticateUser(anyString(), anyString(), anyString()))
				.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS_TRUE_WITH_NEWEMAIL);

		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		
		when(sendEmail.generateOtp(anyString())).thenReturn("1234");
		
		when(sendEmail.validatePin(anyString(),anyString())).thenReturn(true);

		Response response = userService.resendRegEmail(resendRegEmail);

		UserServiceResponse actualResponse = (UserServiceResponse) response.getEntity();

		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo("Email, FirstNmae and LastName are mandatory"));
	}
	
	
	
	/**
	 * Test Resend the email for Create User user when user is providing required details - when
	 * pin is not expired
	 * @throws Exception 
	 */
	@Test
	public void testResendChangeEmailTestWhenFirstNameIsNotMatchingWithOpenAm() throws Exception {

		ResendRegEmailRequest resendRegEmail = DtoMockData.buildRegEmailChangeRequest();
		
		resendRegEmail.setFirstName("godigital123");
		resendRegEmail.setLastName("china");
		resendRegEmail.setEmail("godtest331@mailinator.com");
		
		when(legthValidator.validate(anyString(),anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(),anyString())).thenReturn(true);

		when(productService.authenticateUser(anyString(), anyString(), anyString()))
				.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS_TRUE_WITH_NEWEMAIL);

		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		
		when(sendEmail.generateOtp(anyString())).thenReturn("1234");
		
		when(sendEmail.validatePin(anyString(),anyString())).thenReturn(true);

		Response response = userService.resendRegEmail(resendRegEmail);

		UserServiceResponse actualResponse = (UserServiceResponse) response.getEntity();

		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo("FirstName and LastName are not mached with Email given!!"));
	}

	/**
	 * Test Resend the email for updated user when user is providing required details - when
	 * pin is not expired
	 * @throws Exception 
	 */
	@Test
	public void testResendChangeEmailTestWhenOldEmailIsAvailable() throws Exception {

		ResendEmailChangeRequest emailChangeRequest = DtoMockData.buildResendEmailChangeRequest();
		
		emailChangeRequest.setFirstName("godigital");
		emailChangeRequest.setLastName("China");
		emailChangeRequest.setNewEmail("godtestNew@mailinator.com");
		emailChangeRequest.setOldEmail("godtest33@mailinator.com");
		
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

		Response response = userService.resendChangeEmail(emailChangeRequest);

		UserServiceResponse actualResponse = (UserServiceResponse) response.getEntity();

		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo("newEmail or FirstName or LastName are not mached with Email given!!"));
	}

}

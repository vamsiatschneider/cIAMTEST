package com.idms.service;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

import javax.ws.rs.BadRequestException;
import javax.ws.rs.InternalServerErrorException;
import javax.ws.rs.NotAuthorizedException;
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
import com.idms.model.UpdateUserRequest;
import com.idms.model.UpdateUserResponse;
import com.idms.product.client.OpenAMProvisionalService;
import com.idms.product.client.OpenAMService;
import com.idms.product.client.OpenAMTokenService;
import com.idms.product.model.OpenAmUserRequest;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Option;
import com.se.idms.cache.validate.IValidator;
import com.se.idms.cache.validate.impl.LengthValidatorImpl;
import com.se.idms.cache.validate.impl.PickListValidatorImpl;
import com.se.idms.dto.ParseValuesByOauthHomeWorkContextDto;
import com.se.idms.dto.UserServiceResponse;
import com.se.idms.util.EmailValidator;
import com.se.idms.util.PhoneValidator;
import com.se.idms.util.UserConstants;
import com.se.idms.util.ValidatingInvocationHandler;

public class UpdateUserTest implements PropertyVariables{

	/**
	 * Class under test.
	 */
	@InjectMocks
	private UserService userService = new UserServiceImpl();

	@Mock
	private IdmsMapper mapper;

	@Mock
	private IValidator pickListValidator = new PickListValidatorImpl();;

	@Mock
	private IValidator multiPickListValidator;

	@Mock
	private IValidator legthValidator = new LengthValidatorImpl();
	
	@Mock
	private EmailValidator emailValidator;
	
	@Mock
	private PhoneValidator phoneValidator;
	
	@Rule
	public ExpectedException thrown = ExpectedException.none();
	
	@Mock
	private OpenAMService productService;
	
	@Mock
	private OpenAMProvisionalService provisionalService;
	
	@Mock
	private OpenAMTokenService openAMTokenService;
	
	@Mock
	private org.springframework.cache.ehcache.EhCacheCacheManager cacheManager;
	
	@Mock
	private EhCacheCache cache;
	
	@Mock
	private SendEmail sendEmail;
	
	@Mock
	private UIMSUserManagerSoapService uimsUserManagerSoapService;
	
	@Mock
	private ParseValuesByOauthHomeWorkContextDto valuesByOauthHomeWorkContext;
	
	/**
	 * Initialize mocks.
	 */
	@Before
	public void before() {
		MockitoAnnotations.initMocks(this);
		userService = ValidatingInvocationHandler.createValidatingServiceProxy(userService, UserService.class);
		legthValidator = ValidatingInvocationHandler.createValidatingServiceProxy(legthValidator, IValidator.class);
		pickListValidator = ValidatingInvocationHandler.createValidatingServiceProxy(pickListValidator, IValidator.class);
		
	}
	
	/**
	 * Test creation of new task when taskQueueName and taskSubQueueName are
	 * provided - when no duplicate task exists.
	 * @throws Exception 
	 */
	@Test
	public void testUpdateUser() throws Exception {
		
		/*PMD Violation UnusedLocalVariable
		 * Configuration conf =
		 * Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		 * 
		 * DocumentContext productDocCtx =
		 * JsonPath.using(conf).parse(DomainMockData.EMAIL_OTP);
		 * 
		 * InputStream stream = new
		 * ByteArrayInputStream(DomainMockData.ENTITY.getBytes(StandardCharsets.UTF_8));
		 * Response otpResponse =
		 * Response.status(Response.Status.OK).entity(stream).build();
		 */
		
		// Setup
		UpdateUserRequest userRequest = DtoMockData.buildUpdateUserRequset();
		OpenAmUserRequest openAmReq = DtoMockData.buildUserRegistrationOpenAmRequset();
		
		when(emailValidator.validate(anyString())).thenReturn(true);
		when(legthValidator.validate(anyString(),anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(),anyString())).thenReturn(true);
		
		//Mocking Id 
		
		//when(Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build()).thenReturn(conf);
		
		when(phoneValidator.validate(anyString())).thenReturn(true);
		
		when(mapper.map(userRequest, OpenAmUserRequest.class)).thenReturn(openAmReq);
		
		when(openAMTokenService.getUserInfoByAccessToken(anyString(), anyString())).thenReturn(DomainMockData.USER_INFO);
		
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);
		
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS);
		
		when(productService.updateUser(anyString(), anyString(),anyString())).thenReturn(DomainMockData.JSON_REQUEST);
		
		when(provisionalService.userRegistration(anyString(), anyString(), anyString())).thenReturn(DomainMockData.PROVISIONAL_USER_REGISTRATION);
		
		//when(provisionalService.otpAuthentication(anyString(),anyString(), anyString(), anyString(), anyString())).thenReturn(otpResponse);
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		
		when(sendEmail.generateOtp(anyString())).thenReturn(anyString());
		
		Response response = userService.updateUser("Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20",null,null, userRequest);
		
		UpdateUserResponse actualResponse = (UpdateUserResponse)response.getEntity();
		
		assertThat("Status ", actualResponse.getStatus(), equalTo("Success"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.UPDATE_USER_SUCCESS_MESSAGE));
		
		
	}
	
	/**
	 * Test creation of new task when taskQueueName and taskSubQueueName are
	 * provided - when no duplicate task exists.
	 */
	@Test
	public void testUpdateUserWhenUserNotSendingAuthorizationToken() {
		
		/*PMD Violation UnusedLocalVariable
		 * Configuration conf =
		 * Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		 * 
		 * DocumentContext productDocCtx =
		 * JsonPath.using(conf).parse(DomainMockData.EMAIL_OTP);
		 */
		
		// Setup
		UpdateUserRequest userRequest = DtoMockData.buildUpdateUserRequset();
		
		OpenAmUserRequest openAmReq = DtoMockData.buildUserRegistrationOpenAmRequset();
		
		when(emailValidator.validate(anyString())).thenReturn(true);
		when(legthValidator.validate(anyString(),anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(phoneValidator.validate(anyString())).thenReturn(true);
		
		//Mocking Id 
		
		//when(Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build()).thenReturn(conf);
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		
		when(mapper.map(userRequest, OpenAmUserRequest.class)).thenReturn(openAmReq);
		
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);
		
		when(openAMTokenService.getUserInfoByAccessToken(anyString(), anyString())).thenThrow(new NotAuthorizedException("HTTP 401 Unauthorized",Response.Status.UNAUTHORIZED));
		
		Response response = userService.updateUser("Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20",null,null, userRequest);
		
		UserServiceResponse actualResponse = (UserServiceResponse)response.getEntity();
		
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo("Error in Updating User.  "));
		
	}
	
	/**
	 * Test creation of UpdateUser when user sent BadRequest
	 */
	@Test
	public void testUpdateUserWhenUserSendBadRequest() {
		
		// Setup
		UpdateUserRequest userRequest = DtoMockData.buildUpdateUserRequset();
		
		userRequest.getUserRecord().setIDMS_Profile_update_source_c(null);
		
		OpenAmUserRequest openAmReq = DtoMockData.buildUserRegistrationOpenAmRequset();
		
		when(emailValidator.validate(anyString())).thenReturn(true);
		when(legthValidator.validate(anyString(),anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(phoneValidator.validate(anyString())).thenReturn(true);
		
		//Mocking Id 
		
		when(mapper.map(userRequest, OpenAmUserRequest.class)).thenReturn(openAmReq);
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		
		when(openAMTokenService.getUserInfoByAccessToken(anyString(), anyString())).thenReturn(DomainMockData.USER_INFO);
		
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);
		
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		
		when(provisionalService.userRegistration(anyString(), anyString(), anyString())).thenThrow(new BadRequestException());
		
		//when(provisionalService.otpAuthentication(anyString(), anyString(), anyString(), anyString())).thenReturn(DomainMockData.EMAIL_OTP);
		
		Response response = userService.updateUser("Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20",null,null, userRequest);
		
		UserServiceResponse actualResponse = (UserServiceResponse)response.getEntity();
		
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo("Required Fields Missing - IDMS_Profile_update_source__c"));
		
	}

	/**
	 * Test creation of UpdateUser when external service return InternalServerErrorException
	 */
	@Test
	public void testUpdateUserWhenExternalServieReturnsInternalServerException() {
		
		// Setup
		UpdateUserRequest userRequest = DtoMockData.buildUpdateUserRequset();
		
		OpenAmUserRequest openAmReq = DtoMockData.buildUserRegistrationOpenAmRequset();
		
		when(emailValidator.validate(anyString())).thenReturn(true);
		when(legthValidator.validate(anyString(),anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(phoneValidator.validate(anyString())).thenReturn(true);
		
		//Mocking Id 
		
		when(mapper.map(userRequest, OpenAmUserRequest.class)).thenReturn(openAmReq);
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		
		when(openAMTokenService.getUserInfoByAccessToken(anyString(), anyString())).thenReturn(DomainMockData.USER_INFO);
		
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);
		
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS);
		
		when(provisionalService.userRegistration(anyString(), anyString(), anyString())).thenThrow(new InternalServerErrorException());
		
		//when(provisionalService.otpAuthentication(anyString(), anyString(), anyString(), anyString())).thenReturn(DomainMockData.EMAIL_OTP);
		

		Response response = userService.updateUser("Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20",null,null, userRequest);
		
		UserServiceResponse actualResponse = (UserServiceResponse)response.getEntity();
		
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo("Error in Updating User.  "));
		
	}
	
	public void setCacheManager(org.springframework.cache.ehcache.EhCacheCacheManager cacheManager) {
		this.cacheManager = cacheManager;
	}


}

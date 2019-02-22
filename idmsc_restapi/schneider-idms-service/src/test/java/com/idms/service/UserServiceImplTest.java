package com.idms.service;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import javax.ws.rs.BadRequestException;
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
import com.idms.model.CreateUserRequest;
import com.idms.model.CreateUserResponse;
import com.idms.model.IDMSUserResponse;
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
import com.se.idms.dto.ErrorResponse;
import com.se.idms.dto.UserServiceResponse;
import com.se.idms.util.EmailValidator;
import com.se.idms.util.PhoneValidator;
import com.se.idms.util.UserConstants;
import com.se.idms.util.ValidatingInvocationHandler;

public class UserServiceImplTest implements PropertyVariables{

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
	private IValidator lengthValidator = new LengthValidatorImpl();
	
	@Mock
	private static EmailValidator emailValidator;
	
	@Mock
	private PhoneValidator 	phoneValidator;

	
	@Rule
	public ExpectedException thrown = ExpectedException.none();
	
	@Mock
	private OpenAMService productService;
	
	@Mock
	private OpenAMTokenService openAMTokenService;
	
	@Mock
	private IDMSUserResponse idmsuserResponse;
	
	@Mock
	private UserServiceResponse userResponse;
	
	//@Resource//(name="cacheManager")
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
		lengthValidator = ValidatingInvocationHandler.createValidatingServiceProxy(lengthValidator, IValidator.class);
		pickListValidator = ValidatingInvocationHandler.createValidatingServiceProxy(pickListValidator, IValidator.class);
	}

	/**
	 * Test creation of new task when taskQueueName and taskSubQueueName are
	 * provided - when no duplicate task exists.
	 * @throws Exception 
	 */
	@Test
	public void testUserRegistration() throws Exception {
		
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		
		DocumentContext productDocCtx = JsonPath.using(conf).parse(DomainMockData.EMAIL_OTP);;
		// Setup
		CreateUserRequest userRequest = DtoMockData.buildUserRegistrationRequset();
		
		OpenAmUserRequest openAMRequest = DtoMockData.buildUserRegistrationOpenAmRequset();
		
		/*UserV6 userV6 = DtoMockData.buildUserV6Reuest();
		
		CompanyV3 companyV3 = DtoMockData.buildCompanyV3Reuest();*/
		
		when(lengthValidator.validate(anyString(),anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(),anyString())).thenReturn(true);
		
		//Mocking Id 
		
		
		when(phoneValidator.validate(anyString())).thenReturn(true);
		
		when(mapper.map(userRequest, OpenAmUserRequest.class)).thenReturn(openAMRequest);
		
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);
		
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS);
		
		
		InputStream is = new ByteArrayInputStream(DomainMockData.USER_REGISTRATION.getBytes());
		Response userRegRes = Response.status(Response.Status.OK).entity(is).build();
		
		when(productService.userRegistration(anyString(), anyString(), anyString())).thenReturn(userRegRes);
		
		
		when(emailValidator.validate(anyString())).thenReturn(true);
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		
		when(sendEmail.generateOtp(anyString())).thenReturn("1234");
		
		/*when(mapper.map(userRequest, CompanyV3.class)).thenReturn(companyV3);
		
		when(mapper.map(userRequest, UserV6.class)).thenReturn(userV6);
		
		CompanyV3 company = mapper.map(userRequest, CompanyV3.class);
		UserV6 identity = mapper.map(userRequest, UserV6.class);*/
		
		/*String createUIMSResponse = uimsUserManagerSoapService.createUIMSUserAndCompany(anyString(), identity,
				anyString(), company,  anyString(), anyString(), anyString(), anyString(), anyString(), userRequest);
		
		when(uimsUserManagerSoapService.createUIMSUserAndCompany(anyString(), identity,
				anyString(), company,  anyString(), anyString(), anyString(), anyString(), anyString(), userRequest)).thenReturn(anyString());*/
		
		when(mapper.map(userRequest, IDMSUserResponse.class)).thenReturn(idmsuserResponse);
		
		Response response = userService.userRegistration(anyString(),anyString(),userRequest);
		
		CreateUserResponse actualResponse = (CreateUserResponse)response.getEntity();
		
		assertThat("Status ", actualResponse.getStatus(), equalTo("Success"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.CREATE_USER_SUCCESS_MESSAGE));
		
		
	}

	/**
	 * Test creation of new task when taskQueueName and taskSubQueueName are
	 * provided - when no duplicate task exists.
	 */
	@Test
	public void testUserRegistrationWhenUserNotSendingAuthorizationToken() {
		
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		
		DocumentContext productDocCtx = JsonPath.using(conf).parse(DomainMockData.EMAIL_OTP);
		// Setup
		CreateUserRequest userRequest = DtoMockData.buildUserRegistrationRequset();
		
		OpenAmUserRequest openAmReq = DtoMockData.buildUserRegistrationOpenAmRequset();
		
		when(emailValidator.validate(anyString())).thenReturn(true);
		when(lengthValidator.validate(anyString(),anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(),anyString())).thenReturn(true);
		
		//Mocking Id 
		
		//when(Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build()).thenReturn(conf);
		when(phoneValidator.validate(anyString())).thenReturn(true);
		
		when(mapper.map(userRequest, OpenAmUserRequest.class)).thenReturn(openAmReq);
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenThrow(new BadRequestException());
		
		InputStream is = new ByteArrayInputStream(DomainMockData.USER_REGISTRATION.getBytes());
		Response userRegRes = Response.status(Response.Status.OK).entity(is).build();
		
		when(productService.userRegistration(anyString(), anyString(), anyString())).thenReturn(userRegRes);
		
		Response response = userService.userRegistration(anyString(),anyString(),userRequest);
		
		UserServiceResponse actualResponse = (UserServiceResponse)response.getEntity();
		
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo("Error in creating user."));
		
	}
	
	/**
	 * Test creation of new task when taskQueueName and taskSubQueueName are
	 * provided - when no duplicate task exists.
	 */
	@Test
	public void testUserRegistrationWhenAlreadyUserExists() {
		
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		
		DocumentContext productDocCtx = JsonPath.using(conf).parse(DomainMockData.EMAIL_OTP);;
		// Setup
		CreateUserRequest userRequest = DtoMockData.buildUserRegistrationRequset();
		
		OpenAmUserRequest openAMRequest = DtoMockData.buildUserRegistrationOpenAmRequset();
		
		when(lengthValidator.validate(anyString(),anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(phoneValidator.validate(anyString())).thenReturn(true);
		when(emailValidator.validate(anyString())).thenReturn(true);
		
		//Mocking Id 
		
		when(mapper.map(userRequest, OpenAmUserRequest.class)).thenReturn(openAMRequest);
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);
		
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS_TRUE);
		
		Response response = userService.userRegistration(anyString(),anyString(),userRequest);
		
		ErrorResponse actualResponse = (ErrorResponse)response.getEntity();
		
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo("User Already exists"));
		
	}
	
	@Test
	public void testUserRegistrationWhenFirstNameIsEmpty() {
		// Setup
		CreateUserRequest userRequest = DtoMockData.buildUserRegistrationRequset();

		when(lengthValidator.validate(anyString(), anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(phoneValidator.validate(anyString())).thenReturn(true);
		
		userRequest.getUserRecord().setFirstName(null);
		
		Response response = userService.userRegistration(anyString(),anyString(),userRequest);
		
		ErrorResponse errorResponse = (ErrorResponse)response.getEntity();
		assertThat("Status ", errorResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", errorResponse.getMessage(), equalTo("Required Fields Missing - FirstName"));
				
		
	}
	
	@Test
	public void testUserRegistrationWhenLastNameIsEmpty() {
		// Setup
		CreateUserRequest userRequest = DtoMockData.buildUserRegistrationRequset();

		when(lengthValidator.validate(anyString(), anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(phoneValidator.validate(anyString())).thenReturn(true);
		
		userRequest.getUserRecord().setLastName(null);
		Response response = userService.userRegistration(anyString(),anyString(),userRequest);
		
		ErrorResponse errorResponse = (ErrorResponse)response.getEntity();
		assertThat("Status ", errorResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", errorResponse.getMessage(), equalTo("Required Fields Missing - LastName"));
				
		
	}
	
	@Test
	public void testUserRegistrationWhenIDMSRegestrationSourceIsEmpty() {

		// Setup
		CreateUserRequest userRequest = DtoMockData.buildUserRegistrationRequset();

		when(lengthValidator.validate(anyString(), anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(phoneValidator.validate(anyString())).thenReturn(true);

		userRequest.getUserRecord().setIDMS_Registration_Source__c(null);
		Response response = userService.userRegistration(anyString(), anyString(), userRequest);

		ErrorResponse errorResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", errorResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", errorResponse.getMessage(),
				equalTo("Required Fields Missing - IDMS_Registration_Source__c"));
	}
	
	@Test
	public void testUserRegistrationWhenIDMSUserContextIsEmpty() {

		// Setup
		CreateUserRequest userRequest = DtoMockData.buildUserRegistrationRequset();

		when(lengthValidator.validate(anyString(), anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(phoneValidator.validate(anyString())).thenReturn(true);

		userRequest.getUserRecord().setIDMS_User_Context__c(null);

		Response response = userService.userRegistration(anyString(), anyString(), userRequest);

		ErrorResponse errorResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", errorResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", errorResponse.getMessage(), equalTo("Required Fields Missing - IDMS_User_Context__c"));
	}
	
	@Test
	public void testUserRegistrationWhenPasswordPolicyMismatch() {

		// Setup
		CreateUserRequest userRequest = DtoMockData.buildUserRegistrationRequset();

		when(lengthValidator.validate(anyString(), anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(phoneValidator.validate(anyString())).thenReturn(true);
		// Mocking Id

		userRequest.setPassword("abc");

		Response response = userService.userRegistration(anyString(), anyString(), userRequest);

		ErrorResponse errorResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", errorResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", errorResponse.getMessage(), equalTo(UserConstants.PR_POLICY));
	}
	
	@Test
	public void testUserRegistrationWhenClientIdAndClientSecretEmpty(){
		
		CreateUserRequest userRequest = DtoMockData.buildUserRegistrationRequset();

		when(lengthValidator.validate(anyString(), anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(phoneValidator.validate(anyString())).thenReturn(true);
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		
		userRequest.getUserRecord().setIDMS_Registration_Source__c("UIMS");
		
		Response response = userService.userRegistration(null,null,userRequest);
		
		UserServiceResponse actualResponse = (UserServiceResponse)response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo("Uims ClientId and ClientSecret are mandatory"));
		
	}
	
	public void setCacheManager(org.springframework.cache.ehcache.EhCacheCacheManager cacheManager) {
		this.cacheManager = cacheManager;
	}

}

package com.idms.service;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.Assert.assertEquals;
import static org.junit.Assume.assumeTrue;
import static org.mockito.Matchers.anyObject;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.when;
import com.se.uims.usermanager.UserV6;
import java.io.ByteArrayInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.Writer;

import javax.ws.rs.BadRequestException;
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
import org.springframework.http.HttpStatus;

import com.idms.mapper.IdmsMapper;
import com.idms.model.CreateUserRequest;
import com.idms.model.CreateUserResponse;
import com.idms.service.util.AsyncUtil;
import com.opencsv.CSVWriter;
import com.idms.model.IDMSUserResponse;
import com.idms.product.client.IFWService;
import com.idms.product.client.OpenAMService;
import com.idms.product.client.OpenAMTokenService;
import com.idms.product.client.OpenDjService;
import com.idms.service.impl.IFWTokenServiceImpl;
import com.idms.service.uims.sync.UIMSAuthenticatedUserManagerSoapServiceSync;
import com.idms.service.uims.sync.UIMSUserManagerSoapServiceSync;
import com.schneider.idms.salesforce.service.SaleforceServiceImpl;
import com.schneider.idms.salesforce.service.SalesforceSyncServiceImpl;
import com.se.idms.cache.validate.IValidator;
import com.se.idms.dto.ErrorResponse;
import com.se.idms.dto.UIMSResponse;
import com.se.idms.dto.UserServiceResponse;
import com.se.idms.util.EmailValidator;
import com.se.idms.util.PhoneValidator;
import com.se.idms.util.UserConstants;
import com.idms.product.model.OpenAmUserRequest;
import com.se.idms.cache.validate.impl.LengthValidatorImpl;
import com.se.idms.cache.validate.impl.PickListValidatorImpl;
import com.se.idms.util.ValidatingInvocationHandler;
import com.schneider.ims.service.uimsv2.CompanyV3;

public class UserServiceImplTest implements PropertyVariables{

	/**
	 * Class under test.
	 */
	@InjectMocks
	private UserService userService = new UserServiceImpl();

	@Mock
	private IdmsMapper mapper;
	
	@Mock
	private IValidator pickListValidator = new PickListValidatorImpl();

	@Mock
	private IValidator multiPickListValidator;

	@Mock
	private IValidator legthValidator = new LengthValidatorImpl();
	
	
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
	
	@Mock
	private IFWService ifwService;

	@Mock
	private UIMSCompanyManagerSoapService companyManagerSoapService;

	@Mock
	private UIMSAccessManagerSoapService uimsAccessManagerSoapService;

	@Mock
	private UimsSetPasswordSoapService uimsSetPasswordSoapService;

	@Mock
	private UIMSUserManagerSoapServiceSync uimsUserManagerSync;

	@Mock
	private UIMSAuthenticatedUserManagerSoapServiceSync uimsAuthenticatedUserManagerSoapServiceSync;

	@Mock
	private SaleforceServiceImpl datePopulationSerivce;

	@Mock
	protected OpenDjService openDJService;

	@Mock
	private SalesforceSyncServiceImpl sfSyncServiceImpl;

	@Mock
	private IFWTokenServiceImpl ifwTokenServiceImpl;
	
	

	/**
	 * Initialize mocks.
	 */
	@Before
	public void before() {
		try {
			MockitoAnnotations.initMocks(this);
			userService = ValidatingInvocationHandler.createValidatingServiceProxy(userService, UserService.class);
			legthValidator = ValidatingInvocationHandler.createValidatingServiceProxy(legthValidator, IValidator.class);
			pickListValidator = ValidatingInvocationHandler.createValidatingServiceProxy(pickListValidator, IValidator.class);
		}
		catch(Exception e) {
			e.printStackTrace();
		}
	}
	/**
	 * Test creation of new task when taskQueueName and taskSubQueueName are
	 * provided - when no duplicate task exists.
	 * @throws Exception 
	 */
	@Test
	public void testUserRegistration() throws Exception {
		
		// Setup
		CreateUserRequest userRequest = DtoMockData.buildUserRegistrationRequset();
		OpenAmUserRequest openAmReq = DtoMockData.buildUserRegistrationOpenAmRequset();
		//CompanyV3 company=DtoMockData.buildCompanyRequest();
		//UserV6 identity=DtoMockData.buildUserV6Reuest();
		when(legthValidator.validate(anyString(), anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(phoneValidator.validate(anyString())).thenReturn(true);
		when(mapper.map(userRequest, OpenAmUserRequest.class)).thenReturn(openAmReq);
		
		InputStream is = new ByteArrayInputStream(DomainMockData.USER_REGISTRATION.getBytes());
		Response userRegRes = Response.status(Response.Status.OK).entity(is).build();
		when(productService.userRegistration(anyString(), anyString(), anyString())).thenReturn(userRegRes);
		when(openDJService.getUser(anyString(), anyString(),anyString())).thenReturn(DomainMockData.getAppDetails());
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS);
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_MAIL_USER);
		//when(emailValidator.validate(anyString())).thenReturn(true);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(sendEmail.generateOtp(anyString())).thenReturn("1234");
		when(sendEmail.validatePin(anyString(),anyString())).thenReturn(true);
		when(mapper.map(userRequest, IDMSUserResponse.class)).thenReturn(idmsuserResponse);
		
		
		/*when(mapper.map(userRequest, CompanyV3.class)).thenReturn(companyV3);
		
		when(mapper.map(userRequest, UserV6.class)).thenReturn(userV6);
		
		CompanyV3 company = mapper.map(userRequest, CompanyV3.class);
		UserV6 identity = mapper.map(userRequest, UserV6.class);*/
		
		/*String createUIMSResponse = uimsUserManagerSoapService.createUIMSUserAndCompany(anyString(), identity,
				anyString(), company,  anyString(), anyString(), anyString(), anyString(), anyString(), userRequest);
		
		when(uimsUserManagerSoapService.createUIMSUserAndCompany(anyString(), identity,
				anyString(), company,  anyString(), anyString(), anyString(), anyString(), anyString(), userRequest)).thenReturn(anyString());*/
		
	
		Response response = userService.userRegistration(anyString(),anyString(),anyString(),userRequest);
		CreateUserResponse actualResponse = (CreateUserResponse)response.getEntity();
		
		assertThat("Status ", actualResponse.getStatus(), equalTo("Success"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.CREATE_USER_SUCCESS_MESSAGE));
		
		
	}

	/**
	 * Test creation of new task when taskQueueName and taskSubQueueName are
	 * provided - when no duplicate task exists.
	 */
	@Test
	public void testUserRegistrationWhenUserNotSendingAuthorizationToken() throws Exception {
	
		CreateUserRequest userRequest = DtoMockData.buildUserRegistrationRequset();
		OpenAmUserRequest openAmReq = DtoMockData.buildUserRegistrationOpenAmRequset();
		when(legthValidator.validate(anyString(), anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(phoneValidator.validate(anyString())).thenReturn(true);
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS);
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenThrow(new NotAuthorizedException("HTTP 401 Unauthorized",Response.Status.UNAUTHORIZED));
		when(openDJService.getUser(anyString(), anyString(),anyString())).thenReturn(DomainMockData.getAppDetails());
		//Mocking Id 
		Response response = userService.userRegistration(anyString(),anyString(),anyString(),userRequest);
		assertEquals(HttpStatus.UNAUTHORIZED, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse)response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo("Authorization Failed"));
		
	}
	
	/**
	 * Test creation of new task when taskQueueName and taskSubQueueName are
	 * provided - when no duplicate task exists.
	 */
	@Test
	public void testUserRegistrationWhenAlreadyUserExists() {
		
		// Setup
		CreateUserRequest userRequest = DtoMockData.buildUserRegistrationRequset();
		OpenAmUserRequest openAMRequest = DtoMockData.buildUserRegistrationOpenAmRequset();
		
		when(legthValidator.validate(anyString(),anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(phoneValidator.validate(anyString())).thenReturn(true);
		when(emailValidator.validate(anyString())).thenReturn(true);
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS_TRUE);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_MAIL_USER);
		when(openDJService.getUser(anyString(), anyString(),anyString())).thenReturn(DomainMockData.getAppDetails());
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(mapper.map(userRequest, OpenAmUserRequest.class)).thenReturn(openAMRequest);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		
		Response response = userService.userRegistration(anyString(),anyString(),anyString(),userRequest);
		ErrorResponse actualResponse = (ErrorResponse)response.getEntity();
		assertEquals(HttpStatus.CONFLICT, HttpStatus.valueOf(response.getStatus()));
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo("User Already exists"));
		
	}
	
	@Test
	public void testUserRegistrationWhenFirstNameIsEmpty() {
		// Setup
		CreateUserRequest userRequest = DtoMockData.buildUserRegistrationRequset();
		InputStream is = new ByteArrayInputStream(DomainMockData.USER_REGISTRATION.getBytes());
		Response userRegRes = Response.status(Response.Status.OK).entity(is).build();
		when(legthValidator.validate(anyString(), anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(phoneValidator.validate(anyString())).thenReturn(true);
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_MAIL_USER);
		when(openDJService.getUser(anyString(), anyString(),anyString())).thenReturn(DomainMockData.getAppDetails());
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);
		
		userRequest.getUserRecord().setFirstName(null);
		Response response = userService.userRegistration(anyString(),anyString(),anyString(),userRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse errorResponse = (ErrorResponse)response.getEntity();
		assertThat("Status ", errorResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", errorResponse.getMessage(), equalTo("Required Fields Missing - FirstName"));
				
		
	}
	
	@Test
	public void testUserRegistrationWhenLastNameIsEmpty() {
		// Setup
		CreateUserRequest userRequest = DtoMockData.buildUserRegistrationRequset();

		when(legthValidator.validate(anyString(), anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(phoneValidator.validate(anyString())).thenReturn(true);
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_MAIL_USER);
		when(openDJService.getUser(anyString(), anyString(),anyString())).thenReturn(DomainMockData.getAppDetails());
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);
		userRequest.getUserRecord().setLastName(null);
		
		Response response = userService.userRegistration(anyString(),anyString(),anyString(),userRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse errorResponse = (ErrorResponse)response.getEntity();
		assertThat("Status ", errorResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", errorResponse.getMessage(), equalTo("Required Fields Missing - LastName"));
				
		
	}
	
	@Test
	public void testUserRegistrationWhenIDMSRegistrationSourceIsEmpty() {

		CreateUserRequest userRequest = DtoMockData.buildUserRegistrationRequset();
		when(legthValidator.validate(anyString(), anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(phoneValidator.validate(anyString())).thenReturn(true);
		userRequest.getUserRecord().setIDMS_Registration_Source__c(null);
		
		Response response = userService.userRegistration(anyString(),anyString(), anyString(), userRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse errorResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", errorResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", errorResponse.getMessage(),
				equalTo("Registration source is null or empty"));
	}
	
	@Test
	public void testUserRegistrationWhenIDMSUserContextIsEmpty() {

		// Setup
		CreateUserRequest userRequest = DtoMockData.buildUserRegistrationRequset();
		when(legthValidator.validate(anyString(), anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(phoneValidator.validate(anyString())).thenReturn(true);
		when(openDJService.getUser(anyString(), anyString(),anyString())).thenReturn(DomainMockData.getAppDetails());
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS);
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);
		
		userRequest.getUserRecord().setIDMS_User_Context__c(null);
		Response response = userService.userRegistration(anyString(),anyString(), anyString(), userRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse errorResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", errorResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", errorResponse.getMessage(), equalTo("Required Fields Missing - IDMS_User_Context__c"));
	}
	
	@Test
	public void testUserRegistrationWhenPasswordPolicyMismatch() {

		// Setup
		CreateUserRequest userRequest = DtoMockData.buildUserRegistrationRequset();

		when(legthValidator.validate(anyString(), anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(phoneValidator.validate(anyString())).thenReturn(true);
		when(openDJService.getUser(anyString(), anyString(),anyString())).thenReturn(DomainMockData.getAppDetails());
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS);
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);

		userRequest.setPassword("abc");
		Response response = userService.userRegistration(anyString(),anyString(), anyString(), userRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse errorResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", errorResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", errorResponse.getMessage(), equalTo(UserConstants.PASSWORD_WITH_USER_REG_BLCOKED));
	}
	
	@Test
	public void testUserRegistrationWhenClientIdAndClientSecretEmpty(){
		
		CreateUserRequest userRequest = DtoMockData.buildUserRegistrationRequset();
		when(legthValidator.validate(anyString(), anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(phoneValidator.validate(anyString())).thenReturn(true);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_MAIL_USER);
		when(openDJService.getUser(anyString(), anyString(),anyString())).thenReturn(DomainMockData.getAppDetails());
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);
		
		userRequest.getUserRecord().setIDMS_Registration_Source__c("UIMS");
		userRequest.getUserRecord().setIDMS_Federated_ID__c("iDMS_Federated_ID__c");
		Response response = userService.userRegistration(null,null,null,userRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		UIMSResponse actualResponse = (UIMSResponse)response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getResults().getMessage(), equalTo("Uims ClientId and ClientSecret are mandatory"));
		
	}
	
	public void setCacheManager(org.springframework.cache.ehcache.EhCacheCacheManager cacheManager) {
		this.cacheManager = cacheManager;
	}

}

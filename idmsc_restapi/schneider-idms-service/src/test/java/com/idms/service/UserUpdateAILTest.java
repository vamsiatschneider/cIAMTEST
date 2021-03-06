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
import com.idms.model.AILRequest;
import com.idms.product.client.OpenAMService;
import com.idms.product.client.OpenAMTokenService;
import com.se.idms.cache.validate.IValidator;
import com.se.idms.cache.validate.impl.LengthValidatorImpl;
import com.se.idms.cache.validate.impl.PickListValidatorImpl;
import com.se.idms.dto.AILResponse;
import com.se.idms.dto.ErrorResponse;
import com.se.idms.util.UserConstants;
import com.se.idms.util.ValidatingInvocationHandler;

public class UserUpdateAILTest {

	/**
	 * Class under test.
	 */
	@InjectMocks
	private UserService userService = new UserServiceImpl();

	@Mock
	private UserServiceImpl userServiceImpl = new UserServiceImpl();
	

	@Mock
	private OpenAMTokenService openAMTokenService;
	
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
	private org.springframework.cache.ehcache.EhCacheCacheManager cacheManager;
	
	@Mock
	private EhCacheCache cache;
	
	@Mock
	private UIMSAccessManagerSoapService uimsAccessManagerSoapService;
	
	
	private String clientId ="uimsClientId";
	
	private String clientSecret ="clientSecret";
	

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
	 * Test Updating the User AIL when user provided all the details - 
	 * @throws Exception 
	 */
	@Test
	public void testUserUpdateAIL() {
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		AILRequest aRequest = DtoMockData.buildUserUpdateAILRequest();
		aRequest.getUserAILRecord().setIDMS_Profile_update_source__c("PRM");
		aRequest.getUserAILRecord().setIDMSAcl__c("PRM");
		aRequest.getUserAILRecord().setIDMSAclType__c("Application");
		aRequest.getUserAILRecord().setIDMSOperation__c("Grant");
		aRequest.getUserAILRecord().setIDMSUser__c("cn00eed94b51-1d31-4d24-948f-0a69aab4b7e0");
		when(legthValidator.validate(anyString(), anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(userServiceImpl.isAILApp(anyString(),anyString())).thenReturn(true);
		when(productService.authenticateUser(anyString(), anyString(), anyString()))
				.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(openAMTokenService.getUserDetails(anyString())).thenReturn(DomainMockData.TECHNICAL_USER);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER_AIL);
		
		Response response = userService.updateAIL(token,clientId, clientSecret,aRequest);
		assertEquals(HttpStatus.OK, HttpStatus.valueOf(response.getStatus()));
		AILResponse actualResponse = (AILResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Success"));
		assertThat("Message ", actualResponse.getMessage(), equalTo("User AIL updated successfully"));
		
	}
	
	/**
	 * Test Updating the User AIL when user provided all the details - exception updateSource
	 *  
	 */
	@Test
	public void testUserUpdateAILWhenProfileUpdateSourceIsempty() {
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		AILRequest aRequest = DtoMockData.buildUserUpdateAILRequest();
		aRequest.getUserAILRecord().setIDMS_Profile_update_source__c(null);
		aRequest.getUserAILRecord().setIDMSAcl__c("PRM");
		aRequest.getUserAILRecord().setIDMSAclType__c("Application");
		aRequest.getUserAILRecord().setIDMSOperation__c("Grant");
		aRequest.getUserAILRecord().setIDMSUser__c("cn00eed94b51-1d31-4d24-948f-0a69aab4b7e0");

		when(legthValidator.validate(anyString(), anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(productService.authenticateUser(anyString(), anyString(), anyString()))
				.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER_AIL);
		
		Response response = userService.updateAIL(token,clientId, clientSecret, aRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.MANDATORY_PROFILE_UPDATE_SOURCE));
		
	}

	/**
	 * Test Updating the User AIL when user provided all the details - except ACL_C
	 *  
	 */
	@Test
	public void testUserUpdateAILWhenACL_CIsempty() {
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		AILRequest aRequest = DtoMockData.buildUserUpdateAILRequest();
		aRequest.getUserAILRecord().setIDMS_Profile_update_source__c("PRM");
		aRequest.getUserAILRecord().setIDMSAcl__c(null);
		aRequest.getUserAILRecord().setIDMSAclType__c("Application");
		aRequest.getUserAILRecord().setIDMSOperation__c("Grant");
		aRequest.getUserAILRecord().setIDMSUser__c("cn00eed94b51-1d31-4d24-948f-0a69aab4b7e0");
		when(legthValidator.validate(anyString(), anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(productService.authenticateUser(anyString(), anyString(), anyString()))
				.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER_AIL);
		
		Response response = userService.updateAIL(token,clientId, clientSecret, aRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.MANDATORY_ACL));
		
	}
	
	/**
	 * Test Updating the User AIL when user provided all the details - except ACL_Type
	 *  
	 */
	@Test
	public void testUserUpdateAILWhenAclTypeIsempty() {
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		AILRequest aRequest = DtoMockData.buildUserUpdateAILRequest();
		aRequest.getUserAILRecord().setIDMS_Profile_update_source__c("PRM");
		aRequest.getUserAILRecord().setIDMSAcl__c("PRM");
		aRequest.getUserAILRecord().setIDMSAclType__c(null);
		aRequest.getUserAILRecord().setIDMSOperation__c("Grant");
		aRequest.getUserAILRecord().setIDMSUser__c("cn00eed94b51-1d31-4d24-948f-0a69aab4b7e0");
		when(legthValidator.validate(anyString(), anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(productService.authenticateUser(anyString(), anyString(), anyString()))
				.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER_AIL);
		when(openAMTokenService.getUserDetails(anyString())).thenReturn(DomainMockData.TECHNICAL_USER);
		
		Response response = userService.updateAIL(token,clientId, clientSecret, aRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.INVALID_ACL_TYPE));
		
	}
	
	/**
	 * Test Updating the User AIL when user provided all the details - except Operation
	 *  
	 */
	@Test
	public void testUserUpdateAILWhenOperationIsempty() {
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		AILRequest aRequest = DtoMockData.buildUserUpdateAILRequest();
		aRequest.getUserAILRecord().setIDMS_Profile_update_source__c("PRM");
		aRequest.getUserAILRecord().setIDMSAcl__c("PRM");
		aRequest.getUserAILRecord().setIDMSAclType__c("Application");
		aRequest.getUserAILRecord().setIDMSOperation__c(null);
		aRequest.getUserAILRecord().setIDMSUser__c("cn00eed94b51-1d31-4d24-948f-0a69aab4b7e0");
		when(legthValidator.validate(anyString(), anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(productService.authenticateUser(anyString(), anyString(), anyString()))
				.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER_AIL);
		when(openAMTokenService.getUserDetails(anyString())).thenReturn(DomainMockData.TECHNICAL_USER);
		
		Response response = userService.updateAIL(token,clientId, clientSecret, aRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.INVALID_OPERATION));
		
	}

	/**
	 * Test Updating the User AIL when user provided all the details - except User__C
	 *  
	 */
	@Test
	public void testUserUpdateAILWhenUser__cIsempty() {
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		AILRequest aRequest = DtoMockData.buildUserUpdateAILRequest();
		aRequest.getUserAILRecord().setIDMS_Profile_update_source__c("PRM");
		aRequest.getUserAILRecord().setIDMSAcl__c("PRM");
		aRequest.getUserAILRecord().setIDMSAclType__c("Application");
		aRequest.getUserAILRecord().setIDMSOperation__c("Grant");
		aRequest.getUserAILRecord().setIDMSUser__c(null);

		when(legthValidator.validate(anyString(), anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(productService.authenticateUser(anyString(), anyString(), anyString()))
				.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER_AIL);
		when(openAMTokenService.getUserDetails(anyString())).thenReturn(DomainMockData.TECHNICAL_USER);
		
		Response response = userService.updateAIL(token,clientId, clientSecret, aRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.MANDATORY_ID));
		
	}
	
	/**
	 * Test Updating the User AIL when user provided all the details - except User__C
	 *  
	 */
	@Test
	public void testUserUpdateAILWhenUpdateSourceIsUIMSAndUIMSCredentialsIsempty() {
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		AILRequest aRequest = DtoMockData.buildUserUpdateAILRequest();
		aRequest.getUserAILRecord().setIDMS_Profile_update_source__c("UIMS");
		aRequest.getUserAILRecord().setIDMSAcl__c("PRM");
		aRequest.getUserAILRecord().setIDMSAclType__c("Application");
		aRequest.getUserAILRecord().setIDMSOperation__c("Grant");
		aRequest.getUserAILRecord().setIDMS_Federated_ID__c("cn00eed94b51-1d31-4d24-948f-0a69aab4b7e0");
		when(legthValidator.validate(anyString(), anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(), anyString())).thenReturn(true);
		when(productService.authenticateUser(anyString(), anyString(), anyString()))
				.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER_AIL);
		
		Response response = userService.updateAIL(token ,clientId, clientSecret, aRequest);
		assertEquals(HttpStatus.UNAUTHORIZED, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.INVALID_UIMS_CREDENTIALS));
		
	}
}

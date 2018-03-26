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
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.ehcache.EhCacheCache;

import com.idms.mapper.IdmsMapper;
import com.idms.model.AILRequest;
import com.idms.product.client.OpenAMService;
import com.se.idms.cache.validate.IValidator;
import com.se.idms.cache.validate.impl.LengthValidatorImpl;
import com.se.idms.cache.validate.impl.PickListValidatorImpl;
import com.se.idms.dto.AILResponse;
import com.se.idms.dto.UserServiceResponse;
import com.se.idms.util.UserConstants;
import com.se.idms.util.ValidatingInvocationHandler;

public class UserUpdateAILTest {

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
	
	@Mock
	private UIMSAccessManagerSoapService uimsAccessManagerSoapService;
	
	private String clientId ="uimsClientId";
	
	private String clientSecret ="clientSecret";
	
	@Value("${uimsClientId}")
	private String uimsClientId;

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

		AILRequest aRequest = DtoMockData.buildUserUpdateAILRequest();

		aRequest.getUserAILRecord().setIDMS_Profile_update_source__c("PRM");
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
		
		Response response = userService.updateAIL(anyString(), anyString(), aRequest);
		
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
		
		Response response = userService.updateAIL(clientId, clientSecret, aRequest);
		
		UserServiceResponse actualResponse = (UserServiceResponse) response.getEntity();
		
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.MANDATORY_PROFILE_UPDATE_SOURCE));
		
	}

	/**
	 * Test Updating the User AIL when user provided all the details - except ACL_C
	 *  
	 */
	@Test
	public void testUserUpdateAILWhenACL_CIsempty() {

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
		
		Response response = userService.updateAIL(clientId, clientSecret, aRequest);
		
		UserServiceResponse actualResponse = (UserServiceResponse) response.getEntity();
		
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.MANDATORY_ACL));
		
	}
	
	/**
	 * Test Updating the User AIL when user provided all the details - except ACL_Type
	 *  
	 */
	@Test
	public void testUserUpdateAILWhenAclTypeIsempty() {

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
		
		Response response = userService.updateAIL(clientId, clientSecret, aRequest);
		
		UserServiceResponse actualResponse = (UserServiceResponse) response.getEntity();
		
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.INVALID_ACL_TYPE));
		
	}
	
	/**
	 * Test Updating the User AIL when user provided all the details - except Operation
	 *  
	 */
	@Test
	public void testUserUpdateAILWhenOperationIsempty() {

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
		
		Response response = userService.updateAIL(clientId, clientSecret, aRequest);
		
		UserServiceResponse actualResponse = (UserServiceResponse) response.getEntity();
		
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.INVALID_OPERATION));
		
	}

	/**
	 * Test Updating the User AIL when user provided all the details - except User__C
	 *  
	 */
	@Test
	public void testUserUpdateAILWhenUser__cIsempty() {

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
		
		Response response = userService.updateAIL(clientId, clientSecret, aRequest);
		
		UserServiceResponse actualResponse = (UserServiceResponse) response.getEntity();
		
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.MANDATORY_ID));
		
	}
	
	/**
	 * Test Updating the User AIL when user provided all the details - except User__C
	 *  
	 */
	@Test
	public void testUserUpdateAILWhenUpdateSourceIsUIMSAndFederatedIdIsempty() {

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
		
		Response response = userService.updateAIL(clientId, clientSecret, aRequest);
		
		UserServiceResponse actualResponse = (UserServiceResponse) response.getEntity();
		
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.INVALID_UIMS_CREDENTIALS));
		
	}
}

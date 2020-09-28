package com.idms.service;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.when;

import javax.ws.rs.BadRequestException;
import javax.ws.rs.NotAuthorizedException;
import javax.ws.rs.NotFoundException;
import javax.ws.rs.core.Response;

import org.json.simple.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.cache.ehcache.EhCacheCache;
import org.springframework.http.HttpStatus;

import com.idms.model.ActivateUserRequest;
import com.idms.product.client.OpenAMService;
import com.se.idms.util.UserConstants;

public class ActivateUserTest implements PropertyVariables{

	/**
	 * Class under test.
	 */
	@InjectMocks
	private UserService userService = new UserServiceImpl();

	@Mock
	private OpenAMService productService;
	
	@Mock
	private org.springframework.cache.ehcache.EhCacheCacheManager cacheManager;
	
	@Mock
	private EhCacheCache cache;
	
	@Mock
	private UIMSUserManagerSoapService uimsUserManagerSoapService;
	
	/**
	 * Initialize mocks.
	 */
	@Before
	public void before() {
		MockitoAnnotations.initMocks(this);
	}
	
	/**
	 * Test Activate user by providing id and registration source - when
	 * id is null
	 */
	@Test
	public void testActivateUserWhenIdIsNull() {
		
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		ActivateUserRequest activateUserRequest = DtoMockData.buildActivateUserRequest();
		activateUserRequest.getUserRecord().setIDMS_Registration_Source__c("IDMSWork");
		activateUserRequest.getUserRecord().setId(null);
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		when(productService.updateUser(anyString(), anyString(), anyString())).thenReturn("");

		Response response = userService.activateUser(token,null,null, activateUserRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		JSONObject actualResponse = (JSONObject)response.getEntity();
		assertThat("Status ", actualResponse.get(UserConstants.STATUS), equalTo(UserConstants.STATUS_ERROR));
		assertThat("Message ", actualResponse.get(UserConstants.MESSAGE), equalTo(UserConstants.MANDATORY_FEDERATION_ID));
	}
	
	/**
	 * Test Activate user by providing id and registration source - when
	 * IDMS_Federated_ID__c is null
	 */
	@Test
	public void testActivateUserWhenIDMS_Federated_ID__cIsNull() {
		
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		ActivateUserRequest activateUserRequest = DtoMockData.buildActivateUserRequest();
		activateUserRequest.getUserRecord().setIDMS_Registration_Source__c("UIMS");
		activateUserRequest.getUserRecord().setIDMS_Federated_ID__c(null);
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		when(productService.updateUser(anyString(), anyString(), anyString())).thenReturn("");
		
		Response response = userService.activateUser(token,null,null, activateUserRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		JSONObject actualResponse = (JSONObject)response.getEntity();
		assertThat("Status ", actualResponse.get(UserConstants.STATUS), equalTo(UserConstants.STATUS_ERROR));
		assertThat("Message ", actualResponse.get(UserConstants.MESSAGE), equalTo(UserConstants.MANDATORY_FEDERATION_ID));
	}
	

	/**
	 * Test Activate user by providing id and registration source - when
	 * IDMS_Federated_ID__c is null
	 */
	@Test
	public void testActivateUserWhenRegistration_SourceIsNull() {
		
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		ActivateUserRequest activateUserRequest = DtoMockData.buildActivateUserRequest();
		activateUserRequest.getUserRecord().setIDMS_Registration_Source__c(null);
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		when(productService.updateUser(anyString(), anyString(), anyString())).thenReturn("");
		
		Response response = userService.activateUser(token,null,null, activateUserRequest);
		JSONObject actualResponse = (JSONObject)response.getEntity();
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		assertThat("Status ", actualResponse.get(UserConstants.STATUS), equalTo(UserConstants.STATUS_ERROR));
		assertThat("Message ", actualResponse.get(UserConstants.MESSAGE), equalTo(UserConstants.REGISTRATION_SOURCE_MISSING));
	}
	
	/**
	 * Test Activate user by providing id and registration source - when
	 * AuthenticateUserThrowsException
	 */
	@Test
	public void testActivateUserWhenAuthenticateUserThrowsException() {
		
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		ActivateUserRequest activateUserRequest = DtoMockData.buildActivateUserRequest();
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenThrow(new BadRequestException());
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		when(productService.updateUser(anyString(), anyString(), anyString())).thenReturn("");
		
		Response response = userService.activateUser(token,null,null, activateUserRequest);
		JSONObject actualResponse = (JSONObject)response.getEntity();
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		assertThat("Status ", actualResponse.get(UserConstants.STATUS), equalTo(UserConstants.STATUS_ERROR));
		assertThat("Message ", actualResponse.get(UserConstants.MESSAGE), equalTo(UserConstants.USER_NOT_FOUND));
	}
	
	/**
	 * Test Activate user by providing id and registration source - when
	 * NotAuthorizedException
	 */
	@Test
	public void testActivateUser_NotFoundException() {
		
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		ActivateUserRequest activateUserRequest = DtoMockData.buildActivateUserRequest();
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenThrow(new NotAuthorizedException(""));
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		when(productService.updateUser(anyString(), anyString(), anyString())).thenReturn("");
		
		Response response = userService.activateUser(token,null,null, activateUserRequest);
		assertEquals(HttpStatus.UNAUTHORIZED, HttpStatus.valueOf(response.getStatus()));
		JSONObject actualResponse = (JSONObject)response.getEntity();
		assertThat("Status ", actualResponse.get(UserConstants.STATUS), equalTo(UserConstants.STATUS_ERROR));
		assertThat("Message ", actualResponse.get(UserConstants.MESSAGE), equalTo(UserConstants.USER_NOT_FOUND));
	}
	
	/**
	 * Test Activate user by providing clientId and  clientSecret is Null
	 * when IDMS_Registration_Source__c is "UIMS"
	 */
	@Test
	public void testActivateUserWhenClientIdAndClientSecretIsNull() {
		
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		ActivateUserRequest activateUserRequest = DtoMockData.buildActivateUserRequest();
		activateUserRequest.getUserRecord().setIDMS_Registration_Source__c("UIMS");
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenThrow(new NotFoundException(""));
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		when(productService.updateUser(anyString(), anyString(), anyString())).thenReturn("");
		
		Response response = userService.activateUser(token,null,null, activateUserRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		JSONObject actualResponse = (JSONObject)response.getEntity();
		assertThat("Status ", actualResponse.get(UserConstants.STATUS), equalTo(UserConstants.STATUS_ERROR));
		assertThat("Message ", actualResponse.get(UserConstants.MESSAGE), equalTo(UserConstants.UIMS_CLIENTID_SECRET));
	}
	
	/**
	 * Test Activate user by providing clientId and  clientSecret is Null
	 * when IDMS_Registration_Source__c is "UIMS"
	 */
	@Test
	public void testActivateUserWhenClientIdAndClientSecretIsNotNullAndPropertiesNotSet() {
		
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		ActivateUserRequest activateUserRequest = DtoMockData.buildActivateUserRequest();
		activateUserRequest.getUserRecord().setIDMS_Registration_Source__c("UIMS");
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenThrow(new NotFoundException(""));
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		when(productService.updateUser(anyString(), anyString(), anyString())).thenReturn("");
		
		Response response = userService.activateUser(token,"uimsclient","uims@idms#2017", activateUserRequest);
		assertEquals(HttpStatus.UNAUTHORIZED, HttpStatus.valueOf(response.getStatus()));
		JSONObject actualResponse = (JSONObject)response.getEntity();
		assertThat("Status ", actualResponse.get(UserConstants.STATUS), equalTo(UserConstants.STATUS_ERROR));
		assertThat("Message ", actualResponse.get(UserConstants.MESSAGE), equalTo(UserConstants.INVALID_UIMS_CREDENTIALS));
	}

	/**
	 * Test Activate user by providing id and registration source - when
	 * registration source is same with created user
	 */
	@Test
	public void testActivateUser() {
		
		String token = "Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20";
		ActivateUserRequest activateUserRequest = DtoMockData.buildActivateUserRequest();
		activateUserRequest.getUserRecord().setIDMS_Registration_Source__c("IDMSWork");
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		when(productService.updateUser(anyString(), anyString(), anyString())).thenReturn("");
		
		Response response = userService.activateUser(token,null,null, activateUserRequest);
		assertEquals(HttpStatus.OK, HttpStatus.valueOf(response.getStatus()));
		JSONObject actualResponse = (JSONObject)response.getEntity();
		assertThat("Status ", actualResponse.get(UserConstants.STATUS), equalTo("Success"));
		assertThat("Message ", actualResponse.get(UserConstants.MESSAGE), equalTo(UserConstants.USER_ACTIVATED));
	}
	
	public void setCacheManager(org.springframework.cache.ehcache.EhCacheCacheManager cacheManager) {
		this.cacheManager = cacheManager;
	}
}

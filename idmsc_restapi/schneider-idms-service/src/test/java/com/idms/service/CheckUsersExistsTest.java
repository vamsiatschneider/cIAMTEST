package com.idms.service;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Matchers.anyBoolean;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.when;

import org.json.simple.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.cache.ehcache.EhCacheCache;

import com.idms.product.client.IFWService;
import com.idms.product.client.OpenAMService;
import com.idms.product.client.SalesForceService;
import com.idms.service.impl.IFWTokenServiceImpl;
import com.schneider.idms.salesforce.service.SalesforceSyncServiceImpl;

/**
 * Test class for Check user exists endpoint
 *
 */
public class CheckUsersExistsTest {
	
	@InjectMocks
	private UserService userService = new UserServiceImpl();

	@Mock
	private OpenAMService productService;
	
	@Mock
	private org.springframework.cache.ehcache.EhCacheCacheManager cacheManager;// = new EhCacheCacheManager();
	
	@Mock
	private EhCacheCache cache;
	
	@Mock
	private IFWService ifwService;
	
	@Mock
	private SalesforceSyncServiceImpl sfSyncServiceImpl;
	
	@Mock
	private IFWTokenServiceImpl ifwTokenServiceImpl;
	
	@Mock
	private SalesForceService salesForceService;

	/**
	 * Initialize mocks.
	 */
	@Before
	public void before() {
		MockitoAnnotations.initMocks(this);
	}
	
	@Test
	public void testUserExists_Mobile_WithoutGlobalCheck() {
		
		when(productService.authenticateUser(anyString(), anyString(), anyString()))
		.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS);
		
		javax.ws.rs.core.Response response = userService.checkUserExists("13655207991", "false", null);
		JSONObject  actualResponse = (JSONObject)response.getEntity();
		assertThat("Message ", actualResponse.get("message"), equalTo("false"));
	}
	
	@Test
	public void testUserExists_Email_WithoutGlobalCheck() {
		
		when(productService.authenticateUser(anyString(), anyString(), anyString()))
		.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS);
		
		javax.ws.rs.core.Response response = userService.checkUserExists("dummy1@mailinator.com", "false", null);
		JSONObject  actualResponse = (JSONObject)response.getEntity();
		assertThat("Message ", actualResponse.get("message"), equalTo("false"));
	}
	
	@Test
	public void testUserExists_Mobile_WithGlobalCheck() {
		
		javax.ws.rs.core.Response ifwResponse = DomainMockData.get404IfwResponse();
		when(productService.authenticateUser(anyString(), anyString(), anyString()))
		.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS);
		when(ifwTokenServiceImpl.getIFWToken()).thenReturn(DomainMockData.IFW_USER);
		when(sfSyncServiceImpl.getSFToken()).thenReturn(DomainMockData.SALESFORCE_USER);
		when(ifwService.checkUserExistsWithMobile(anyString(), anyString(), anyString(), anyString(), anyString(), anyString(),
				anyString(), anyBoolean())).thenReturn(ifwResponse);
		
		javax.ws.rs.core.Response response = userService.checkUserExists("13655207991", "true", null);
		JSONObject  actualResponse = (JSONObject)response.getEntity();
		assertThat("Message ", actualResponse.get("message"), equalTo("false"));
	}
	
	@Test
	public void testUserExists_Email_WithGlobalCheck() {
		
		javax.ws.rs.core.Response ifwResponse = DomainMockData.get404IfwResponse();
		when(productService.authenticateUser(anyString(), anyString(), anyString()))
		.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS);
		when(ifwTokenServiceImpl.getIFWToken()).thenReturn(DomainMockData.IFW_USER);
		when(sfSyncServiceImpl.getSFToken()).thenReturn(DomainMockData.SALESFORCE_USER);
		when(ifwService.checkUserExistsWithMobile(anyString(), anyString(), anyString(), anyString(), anyString(), anyString(),
				anyString(), anyBoolean())).thenReturn(ifwResponse);
		
		javax.ws.rs.core.Response response = userService.checkUserExists("dummy1@mailinator.com", "true", null);
		JSONObject  actualResponse = (JSONObject)response.getEntity();
		assertThat("Message ", actualResponse.get("message"), equalTo("false"));
	}
	
	@Test
	public void testUserExists_ExistingMobile_WithoutGlobalCheck() {
		
		javax.ws.rs.core.Response ifwResponse = DomainMockData.get404IfwResponse();
		when(productService.authenticateUser(anyString(), anyString(), anyString()))
		.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS);
		when(ifwTokenServiceImpl.getIFWToken()).thenReturn(DomainMockData.IFW_USER);
		when(sfSyncServiceImpl.getSFToken()).thenReturn(DomainMockData.SALESFORCE_USER);
		when(ifwService.checkUserExistsWithMobile(anyString(), anyString(), anyString(), anyString(), anyString(), anyString(),
				anyString(), anyBoolean())).thenReturn(ifwResponse);
		
		javax.ws.rs.core.Response response = userService.checkUserExists("13655207991", "false", null);
		JSONObject  actualResponse = (JSONObject)response.getEntity();
		assertThat("Message ", actualResponse.get("message"), equalTo("false"));
	}
	
	@Test
	public void testUserExists_ExistingEmail_WithoutGlobalCheck() {
		
		javax.ws.rs.core.Response ifwResponse = DomainMockData.get404IfwResponse();
		when(productService.authenticateUser(anyString(), anyString(), anyString()))
		.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS);
		when(ifwTokenServiceImpl.getIFWToken()).thenReturn(DomainMockData.IFW_USER);
		when(sfSyncServiceImpl.getSFToken()).thenReturn(DomainMockData.SALESFORCE_USER);
		when(ifwService.checkUserExistsWithMobile(anyString(), anyString(), anyString(), anyString(), anyString(), anyString(),
				anyString(), anyBoolean())).thenReturn(ifwResponse);
		
		javax.ws.rs.core.Response response = userService.checkUserExists("dummy1@mailinator.com", "false", null);
		JSONObject  actualResponse = (JSONObject)response.getEntity();
		assertThat("Message ", actualResponse.get("message"), equalTo("false"));
	}
	
	@Test
	public void testUserExists_ExistingMobile_WithGlobalCheck() {
		
		javax.ws.rs.core.Response ifwResponse = DomainMockData.getOkIfwResponse();
		when(productService.authenticateUser(anyString(), anyString(), anyString()))
		.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS_TRUE);
		when(ifwTokenServiceImpl.getIFWToken()).thenReturn(DomainMockData.IFW_USER);
		when(sfSyncServiceImpl.getSFToken()).thenReturn(DomainMockData.SALESFORCE_USER);
		when(ifwService.checkUserExistsWithMobile(anyString(), anyString(), anyString(), anyString(), anyString(), anyString(),
				anyString(), anyBoolean())).thenReturn(ifwResponse);
		
		javax.ws.rs.core.Response response = userService.checkUserExists("13655207991", "true", null);
		JSONObject  actualResponse = (JSONObject)response.getEntity();
		assertThat("Message ", actualResponse.get("message"), equalTo("true"));
	}
	
	public void testUserExists_ExistingEmail_WithGlobalCheck() {
		
		javax.ws.rs.core.Response ifwResponse = DomainMockData.getOkIfwResponse();
		when(productService.authenticateUser(anyString(), anyString(), anyString()))
		.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS_TRUE);
		when(ifwTokenServiceImpl.getIFWToken()).thenReturn(DomainMockData.IFW_USER);
		when(sfSyncServiceImpl.getSFToken()).thenReturn(DomainMockData.SALESFORCE_USER);
		when(ifwService.checkUserExistsWithEmail(anyString(), anyString(), anyString(), anyString(), anyString(), anyString(),
				anyString(), anyBoolean())).thenReturn(ifwResponse);
		
		javax.ws.rs.core.Response response = userService.checkUserExists("dummyId@mailinator.com", "true", null);
		JSONObject  actualResponse = (JSONObject)response.getEntity();
		assertThat("Message ", actualResponse.get("message"), equalTo("true"));
	}
}

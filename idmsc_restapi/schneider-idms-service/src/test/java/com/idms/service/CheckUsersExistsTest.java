package com.idms.service;

import static com.jayway.restassured.RestAssured.given;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.when;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.cache.ehcache.EhCacheCache;

import com.idms.mapper.IdmsMapper;
import com.idms.product.client.IFWService;
import com.idms.product.client.OpenAMService;
import com.idms.product.client.SalesForceService;
import com.jayway.restassured.path.json.JsonPath;
import com.jayway.restassured.response.Response;
import com.se.idms.cache.validate.IValidator;
import com.se.idms.cache.validate.impl.LengthValidatorImpl;
import com.se.idms.cache.validate.impl.PickListValidatorImpl;
import com.se.idms.dto.UserExistsResponse;
import com.se.idms.util.ValidatingInvocationHandler;

/**
 * Test class for Check user exists endpoint
 * @author Aravindh Kumar
 *
 */
public class CheckUsersExistsTest {
	
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
	private IFWService ifwService;
	
	@Mock
	private SalesForceService salesForceService;

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
	
	@Test
	public void testCheckUserExits() {
		
		when(productService.authenticateUser(anyString(), anyString(), anyString()))
		.thenReturn(DomainMockData.AUTHENTICATION_JSON);

		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);

		when(cacheManager.getCache(anyString())).thenReturn(cache);
		
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS);
		
		when(ifwService.getIFWToken(anyString(),anyString(), anyString(), anyString())).thenReturn(DomainMockData.IFW_USER);
		
		when(salesForceService.getSalesForceToken(anyString(),anyString(), anyString(),anyString(), anyString(), anyString())).thenReturn(DomainMockData.SALESFORCE_USER);
		
		javax.ws.rs.core.Response response =   userService.checkUserExists("", "false");
		
		UserExistsResponse  actualResponse = (UserExistsResponse)response.getEntity();
		
		assertThat("Message ", actualResponse.getMessage(), equalTo("false"));
	}
	
	@Test
	public void testCheckUserExitsWhenAlreadyUserAvailable() {
		
		when(productService.authenticateUser(anyString(), anyString(), anyString()))
		.thenReturn(DomainMockData.AUTHENTICATION_JSON);

		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);

		when(cacheManager.getCache(anyString())).thenReturn(cache);
		
		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS_TRUE);
		
		when(ifwService.getIFWToken(anyString(),anyString(), anyString(), anyString())).thenReturn(DomainMockData.IFW_USER);
		
		when(salesForceService.getSalesForceToken(anyString(),anyString(), anyString(),anyString(), anyString(), anyString())).thenReturn(DomainMockData.SALESFORCE_USER);
		
		javax.ws.rs.core.Response response =   userService.checkUserExists("suresh.update.4@mailinator.com", "true");
		
		UserExistsResponse  actualResponse = (UserExistsResponse)response.getEntity();
		
		assertThat("Message ", actualResponse.getMessage(), equalTo("true"));
	}

	final String ROOT_URL = "http://localhost:8080/IDMS/services/apexrest/IDMSUser/";

	@Test
	public void testCheckUserSuccess() {
		
		given().when().get(ROOT_URL + "arvind.test1@mailinator.com").then().statusCode(200);

	}

	@Test
	public void testCheckUserInvalidUser() {
		
		Response response = given().when().get(ROOT_URL + "INVALID");
		assertEquals(404, response.getStatusCode());
		String json = response.asString();
		
		JsonPath jsonPath = new JsonPath(json);
		assertEquals("false", jsonPath.get("message"));
	}

	@Test
	public void testCheckUserData() {
		
		Response response = given().when().get(ROOT_URL + "arvind.test1@mailinator.com");
		assertEquals(200, response.getStatusCode());
		String json = response.asString();
		
		JsonPath jsonPath = new JsonPath(json);
		assertEquals("true", jsonPath.get("message"));
	}
}

package com.idms.service;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.doNothing;
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

import com.idms.mapper.IdmsMapper;
import com.idms.product.client.OpenAMService;
import com.se.idms.cache.validate.IValidator;
import com.se.idms.cache.validate.impl.LengthValidatorImpl;
import com.se.idms.cache.validate.impl.PickListValidatorImpl;
import com.se.idms.dto.SetPasswordErrorResponse;
import com.se.idms.dto.SetPasswordRequest;
import com.se.idms.dto.SetPasswordResponse;
import com.se.idms.util.UserConstants;
import com.se.idms.util.ValidatingInvocationHandler;

public class SetPasswordTest {

	/**
	 * Class under test.
	 */
	@InjectMocks
	private UserService userService = new UserServiceImpl();

	@Mock
	private IdmsMapper idmsMapper;

	@Mock
	private IValidator pickListValidator = new PickListValidatorImpl();

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
	private UimsSetPasswordSoapService uimsSetPasswordSoapService;

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
	 * Test SetPassword when user provided all the details - 
	 * @throws Exception 
	 */
	@Test
	public void testSetPassword() throws Exception{
		
		SetPasswordRequest setPasswordRequest =	DtoMockData.buildSetPasswordRequest();
		setPasswordRequest.setId("cn002a48ce80-ccc3-4035-a905-e571a5d86505");
		when(legthValidator.validate(anyString(),anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(productService.authenticateUser(anyString(), anyString(), anyString()))
				.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(sendEmail.validatePin(anyString(),anyString())).thenReturn(true);
		when(uimsSetPasswordSoapService.setUIMSPassword(anyString(), anyString(), anyString(), anyString(), anyString(), anyString(), anyString())).thenReturn(true);
		
		Response response = userService.setPassword("", "", "", setPasswordRequest);
		assertEquals(HttpStatus.OK, HttpStatus.valueOf(response.getStatus()));
		SetPasswordResponse actualResponse = (SetPasswordResponse)response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Success"));
		assertThat("Message ", actualResponse.getMessage(), equalTo("Password Updated successfully"));
	}
	
	/**
	 * Test SetPassword when user provided all the details - when updatesource null
	 * @throws Exception 
	 */
	@Test
	public void testSetPassword_EmptyUpdateSource() throws Exception{
		
		SetPasswordRequest setPasswordRequest =	DtoMockData.buildSetPasswordRequest();
		setPasswordRequest.setIDMS_Profile_update_source(null);
		
		when(legthValidator.validate(anyString(),anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(),anyString())).thenReturn(true);

		Response response = userService.setPassword("", "", "", setPasswordRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		SetPasswordErrorResponse actualResponse = (SetPasswordErrorResponse)response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.PROFILE_UPDATE_SOURCE));
	}
	
	/**
	 * Test SetPassword when user provided all the details - when ID null
	 * @throws Exception 
	 */
	@Test
	public void testSetPassword_EmptyId() throws Exception{
		
		SetPasswordRequest setPasswordRequest =	DtoMockData.buildSetPasswordRequest();
		setPasswordRequest.setId(null);
		
		when(legthValidator.validate(anyString(),anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(),anyString())).thenReturn(true);

		Response response = userService.setPassword("", "", "", setPasswordRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		SetPasswordErrorResponse actualResponse = (SetPasswordErrorResponse)response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.MANDATORY_FEDERATION_ID));
	}
	
	/**
	 * Test SetPassword when user provided all the details - when ID null
	 * @throws Exception 
	 */
	@Test
	public void testSetPassword_InvalidId() throws Exception{
		
		SetPasswordRequest setPasswordRequest =	DtoMockData.buildSetPasswordRequest();
		
		when(legthValidator.validate(anyString(),anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(),anyString())).thenReturn(true);

		Response response = userService.setPassword("", "", "", setPasswordRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		SetPasswordErrorResponse actualResponse = (SetPasswordErrorResponse)response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo("User Id should start with cn00"));
	}
	
	/**
	 * Test SetPassword when user provided all the details - when ID null
	 * @throws Exception 
	 */
	@Test
	public void testSetPassword_InvalidPassword() throws Exception{
		
		SetPasswordRequest setPasswordRequest =	DtoMockData.buildSetPasswordRequest();
		setPasswordRequest.setId("cn002a48ce80-ccc3-4035-a905-e571a5d86505");
		setPasswordRequest.setNewPwd("welcome123");
		
		when(legthValidator.validate(anyString(),anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(),anyString())).thenReturn(true);

		Response response = userService.setPassword("", "", "", setPasswordRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		SetPasswordErrorResponse actualResponse = (SetPasswordErrorResponse)response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo("New password is not following password policy"));
	}
	
	@Test
	public void testSetPassword_WithoutUIFlag() throws Exception{
		
		SetPasswordRequest setPasswordRequest =	DtoMockData.buildSetPasswordRequest();
		setPasswordRequest.setId("cn002a48ce80-ccc3-4035-a905-e571a5d86505");
		setPasswordRequest.setUIFlag("false");
		
		when(legthValidator.validate(anyString(),anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(),anyString())).thenReturn(true);

		Response response = userService.setPassword("", "", "", setPasswordRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		SetPasswordErrorResponse actualResponse = (SetPasswordErrorResponse)response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.OPERATION_BLCOKED));
	}
	
	@Test
	public void testSetPassword_EmptyPassword() throws Exception{
		
		SetPasswordRequest setPasswordRequest =	DtoMockData.buildSetPasswordRequest();
		setPasswordRequest.setId("cn002a48ce80-ccc3-4035-a905-e571a5d86505");
		setPasswordRequest.setNewPwd("");
		
		when(legthValidator.validate(anyString(),anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(),anyString())).thenReturn(true);

		Response response = userService.setPassword("", "", "", setPasswordRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		SetPasswordErrorResponse actualResponse = (SetPasswordErrorResponse)response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo("New password is not following password policy"));
	}
}

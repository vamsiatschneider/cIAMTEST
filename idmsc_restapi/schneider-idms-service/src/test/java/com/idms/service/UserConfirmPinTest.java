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
import com.idms.model.ConfirmPinRequest;
import com.idms.model.ConfirmPinResponse;
import com.idms.product.client.OpenAMService;
import com.se.idms.cache.validate.IValidator;
import com.se.idms.cache.validate.impl.LengthValidatorImpl;
import com.se.idms.cache.validate.impl.PickListValidatorImpl;
import com.se.idms.dto.PasswordRecoveryResponse;
import com.se.idms.util.UserConstants;
import com.se.idms.util.ValidatingInvocationHandler;

public class UserConfirmPinTest {

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
	 * Test Submitting of new pin when new user is registered provided - when
	 * pin is not expired
	 * @throws Exception 
	 */
	@Test
	public void testUserPinConfirmation() throws Exception {

		ConfirmPinRequest confirmPinRequest = DtoMockData.buildUserPinConfirmationRequset();
		
		when(legthValidator.validate(anyString(),anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(),anyString())).thenReturn(true);
		when(multiPickListValidator.validate(anyString(),anyString())).thenReturn(true);

		when(productService.authenticateUser(anyString(), anyString(), anyString()))
				.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		
		when(cacheManager.getCache(anyString())).thenReturn(cache);

		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		
		when(sendEmail.generateOtp(anyString())).thenReturn("1234");
		
		when(sendEmail.validatePin(anyString(),anyString())).thenReturn(true);

		Response response = userService.userPinConfirmation(confirmPinRequest);

		PasswordRecoveryResponse actualResponse = (PasswordRecoveryResponse) response.getEntity();

		assertThat("Status ", actualResponse.getStatus(), equalTo("Success"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.PIN_VALIDATED_SUCCESS));
	}

	
	/**
	 * Test Submitting of new pin when new user is registered provided - when
	 * When Id is not provided
	 */
	@Test
	public void testUserPinConfirmationWhenIdIsEmpty() {

		ConfirmPinRequest confirmPinRequest = DtoMockData.buildUserPinConfirmationRequset();
		confirmPinRequest.setId("");

		Response response = userService.userPinConfirmation(confirmPinRequest);

		ConfirmPinResponse actualResponse = (ConfirmPinResponse) response.getEntity();

		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.MANDATORY_ID));
	}

	/**
	 * Test Submitting of new pin when new user is registered provided - when
	 * pin is not provided
	 */
	@Test
	public void testUserPinConfirmationWhenPincodeIsEmpty() {

		ConfirmPinRequest confirmPinRequest = DtoMockData.buildUserPinConfirmationRequset();
		confirmPinRequest.setPinCode(null);

		Response response = userService.userPinConfirmation(confirmPinRequest);

		ConfirmPinResponse actualResponse = (ConfirmPinResponse) response.getEntity();

		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.MANDATORY_PINCODE));
	}

	/**
	 * Test Submitting of new pin when new user is registered provided - when
	 * IDMS_Profile_update_source__c is not provided
	 */
	@Test
	public void testUserPinConfirmationWhenIDMS_Profile_update_source__cIsNotPrivided() {

		ConfirmPinRequest confirmPinRequest = DtoMockData.buildUserPinConfirmationRequset();
		confirmPinRequest.setIDMS_Profile_update_source(null);

		Response response = userService.userPinConfirmation(confirmPinRequest);

		ConfirmPinResponse actualResponse = (ConfirmPinResponse) response.getEntity();

		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.PROFILE_UPDATE_SOURCE));
	}
	
	/**
	 * Test Submitting of new pin when new user is registered provided - when
	 * IDMS_Profile_update_source__c is not provided
	 * @throws Exception 
	 */
	@Test
	public void testUserPinConfirmationWhenPasswordPolicyIsNotMatching() throws Exception {

		ConfirmPinRequest confirmPinRequest = DtoMockData.buildUserPinConfirmationRequset();
		confirmPinRequest.setPassword("Password");

		when(legthValidator.validate(anyString(), anyString())).thenReturn(true);
		when(pickListValidator.validate(anyString(), anyString())).thenReturn(true);

		when(productService.authenticateUser(anyString(), anyString(), anyString()))
				.thenReturn(DomainMockData.AUTHENTICATION_JSON);

		when(cacheManager.getCache(anyString())).thenReturn(cache);
		
		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		
		Response response = userService.userPinConfirmation(confirmPinRequest);

		ConfirmPinResponse actualResponse = (ConfirmPinResponse) response.getEntity();

		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.PR_POLICY));
	}
		
}

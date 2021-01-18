package com.idms.service;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;

import java.io.InputStream;

import javax.ws.rs.core.Response;
import org.mockito.Mock;
import mockit.MockUp;
import mockit.integration.junit4.JMockit;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.MockitoAnnotations;
import org.springframework.cache.ehcache.EhCacheCache;
import org.springframework.test.util.ReflectionTestUtils;

import com.idms.mapper.IdmsMapper;
import com.idms.model.CreateUserRequest;
import com.idms.model.IDMSUserResponse;
import com.idms.product.client.OpenAMService;
import com.idms.product.client.OpenDjService;
import com.idms.product.model.OpenAmUserRequest;
import com.idms.service.util.AsyncUtil;
import com.se.idms.cache.validate.IValidator;
import com.se.idms.cache.validate.impl.PickListValidatorImpl;
import com.se.idms.dto.UIMSResponse;
import com.se.idms.util.EmailValidator;
import com.se.idms.util.PhoneValidator;
import com.se.idms.util.ValidatingInvocationHandler;

//JMockit will run on JDK and not on JRE
@RunWith(JMockit.class)
public class UserRegistrationTest {
	@InjectMocks
	private UserServiceImpl userService = new UserServiceImpl();
	
	@Mock
	OpenDjService openDJService;
	
	@Mock
	private IdmsMapper mapper;
	
	@Mock
	private IValidator pickListValidator = new PickListValidatorImpl();

	@Mock
	private IValidator multiPickListValidator;

	@Mock
	private IValidator legthValidator;	
	
	@Mock
	private static EmailValidator emailValidator;
	
	@Mock
	private PhoneValidator 	phoneValidator;

	@Rule 
	public ExpectedException thrown = ExpectedException.none();
	 
	@Mock
	private OpenAMService productService;
	
	@Mock
	private org.springframework.cache.ehcache.EhCacheCacheManager cacheManager;
	
	@Mock
	private EhCacheCache cache;
	
	@Mock
	private IDMSUserResponse idmsuserResponse;
	
	
	
	/**
	 * Initialize mocks.
	 */
	@Before
	public void before() {
		try {
			MockitoAnnotations.initMocks(this);
			legthValidator = ValidatingInvocationHandler.createValidatingServiceProxy(legthValidator, IValidator.class);
			pickListValidator = ValidatingInvocationHandler.createValidatingServiceProxy(pickListValidator, IValidator.class);
		}
		catch(Exception e) {
			e.printStackTrace();
		}
	}
	

	@Test
	public void testUserRegistration() throws Exception {
		
		// Setup
		CreateUserRequest userRequest = DtoMockData.buildUIMSRegistrationRequest();
		OpenAmUserRequest openAmReq = DtoMockData.buildOpenAmRequset();
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
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(mapper.map(userRequest, IDMSUserResponse.class)).thenReturn(idmsuserResponse);
		ReflectionTestUtils.setField(userService, "uimsClientId", "client");
		ReflectionTestUtils.setField(userService, "uimsClientSecret", "clientPwd");
		
		//Solution for static
		new MockUp<AsyncUtil>() {
		        @mockit.Mock
		        public boolean generateCSV(String value,String value2) {
		            return true;
		        }
		    };
	
		Response response = userService.userRegistration("","client", "clientPwd", userRequest);
		UIMSResponse actualResponse = (UIMSResponse)response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Success"));	
		
	}
	
}

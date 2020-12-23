package com.idms.service;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.when;

import javax.ws.rs.NotFoundException;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;
import org.springframework.http.HttpStatus;

import com.idms.product.client.OpenAMService;
import com.idms.product.client.OpenAMTokenService;
import com.idms.product.model.OpenAMGetUserWorkResponse;
import com.jayway.jsonpath.DocumentContext;
import com.se.idms.dto.ParseValuesByOauthHomeWorkContextDto;

/**
 * Test class for Get User endpoint
 * @author Aravindh Kumar
 *
 */
public class GetUserTest {

	@InjectMocks
	private UserService userService = new UserServiceImpl();

	@Mock
	private OpenAMService openAMService;
	
	@Mock
	private OpenAMTokenService openAMTokenService;
	
	@Mock
	private org.springframework.cache.ehcache.EhCacheCacheManager cacheManager;
	
	
	@Mock
	private ParseValuesByOauthHomeWorkContextDto valuesByOauthHomeWorkContext;
	
	@Before
	public void before() {
		MockitoAnnotations.initMocks(this);
	}
	
	@Test
	public void testGetUser_ValidId() {
		
		when(openAMTokenService.getUserDetails(anyString())).thenReturn(DomainMockData.TECHNICAL_USER);
		when(openAMService.authenticateUser(anyString(), anyString(), anyString()))
		.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(openAMService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_USER);
		doAnswer(new Answer<Object>() {
			@Override
			public Object answer(InvocationOnMock invocation) throws Throwable {
				Object[] arguments = invocation.getArguments();
				OpenAMGetUserWorkResponse response =(OpenAMGetUserWorkResponse)arguments[0];
				response.setUsername("TestUser");
				return null;
			}
		}).when(valuesByOauthHomeWorkContext).parseValuesWorkContext(any(OpenAMGetUserWorkResponse.class), any(DocumentContext.class));

		String userId = "cn00Nfbt-XRzP-zytK-VE68-THyztdBHxvg8";
		javax.ws.rs.core.Response response = userService.getUser(anyString(),"Bearer 8rcWmUAB_-iRhprn4JH49rbMgW4", userId);
		assertEquals(HttpStatus.OK, HttpStatus.valueOf(response.getStatus()));
		OpenAMGetUserWorkResponse  actualResponse = (OpenAMGetUserWorkResponse)response.getEntity();
		assertThat("Message ", actualResponse.getUsername(), equalTo("TestUser"));
	}
	
	@Test
	public void testGetUser_NullId() {
		
		when(openAMTokenService.getUserDetails(anyString())).thenReturn(DomainMockData.TECHNICAL_USER);
		when(openAMService.authenticateUser(anyString(), anyString(), anyString()))
		.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(openAMService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_INVALID_USER);

		String userId = null;
		javax.ws.rs.core.Response response = userService.getUser(anyString(),"Bearer 8rcWmUAB_-iRhprn4JH49rbMgW4", userId);
		assertEquals(HttpStatus.NOT_FOUND, HttpStatus.valueOf(response.getStatus()));
		JSONArray jsonArray = (JSONArray)response.getEntity();
		JSONObject actualResponse = (JSONObject)jsonArray.get(0);
		assertThat("ErrorCode: ", actualResponse.get("errorCode"), equalTo("NOT_FOUND"));
		assertThat("message: ", actualResponse.get("message"), equalTo("Provided external ID field does not exist or is  not accessible: "+userId));
	}
	
	@Test
	public void testGetUser_EmptyId() {
		
		when(openAMTokenService.getUserDetails(anyString())).thenReturn(DomainMockData.TECHNICAL_USER);
		when(openAMService.authenticateUser(anyString(), anyString(), anyString()))
		.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(openAMService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_INVALID_USER);

		String userId = "";
		javax.ws.rs.core.Response response = userService.getUser(anyString(),"Bearer 8rcWmUAB_-iRhprn4JH49rbMgW4", userId);
		assertEquals(HttpStatus.NOT_FOUND, HttpStatus.valueOf(response.getStatus()));
		JSONArray jsonArray = (JSONArray)response.getEntity();
		JSONObject actualResponse = (JSONObject)jsonArray.get(0);
		assertThat("ErrorCode: ", actualResponse.get("errorCode"), equalTo("NOT_FOUND"));
		assertThat("message: ", actualResponse.get("message"), equalTo("Provided external ID field does not exist or is  not accessible: "+userId));
	}
	
	@Test
	public void testGetUser_Exception() {
		
		when(openAMTokenService.getUserDetails(anyString())).thenReturn(DomainMockData.TECHNICAL_USER);
		when(openAMService.authenticateUser(anyString(), anyString(), anyString()))
		.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(openAMService.getUser(anyString(), anyString())).thenThrow(new NotFoundException());

		String userId = "cn00Nfbt-XRzP-zytK-VE68-THyztdBHabc8";
		javax.ws.rs.core.Response response = userService.getUser(anyString(),"Bearer 8rcWmUAB_-iRhprn4JH49rbMgW4", userId);
		assertEquals(HttpStatus.UNAUTHORIZED, HttpStatus.valueOf(response.getStatus()));
		JSONArray jsonArray = (JSONArray)response.getEntity();
		JSONObject actualResponse = (JSONObject)jsonArray.get(0);
		assertThat("ErrorCode: ", actualResponse.get("errorCode"), equalTo("Unauthorized"));
		assertThat("message: ", actualResponse.get("message"), equalTo("OpenAM issue of Authorization for "+userId));
	}
}

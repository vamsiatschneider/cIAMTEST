package com.idms.service;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.Assert.assertEquals;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.when;

import javax.ws.rs.core.Response;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.cache.ehcache.EhCacheCache;
import org.springframework.http.HttpStatus;

import com.idms.model.IFWUser;
import com.idms.model.SocialProfileUpdateRequest;
import com.idms.model.SocialProfileUpdateResponse;
import com.idms.product.client.OpenAMService;
import com.idms.product.client.OpenAMTokenService;
import com.idms.product.client.OpenDjService;
import com.se.idms.dto.ErrorResponse;
import com.se.idms.util.UserConstants;

public class UpdateSocialProfileTest {

	@InjectMocks
	private UserServiceImpl userService = new UserServiceImpl();
	
	@Mock
	private OpenAMService openAMService;
	
	@Mock
	private OpenDjService openDJService;
	
	@Mock
	private OpenAMTokenService openAMTokenService;
	
	@Mock
	private org.springframework.cache.ehcache.EhCacheCacheManager cacheManager;
	
	@Mock
	private EhCacheCache cache;
	
	@Mock
	private SendEmail sendEmail;
	
	/**
	 * Initialize mocks.
	 */
	@Before
	public void before() {
		MockitoAnnotations.initMocks(this);
	}
	
	@Test
	public void testUpdateSocialProfile() throws Exception {

		String authorizedToken = "KSH2yCiULN0tKfxqKpXv2jAnwsc";
		SocialProfileUpdateRequest request = new SocialProfileUpdateRequest();
		IFWUser userRecord = DomainMockData.getIFWUser();
		request.setUserRecord(userRecord);
		request.setUIFlag("true");
		
		when(openAMService.authenticateUser(anyString(), anyString(), anyString()))
		.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(openAMService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS);
		when(openAMService.getUser(anyString(),anyString())).thenReturn(DomainMockData.GET_MAIL_USER);
		when(openAMService.updateSocialProfile(anyString(),anyString(),anyString(),anyString())).thenReturn(DomainMockData.getSocialProfileUpdateSuccess());
		when(openDJService.getUser(anyString(), anyString(),anyString())).thenReturn(DomainMockData.getAppDetails());
		when(sendEmail.generateOtp(anyString())).thenReturn("1234");
		when(sendEmail.validatePin(anyString(),anyString())).thenReturn(true);
		
		Response response = userService.updateSocialProfile(authorizedToken, request);
		assertEquals(HttpStatus.OK, HttpStatus.valueOf(response.getStatus()));
		SocialProfileUpdateResponse actualResponse = (SocialProfileUpdateResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Success"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.SOCIAL_PROFILE_UPDATE_SUCCESS));
	}
	
	@Test
	public void testUpdateSocialProfileValidToken() throws Exception {

		String authorizedToken = "KSH2yCiULN0tKfxqKpXv2jAnwsc";
		SocialProfileUpdateRequest request = new SocialProfileUpdateRequest();
		IFWUser userRecord = DomainMockData.getIFWUser();
		request.setUserRecord(userRecord);
		request.setUIFlag("false");
		
		when(openAMTokenService.getUserDetails(authorizedToken)).thenReturn(DomainMockData.TECHNICAL_USER);
		when(openAMService.authenticateUser(anyString(), anyString(), anyString()))
		.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(openAMService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS);
		when(openAMService.getUser(anyString(),anyString())).thenReturn(DomainMockData.GET_MAIL_USER);
		when(openAMService.updateSocialProfile(anyString(),anyString(),anyString(),anyString())).thenReturn(DomainMockData.getSocialProfileUpdateSuccess());
		when(openDJService.getUser(anyString(), anyString(),anyString())).thenReturn(DomainMockData.getAppDetails());
		when(sendEmail.generateOtp(anyString())).thenReturn("1234");
		when(sendEmail.validatePin(anyString(),anyString())).thenReturn(true);
		
		Response response = userService.updateSocialProfile(authorizedToken, request);
		assertEquals(HttpStatus.OK, HttpStatus.valueOf(response.getStatus()));
		SocialProfileUpdateResponse actualResponse = (SocialProfileUpdateResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Success"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.SOCIAL_PROFILE_UPDATE_SUCCESS));
	}
	
	@Test
	public void testUpdateSocialProfileInvalidToken() throws Exception {

		String authorizedToken = "KSH2yCiULN0tKfxqKpXv2jAnwsc";
		SocialProfileUpdateRequest request = new SocialProfileUpdateRequest();
		IFWUser userRecord = DomainMockData.getIFWUser();
		request.setUserRecord(userRecord);
		request.setUIFlag("false");
		
		when(openAMTokenService.getUserDetails(authorizedToken)).thenReturn(DomainMockData.GET_USER);
		Response response = userService.updateSocialProfile(authorizedToken, request);
		assertEquals(HttpStatus.UNAUTHORIZED, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo(HttpStatus.UNAUTHORIZED.toString()));
		assertThat("Message ", actualResponse.getMessage(), equalTo("Unauthorized or session expired"));
	}
	
	@Test
	public void testUpdateSocialProfileEmptyToken() throws Exception {

		String authorizedToken = "";
		SocialProfileUpdateRequest request = new SocialProfileUpdateRequest();
		IFWUser userRecord = DomainMockData.getIFWUser();
		request.setUserRecord(userRecord);
		request.setUIFlag("false");
		
		when(openAMTokenService.getUserDetails(authorizedToken)).thenReturn(DomainMockData.GET_USER);
		Response response = userService.updateSocialProfile(authorizedToken, request);
		assertEquals(HttpStatus.UNAUTHORIZED, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo(HttpStatus.UNAUTHORIZED.toString()));
		assertThat("Message ", actualResponse.getMessage(), equalTo("Unauthorized or session expired"));
	}
	
	@Test
	public void testUpdateSocialProfileEmptyFedId() throws Exception {

		String authorizedToken = "KSH2yCiULN0tKfxqKpXv2jAnwsc";
		SocialProfileUpdateRequest request = new SocialProfileUpdateRequest();
		IFWUser userRecord = DomainMockData.getIFWUser();
		userRecord.setIDMS_Federated_ID__c("");
		request.setUserRecord(userRecord);
		request.setUIFlag("true");
		
		Response response = userService.updateSocialProfile(authorizedToken, request);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo(HttpStatus.BAD_REQUEST.name()));
		assertThat("Message ", actualResponse.getMessage(), equalTo("Federation ID cannot be null or empty!"));
	}
	
	@Test
	public void testUpdateSocialProfileEmptyFirstName() throws Exception {

		String authorizedToken = "KSH2yCiULN0tKfxqKpXv2jAnwsc";
		SocialProfileUpdateRequest request = new SocialProfileUpdateRequest();
		IFWUser userRecord = DomainMockData.getIFWUser();
		userRecord.setFirstName("");
		request.setUserRecord(userRecord);
		request.setUIFlag("true");
		
		Response response = userService.updateSocialProfile(authorizedToken, request);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo(HttpStatus.BAD_REQUEST.name()));
		assertThat("Message ", actualResponse.getMessage(), equalTo("First Name cannot be null or empty!"));
	}
	
	@Test
	public void testUpdateSocialProfileEmptyLastName() throws Exception {

		String authorizedToken = "KSH2yCiULN0tKfxqKpXv2jAnwsc";
		SocialProfileUpdateRequest request = new SocialProfileUpdateRequest();
		IFWUser userRecord = DomainMockData.getIFWUser();
		userRecord.setLastName("");
		request.setUserRecord(userRecord);
		request.setUIFlag("true");
		
		Response response = userService.updateSocialProfile(authorizedToken, request);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo(HttpStatus.BAD_REQUEST.name()));
		assertThat("Message ", actualResponse.getMessage(), equalTo("Last Name cannot be null or empty!"));
	}
	
	@Test
	public void testUpdateSocialProfileEmptyEmailId() throws Exception {

		String authorizedToken = "KSH2yCiULN0tKfxqKpXv2jAnwsc";
		SocialProfileUpdateRequest request = new SocialProfileUpdateRequest();
		IFWUser userRecord = DomainMockData.getIFWUser();
		userRecord.setEmail("");
		request.setUserRecord(userRecord);
		request.setUIFlag("true");
		
		Response response = userService.updateSocialProfile(authorizedToken, request);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo(HttpStatus.BAD_REQUEST.name()));
		assertThat("Message ", actualResponse.getMessage(), equalTo("Email Id cannot be null or empty!"));
	}
	
	@Test
	public void testUpdateSocialProfileEmptyTnCFlag() throws Exception {

		String authorizedToken = "KSH2yCiULN0tKfxqKpXv2jAnwsc";
		SocialProfileUpdateRequest request = new SocialProfileUpdateRequest();
		IFWUser userRecord = DomainMockData.getIFWUser();
		userRecord.setTncFlag("");
		request.setUserRecord(userRecord);
		request.setUIFlag("true");
		
		Response response = userService.updateSocialProfile(authorizedToken, request);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo(HttpStatus.BAD_REQUEST.name()));
		assertThat("Message ", actualResponse.getMessage(), equalTo("T & C flag cannot be null or empty!"));
	}
	
	@Test
	public void testUpdateSocialProfileEmptyEmailOptIn() throws Exception {

		String authorizedToken = "KSH2yCiULN0tKfxqKpXv2jAnwsc";
		SocialProfileUpdateRequest request = new SocialProfileUpdateRequest();
		IFWUser userRecord = DomainMockData.getIFWUser();
		userRecord.setIDMS_Email_opt_in__c("");
		request.setUserRecord(userRecord);
		request.setUIFlag("true");
		
		Response response = userService.updateSocialProfile(authorizedToken, request);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo(HttpStatus.BAD_REQUEST.name()));
		assertThat("Message ", actualResponse.getMessage(), equalTo("Email Opt In cannot be null or empty!"));
	}
}

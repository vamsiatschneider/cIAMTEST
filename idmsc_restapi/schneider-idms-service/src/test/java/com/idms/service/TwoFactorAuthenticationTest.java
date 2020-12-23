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
import org.springframework.test.util.ReflectionTestUtils;

import com.idms.model.DeviceProfileRequest;
import com.idms.model.MFAEnableResponse;
import com.idms.model.MFARequest;
import com.idms.model.SocialProfileUpdateResponse;
import com.idms.product.client.OpenAMService;
import com.idms.product.client.OpenAMTokenService;
import com.se.idms.dto.ErrorResponse;
import com.se.idms.util.UserConstants;

public class TwoFactorAuthenticationTest {

	@InjectMocks
	private UserServiceImpl userService = new UserServiceImpl();
	
	@Mock
	private OpenAMService openAMService;
	

	@Mock
	private OpenAMTokenService openAMTokenService;
	
	@Mock
	private org.springframework.cache.ehcache.EhCacheCacheManager cacheManager;
	
	@Mock
	private EhCacheCache cache;
	
	/**
	 * Initialize mocks.
	 */
	@Before
	public void before() {
		MockitoAnnotations.initMocks(this);
	}
	
	@Test
	public void testEnableMfaFirstTimeUser() {
		String userId = "cn00Nfbt-XRzP-zytK-VE68-THyztdBHxvg8";
		String authorizedToken = "KSH2yCiULN0tKfxqKpXv2jAnwsc";
		MFARequest mfaRequest = new MFARequest();
		mfaRequest.setIs2FAEnabled("false");
		mfaRequest.setUIFlag("true");
		mfaRequest.setIsFirstTimeUser("true");
		
		when(openAMService.authenticateUser(anyString(), anyString(), anyString()))
		.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(openAMService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS);
		when(openAMService.getUser(anyString(),anyString())).thenReturn(DomainMockData.GET_MAIL_USER);
		when(openAMService.updateMFADetails(anyString(),anyString(),anyString(),anyString())).thenReturn(DomainMockData.getEnableMFASuccess());
		
		Response response = userService.enableMFA(userId, authorizedToken, mfaRequest);
		assertEquals(HttpStatus.OK, HttpStatus.valueOf(response.getStatus()));
		MFAEnableResponse actualResponse = (MFAEnableResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Success"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.MFA_SUCCESS));
	}

	@Test
	public void testEnableMfa2FAEnabledUser() {
		String userId = "cn00Nfbt-XRzP-zytK-VE68-THyztdBHxvg8";
		String authorizedToken = "KSH2yCiULN0tKfxqKpXv2jAnwsc";
		MFARequest mfaRequest = new MFARequest();
		mfaRequest.setIs2FAEnabled("true");
		mfaRequest.setUIFlag("true");
		mfaRequest.setIsFirstTimeUser("false");
		
		when(openAMService.authenticateUser(anyString(), anyString(), anyString()))
		.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(openAMService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS);
		when(openAMService.getUser(anyString(),anyString())).thenReturn(DomainMockData.GET_MAIL_USER);
		when(openAMService.updateMFADetails(anyString(),anyString(),anyString(),anyString())).thenReturn(DomainMockData.getEnableMFASuccess());
		
		Response response = userService.enableMFA(userId, authorizedToken, mfaRequest);
		assertEquals(HttpStatus.OK, HttpStatus.valueOf(response.getStatus()));
		MFAEnableResponse actualResponse = (MFAEnableResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Success"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.MFA_SUCCESS));
	}
	
	@Test
	public void testEnableMfaValidToken() {
		String userId = "cn00Nfbt-XRzP-zytK-VE68-THyztdBHxvg8";
		String authorizedToken = "KSH2yCiULN0tKfxqKpXv2jAnwsc";
		MFARequest mfaRequest = new MFARequest();
		mfaRequest.setIs2FAEnabled("false");
		mfaRequest.setUIFlag("false");
		mfaRequest.setIsFirstTimeUser("true");
		
		when(openAMTokenService.getUserDetails(authorizedToken)).thenReturn(DomainMockData.TECHNICAL_USER);
		when(openAMService.authenticateUser(anyString(), anyString(), anyString()))
		.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(openAMService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS);
		when(openAMService.getUser(anyString(),anyString())).thenReturn(DomainMockData.GET_MAIL_USER);
		when(openAMService.updateMFADetails(anyString(),anyString(),anyString(),anyString())).thenReturn(DomainMockData.getEnableMFASuccess());
		
		Response response = userService.enableMFA(userId, authorizedToken, mfaRequest);
		assertEquals(HttpStatus.OK, HttpStatus.valueOf(response.getStatus()));
		MFAEnableResponse actualResponse = (MFAEnableResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Success"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.MFA_SUCCESS));
	}
	
	@Test
	public void testEnableMfaEmptyUserId() {
		String userId = "";
		String authorizedToken = "KSH2yCiULN0tKfxqKpXv2jAnwsc";
		MFARequest mfaRequest = new MFARequest();
		mfaRequest.setIs2FAEnabled("false");
		mfaRequest.setUIFlag("true");
		mfaRequest.setIsFirstTimeUser("true");
		
		Response response = userService.enableMFA(userId, authorizedToken, mfaRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.MANDATORY_USERID));
	}
	@Test
	public void testEnableMfaEmpty2FAValue() {
		String userId = "";
		String authorizedToken = "KSH2yCiULN0tKfxqKpXv2jAnwsc";
		MFARequest mfaRequest = new MFARequest();
		mfaRequest.setIs2FAEnabled("");
		mfaRequest.setUIFlag("true");
		mfaRequest.setIsFirstTimeUser("true");
		
		Response response = userService.enableMFA(userId, authorizedToken, mfaRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.INVALID_2FA_VALUE));
	}
	
	@Test
	public void testEnableMfaInvalidToken() {
		String userId = "";
		String authorizedToken = "KSH2yCiULN0tKfxqKpXv2jAnwsc";
		MFARequest mfaRequest = new MFARequest();
		mfaRequest.setIs2FAEnabled("false");
		mfaRequest.setUIFlag("false");
		mfaRequest.setIsFirstTimeUser("true");
		
		when(openAMTokenService.getUserDetails(authorizedToken)).thenReturn(DomainMockData.GET_USER);
		Response response = userService.enableMFA(userId, authorizedToken, mfaRequest);
		assertEquals(HttpStatus.UNAUTHORIZED, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo(HttpStatus.UNAUTHORIZED.toString()));
		assertThat("Message ", actualResponse.getMessage(), equalTo("Unauthorized or session expired"));
	}
	
	@Test
	public void testEnableMfaEmptyToken() {
		String userId = "";
		String authorizedToken = "";
		MFARequest mfaRequest = new MFARequest();
		mfaRequest.setIs2FAEnabled("false");
		mfaRequest.setUIFlag("false");
		mfaRequest.setIsFirstTimeUser("true");
		
		when(openAMTokenService.getUserDetails(authorizedToken)).thenReturn(DomainMockData.GET_USER);
		Response response = userService.enableMFA(userId, authorizedToken, mfaRequest);
		assertEquals(HttpStatus.UNAUTHORIZED, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo(HttpStatus.UNAUTHORIZED.toString()));
		assertThat("Message ", actualResponse.getMessage(), equalTo("Unauthorized or session expired"));
	}
	
	@Test
	public void testSaveDeviceProfile() throws Exception {
		String userId = "cn00Nfbt-XRzP-zytK-VE68-THyztdBHxvg8";
		String authorizedToken = "KSH2yCiULN0tKfxqKpXv2jAnwsc";
		DeviceProfileRequest deviceProfileRequest = DomainMockData.getDeviceProfileRequest();

		when(openAMService.authenticateUser(anyString(), anyString(), anyString()))
		.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(openAMService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS);
		when(openAMService.getUser(anyString(),anyString())).thenReturn(DomainMockData.GET_MAIL_USER);
		ReflectionTestUtils.setField(userService, "maxDeviceProfilesAllowed", "5");
		when(openAMService.updateMFADetails(anyString(),anyString(),anyString(),anyString())).thenReturn(DomainMockData.getEnableMFASuccess());
		when(openAMService.updateUser(anyString(),anyString(),anyString())).thenReturn(DomainMockData.GET_USER_WITH_DEVICE_PROFILE);
		
		Response response = userService.saveDeviceProfile(authorizedToken, userId, deviceProfileRequest );
		assertEquals(HttpStatus.OK, HttpStatus.valueOf(response.getStatus()));
		SocialProfileUpdateResponse actualResponse = (SocialProfileUpdateResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Success"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.DEVICE_SAVE_SUCCESS));
	}
	
	@Test
	public void testSaveDeviceProfileValidToken() throws Exception {
		String userId = "cn00Nfbt-XRzP-zytK-VE68-THyztdBHxvg8";
		String authorizedToken = "KSH2yCiULN0tKfxqKpXv2jAnwsc";
		DeviceProfileRequest deviceProfileRequest = DomainMockData.getDeviceProfileRequest();
        deviceProfileRequest.setUiFlag("false");
	
        when(openAMTokenService.getUserDetails(authorizedToken)).thenReturn(DomainMockData.TECHNICAL_USER);
        when(openAMService.authenticateUser(anyString(), anyString(), anyString()))
		.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(openAMService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS);
		when(openAMService.getUser(anyString(),anyString())).thenReturn(DomainMockData.GET_MAIL_USER);
		ReflectionTestUtils.setField(userService, "maxDeviceProfilesAllowed", "5");
		when(openAMService.updateMFADetails(anyString(),anyString(),anyString(),anyString())).thenReturn(DomainMockData.getEnableMFASuccess());
		when(openAMService.updateUser(anyString(),anyString(),anyString())).thenReturn(DomainMockData.GET_USER_WITH_DEVICE_PROFILE);
		
		Response response = userService.saveDeviceProfile(authorizedToken, userId, deviceProfileRequest );
		assertEquals(HttpStatus.OK, HttpStatus.valueOf(response.getStatus()));
		SocialProfileUpdateResponse actualResponse = (SocialProfileUpdateResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Success"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.DEVICE_SAVE_SUCCESS));
	}
	
	@Test
	public void testSaveDeviceProfileEmptyUserId() throws Exception {
		String userId = "";
		String authorizedToken = "KSH2yCiULN0tKfxqKpXv2jAnwsc";
		DeviceProfileRequest deviceProfileRequest = DomainMockData.getDeviceProfileRequest();

		Response response = userService.saveDeviceProfile(authorizedToken, userId, deviceProfileRequest );
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo(HttpStatus.BAD_REQUEST.toString()));
		assertThat("Message ", actualResponse.getMessage(), equalTo("UserId is mandatory!!"));
	}
	
	@Test
	public void testSaveDeviceProfileNullDeviceDetails() throws Exception {
		String userId = "";
		String authorizedToken = "KSH2yCiULN0tKfxqKpXv2jAnwsc";
		DeviceProfileRequest deviceProfileRequest = DomainMockData.getDeviceProfileRequest();
        deviceProfileRequest.setDevicePrint(null);
        
		Response response = userService.saveDeviceProfile(authorizedToken, userId, deviceProfileRequest );
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo(HttpStatus.BAD_REQUEST.toString()));
		assertThat("Message ", actualResponse.getMessage(), equalTo("Device Details are mandatory!!"));
	}
	
	@Test
	public void testSaveDeviceProfileInvalidUserResponse() throws Exception {
		String userId = "cn00Nfbt-XRzP-zytK-VE68-THyztdBHxvg8";
		String authorizedToken = "KSH2yCiULN0tKfxqKpXv2jAnwsc";
		DeviceProfileRequest deviceProfileRequest = DomainMockData.getDeviceProfileRequest();

		when(openAMService.authenticateUser(anyString(), anyString(), anyString()))
		.thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(cacheManager.getCache(anyString())).thenReturn(cache);
		when(openAMService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS);
		when(openAMService.getUser(anyString(),anyString())).thenReturn(DomainMockData.GET_MAIL_USER);
		ReflectionTestUtils.setField(userService, "maxDeviceProfilesAllowed", "5");
		when(openAMService.updateMFADetails(anyString(),anyString(),anyString(),anyString())).thenReturn(DomainMockData.getEnableMFASuccess());
		when(openAMService.updateUser(anyString(),anyString(),anyString())).thenReturn(null);
		
		Response response = userService.saveDeviceProfile(authorizedToken, userId, deviceProfileRequest );
		assertEquals(HttpStatus.INTERNAL_SERVER_ERROR, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo(HttpStatus.INTERNAL_SERVER_ERROR.toString()));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.DEVICE_SAVE_FAILED));
	}
	
	
	@Test
	public void testSaveDeviceProfileInvalidToken() throws Exception {
		String userId = "cn00Nfbt-XRzP-zytK-VE68-THyztdBHxvg8";
		String authorizedToken = "KSH2yCiULN0tKfxqKpXv2jAnwsc";
		DeviceProfileRequest deviceProfileRequest = DomainMockData.getDeviceProfileRequest();
        deviceProfileRequest.setUiFlag("false");
		
        when(openAMTokenService.getUserDetails(authorizedToken)).thenReturn(DomainMockData.GET_USER);
		Response response = userService.saveDeviceProfile(authorizedToken, userId, deviceProfileRequest );
		assertEquals(HttpStatus.UNAUTHORIZED, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo(HttpStatus.UNAUTHORIZED.toString()));
		assertThat("Message ", actualResponse.getMessage(), equalTo("Unauthorized or session expired!!"));
	}
}

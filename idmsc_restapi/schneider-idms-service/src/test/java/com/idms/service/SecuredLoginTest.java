package com.idms.service;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.Assert.*;

import javax.ws.rs.core.Response;

  
//import org.powermock.core.classloader.annotations.PrepareForTest;  
//import org.powermock.modules.junit4.PowerMockRunner;  
import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;

import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.cache.ehcache.EhCacheCache;
import org.springframework.http.HttpStatus;

import com.idms.model.UserMFADataRequest;
import com.idms.product.client.OpenAMService;
import com.idms.product.client.OpenAMTokenService;
import com.idms.product.client.OpenDjService;

import com.se.idms.dto.ErrorResponse;
import com.se.idms.util.UserConstants;


public class SecuredLoginTest {
	
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
	Response authenticateResponse;
	
	/**
	 * Initialize mocks.
	 */
	@Before
	public void before() {
		MockitoAnnotations.initMocks(this);
	}
	
	@Test
	public void emptyAuthId() {
		UserMFADataRequest userMfaDataReq=new UserMFADataRequest();
		userMfaDataReq.setAuthId("");
		
		Response response = userService.securedLoginNext(userMfaDataReq);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.AUTHID_EMPTY));
	}
	
	@Test
	public void emptyStageId() {
		UserMFADataRequest userMfaDataReq=new UserMFADataRequest();
		userMfaDataReq.setAuthId("efjjitaaqwaeretfyguuiuiikktt");
		userMfaDataReq.setStageName("");
		Response response = userService.securedLoginNext(userMfaDataReq);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.STAGENAME_EMPTY));
	}
	@Test
	public void missingLoginUser() {
		UserMFADataRequest userMfaDataReq=new UserMFADataRequest();
		userMfaDataReq.setAuthId("eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.caaDzs8OeN68_sZhFxMX_4MJsbqUyxQcvOLuUF1n3aE");
		userMfaDataReq.setStageName("deviceStage");
		userMfaDataReq.setStageData("DeviceIdMatch");
		Response response = userService.securedLoginNext(userMfaDataReq);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.USER_EMPTY));
	}
	@Test
	public void missingAppName() {
		UserMFADataRequest userMfaDataReq=new UserMFADataRequest();
		userMfaDataReq.setAuthId("eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.caaDzs8OeN68_sZhFxMX_4MJsbqUyxQcvOLuUF1n3aE");
		userMfaDataReq.setStageName("deviceStage");
		userMfaDataReq.setStageData("DeviceIdMatch");
		userMfaDataReq.setLoginUser("testUser@gmail.com");
		Response response = userService.securedLoginNext(userMfaDataReq);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.APPNAME_EMPTY));
	}
	@Test
	public void missingLoginDetails() {
		Response response = userService.securedLogin("", "", "", "");
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.MISSING_USERLOGINDETAILS));
	}
	

}

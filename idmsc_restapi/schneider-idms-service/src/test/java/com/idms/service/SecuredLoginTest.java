package com.idms.service;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.Assert.*;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.when;

import java.io.IOException;

import javax.ws.rs.core.NewCookie;
import javax.ws.rs.core.Response;

import org.apache.http.client.ClientProtocolException;
import org.json.simple.JSONObject;
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
import com.idms.service.util.AsyncUtil;
import com.idms.service.util.ChinaIdmsUtil;
import com.se.idms.dto.ErrorResponse;
import com.se.idms.util.UserConstants;

import mockit.MockUp;


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
	
	@Mock
	NewCookie amlbNewCookie;
	
	/**
	 * Initialize mocks.
	 */
	@Before
	public void before() {
		MockitoAnnotations.initMocks(this);
	}
	
	@Test
	public void securedLoginNextTestWhenAuthIDIsNull() {
		UserMFADataRequest userMfaDataReq=new UserMFADataRequest();
		userMfaDataReq.setAuthId("");
		Response response = userService.securedLoginNext(amlbNewCookie,userMfaDataReq);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.AUTHID_EMPTY));
	}
	
	@Test
	public void securedLoginNextTestWhenStageIDIsNull() {
		UserMFADataRequest userMfaDataReq=new UserMFADataRequest();
		userMfaDataReq.setAuthId("efjjitaaqwaeretfyguuiuiikktt");
		userMfaDataReq.setStageName("");
		Response response = userService.securedLoginNext(amlbNewCookie,userMfaDataReq);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.STAGENAME_EMPTY));
	}
	@Test
	public void securedLoginNextTestWhenUserNameIsNull() {
		UserMFADataRequest userMfaDataReq=new UserMFADataRequest();
		userMfaDataReq.setAuthId("eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.caaDzs8OeN68_sZhFxMX_4MJsbqUyxQcvOLuUF1n3aE");
		userMfaDataReq.setStageName("deviceStage");
		userMfaDataReq.setStageData("DeviceIdMatch");
		Response response = userService.securedLoginNext(amlbNewCookie,userMfaDataReq);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.USER_EMPTY));
	}
	@Test
	public void securedLoginNextTestWhenAppnameIsNull() {
		UserMFADataRequest userMfaDataReq=new UserMFADataRequest();
		userMfaDataReq.setAuthId("eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.caaDzs8OeN68_sZhFxMX_4MJsbqUyxQcvOLuUF1n3aE");
		userMfaDataReq.setStageName("deviceStage");
		userMfaDataReq.setStageData("DeviceIdMatch");
		userMfaDataReq.setLoginUser("testUser@gmail.com");
		Response response = userService.securedLoginNext(amlbNewCookie,userMfaDataReq);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.APPNAME_EMPTY));
	}
	@Test
	public void securedLoginTestWhenLoginDetailsAreNull() {
		Response response = userService.securedLogin("", "", "", "");
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		ErrorResponse actualResponse = (ErrorResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.MISSING_USERLOGINDETAILS));
	}
	@Test
	public void securedLoginTest() {
		String userName="test@getnada.com";
		String password="Password1";
		String realm="/se";
		String appName="AutomationHome";
		when(openAMService.authenticateUser(anyString(), anyString(), anyString()))
		.thenReturn(DomainMockData.AUTHENTICATE_JSON);
		when(openAMService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS_TRUE);
		new MockUp<ChinaIdmsUtil>() {
	        @mockit.Mock
	        public Response executeHttpClient(String frVersion, String uri, String realm, String userName, String password)
	    			throws ClientProtocolException, IOException
	        {
	            return Response.status(Response.Status.OK).entity(DomainMockData.AUTHENTICATION_JSON).build();
	        }
	    };
	    
	    new MockUp<AsyncUtil>() {
	        @mockit.Mock
	        public boolean generateCSV(String value,String value2) {
	            return true;
	        }
	    };
	    Response response = userService.securedLogin(userName,password,realm,appName);
		String Token = (String)response.getEntity();
		assertEquals(HttpStatus.OK, HttpStatus.valueOf(response.getStatus()));
		
	}
	@Test
	public void securedLoginTestwhenMFAisON() {
		String userName="test@getnada.com";
		String password="Password1";
		String realm="/se";
		String appName="AutomationHome";
		
		when(openAMService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);
		when(openAMService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS_TRUE);
		new MockUp<ChinaIdmsUtil>() {
	        @mockit.Mock
	        public Response executeHttpClient(String frVersion, String uri, String realm, String userName, String password)
	    			throws ClientProtocolException, IOException
	        {
	            return Response.status(Response.Status.OK).entity(DomainMockData.MFA_AUTHENTICATION_JSON).build();
	        }
	    };
	    
	    new MockUp<AsyncUtil>() {
	        @mockit.Mock
	        public boolean generateCSV(String value,String value2) {
	            return true;
	        }
	    };
	    
		Response response = userService.securedLogin(userName, password, realm, appName);
		JSONObject actualResponse=(JSONObject) response.getEntity();
		assertEquals(HttpStatus.OK, HttpStatus.valueOf(response.getStatus()));
		assertThat("Message ", actualResponse.get("stage"), equalTo(UserConstants.STAGE_DEVICEIDMATCH2));
	}
	
	@Test
	public void securedLoginNextTest() {
		UserMFADataRequest userMfaDataReq = new UserMFADataRequest();
    	userMfaDataReq.setAuthId("eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.caaDzs8OeN68_sZhFxMX_4MJsbqUyxQcvOLuUF1n3aE");
		userMfaDataReq.setStageName("deviceStage");
		userMfaDataReq.setStageData("{\"screen\":{\"screenWidth\":1280,\"screenHeight\":720,\"screenColourDepth\":24},\"timezone\":{\"timezone\":-330},\"plugins\":{\"installedPlugins\":\"internal-pdf-viewer;mhjfbmdgcfjbbpaeojofohoefgiehjai;internal-nacl-plugin;\"},\"fonts\":{\"installedFonts\":\"cursive;monospace;serif;sans-serif;fantasy;default;Arial;Arial Black;Arial Narrow;Bookman Old Style;Bradley Hand ITC;Century;Century Gothic;Comic Sans MS;Courier;Courier New;Georgia;Impact;Lucida Console;Monotype Corsiva;Papyrus;Tahoma;Times;Times New Roman;Trebuchet MS;Verdana;\"},\"userAgent\":\"Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/88.0.4324.150 Safari/537.36\",\"appName\":\"Netscape\",\"appCodeName\":\"Mozilla\",\"appVersion\":\"5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/88.0.4324.150 Safari/537.36\",\"platform\":\"Win32\",\"product\":\"Gecko\",\"productSub\":\"20030107\",\"vendor\":\"Google Inc.\",\"language\":\"en-US\"}");
		userMfaDataReq.setLoginUser("testUser@gmail.com");
		userMfaDataReq.setAppName("oauthSampleApp");
		new MockUp<ChinaIdmsUtil>() {
	        @mockit.Mock
	        public Response executeHttpDeviceClient(String frVersion, String uri, String realm, String authId, String deviceOrOTPData, String fileName, NewCookie cookie)
	    			throws ClientProtocolException, IOException {
	        	return Response.status(Response.Status.OK).entity(DomainMockData.AUTHENTICATION_JSON).build();
	        }
	    };
	    new MockUp<AsyncUtil>() {
	        @mockit.Mock
	        public boolean generateCSV(String value,String value2) {
	            return true;
	        }
	    };
	   
		Response response = userService.securedLoginNext(amlbNewCookie, userMfaDataReq);
		assertEquals(HttpStatus.OK, HttpStatus.valueOf(response.getStatus()));
	}
	

}

package com.idms.service;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.Assert.assertEquals;

import javax.ws.rs.core.Response;

import org.junit.Before;
import org.junit.Test;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.http.HttpStatus;

import com.idms.model.SendInvitationRequest;
import com.se.idms.cache.utils.EmailConstants;
import com.se.idms.dto.UserServiceResponse;
import com.se.idms.util.UserConstants;
import com.se.idms.util.ValidatingInvocationHandler;

public class SendInvitationTest {
	
	@InjectMocks
	private UserService userService = new UserServiceImpl();
	
	@Mock
	private SendEmail sendEmail;
	
	@Before
	public void before() {
		MockitoAnnotations.initMocks(this);
		userService = ValidatingInvocationHandler.createValidatingServiceProxy(userService, UserService.class);
	}
	
	@Test
	public void testSendInvitation() throws Exception{
		
		SendInvitationRequest sendInvitaionRequest = DtoMockData.buildSendInvitationRequset();
		sendEmail.sendInvitationEmail(EmailConstants.SENDINVITATION_OPT_TYPE, sendInvitaionRequest.getRedirectUrl(), 
				sendInvitaionRequest.getEmail(), sendInvitaionRequest.getInvitationId());
		
		Response response = userService.sendInvitation("Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20", sendInvitaionRequest);
		assertEquals(HttpStatus.OK, HttpStatus.valueOf(response.getStatus()));
		UserServiceResponse actualResponse = (UserServiceResponse)response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Success"));
		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.SET_INVITATION_SUCCESS_MESSAGE));
	}
	
	@Test
	public void testSendInvitation_EmptyEmailId() throws Exception{
		
		SendInvitationRequest sendInvitaionRequest = DtoMockData.buildSendInvitationRequset();
		sendInvitaionRequest.setEmail("");
		sendEmail.sendInvitationEmail(EmailConstants.SENDINVITATION_OPT_TYPE, sendInvitaionRequest.getRedirectUrl(), 
				sendInvitaionRequest.getEmail(), sendInvitaionRequest.getInvitationId());
		
		Response response = userService.sendInvitation("Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20", sendInvitaionRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		UserServiceResponse actualResponse = (UserServiceResponse)response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo("Email, InvitationId and RedirectUrl are mandatory"));
		
	}
	
	@Test
	public void testSendInvitation_NullEmailId() throws Exception{
		
		SendInvitationRequest sendInvitaionRequest = DtoMockData.buildSendInvitationRequset();
		sendInvitaionRequest.setEmail(null);
		sendEmail.sendInvitationEmail(EmailConstants.SENDINVITATION_OPT_TYPE, sendInvitaionRequest.getRedirectUrl(), 
				sendInvitaionRequest.getEmail(), sendInvitaionRequest.getInvitationId());
		
		Response response = userService.sendInvitation("Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20", sendInvitaionRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		UserServiceResponse actualResponse = (UserServiceResponse)response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo("Email, InvitationId and RedirectUrl are mandatory"));
		
	}
	
	@Test
	public void testSendInvitation_NullInvitationId() throws Exception{
		
		SendInvitationRequest sendInvitationRequest = DtoMockData.buildSendInvitationRequset();
		sendInvitationRequest.setInvitationId(null);
		sendEmail.sendInvitationEmail(EmailConstants.SENDINVITATION_OPT_TYPE, sendInvitationRequest.getRedirectUrl(), 
				sendInvitationRequest.getEmail(), sendInvitationRequest.getInvitationId());
		
		Response response = userService.sendInvitation("Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20", sendInvitationRequest);
		UserServiceResponse actualResponse = (UserServiceResponse)response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo("Email, InvitationId and RedirectUrl are mandatory"));
		
	}
	
	@Test
	public void testSendInvitation_EmptyInvitationId() throws Exception{
		
		SendInvitationRequest sendInvitaionRequest = DtoMockData.buildSendInvitationRequset();
		sendInvitaionRequest.setInvitationId("");
		sendEmail.sendInvitationEmail(EmailConstants.SENDINVITATION_OPT_TYPE, sendInvitaionRequest.getRedirectUrl(), 
				sendInvitaionRequest.getEmail(), sendInvitaionRequest.getInvitationId());
		
		Response response = userService.sendInvitation("Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20", sendInvitaionRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		UserServiceResponse actualResponse = (UserServiceResponse)response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo("Email, InvitationId and RedirectUrl are mandatory"));
		
	}
	
	@Test
	public void testsendInvitation_NullRedirectUrl() throws Exception{
		
		SendInvitationRequest sendInvitaionRequest = DtoMockData.buildSendInvitationRequset();
		sendInvitaionRequest.setRedirectUrl(null);
		sendEmail.sendInvitationEmail(EmailConstants.SENDINVITATION_OPT_TYPE, sendInvitaionRequest.getRedirectUrl(), 
				sendInvitaionRequest.getEmail(), sendInvitaionRequest.getInvitationId());
		
		Response response = userService.sendInvitation("Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20", sendInvitaionRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		UserServiceResponse actualResponse = (UserServiceResponse)response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo("Email, InvitationId and RedirectUrl are mandatory"));
		
	}
	
	@Test
	public void testsendInvitation_EmptyRedirectUrl() throws Exception{
		
		SendInvitationRequest sendInvitaionRequest = DtoMockData.buildSendInvitationRequset();
		sendInvitaionRequest.setRedirectUrl("");
		sendEmail.sendInvitationEmail(EmailConstants.SENDINVITATION_OPT_TYPE, sendInvitaionRequest.getRedirectUrl(), 
				sendInvitaionRequest.getEmail(), sendInvitaionRequest.getInvitationId());
		
		Response response = userService.sendInvitation("Bearer 989d8f87-54da-40f1-9d89-2c285ad5ea20", sendInvitaionRequest);
		assertEquals(HttpStatus.BAD_REQUEST, HttpStatus.valueOf(response.getStatus()));
		UserServiceResponse actualResponse = (UserServiceResponse)response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo("Email, InvitationId and RedirectUrl are mandatory"));
		
	}

}

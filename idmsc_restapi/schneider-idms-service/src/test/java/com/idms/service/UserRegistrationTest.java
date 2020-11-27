package com.idms.service;

/*import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.Assert.*;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.when;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import javax.ws.rs.core.Response;*/

import org.junit.Test;
//import org.powermock.api.mockito.PowerMockito;

/*import com.idms.model.CreateUserRequest;
import com.idms.model.CreateUserResponse;
import com.idms.model.IDMSUserResponse;
import com.idms.product.model.OpenAmUserRequest;
import com.idms.service.util.AsyncUtil;
import com.se.idms.util.UserConstants;*/

//Need to be fixed for UserRegistration
public class UserRegistrationTest {

	
	@Test
	public void testUserRegistration() throws Exception {
		
//		// Setup
//		CreateUserRequest userRequest = DtoMockData.buildUserRegistrationRequset();
//		OpenAmUserRequest openAmReq = DtoMockData.buildUserRegistrationOpenAmRequset();
//		//CompanyV3 company=DtoMockData.buildCompanyRequest();
//		//UserV6 identity=DtoMockData.buildUserV6Reuest();
//		
//		when(legthValidator.validate(anyString(), anyString())).thenReturn(true);
//		when(pickListValidator.validate(anyString(), anyString())).thenReturn(true);
//		when(multiPickListValidator.validate(anyString(), anyString())).thenReturn(true);
//		when(phoneValidator.validate(anyString())).thenReturn(true);
//		when(mapper.map(userRequest, OpenAmUserRequest.class)).thenReturn(openAmReq);
//		//ReflectionTestUtils.setField(userService, "registrationCsvPath", "/tomcat/apache-tomcat-8.5.11/logs/reg_logs.csv");
//		/*try {
//		ReflectionTestUtils.setField(userService, "registrationCsvPath", "reg_logs.csv");
//		}
//		catch(Exception e) {
//			System.out.println(e.getMessage());
//			System.out.println(e.getCause());
//		}*/
//		InputStream is = new ByteArrayInputStream(DomainMockData.USER_REGISTRATION.getBytes());
//		Response userRegRes = Response.status(Response.Status.OK).entity(is).build();
//		when(productService.userRegistration(anyString(), anyString(), anyString())).thenReturn(userRegRes);
//		when(openDJService.getUser(anyString(), anyString(),anyString())).thenReturn(DomainMockData.getAppDetails());
//		when(productService.checkUserExistsWithEmailMobile(anyString(), anyString())).thenReturn(DomainMockData.USER_EXISTS);
//		when(productService.authenticateUser(anyString(), anyString(), anyString())).thenReturn(DomainMockData.AUTHENTICATION_JSON);
//		when(productService.getUser(anyString(), anyString())).thenReturn(DomainMockData.GET_MAIL_USER);
//		//when(emailValidator.validate(anyString())).thenReturn(true);
//		when(cacheManager.getCache(anyString())).thenReturn(cache);
//		when(sendEmail.generateOtp(anyString())).thenReturn("1234");
//		when(sendEmail.validatePin(anyString(),anyString())).thenReturn(true);
//		when(mapper.map(userRequest, IDMSUserResponse.class)).thenReturn(idmsuserResponse);
//		PowerMockito.mockStatic(AsyncUtil.class);
//		when(AsyncUtil.generateCSV(anyString(), anyString())).thenReturn(true);
//		/*
//		 * try { when(AsyncUtil.generateCSV(anyString(),anyString())).thenReturn(true);
//		 * } catch(Exception e) {
//		 * 
//		 * }
//		 
//		is = new ByteArrayInputStream(DomainMockData.USER_REGISTRATION.getBytes());
//		Response regRes = Response.status(Response.Status.OK).entity(is).build();*/
//		/*when(mapper.map(userRequest, CompanyV3.class)).thenReturn(companyV3);
//		
//		when(mapper.map(userRequest, UserV6.class)).thenReturn(userV6);
//		
//		CompanyV3 company = mapper.map(userRequest, CompanyV3.class);
//		UserV6 identity = mapper.map(userRequest, UserV6.class);*/
//		
//		/*String createUIMSResponse = uimsUserManagerSoapService.createUIMSUserAndCompany(anyString(), identity,
//				anyString(), company,  anyString(), anyString(), anyString(), anyString(), anyString(), userRequest);
//		
//		when(uimsUserManagerSoapService.createUIMSUserAndCompany(anyString(), identity,
//				anyString(), company,  anyString(), anyString(), anyString(), anyString(), anyString(), userRequest)).thenReturn(anyString());*/
//		
//	
//		Response response = userService.userRegistration("","client", "clientPwd", userRequest);
//		CreateUserResponse actualResponse = (CreateUserResponse)response.getEntity();
//		
//		assertThat("Status ", actualResponse.getStatus(), equalTo("Success"));
//		assertThat("Message ", actualResponse.getMessage(), equalTo(UserConstants.CREATE_USER_SUCCESS_MESSAGE));
//		
//		
	}


}

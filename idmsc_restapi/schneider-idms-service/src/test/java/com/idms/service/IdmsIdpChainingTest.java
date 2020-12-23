package com.idms.service;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.equalTo;
import static org.junit.Assert.assertNotNull;

import javax.ws.rs.core.Response;

import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import com.se.idms.cache.validate.IValidator;
import com.se.idms.cache.validate.impl.LengthValidatorImpl;
import com.se.idms.cache.validate.impl.PickListValidatorImpl;
import com.se.idms.dto.UserServiceResponse;
import com.se.idms.util.ValidatingInvocationHandler;

public class IdmsIdpChainingTest {

	/**
	 * Class under test.
	 */
	@InjectMocks
	private UserService userService = new UserServiceImpl();

	@Mock
	private IValidator pickListValidator = new PickListValidatorImpl();;

	@Mock
	private IValidator legthValidator = new LengthValidatorImpl();

	@Rule
	public ExpectedException thrown = ExpectedException.none();



	
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
	 * Test IdmsIDP Chaining the email to return to decoded url - 
	 * 
	 * @throws Exception 
	 */
	@Test
	public void testIdmsIDPChainingTest() {
		
		String idToken1 = "", idToken2 = "", idButton = "", gotoOnFail = "";
		String gotoUrl = "aHR0cHM6Ly9pbXMtc3FlLmJ0c2VjLmRldi5zY2huZWlkZXItZWxlY3RyaWMuY29tL29wZW5zc28vaWRwc3NvaW5pdD9tZXRhQWxpYXM9JTJGaWRwJnNwRW50aXR5SUQ9aHR0cHMlM0ElMkYlMkZ1YXRiZm8xLXNlY29tbXVuaXRpZXMuY3MxOC5mb3JjZS5jb20lMkZpZGVudGl0eSUyRndvcmsmUmVsYXlTdGF0ZT1odHRwcyUzQSUyRiUyRmlkZW50aXR5LWludC5zY2huZWlkZXItZWxlY3RyaWMuY29tJTJGdWklMkYlMjMlMjElMkZsb2dpbiUzRmFwcCUzRG9hdXRoU2FtcGxlQXBw";
		String sunQueryParamsString = "c3BFbnRpdHlJRD1odHRwczovL3VhdGJmbzEtc2Vjb21tdW5pdGllcy5jczE4LmZvcmNlLmNvbS9pZGVudGl0eS93b3Jr";
		String encoded = "true", errorMessage = "", gxCharset = "UTF-8";

		Response response = userService.idmsIdpChaning(idToken1, idToken2, idButton, gotoUrl, gotoOnFail, sunQueryParamsString, encoded, errorMessage, gxCharset);
		assertThat("Status ", response.getStatus(), equalTo(301));
	}
	
	/**
	 * Test IdmsIDP Chaining the email to return to decoded url - 
	 * 
	 * @throws Exception 
	 */
	@Test
	public void testIdmsIDPChainingTestWhenErrorMessageIsAuthFailed() throws Exception {
		
		String idToken1 = "", idToken2 = "", idButton = "", gotoOnFail = "";
		String gotoUrl = "aHR0cHM6Ly9pbXMtc3FlLmJ0c2VjLmRldi5zY2huZWlkZXItZWxlY3RyaWMuY29tL29wZW5zc28vaWRwc3NvaW5pdD9tZXRhQWxpYXM9JTJGaWRwJnNwRW50aXR5SUQ9aHR0cHMlM0ElMkYlMkZ1YXRiZm8xLXNlY29tbXVuaXRpZXMuY3MxOC5mb3JjZS5jb20lMkZpZGVudGl0eSUyRndvcmsmUmVsYXlTdGF0ZT1odHRwcyUzQSUyRiUyRmlkZW50aXR5LWludC5zY2huZWlkZXItZWxlY3RyaWMuY29tJTJGdWklMkYlMjMlMjElMkZsb2dpbiUzRmFwcCUzRG9hdXRoU2FtcGxlQXBw";
		String sunQueryParamsString = "c3BFbnRpdHlJRD1odHRwczovL3VhdGJmbzEtc2Vjb21tdW5pdGllcy5jczE4LmZvcmNlLmNvbS9pZGVudGl0eS93b3Jr";
		String encoded = "true", errorMessage = "auth.failed", gxCharset = "UTF-8";

		Response response = userService.idmsIdpChaning(idToken1, idToken2, idButton, gotoUrl, gotoOnFail, sunQueryParamsString, encoded, errorMessage, gxCharset);
		String location = (String)	response.getHeaderString("Location");
		assertNotNull(location);
		assertThat("Status ", response.getStatus(), equalTo(301));
	}
	
	/**
	 * Test IdmsIDP Chaining the email to return to decoded url - 
	 * 
	 * @throws Exception 
	 */
	@Test
	public void testIdmsIDPChainingTestWhenUrlIsEmpty() {
		
		String idToken1 = "", idToken2 = "", idButton = "", gotoUrl = "";
		String gotoOnFail = "", encoded = "true";
		String sunQueryParamsString = "c3BFbnRpdHlJRD1odHRwczovL3VhdGJmbzEtc2Vjb21tdW5pdGllcy5jczE4LmZvcmNlLmNvbS9pZGVudGl0eS93b3Jr";
		String errorMessage = "auth.failed", gxCharset = "UTF-8";

		Response response = userService.idmsIdpChaning(idToken1, idToken2, idButton, gotoUrl, gotoOnFail, sunQueryParamsString, encoded, errorMessage, gxCharset);
		UserServiceResponse actualResponse = (UserServiceResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo("goto value is mandatory"));
	}

	/**
	 * Test IdmsIDP Chaining the email to return to decoded url - 
	 * 
	 * @throws Exception 
	 */
	@Test
	public void testIdmsIDPChainingTestWhenSunQueryParamsStringIsEmpty() throws Exception {
		
		String idToken1 = "", idToken2 = "", idButton = "", gotoOnFail = "";
		String gotoUrl = "aHR0cHM6Ly9pbXMtc3FlLmJ0c2VjLmRldi5zY2huZWlkZXItZWxlY3RyaWMuY29tL29wZW5zc28vaWRwc3NvaW5pdD9tZXRhQWxpYXM9JTJGaWRwJnNwRW50aXR5SUQ9aHR0cHMlM0ElMkYlMkZ1YXRiZm8xLXNlY29tbXVuaXRpZXMuY3MxOC5mb3JjZS5jb20lMkZpZGVudGl0eSUyRndvcmsmUmVsYXlTdGF0ZT1odHRwcyUzQSUyRiUyRmlkZW50aXR5LWludC5zY2huZWlkZXItZWxlY3RyaWMuY29tJTJGdWklMkYlMjMlMjElMkZsb2dpbiUzRmFwcCUzRG9hdXRoU2FtcGxlQXBw";
		String sunQueryParamsString = "", encoded = "true";
		String  errorMessage = "auth.failed", gxCharset = "UTF-8";
		
		Response response = userService.idmsIdpChaning(idToken1, idToken2, idButton, gotoUrl, gotoOnFail, sunQueryParamsString, encoded, errorMessage, gxCharset);
		UserServiceResponse actualResponse = (UserServiceResponse) response.getEntity();
		assertThat("Status ", actualResponse.getStatus(), equalTo("Error"));
		assertThat("Message ", actualResponse.getMessage(), equalTo("sunQueryParamsString value is mandatory"));
	}
}

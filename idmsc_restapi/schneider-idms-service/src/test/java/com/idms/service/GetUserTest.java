package com.idms.service;

import static com.jayway.restassured.RestAssured.given;
import static org.junit.Assert.assertEquals;

import org.junit.Test;

import com.jayway.restassured.path.json.JsonPath;
import com.jayway.restassured.response.Response;

/**
 * Test class for Get User endpoint
 * @author Aravindh Kumar
 *
 */
public class GetUserTest {

	final String ROOT_URL = "http://localhost:8080/IDMS/services/apexrest/";

	@Test
	public void testGetUserSuccess() {
		
		System.out.println("Testing GET user");
		given().when().get(ROOT_URL + "users/test").then().statusCode(200);

	}

	@Test
	public void testGetUserInvalidUser() {
		
		System.out.println("Testing GET user endpoint by passing invalid user");
		given().when().get(ROOT_URL + "users/invalid").then().statusCode(404);

	}

	@Test
	public void testGetUserData() {
		System.out.println("Testing GET user to verify some attributes");
		
		Response response = given().when().get(ROOT_URL + "users/test");
		assertEquals(200, response.getStatusCode());
		String json = response.asString();
		
		JsonPath jsonPath = new JsonPath(json);
		assertEquals("demo3@example.com", jsonPath.get("Email"));
		assertEquals("test", jsonPath.get("FirstName"));
		assertEquals("test", jsonPath.get("LastName"));
		assertEquals("@home", jsonPath.get("IDMS_User_Context__c"));
	}
}

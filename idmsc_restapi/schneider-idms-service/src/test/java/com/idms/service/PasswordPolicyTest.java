package com.idms.service;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;
import org.mockito.InjectMocks;

public class PasswordPolicyTest {

	@InjectMocks
	private UserServiceImpl userService = new UserServiceImpl();

	@Test
	public void testPasswordPolicy_Email_1stScenario() {
		String firstName = "first";
		String lastName = "last";
		String email = "testpwdfix1@mailinator.com";
		String mobile = "";
		String userPassword = "Welcome1";
		boolean policyPassed = userService.checkPasswordPolicy(userPassword , firstName, lastName, email, mobile);
		assertTrue(policyPassed);
	}
	
	@Test
	public void testPasswordPolicy_Email_2ndScenario() {
		String firstName = "first";
		String lastName = "last";
		String email = "testpwdfix1@mailinator.com";
		String mobile = "";
		String userPassword = "ABCD_123";
		boolean policyPassed = userService.checkPasswordPolicy(userPassword , firstName, lastName, email, mobile);
		assertTrue(policyPassed);
	}
	@Test
	public void testPasswordPolicy_Email_3rdScenario() {
		String firstName = "first";
		String lastName = "last";
		String email = "testpwdfix1@mailinator.com";
		String mobile = "";
		String userPassword = "abcd_123";
		boolean policyPassed = userService.checkPasswordPolicy(userPassword , firstName, lastName, email, mobile);
		assertTrue(policyPassed);
	}
	@Test
	public void testPasswordPolicy_MobileSuccess() {
		String firstName = "first";
		String lastName = "last";
		String email = "";
		String mobile = "13445545646";
		String userPassword = "Welcome1";
		boolean policyPassed = userService.checkPasswordPolicy(userPassword , firstName, lastName, email, mobile);
		assertTrue(policyPassed);
	}

	@Test
	public void testPasswordPolicy_InvalidPwdOnlyLChars() {
		String firstName = "first";
		String lastName = "last";
		String email = "testpwdfix1@mailinator.com";
		String mobile = "";
		String userPassword = "abbfbdbd";
		boolean policyPassed = userService.checkPasswordPolicy(userPassword , firstName, lastName, email, mobile);
		assertFalse(policyPassed);
	}
	
	@Test
	public void testPasswordPolicy_InvalidPwdOnlyUChars() {
		String firstName = "first";
		String lastName = "last";
		String email = "testpwdfix1@mailinator.com";
		String mobile = "";
		String userPassword = "ABBFBDBD";
		boolean policyPassed = userService.checkPasswordPolicy(userPassword , firstName, lastName, email, mobile);
		assertFalse(policyPassed);
	}
	
	@Test
	public void testPasswordPolicy_InvalidPwdOnlySpChars() {
		String firstName = "first";
		String lastName = "last";
		String email = "testpwdfix1@mailinator.com";
		String mobile = "";
		String userPassword = "$%$&&&&$$$";
		boolean policyPassed = userService.checkPasswordPolicy(userPassword , firstName, lastName, email, mobile);
		assertFalse(policyPassed);
	}
	@Test
	public void testPasswordPolicy_InvalidPwdLength() {
		String firstName = "first";
		String lastName = "last";
		String email = "testpwdfix1@mailinator.com";
		String mobile = "";
		String userPassword = "Welcme1";
		boolean policyPassed = userService.checkPasswordPolicy(userPassword , firstName, lastName, email, mobile);
		assertFalse(policyPassed);
	}
	@Test
	public void testPasswordPolicy_InvalidLength() {
		String firstName = "first";
		String lastName = "last";
		String email = "testpwdfix1@mailinator.com";
		String mobile = "";
		String userPassword = "Welcme1";
		boolean policyPassed = userService.checkPasswordPolicy(userPassword , firstName, lastName, email, mobile);
		assertFalse(policyPassed);
	}
	
	@Test
	public void testPasswordPolicy_ContainsFName() {
		String firstName = "first";
		String lastName = "last";
		String email = "testpwdfix1@mailinator.com";
		String mobile = "";
		String userPassword = "Wfirstme1";
		boolean policyPassed = userService.checkPasswordPolicy(userPassword , firstName, lastName, email, mobile);
		assertFalse(policyPassed);
	}
	
	@Test
	public void testPasswordPolicy_ContainsLName() {
		String firstName = "first";
		String lastName = "last";
		String email = "testpwdfix1@mailinator.com";
		String mobile = "";
		String userPassword = "Wlastme1";
		boolean policyPassed = userService.checkPasswordPolicy(userPassword , firstName, lastName, email, mobile);
		assertFalse(policyPassed);
	}
}

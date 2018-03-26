package com.se.idms.util;

import static org.junit.Assert.*;

import org.junit.Test;

/**
 * Test to validate email
 * @author Aravindh Kumar
 *
 */
public class EmailValidatorTest {
	
private static EmailValidator emailValidator = null;
	
	static {
		emailValidator = EmailValidator.getInstance();
	}

	@Test
	public void tesEmailValidator() {
		System.out.println(emailValidator.validate("abc@abc.com"));
		assertEquals(true, emailValidator.validate("abc@abc.com"));
	}
	
	@Test
	public void testNonEmailFormat() {
		System.out.println(emailValidator.validate("abc.com"));
		assertEquals(false, emailValidator.validate("abc.com"));
	}
	
	@Test
	public void testNonEmailFormat1() {
		System.out.println(emailValidator.validate("abc@abc"));
		assertEquals(false, emailValidator.validate("abc@abc"));
	}
}

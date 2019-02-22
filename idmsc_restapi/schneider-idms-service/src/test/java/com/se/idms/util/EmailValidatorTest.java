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
		assertEquals(true, emailValidator.validate("abc@abc.com"));
	}
	
	@Test
	public void testNonEmailFormat() {
		assertEquals(false, emailValidator.validate("abc.com"));
	}
	
	@Test
	public void testNonEmailFormat1() {
		assertEquals(false, emailValidator.validate("abc@abc"));
	}
}

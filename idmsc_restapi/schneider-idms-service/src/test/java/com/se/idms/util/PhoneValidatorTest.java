package com.se.idms.util;

import static org.junit.Assert.*;

import org.junit.Before;
import org.junit.Test;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Test to validate phone
 * @author Aravindh Kumar
 *
 */
public class PhoneValidatorTest {

	/**
	 * Logger instance.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(PhoneValidatorTest.class);

	private PhoneValidator phoneValidator;

	@Before
	public void setUp() {
		phoneValidator = new PhoneValidator();
	}

	@Test
	public void testPhoneValidatorWithString() {
		LOGGER.info("phoneValidator.validate(abcs)");
		assertEquals(false, phoneValidator.validate("abcs"));
	}

	@Test
	public void testPhoneValidatorWithSpace() {
		// LOGGER.info(""+phoneValidator.validate("abcs"));
		assertEquals(false, phoneValidator.validate("346-534,7765"));
	}

	@Test
	public void testPhoneValidatorWithExtension() {
		// LOGGER.info(""+phoneValidator.validate("abcs"));
		assertEquals(false, phoneValidator.validate("773-702-5800|234567"));
	}

	@Test
	public void testPhoneValidatorWith10Digits() {
		LOGGER.info("phoneValidator.validate(12345678900)");
		assertEquals(true, phoneValidator.validate("12345678900"));
	}

	@Test
	public void testPhoneValidatorWithHypen() {
		LOGGER.info("phoneValidator.validate(123-456-7890)");
		assertEquals(true, phoneValidator.validate("123-456-7890"));
	}

	@Test
	public void testPhoneValidatorWithSpace_1() {
		LOGGER.info("phoneValidator.validate(123 456 7890)");
		assertEquals(true, phoneValidator.validate("123 456 7890"));
	}

	@Test
	public void testPhoneValidatorWithoutHypen() {
		LOGGER.info("phoneValidator.validate(123.456.7890)");
		assertEquals(true, phoneValidator.validate("123-456-7890"));
	}

	@Test
	public void testPhoneValidatorWithExtension_1() {
		LOGGER.info("phoneValidator.validate(123-456-7890 ext1234)");
		assertEquals(true, phoneValidator.validate("123-456-7890 ext1234"));
	}

	@Test
	public void testPhoneValidatorWithBrace() {
		LOGGER.info("phoneValidator.validate((123)-456-7890)");
		assertEquals(true, phoneValidator.validate("(123)-456-7890"));
	}

}

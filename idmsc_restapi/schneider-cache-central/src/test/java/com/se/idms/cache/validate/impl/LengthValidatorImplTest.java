package com.se.idms.cache.validate.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;

import org.junit.Test;

public class LengthValidatorImplTest {

	@Test
	public void testPositiveScenario() {
		
		LengthValidatorImpl impl = new LengthValidatorImpl();
		String key = "IDMS_Email_opt_in__c";
		Object value = 1;

		assertEquals(true, impl.validate(key, value));
	}
	
	
	@Test
	public void testNegativeScenario() {
		
		LengthValidatorImpl impl = new LengthValidatorImpl();
		String key = "IDMS_Email_opt_in__c";
		Object value = 2;

		
		assertNotSame("Expecting the result to be 30", "31", impl.validate(key, value));
	}

}

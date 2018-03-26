package com.se.idms.cache.validate.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;

import org.junit.Test;


public class MandatoryValidatorImplTest {

	@Test
	public void testPositiveScenario() {
		
		MandatoryValidatorImpl mandatoryValidatorImpl = new MandatoryValidatorImpl();
		String key = "IDMS_Mandatory";
		Object value = "IDMS_User_Context__c";

		System.out.println(mandatoryValidatorImpl.validate(key, value));
		
		assertEquals(true, mandatoryValidatorImpl.validate(key, value));
	}
	
	
	@Test
	public void testNegativeScenario() {
		
		MandatoryValidatorImpl mandatoryValidatorImpl = new MandatoryValidatorImpl();
		String key = "IDMS_Mandatory";
		Object value = "IDMS_User_Context__c";

		System.out.println(mandatoryValidatorImpl.validate(key, value));
		
		assertNotSame(false, mandatoryValidatorImpl.validate(key, value));
	}
}

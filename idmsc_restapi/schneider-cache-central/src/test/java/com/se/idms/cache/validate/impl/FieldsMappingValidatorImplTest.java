package com.se.idms.cache.validate.impl;

import static org.junit.Assert.*;

import org.junit.Test;

public class FieldsMappingValidatorImplTest {

	@Test
	public void testPositiveScenario() {
		
		FieldsMappingValidatorImpl fieldsMappingValidatorImpl = new FieldsMappingValidatorImpl();
		String key = "MobilePhone";
		Object value = "mobile";
		
		System.out.println(fieldsMappingValidatorImpl.validate(key, value));
		assertEquals(true, fieldsMappingValidatorImpl.validate(key, value));
	}
	
	
	@Test
	public void testNegativeScenario() {
		
		FieldsMappingValidatorImpl fieldsMappingValidatorImpl = new FieldsMappingValidatorImpl();
		String key = "MobilePhone";
		Object value = "mobile";
		
		System.out.println(fieldsMappingValidatorImpl.validate(key, value));
		assertNotSame(false, fieldsMappingValidatorImpl.validate(key, value));
		
	}

}

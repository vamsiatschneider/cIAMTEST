package com.se.idms.cache.validate.impl;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;

import org.junit.Test;

public class MultiPickListValidatorImplTest {
	
	@Test
	public void testPositiveScenario(){
		
		MultiPickListValidatorImpl multiPickListValidatorImpl = new MultiPickListValidatorImpl();
		String key = "IDMSCompanyMarketServed__c";			 
		String multiPickList = "BA4;BA5;BD4";

		boolean flag = multiPickListValidatorImpl.validate(key, multiPickList);
		assertEquals(true, flag);
	}

	@Test
	public void testNegativeScenario(){
		
		MultiPickListValidatorImpl multiPickListValidatorImpl = new MultiPickListValidatorImpl();
		String key = "IDMSCompanyMarketServed__c";			 
		String multiPickList = "BA4;BA5;--";

		boolean flag = multiPickListValidatorImpl.validate(key, multiPickList);
		assertNotSame(true, flag);
	}

}

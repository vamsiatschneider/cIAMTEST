package com.se.idms.cache.validate.impl;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotSame;

public class PickListValidatorImplTest {
		
		@Test
		public void testPositiveScenario(){
			
			PickListValidatorImpl pickListValidatorImpl = new PickListValidatorImpl();
			String key = "Country";
			List<String> listOfCountries = new ArrayList<>();
			listOfCountries.add("AF");
			listOfCountries.add("AL");
			listOfCountries.add("DZ");
		

			boolean flag = pickListValidatorImpl.validate(key, "IN");
			assertEquals(true, flag);
		}

		@Test
		public void testNegativeScenario(){
			PickListValidatorImpl pickListValidatorImpl = new PickListValidatorImpl();
			String key = "Country";
			List<String> listOfCountries = new ArrayList<>();
			listOfCountries.add("AF");
			listOfCountries.add("AL");
			listOfCountries.add("----");
		

			boolean flag = pickListValidatorImpl.validate(key, "--");
			assertNotSame(true, flag);
		}
		
		@Test
		public void testCompanyNbrEmployees(){
			PickListValidatorImpl pickListValidatorImpl = new PickListValidatorImpl();
			String key = "IDMSCompanyNbrEmployees__c";

			boolean flag = pickListValidatorImpl.validate(key, "2 to 5");
			assertEquals(true, flag);
		}

}

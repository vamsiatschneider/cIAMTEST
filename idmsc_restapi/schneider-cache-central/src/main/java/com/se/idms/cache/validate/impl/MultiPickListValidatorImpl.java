package com.se.idms.cache.validate.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;

import org.springframework.stereotype.Component;

import com.se.idms.cache.api.CacheBuilder;
import com.se.idms.cache.api.CacheManagerProvider;
import com.se.idms.cache.api.CacheManagerProviderImpl;
import com.se.idms.cache.utils.IdmsConstants;
import com.se.idms.cache.validate.IValidator;

@Component("multiPickListValidator")
public class MultiPickListValidatorImpl implements IValidator {

	public boolean validate(String key, Object value) {
		CacheManagerProvider cacheManagerProvider = new CacheManagerProviderImpl();
		CacheBuilder cacheBuilder = new CacheBuilder(cacheManagerProvider);
		Properties cacheProperties = cacheBuilder.getProperties(IdmsConstants.IDMS_FIELDSMULTI_PICKLIST_PROPERTIES_PATH);
		String pickListProperty = cacheProperties.getProperty(key);

		// multiPickListProperty will be in the form of comma separated string.
		// Eg.
		// IDMSCompanyMarketServed__c=BA4,BA5,BDZ,BD1,BD3,BD4,BD6,BD9

		System.out.println("properties from cache:" + pickListProperty);
		List<String> companyMarketServedList = Arrays.asList(pickListProperty.split(","));

		System.out.println("companyMarketServedList size" + "-->" + companyMarketServedList.size());
		String multiPickList = (String) value;

		List<String> companyMarketServedListValue = new ArrayList<String>(Arrays.asList(multiPickList.split(";")));
		companyMarketServedListValue.removeAll(companyMarketServedList);

		if (companyMarketServedListValue.size() > 0) {
			return false;
		}

		return true;
	}

}

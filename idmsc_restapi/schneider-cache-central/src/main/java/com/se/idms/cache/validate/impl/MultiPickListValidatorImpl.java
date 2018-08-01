package com.se.idms.cache.validate.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;

import com.se.idms.cache.api.CacheBuilder;
import com.se.idms.cache.api.CacheManagerProvider;
import com.se.idms.cache.api.CacheManagerProviderImpl;
import com.se.idms.cache.utils.IdmsConstants;
import com.se.idms.cache.validate.IValidator;

@Component("multiPickListValidator")
public class MultiPickListValidatorImpl implements IValidator {
	private static final Logger LOGGER = LoggerFactory.getLogger(MultiPickListValidatorImpl.class);

	public boolean validate(String key, Object value) {
		LOGGER.info("Entered validate() -> Start");
		LOGGER.info("Parameter key -> " + key+" ,value -> "+value);
		CacheManagerProvider cacheManagerProvider = new CacheManagerProviderImpl();
		CacheBuilder cacheBuilder = new CacheBuilder(cacheManagerProvider);
		Properties cacheProperties = cacheBuilder.getProperties(IdmsConstants.IDMS_FIELDSMULTI_PICKLIST_PROPERTIES_PATH);
		String pickListProperty = cacheProperties.getProperty(key);

		// multiPickListProperty will be in the form of comma separated string.
		// Eg.
		// IDMSCompanyMarketServed__c=BA4,BA5,BDZ,BD1,BD3,BD4,BD6,BD9

		//LOGGER.info("properties from cache:" + pickListProperty);
		List<String> companyMarketServedList = Arrays.asList(pickListProperty.split(","));

		//LOGGER.info("companyMarketServedList size" + "-->" + companyMarketServedList.size());
		String multiPickList = (String) value;

		List<String> companyMarketServedListValue = new ArrayList<String>(Arrays.asList(multiPickList.split(";")));
		companyMarketServedListValue.removeAll(companyMarketServedList);

		if (companyMarketServedListValue.size() > 0) {
			LOGGER.error("Validation of key:"+key+" ,value:"+value+" is NOT OK! and validate() is Ending");
			return false;
		}

		LOGGER.info("Validation of key:"+key+" ,value:"+value+" is OK! and validate() is Ending");
		return true;
	}

}

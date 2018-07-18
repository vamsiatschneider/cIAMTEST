package com.se.idms.cache.validate.impl;

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

@Component("pickListValidator")
public class PickListValidatorImpl implements IValidator {
	private static final Logger LOGGER = LoggerFactory.getLogger(PickListValidatorImpl.class);

	@Override
	public boolean validate(String key, Object value) {
		LOGGER.info("Entered validate() -> Start");
		LOGGER.info("Parameter key -> " + key+" ,value -> "+value);
		CacheManagerProvider cacheManagerProvider = new CacheManagerProviderImpl();
		CacheBuilder cacheBuilder = new CacheBuilder(cacheManagerProvider);
		Properties cacheProperties = cacheBuilder.getProperties(IdmsConstants.IDMS_FIELDSPICKLIST_PROPERTIES_PATH);
		String pickListProperty = cacheProperties.getProperty(key);

		// pickListProperty will be in the form of comma separated string. Eg.
		// AF,AL,DZ,AD,AO,AI,AQ,AG,AR,AM,AW,AU

		LOGGER.info("properties from cache:" + pickListProperty);
		List<String> pickListCache = Arrays.asList(pickListProperty.split(","));
		LOGGER.info("countryList" + "-->" + pickListCache.size());
		String pickListValue = (String) value;

		LOGGER.info("country list=" + pickListValue);

		/*
		 * for (Object obj : countryPickListValue) {
		 * LOGGER.info(obj.toString().trim()); if
		 * (countryPickListCache.contains(obj) == false) { return false; } }
		 */
		if (pickListCache.contains(pickListValue)) {
			return true;
		}

		return false;
	}
}

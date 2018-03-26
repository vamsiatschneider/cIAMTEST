package com.se.idms.cache.validate.impl;

import java.util.Arrays;
import java.util.List;
import java.util.Properties;

import org.springframework.stereotype.Component;

import com.se.idms.cache.api.CacheBuilder;
import com.se.idms.cache.api.CacheManagerProvider;
import com.se.idms.cache.api.CacheManagerProviderImpl;
import com.se.idms.cache.utils.IdmsConstants;
import com.se.idms.cache.validate.IValidator;

@Component("pickListValidator")
public class PickListValidatorImpl implements IValidator {

	@Override
	public boolean validate(String key, Object value) {
		CacheManagerProvider cacheManagerProvider = new CacheManagerProviderImpl();
		CacheBuilder cacheBuilder = new CacheBuilder(cacheManagerProvider);
		Properties cacheProperties = cacheBuilder.getProperties(IdmsConstants.IDMS_FIELDSPICKLIST_PROPERTIES_PATH);
		String pickListProperty = cacheProperties.getProperty(key);

		// pickListProperty will be in the form of comma separated string. Eg.
		// AF,AL,DZ,AD,AO,AI,AQ,AG,AR,AM,AW,AU

		System.out.println("properties from cache:" + pickListProperty);
		List<String> pickListCache = Arrays.asList(pickListProperty.split(","));
		System.out.println("countryList" + "-->" + pickListCache.size());
		String pickListValue = (String) value;

		System.out.println("country list=" + pickListValue);

		/*
		 * for (Object obj : countryPickListValue) {
		 * System.out.println(obj.toString().trim()); if
		 * (countryPickListCache.contains(obj) == false) { return false; } }
		 */
		if (pickListCache.contains(pickListValue)) {
			return true;
		}

		return false;
	}
}

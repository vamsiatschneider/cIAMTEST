package com.se.idms.cache.validate.impl;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Component;
import org.springframework.util.ResourceUtils;

import com.se.idms.cache.api.CacheBuilder;
import com.se.idms.cache.api.CacheManagerProvider;
import com.se.idms.cache.api.CacheManagerProviderImpl;
import com.se.idms.cache.utils.IdmsConstants;
import com.se.idms.cache.validate.IValidator;

@Component("legthValidator")
public class LengthValidatorImpl implements IValidator {

	private static final Logger LOGGER = LoggerFactory.getLogger(LengthValidatorImpl.class);

	@Override
	public boolean validate(String key, Object value) {
		LOGGER.info("Entered validate() -> Start");
		LOGGER.info("Parameter key -> " + key+" ,value -> "+value);
		try {
			CacheManagerProvider cacheManagerProvider = new CacheManagerProviderImpl();
			CacheBuilder cacheBuilder = new CacheBuilder(cacheManagerProvider);
			Properties cacheProperties = cacheBuilder.getProperties(IdmsConstants.IDMS_FIELDSLENGTH_PROPERTIES_PATH);
			String lengthProperty = cacheProperties.getProperty(key).trim();

			LOGGER.debug("lengthProperty from the cache is:" + lengthProperty);

			String strVal = value.toString();
			Integer len1 = Integer.valueOf(strVal.length());
			Integer len2 = Integer.valueOf(lengthProperty);

			if (len1 <= len2) {
				return true;
			}
		} catch (Exception e) {
			LOGGER.error("LengthValidatorImpl.validate::"+e.getMessage());
		}
		return false;
	}

	
}

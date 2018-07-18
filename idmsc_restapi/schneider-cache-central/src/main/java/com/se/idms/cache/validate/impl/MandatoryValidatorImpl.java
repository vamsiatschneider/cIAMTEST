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

@Component("mandatoryValidator")
public class MandatoryValidatorImpl  implements IValidator{
	private static final Logger LOGGER = LoggerFactory.getLogger(MultiPickListValidatorImpl.class);

	@Override
	public boolean validate(String key, Object value) {
		LOGGER.info("Entered validate() -> Start");
		LOGGER.info("Parameter key -> " + key+" ,value -> "+value);
		CacheManagerProvider cacheManagerProvider = new CacheManagerProviderImpl();
		CacheBuilder cacheBuilder = new CacheBuilder(cacheManagerProvider );
		Properties cacheProperties = cacheBuilder.getProperties(IdmsConstants.IDMS_FIELDSMANDATORY_PROPERTIES_PATH);
		String mandatoryProperty = cacheProperties.getProperty(key);
		
		String[] mandatoryPropertySplitter = mandatoryProperty.split(",");
		List<String> list = Arrays.asList(mandatoryPropertySplitter);
		
		
		LOGGER.info("properties from cache::mandatoryProperty="+mandatoryProperty);
		if(list.contains(value)){
			return true;
		}
		
		return false;
	}

}

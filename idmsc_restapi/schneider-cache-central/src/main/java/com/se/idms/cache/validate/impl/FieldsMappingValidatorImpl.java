package com.se.idms.cache.validate.impl;

import java.util.Properties;

import org.springframework.stereotype.Component;

import com.se.idms.cache.api.CacheBuilder;
import com.se.idms.cache.api.CacheManagerProvider;
import com.se.idms.cache.api.CacheManagerProviderImpl;
import com.se.idms.cache.utils.IdmsConstants;
import com.se.idms.cache.validate.IValidator;

@Component("fieldsMappingValidator")
public class FieldsMappingValidatorImpl  implements IValidator{

	@Override
	public boolean validate(String key, Object value) {
		CacheManagerProvider cacheManagerProvider = new CacheManagerProviderImpl();
		CacheBuilder cacheBuilder = new CacheBuilder(cacheManagerProvider );
		Properties cacheProperties = cacheBuilder.getProperties(IdmsConstants.IDMS_FIELDSMAPPING_PROPERTIES_PATH);
		String fieldMapProperty = cacheProperties.getProperty(key);
		
		System.out.println("properties from cache::fieldMapProperty="+fieldMapProperty);
		
		if(value.equals(fieldMapProperty)){
			return true;
		}

		return false;
	}
	
}

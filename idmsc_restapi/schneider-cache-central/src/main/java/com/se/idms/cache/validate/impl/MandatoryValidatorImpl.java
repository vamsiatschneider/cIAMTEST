package com.se.idms.cache.validate.impl;

import java.util.Arrays;
import java.util.List;
import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.se.idms.cache.api.CacheBuilder;
import com.se.idms.cache.api.CacheManagerProvider;
import com.se.idms.cache.api.CacheManagerProviderImpl;
import com.se.idms.cache.utils.IdmsConstants;
import com.se.idms.cache.validate.IValidator;

@Component("mandatoryValidator")
public class MandatoryValidatorImpl  implements IValidator{
	private static final Logger LOGGER = LoggerFactory.getLogger(MultiPickListValidatorImpl.class);
	
	//CODE-RE-STRUCTURING
	@Value("${fields.mandatory.props.path}")
	private String IDMS_FIELDSMANDATORY_PROPERTIES_PATH;

	@Override
	public boolean validate(String key, Object value) {
		LOGGER.info("Entered validate() -> Start");
		LOGGER.info("Parameter key -> " + key+" ,value -> "+value);
		CacheManagerProvider cacheManagerProvider = new CacheManagerProviderImpl();
		CacheBuilder cacheBuilder = new CacheBuilder(cacheManagerProvider );
		Properties cacheProperties = cacheBuilder.getProperties(IDMS_FIELDSMANDATORY_PROPERTIES_PATH);
		String mandatoryProperty = cacheProperties.getProperty(key);
		
		String[] mandatoryPropertySplitter = mandatoryProperty.split(",");
		List<String> list = Arrays.asList(mandatoryPropertySplitter);
		
		
		//LOGGER.info("properties from cache::mandatoryProperty="+mandatoryProperty);
		if(list.contains(value)){
			LOGGER.info("Mandatory Validation of key:"+key+" ,value:"+value+" is OK! and validate() is Ending");
			return true;
		}
		LOGGER.error("Mandatory Validation of key:"+key+" ,value:"+value+" is NOT OK! and validate() is Ending");
		return false;
	}

}

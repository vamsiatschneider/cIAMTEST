package com.se.idms.cache.validate.impl;

import java.util.Properties;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.se.idms.cache.api.CacheBuilder;
import com.se.idms.cache.api.CacheManagerProvider;
import com.se.idms.cache.api.CacheManagerProviderImpl;
import com.se.idms.cache.validate.IValidator;

@Component("fieldsMappingValidator")
public class FieldsMappingValidatorImpl  implements IValidator{
	private static final Logger LOGGER = LoggerFactory.getLogger(FieldsMappingValidatorImpl.class);

	//CODE-RE-STRUCTURING
	@Value("${fields.mapping.props.path}")
	private String IDMS_FIELDSMAPPING_PROPERTIES_PATH;
	
	@Value("${idms.env}")
	private String IDMS_DEPLOY_ENV;
	
	@Override
	public boolean validate(String key, Object value) {
		LOGGER.info("Entered validate() -> Start");
		LOGGER.info("Parameter key -> " + key+" ,value -> "+value);
		CacheManagerProvider cacheManagerProvider = new CacheManagerProviderImpl();
		CacheBuilder cacheBuilder = new CacheBuilder(cacheManagerProvider );
		//Properties cacheProperties =null;
		if(IDMS_DEPLOY_ENV.equalsIgnoreCase("DEV"))
			IDMS_FIELDSMAPPING_PROPERTIES_PATH=IDMS_FIELDSMAPPING_PROPERTIES_PATH.replaceAll("/", "\\\\");
		Properties cacheProperties = cacheBuilder.getProperties(IDMS_FIELDSMAPPING_PROPERTIES_PATH);
		/*if(CacheBuilder.getPropertiesMap().size()>0){
			CacheBuilder.getPropertiesMap().entrySet().forEach(entry -> {
				LOGGER.info("Key : " + entry.getKey() + " Value : " + entry.getValue());
			});  
			cacheProperties=CacheBuilder.getPropertiesMap().get(IDMS_FIELDSMAPPING_PROPERTIES_PATH);
		}
		//Properties cacheProperties = cacheBuilder.getProperties(IDMS_FIELDSPICKLIST_PROPERTIES_PATH);
		if(cacheProperties==null){
		   cacheProperties = cacheBuilder.getProperties(IDMS_FIELDSMAPPING_PROPERTIES_PATH);
		}*/
		String fieldMapProperty = cacheProperties.getProperty(key);
		
		LOGGER.info("properties from cache::fieldMapProperty="+fieldMapProperty);
		
		if(value.equals(fieldMapProperty)){
			LOGGER.info("Validation of "+key+":"+value+" is OK! and validate() is Ending");
			return true;
		}

		LOGGER.error("Validation of "+key+":"+value+" is NOT OK! and validate() is Ending");
		return false;
	}

	public void setIDMS_FIELDSMAPPING_PROPERTIES_PATH(String iDMS_FIELDSMAPPING_PROPERTIES_PATH) {
		IDMS_FIELDSMAPPING_PROPERTIES_PATH = iDMS_FIELDSMAPPING_PROPERTIES_PATH;
	}

	public void setIDMS_DEPLOY_ENV(String iDMS_DEPLOY_ENV) {
		IDMS_DEPLOY_ENV = iDMS_DEPLOY_ENV;
	}

	public String getIDMS_FIELDSMAPPING_PROPERTIES_PATH() {
		return IDMS_FIELDSMAPPING_PROPERTIES_PATH;
	}

	public String getIDMS_DEPLOY_ENV() {
		return IDMS_DEPLOY_ENV;
	}
	
}

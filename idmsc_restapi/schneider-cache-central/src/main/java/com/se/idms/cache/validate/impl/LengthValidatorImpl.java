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

@Component("legthValidator")
public class LengthValidatorImpl implements IValidator {

	private static final Logger LOGGER = LoggerFactory.getLogger(LengthValidatorImpl.class);
	
	//CODE-RE-STRUCTURING
	@Value("${fields.length.props.path}")
	private String IDMS_FIELDSLENGTH_PROPERTIES_PATH;
	
	@Value("${idms.env}")
	private String IDMS_DEPLOY_ENV;

	@Override
	public boolean validate(String key, Object value) {
		//LOGGER.info("Entered validate() -> Start");
		if(LOGGER.isInfoEnabled())
			LOGGER.info("Parameter key -> " + key+" ,value -> "+value);
		try {
			CacheManagerProvider cacheManagerProvider = new CacheManagerProviderImpl();
			CacheBuilder cacheBuilder = new CacheBuilder(cacheManagerProvider);
			//Properties cacheProperties =null;
			if(IDMS_DEPLOY_ENV.equalsIgnoreCase("DEV"))
				IDMS_FIELDSLENGTH_PROPERTIES_PATH=IDMS_FIELDSLENGTH_PROPERTIES_PATH.replaceAll("/", "\\\\");
			Properties cacheProperties = cacheBuilder.getProperties(IDMS_FIELDSLENGTH_PROPERTIES_PATH);
			/*if(CacheBuilder.getPropertiesMap().size()>0){
				CacheBuilder.getPropertiesMap().entrySet().forEach(entry -> {
					LOGGER.info("Key : " + entry.getKey() + " Value : " + entry.getValue());
				});  
				cacheProperties=CacheBuilder.getPropertiesMap().get(IDMS_FIELDSLENGTH_PROPERTIES_PATH);
			}
			//Properties cacheProperties = cacheBuilder.getProperties(IDMS_FIELDSPICKLIST_PROPERTIES_PATH);
			if(cacheProperties==null){
			   cacheProperties = cacheBuilder.getProperties(IDMS_FIELDSLENGTH_PROPERTIES_PATH);
			}*/
			String lengthProperty = cacheProperties.getProperty(key).trim();
			//LOGGER.info("lengthProperty::"+lengthProperty);
			//LOGGER.debug("lengthProperty from the cache is:" + lengthProperty);
			String strVal = value.toString();
			Integer len1 = Integer.valueOf(strVal.length());
			Integer len2 = Integer.valueOf(lengthProperty);
			if (len1 <= len2) {
				//LOGGER.info("Length Validation of "+key+":"+value+" is OK! and validate() is Ending");
				return true;
			}
		} catch (Exception e) {
			//LOGGER.error("Length Validation of "+key+":"+value+" is having error:"+e.getMessage());
			if(LOGGER.isErrorEnabled())
				LOGGER.error("Exception "+e);
		}
		//LOGGER.error("Length Validation of "+key+":"+value+" is NOT OK! and validate() is Ending");
		return false;
	}

	public void setIDMS_FIELDSLENGTH_PROPERTIES_PATH(String iDMS_FIELDSLENGTH_PROPERTIES_PATH) {
		IDMS_FIELDSLENGTH_PROPERTIES_PATH = iDMS_FIELDSLENGTH_PROPERTIES_PATH;
	}

	public void setIDMS_DEPLOY_ENV(String iDMS_DEPLOY_ENV) {
		IDMS_DEPLOY_ENV = iDMS_DEPLOY_ENV;
	}

	public String getIDMS_FIELDSLENGTH_PROPERTIES_PATH() {
		return IDMS_FIELDSLENGTH_PROPERTIES_PATH;
	}

	public String getIDMS_DEPLOY_ENV() {
		return IDMS_DEPLOY_ENV;
	}

	
}

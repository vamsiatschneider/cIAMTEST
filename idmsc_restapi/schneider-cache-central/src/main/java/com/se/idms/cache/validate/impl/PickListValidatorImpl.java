package com.se.idms.cache.validate.impl;

import java.util.Arrays;
import java.util.List;
import java.util.Map;
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

@Component("pickListValidator")
public class PickListValidatorImpl implements IValidator {
	private static final Logger LOGGER = LoggerFactory.getLogger(PickListValidatorImpl.class);

	//CODE-RE-STRUCTURING
	@Value("${fields.picklist.props.path}")
	private String IDMS_FIELDSPICKLIST_PROPERTIES_PATH;
	
	@Value("${idms.env}")
	private String IDMS_DEPLOY_ENV;
		
	@Override
	public boolean validate(String key, Object value) {
		LOGGER.info("Entered validate() -> Start");
		LOGGER.info("Parameter key -> " + key+" ,value -> "+value);
		CacheManagerProvider cacheManagerProvider = new CacheManagerProviderImpl();
		//Properties cacheProperties=null;
		CacheBuilder cacheBuilder = new CacheBuilder(cacheManagerProvider);
		LOGGER.info("picklist path first:"+IDMS_FIELDSPICKLIST_PROPERTIES_PATH);
		if(IDMS_DEPLOY_ENV.equalsIgnoreCase("DEV"))
			IDMS_FIELDSPICKLIST_PROPERTIES_PATH=IDMS_FIELDSPICKLIST_PROPERTIES_PATH.replaceAll("/", "\\\\");
		LOGGER.info("picklist path after conversion:"+IDMS_FIELDSPICKLIST_PROPERTIES_PATH);
		Properties cacheProperties = cacheBuilder.getProperties(IDMS_FIELDSPICKLIST_PROPERTIES_PATH);
		/*if(CacheBuilder.getPropertiesMap().size()>0){
			CacheBuilder.getPropertiesMap().entrySet().forEach(entry -> {
				LOGGER.info("Key : " + entry.getKey() + " Value : " + entry.getValue());
			});  
			cacheProperties=CacheBuilder.getPropertiesMap().get(IDMS_FIELDSPICKLIST_PROPERTIES_PATH);
		}
		if(cacheProperties==null){
		Properties cacheProperties = cacheBuilder.getProperties(IDMS_FIELDSPICKLIST_PROPERTIES_PATH);
		}*/
		String pickListProperty = cacheProperties.getProperty(key);
		//LOGGER.info("pickListProperty::"+pickListProperty);
		List<String> pickListCache = Arrays.asList(pickListProperty.split(","));
		String pickListValue = (String) value;
		for(String str:pickListCache){
			if(str.toLowerCase().equalsIgnoreCase(pickListValue.toLowerCase())){
				LOGGER.info("Validation of key:"+key+" ,value:"+value+" is OK! and validate() is Ending");
				return true;
			}
		}
		/*if (pickListCache.contains(pickListValue)) {
			LOGGER.info("Validation of key:"+key+" ,value:"+value+" is OK! and validate() is Ending");
			return true;
		}*/
		LOGGER.error("Validation of key:"+key+" ,value:"+value+" is NOT OK! and validate() is Ending");
		return false;
	}

	public void setIDMS_FIELDSPICKLIST_PROPERTIES_PATH(String iDMS_FIELDSPICKLIST_PROPERTIES_PATH) {
		IDMS_FIELDSPICKLIST_PROPERTIES_PATH = iDMS_FIELDSPICKLIST_PROPERTIES_PATH;
	}

	public void setIDMS_DEPLOY_ENV(String iDMS_DEPLOY_ENV) {
		IDMS_DEPLOY_ENV = iDMS_DEPLOY_ENV;
	}

	public String getIDMS_DEPLOY_ENV() {
		return IDMS_DEPLOY_ENV;
	}

	public String getIDMS_FIELDSPICKLIST_PROPERTIES_PATH() {
		return IDMS_FIELDSPICKLIST_PROPERTIES_PATH;
	}
}

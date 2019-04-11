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
			IDMS_FIELDSMANDATORY_PROPERTIES_PATH=IDMS_FIELDSMANDATORY_PROPERTIES_PATH.replaceAll("/", "\\\\");
		Properties cacheProperties = cacheBuilder.getProperties(IDMS_FIELDSMANDATORY_PROPERTIES_PATH);
		/*if(CacheBuilder.getPropertiesMap().size()>0){
			CacheBuilder.getPropertiesMap().entrySet().forEach(entry -> {
				LOGGER.info("Key : " + entry.getKey() + " Value : " + entry.getValue());
			});  
			cacheProperties=CacheBuilder.getPropertiesMap().get(IDMS_FIELDSMANDATORY_PROPERTIES_PATH);
		}
		//Properties cacheProperties = cacheBuilder.getProperties(IDMS_FIELDSPICKLIST_PROPERTIES_PATH);
		if(cacheProperties==null){
		   cacheProperties = cacheBuilder.getProperties(IDMS_FIELDSMANDATORY_PROPERTIES_PATH);
		}*/
		String mandatoryProperty = cacheProperties.getProperty(key);
		LOGGER.info("mandatoryProperty::"+mandatoryProperty);
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

	public void setIDMS_FIELDSMANDATORY_PROPERTIES_PATH(String iDMS_FIELDSMANDATORY_PROPERTIES_PATH) {
		IDMS_FIELDSMANDATORY_PROPERTIES_PATH = iDMS_FIELDSMANDATORY_PROPERTIES_PATH;
	}

	public void setIDMS_DEPLOY_ENV(String iDMS_DEPLOY_ENV) {
		IDMS_DEPLOY_ENV = iDMS_DEPLOY_ENV;
	}

	public String getIDMS_FIELDSMANDATORY_PROPERTIES_PATH() {
		return IDMS_FIELDSMANDATORY_PROPERTIES_PATH;
	}

	public String getIDMS_DEPLOY_ENV() {
		return IDMS_DEPLOY_ENV;
	}

}

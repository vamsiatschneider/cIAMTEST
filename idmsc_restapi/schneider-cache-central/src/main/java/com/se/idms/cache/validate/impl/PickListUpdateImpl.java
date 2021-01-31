package com.se.idms.cache.validate.impl;

import static com.se.idms.cache.utils.CacheUtils.writePropertiesFile;

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
import com.se.idms.cache.validate.PickListUpdate;

@Component("pickListUpdateComponent")
public class PickListUpdateImpl implements PickListUpdate {
	private static final Logger LOGGER = LoggerFactory.getLogger(PickListUpdateImpl.class);

	@Value("${fields.picklist.props.path}")
	private String IDMS_FIELDSPICKLIST_PROPERTIES_PATH;
	
	@Value("${idms.env}")
	private String IDMS_DEPLOY_ENV;
		
	@Override
	public boolean update(String key, String pickListValue) {
		boolean isUpdated = false;
		LOGGER.info("Parameter key -> " + key+" ,value -> "+pickListValue);
		Properties cacheProperties = getProperty();
		String pickListProperty = cacheProperties.getProperty(key);
		List<String> pickListCache = Arrays.asList(pickListProperty.split(","));
		for(String str:pickListCache){
			if(str.toLowerCase().equalsIgnoreCase(pickListValue.toLowerCase())){
				LOGGER.info("The application " + pickListValue +" is already present");
				return true;
			}
		}
		StringBuilder sb = new StringBuilder(pickListProperty);
		sb.append(",");
		sb.append(pickListValue);
		cacheProperties.setProperty(key, sb.toString());
		try {
			isUpdated = writePropertiesFile(cacheProperties, IDMS_FIELDSPICKLIST_PROPERTIES_PATH);
		} catch (Exception ex) {
			LOGGER.info("Update picklist failed with exception : " + ex);
			isUpdated = false;
		}
		return isUpdated;
	}

	private Properties getProperty() {
		CacheManagerProvider cacheManagerProvider = new CacheManagerProviderImpl();
		CacheBuilder cacheBuilder = new CacheBuilder(cacheManagerProvider);
		if(IDMS_DEPLOY_ENV.equalsIgnoreCase("DEV"))
			IDMS_FIELDSPICKLIST_PROPERTIES_PATH=IDMS_FIELDSPICKLIST_PROPERTIES_PATH.replaceAll("/", "\\\\");

		Properties cacheProperties = cacheBuilder.getProperties(IDMS_FIELDSPICKLIST_PROPERTIES_PATH);
		return cacheProperties;
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

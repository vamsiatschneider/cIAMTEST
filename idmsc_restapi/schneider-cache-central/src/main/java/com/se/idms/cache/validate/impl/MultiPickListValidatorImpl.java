package com.se.idms.cache.validate.impl;

import java.util.ArrayList;
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
import com.se.idms.cache.validate.IValidator;

@Component("multiPickListValidator")
public class MultiPickListValidatorImpl implements IValidator {
	private static final Logger LOGGER = LoggerFactory.getLogger(MultiPickListValidatorImpl.class);

	//CODE-RE-STRUCTURING
	@Value("${fields.multi.picklist.props.path}")
	private String IDMS_FIELDSMULTI_PICKLIST_PROPERTIES_PATH;
	
	@Value("${idms.env}")
	private String IDMS_DEPLOY_ENV;
		
	public boolean validate(String key, Object value) {
		//LOGGER.info("Entered validate() -> Start");
		LOGGER.info("Parameter key -> " + key+" ,value -> "+value);
		CacheManagerProvider cacheManagerProvider = new CacheManagerProviderImpl();
		CacheBuilder cacheBuilder = new CacheBuilder(cacheManagerProvider);
		if(IDMS_DEPLOY_ENV.equalsIgnoreCase("DEV"))
			IDMS_FIELDSMULTI_PICKLIST_PROPERTIES_PATH=IDMS_FIELDSMULTI_PICKLIST_PROPERTIES_PATH.replaceAll("/", "\\\\");
		Properties cacheProperties = cacheBuilder.getProperties(IDMS_FIELDSMULTI_PICKLIST_PROPERTIES_PATH);
		//Properties cacheProperties=null;
		/*if(CacheBuilder.getPropertiesMap().size()>0){
			CacheBuilder.getPropertiesMap().entrySet().forEach(entry -> {
				LOGGER.info("Key : " + entry.getKey() + " Value : " + entry.getValue());
			});  
			cacheProperties=CacheBuilder.getPropertiesMap().get(IDMS_FIELDSMULTI_PICKLIST_PROPERTIES_PATH);
		}
		if(cacheProperties==null){
		Properties   cacheProperties = cacheBuilder.getProperties(IDMS_FIELDSMULTI_PICKLIST_PROPERTIES_PATH);
		}*/
		String pickListProperty = cacheProperties.getProperty(key);
		// multiPickListProperty will be in the form of comma separated string.
		// Eg.
		// IDMSCompanyMarketServed__c=BA4,BA5,BDZ,BD1,BD3,BD4,BD6,BD9
		//LOGGER.info("multiPickList properties from cache:" + pickListProperty);
		List<String> companyMarketServedList = Arrays.asList(pickListProperty.split(","));
		String multiPickList = (String) value;

		List<String> companyMarketServedListValue = new ArrayList<String>(Arrays.asList(multiPickList.split(";")));
		companyMarketServedListValue.removeAll(companyMarketServedList);

		if (companyMarketServedListValue.size() > 0) {
			//LOGGER.error("Validation of "+key+":"+value+" is NOT OK! and validate() is Ending");
			return false;
		}

		//LOGGER.info("Validation of "+key+":"+value+" is OK! and validate() is Ending");
		return true;
	}

	public void setIDMS_FIELDSMULTI_PICKLIST_PROPERTIES_PATH(String iDMS_FIELDSMULTI_PICKLIST_PROPERTIES_PATH) {
		IDMS_FIELDSMULTI_PICKLIST_PROPERTIES_PATH = iDMS_FIELDSMULTI_PICKLIST_PROPERTIES_PATH;
	}

	public void setIDMS_DEPLOY_ENV(String iDMS_DEPLOY_ENV) {
		IDMS_DEPLOY_ENV = iDMS_DEPLOY_ENV;
	}

	public String getIDMS_FIELDSMULTI_PICKLIST_PROPERTIES_PATH() {
		return IDMS_FIELDSMULTI_PICKLIST_PROPERTIES_PATH;
	}

	public String getIDMS_DEPLOY_ENV() {
		return IDMS_DEPLOY_ENV;
	}

}

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
import com.se.idms.cache.validate.IValidator;

@Component("appOnboardingFieldsValidator")
public class AppOnboardingFieldsValidatorImpl implements IValidator {

	private static final Logger LOGGER = LoggerFactory.getLogger(AppOnboardingFieldsValidatorImpl.class);

	@Value("${fields.app.onboarding.props.path}")
	private String propsPath;
	
	@Value("${idms.env}")
	private String deploymentEnv;
	
	@Override
	public boolean validate(String key, Object value) {
		LOGGER.info("Entered validate() -> Start");
		LOGGER.info("Parameter key -> " + key+" ,value -> "+value);
		CacheManagerProvider cacheManagerProvider = new CacheManagerProviderImpl();
		CacheBuilder cacheBuilder = new CacheBuilder(cacheManagerProvider);
		if(deploymentEnv.equalsIgnoreCase("DEV"))
			propsPath=propsPath.replaceAll("/", "\\\\");

		Properties cacheProperties = cacheBuilder.getProperties(propsPath);
		String pickListProperty = cacheProperties.getProperty(key);
		List<String> pickListCache = Arrays.asList(pickListProperty.split(","));
		String pickListValue = (String) value;
		for(String str:pickListCache){
			if(str.toLowerCase().equalsIgnoreCase(pickListValue.toLowerCase())){
				return true;
			}
		}
		return false;
	}
}

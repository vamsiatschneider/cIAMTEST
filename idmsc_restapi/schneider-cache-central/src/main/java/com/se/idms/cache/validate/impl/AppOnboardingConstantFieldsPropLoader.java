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
import com.se.idms.cache.validate.PropertyLoader;

@Component("appOnboardingConstantsPropLoader")
public class AppOnboardingConstantFieldsPropLoader implements PropertyLoader{

	private static final Logger LOGGER = LoggerFactory.getLogger(AppOnboardingConstantFieldsPropLoader.class);

	@Value("${fields.app.onboarding.const.props.path}")
	private String propsPath;
	
	@Value("${idms.env}")
	private String deploymentEnv;
	
	@Override
	public Properties getProperties() {
		LOGGER.info("Entered getProperties() -> Start");
		CacheManagerProvider cacheManagerProvider = new CacheManagerProviderImpl();
		CacheBuilder cacheBuilder = new CacheBuilder(cacheManagerProvider);
		if(deploymentEnv.equalsIgnoreCase("DEV"))
			propsPath=propsPath.replaceAll("/", "\\\\");

		Properties cacheProperties = cacheBuilder.getProperties(propsPath);
//		for (String key1: cacheProperties.stringPropertyNames()) {
//            System.out.println(key1 + ": " + cacheProperties.getProperty(key1));
//        }
		return cacheProperties;
	}
}

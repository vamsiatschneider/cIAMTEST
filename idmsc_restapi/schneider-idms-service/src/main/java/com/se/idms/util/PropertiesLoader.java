package com.se.idms.util;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class PropertiesLoader {
	private static final Logger LOGGER = LoggerFactory.getLogger(PropertiesLoader.class);

	/*
	private static String getProps(String key) {
		Properties prop = new Properties();
		InputStream input = null;

		try {

			input = new FileInputStream(ResourceUtils.getFile("classpath:config/properties/application.properties"));

			// load a properties file
			prop.load(input);

			// get the property value and print it out
			return prop.getProperty(key);

		} catch (IOException ex) {
			LOGGER.error("An error occured."+ex.getMessage(),ex);
		} finally {
			if (input != null) {
				try {
					input.close();
				} catch (IOException e) {
					LOGGER.error("An error occured."+e.getMessage(),e);
				}
			}
		}
		return null;
	}*/
}

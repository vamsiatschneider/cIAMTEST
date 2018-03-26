package com.se.idms.util;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import org.springframework.util.ResourceUtils;

public class PropertiesLoader {

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
			ex.printStackTrace();
		} finally {
			if (input != null) {
				try {
					input.close();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}
		return null;
	}
}

/**
 * 
 */
package com.se.idms.cache.utils;

import java.io.*;
import java.util.Properties;

import org.apache.commons.lang3.StringUtils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class CacheUtils {
	
    private static final Logger logger = LoggerFactory.getLogger(CacheUtils.class);

	public static Properties readPropertiesFile(String propertiesPathFileName) throws Exception {
		
		InputStream inputStream = null;
		Properties properties = new Properties();
		
		if (StringUtils.isNotEmpty(propertiesPathFileName)){
			try {
				// load properties file
				inputStream = new FileInputStream(propertiesPathFileName);	
				InputStreamReader inputStreamReader = new InputStreamReader(inputStream, "UTF8");
				properties.load(inputStreamReader);
				return properties;
            }catch (FileNotFoundException e){
            	logger.error("The file " + propertiesPathFileName + " is missing ", e);
                throw new Exception("The properties file: " + propertiesPathFileName
    					+ " could not be accessed.");
            }finally {
				
				if (inputStream != null) {
					try {
						inputStream.close();
					} catch (IOException e) {
						logger.error(e.getMessage() + "\n The Cause: " + e.getCause());
						
					}
				}
			}
		}
		return properties;
	}
}

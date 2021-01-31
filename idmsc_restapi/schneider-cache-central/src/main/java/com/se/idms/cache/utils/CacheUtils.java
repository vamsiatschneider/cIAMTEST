/**
 * 
 */
package com.se.idms.cache.utils;

import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
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
            	if(logger.isErrorEnabled())
            		logger.error("The file " + propertiesPathFileName + " is missing ", e);
                throw new Exception("The properties file: " + propertiesPathFileName
    					+ " could not be accessed.");
            }finally {
				
				if (inputStream != null) {
					try {
						inputStream.close();
					} catch (IOException e) {
						if(logger.isErrorEnabled())
							logger.error(e.getMessage() + "\n The Cause: " + e.getCause());
					}
				}
			}
		}
		return properties;
	}

	public static boolean writePropertiesFile(Properties properties, String propertiesPathFileName) throws Exception {
		boolean isUpdated = false;
		if (StringUtils.isNotBlank(propertiesPathFileName)) {
			FileOutputStream out = null;
			try {
				out = new FileOutputStream(propertiesPathFileName);
				properties.store(out, "Added onboarded application to pickist !!");
				isUpdated = true;
			} catch (FileNotFoundException ex) {
				if (logger.isErrorEnabled())
					logger.error("The file " + propertiesPathFileName + " is missing ", ex);
				throw new Exception("The properties file: " + propertiesPathFileName + " could not be accessed.");
			} finally {
				try {
					out.close();
				} catch (IOException ex) {
					if (logger.isErrorEnabled())
						logger.error(ex.getMessage() + "\n The Cause: " + ex.getCause());
				}
			}
		}
		return isUpdated;
	}
}

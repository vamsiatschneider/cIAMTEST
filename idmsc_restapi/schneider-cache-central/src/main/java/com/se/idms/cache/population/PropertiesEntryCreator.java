/**
 * 
 */
package com.se.idms.cache.population;

import static com.se.idms.cache.utils.CacheUtils.readPropertiesFile;

import java.util.Properties;

import org.springframework.stereotype.Service;

import com.se.idms.cache.exception.CacheException;

import net.sf.ehcache.constructs.blocking.CacheEntryFactory;

@Service("propertiesEntryCreator")
public class PropertiesEntryCreator implements CacheEntryFactory {

	@Override
	public Object createEntry(Object pathFileAndName) throws Exception {

		System.out.println("Properties file is being retrieved from: "+ pathFileAndName);

		try {
			Properties properties = readPropertiesFile(pathFileAndName.toString());
			return properties;

		} catch (Exception e) {
			throw new CacheException("The properties file: "+ pathFileAndName + " could not be accessed.");
		}
	}
	
}

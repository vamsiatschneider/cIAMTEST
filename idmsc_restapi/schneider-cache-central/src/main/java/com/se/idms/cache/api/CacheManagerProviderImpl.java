package com.se.idms.cache.api;

import java.io.File;
import java.io.FileInputStream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.ResourceUtils;

import com.se.idms.cache.CacheTypes;

import net.sf.ehcache.CacheManager;

@Component
public class CacheManagerProviderImpl implements CacheTypes, CacheManagerProvider {

	private static final Logger logger = LoggerFactory.getLogger(CacheManagerProviderImpl.class);
	private static final CacheManager CACHE_MANAGER = createManager();
	
	//CODE-RE-STRUCTURING
	@Value("${app.properties.dir}")
	private static String APP_PROPERTIES_DIR;
	
	/**
	 * Constructor
	 */
	public CacheManagerProviderImpl() {
		
	}	
	
	/**
	 * Create instance of CacheManager object.
	 * 
	 * @return CacheManager
	 */
	private static CacheManager createManager(){
		
		CacheManager manager = null;
		try {
			manager = CacheManager.create(getAppPropertiesDir() + CACHE_CONFIGURATION_FILE);
		} catch (Exception e) {
		//catch (CacheException e) {
			logger.error("Cache-central was not initialized correctly : " + e.getMessage() 
							+ "\n\t The origin of the problem was: " + e.getCause());			
		}		
		return manager;
	}
	
	
	
	/* (non-Javadoc)
	 * @see com.se.cache.api.CacheManagerProvider#getCacheManager()
	 */
	@Override
	public CacheManager getCacheManager() {
		return CACHE_MANAGER;
	}
	
	public static String getAppPropertiesDir() {

		File resourcesDirectory = new File("src/main/resources");
		return APP_PROPERTIES_DIR;
	}
	
	
}

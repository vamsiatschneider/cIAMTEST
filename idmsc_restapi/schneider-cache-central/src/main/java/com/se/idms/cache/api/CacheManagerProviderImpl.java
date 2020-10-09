package com.se.idms.cache.api;

import java.io.File;
import java.io.FileInputStream;

import javax.annotation.PostConstruct;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.ResourceUtils;

import com.se.idms.cache.CacheTypes;

import net.sf.ehcache.CacheManager;

@Component
public class CacheManagerProviderImpl implements CacheTypes, CacheManagerProvider {

	private static final Logger logger = LoggerFactory.getLogger(CacheManagerProviderImpl.class);
	//private static final CacheManager CACHE_MANAGER = createManager();
	private static  CacheManager CACHE_MANAGER;
	//CODE-RE-STRUCTURING
	private  static String APP_PROPERTIES_DIR;

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
			//manager = CacheManager.create(getAppPropertiesDir() + CACHE_CONFIGURATION_FILE);
			if(logger.isInfoEnabled())
				logger.info("cache manager in createManager :"+getAppPropertiesDir());
			manager = CacheManager.create(getAppPropertiesDir());
		} catch (Exception e) {
			// catch (CacheException e) {
			if (logger.isErrorEnabled()) {
				logger.error("Cache-central was not initialized correctly : " + e.getMessage()
						+ "\n\t The origin of the problem was: " + e.getCause());
				logger.error("Exception >" + e);
			}
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
		/* File resourcesDirectory = new File("src/main/resources"); PMD Violation UnusedLocalVariable */
		return APP_PROPERTIES_DIR;
	}

	public String getAPP_PROPERTIES_DIR() {
		return APP_PROPERTIES_DIR;
	}

	//@Value("${app.properties.dir}")
	@Value("${cache_configuration_file}")
	public void setAPP_PROPERTIES_DIR(String appPropsDir) {
		APP_PROPERTIES_DIR = appPropsDir;
		CACHE_MANAGER=createManager();
		//logger.info("CACHE_MANAGER INFO"+CACHE_MANAGER.getName()+":"+CACHE_MANAGER.getStatus().toString()+":"+CACHE_MANAGER.getOriginalConfigurationText()+":"+CACHE_MANAGER.getConfiguration().getDefaultTransactionTimeoutInSeconds()+":"+CACHE_MANAGER.getInstance().getActiveConfigurationText());
	}
	
	
}

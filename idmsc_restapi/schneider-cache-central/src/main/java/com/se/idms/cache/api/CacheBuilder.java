package com.se.idms.cache.api;

import java.util.Properties;

import net.sf.ehcache.CacheManager;
import net.sf.ehcache.Ehcache;
import net.sf.ehcache.Element;
import net.sf.ehcache.constructs.blocking.CacheEntryFactory;
import net.sf.ehcache.constructs.blocking.SelfPopulatingCache;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.se.idms.cache.CacheTypes;
import com.se.idms.cache.exception.CacheException;
import com.se.idms.cache.population.PropertiesEntryCreator;

@Component("cacheBuilder")
public class CacheBuilder implements CacheTypes {
	private static final Logger LOGGER = LoggerFactory.getLogger(CacheBuilder.class);

	private CacheManager cacheManager;
	
	private CacheEntryFactory cacheEntryFactory;
	
	private SelfPopulatingCache selfPopulatingPropertiesCache;
	
	private SelfPopulatingCache selfPopulatingIdmsFieldsCache;

	@Autowired
	public CacheBuilder(CacheManagerProvider cacheManagerProvider) {
		this.cacheManager = cacheManagerProvider.getCacheManager();
	}
	
	public CacheAdapter getCache(String cacheName) {
		return new CacheAdapter(getRawCache(cacheName));
	}

	public void clearAllCaches() {
		cacheManager.clearAll();
	}

	/**
	 * Get the raw cache by passing the cache name
	 * @param cacheName
	 * @return
	 */
	private Ehcache getRawCache(String cacheName) {
		//LOGGER.info("Entered getRawCache() -> Start");
		Ehcache ehcache = cacheManager.getEhcache(cacheName);

		if (ehcache == null) {
			LOGGER.error("The requested Cache: " + cacheName	+ " was not found.");
			throw new CacheException("The requested Cache: " + cacheName	+ " was not found.");
		}

		return ehcache;
	}

	// -----------------------------
	
	/**
	 * 
	 * @param propertiesFile
	 * @return
	 */
	public Properties getProperties(String propertiesFile) {
		//LOGGER.info("Entered getProperties() -> Start");

		if (StringUtils.isNotEmpty(propertiesFile)) {

			IDMSCache propertiesCache = getPropertiesCache(propertiesFile);
//			return (Properties) propertiesCache.get(propertiesFile);
			
			Element element = (Element)propertiesCache.get(propertiesFile);
			//LOGGER.info("Size of the property file :: "+element.getKey() + " Value  "+element.getValue());
			return (Properties) element.getValue();
		}

		LOGGER.error("propertiesFile:"+propertiesFile+" is Empty or Null");
		throw new CacheException("The input parameter for getProperties() is Empty or Null");
	}
	
	/**
	 * get the configuration property cache
	 * @return
	 */
	public CacheAdapter getPropertiesCache(String propertiesFile) {
		LOGGER.info("Entered getPropertiesCache() -> Start");
		
		if (selfPopulatingPropertiesCache == null) {
			LOGGER.info("propertiesFile="+propertiesFile);
			if(propertiesFile.contains("LENGTH")){
				createPropertiesCache("lengthProperties");
			}
			
			if(propertiesFile.contains("MANDATORY")){
				createPropertiesCache("mandatoryProperties");
			}
			
			if(propertiesFile.contains("MAPPING")){
				createPropertiesCache("fieldMapProperties");
			}
			
			if(propertiesFile.contains("MULTYPICKLIST")){
				createPropertiesCache("multiPickListProperties");
			}
			
			if(propertiesFile.contains("PICKLIST")){
				createPropertiesCache("pickListProperties");
			}
			
		}
		LOGGER.info("getPropertiesCache() -> End");
		return new CacheAdapter(selfPopulatingPropertiesCache);
	}
	
	/**
	 * 
	 */
	private void createPropertiesCache(String property){
		//LOGGER.info("Entered createPropertiesCache() -> Start");
		
		cacheEntryFactory = new PropertiesEntryCreator();

//		Ehcache ehcache = getRawCache("configProperties");
		Ehcache ehcache = getRawCache(property);
		
		selfPopulatingPropertiesCache = new SelfPopulatingCache(ehcache, cacheEntryFactory);
	}
	
	
	//------------------------------------------------------
	
	//-------------------- IDMS FIELD MAPPING ----------------------------------
	
/*	*//**
	 * 
	 * @param idmsFieldsFile
	 * @return
	 *//*
	public Properties getIdmsFields(String idmsFieldsFile) {

		if (StringUtils.isNotEmpty(idmsFieldsFile)) {

			IDMSCache idmsFieldCache = getIdmsFieldsCache();
//			return (Properties) idmsFieldCache.get(idmsFieldsFile);
			Element element = (Element)idmsFieldCache.get(idmsFieldsFile);
			System.out.println("Size of the property file :: "+element.getKey() + " Value  "+element.getValue());
			return (Properties) element.getValue();
		}

		throw new CacheException("The input parameter for getIdmsFields() is Empty or Null");
	}
	
	public CacheAdapter getIdmsFieldsCache() {
		
		if (selfPopulatingIdmsFieldsCache == null)
			createIdmsFieldsCache();

		return new CacheAdapter(selfPopulatingIdmsFieldsCache);
	}
	
	private void createIdmsFieldsCache(){
		
		cacheEntryFactory = new PropertiesEntryCreator();

		Ehcache ehcache = getRawCache("fieldMapProperties");
		selfPopulatingIdmsFieldsCache = new SelfPopulatingCache(ehcache, cacheEntryFactory);
	}*/
	//------------------------------------------------------
}

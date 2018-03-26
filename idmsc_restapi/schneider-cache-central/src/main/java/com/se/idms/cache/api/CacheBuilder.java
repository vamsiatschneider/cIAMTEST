package com.se.idms.cache.api;

import java.util.Properties;

import net.sf.ehcache.CacheManager;
import net.sf.ehcache.Ehcache;
import net.sf.ehcache.Element;
import net.sf.ehcache.constructs.blocking.CacheEntryFactory;
import net.sf.ehcache.constructs.blocking.SelfPopulatingCache;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.se.idms.cache.CacheTypes;
import com.se.idms.cache.exception.CacheException;
import com.se.idms.cache.population.PropertiesEntryCreator;

@Component("cacheBuilder")
public class CacheBuilder implements CacheTypes {

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
		Ehcache ehcache = cacheManager.getEhcache(cacheName);

		if (ehcache == null) {
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

		if (StringUtils.isNotEmpty(propertiesFile)) {

			IDMSCache propertiesCache = getPropertiesCache(propertiesFile);
//			return (Properties) propertiesCache.get(propertiesFile);
			
			Element element = (Element)propertiesCache.get(propertiesFile);
			System.out.println("Size of the property file :: "+element.getKey() + " Value  "+element.getValue());
			return (Properties) element.getValue();
		}

		throw new CacheException("The input parameter for getProperties() is Empty or Null");
	}
	
	/**
	 * get the configuration property cache
	 * @return
	 */
	public CacheAdapter getPropertiesCache(String propertiesFile) {
		
		if (selfPopulatingPropertiesCache == null) {
			System.out.println("propertiesFile="+propertiesFile);
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
		return new CacheAdapter(selfPopulatingPropertiesCache);
	}
	
	/**
	 * 
	 */
	private void createPropertiesCache(String property){
		
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

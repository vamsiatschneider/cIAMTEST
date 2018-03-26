package com.se.idms.cache.api;

import java.io.Serializable;

import net.sf.ehcache.Ehcache;
import net.sf.ehcache.Element;
import net.sf.ehcache.config.CacheConfiguration;

public class CacheAdapter implements IDMSCache {

	private Ehcache ehcache;
	
	public CacheAdapter(Ehcache ehcache) {
		this.ehcache = ehcache;
	}
	
	@Override
	public void put(Object keyObject, Object valueObject) {
		if(keyObject == null || valueObject == null) {
			// throw exception 
		} 
		ehcache.put(new Element(keyObject, valueObject));
	}

	@Override
	public Object get(Object object) {
		return nullChecking(ehcache.get(object));
	}

	@Override
	public void clear() {
		ehcache.removeAll();
	}

	@Override
	public void setCachingPeriod(Long hours) {
		Long totalSeconds = hours * SECONDS_PER_HOUR;
		CacheConfiguration cacheConfiguration = ehcache.getCacheConfiguration();
		cacheConfiguration.setTimeToLiveSeconds(totalSeconds);		
	}

	@Override
	public boolean remove(Object object) {
		return ehcache.remove(object);
	}

	@Override
	public boolean remove(Serializable serializable) {
		return ehcache.remove(serializable);
	}
	
	/**
	 * Utility method to perform null checks
	 * 
	 * @param element
	 * @return Object
	 */
	private Object nullChecking(Element element) {
		if (element == null)
			return null;
		return element.getObjectValue();
	}

	@Override
	public Object get(Serializable serializable) {
		return ehcache.get(serializable);
	}
}

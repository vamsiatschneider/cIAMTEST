package com.se.idms.cache.api;

import java.io.Serializable;

import com.se.idms.cache.CacheTypes;

public interface IDMSCache extends CacheTypes {

	/**
	 * Put
	 * 
	 * @param keyObject
	 * @param valueObject
	 */
	void put(Object keyObject, Object valueObject);

	
	/**
	 * Get
	 * 
	 * @param object
	 * @return Object
	 */
	public Object get(Object object);

	/**
	 * Gets an element from the cache.
	 * 
	 * @param serializable
	 * @return Object
	 */
	public Object get(Serializable serializable);

	
	/**
	 * Removes all the elements of the cache
	 */
	void clear();

	
	/**
	 * Establishes the caching period in hours time, for this particular cache.
	 * 
	 * @param hours - Number of hours for a cache elements to expire.
	 */
	void setCachingPeriod(Long hours);
	
	/**
	 * Remove an element from the cache.
	 * 
	 * @param object
	 * @return Object
	 */
	public boolean remove(Object object);

	/**
	 * Remove an element from the cache.
	 * 
	 * @param serializable
	 * @return Object
	 */
	public boolean remove(Serializable serializable);
}

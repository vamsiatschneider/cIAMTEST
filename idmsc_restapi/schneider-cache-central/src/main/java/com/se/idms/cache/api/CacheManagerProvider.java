package com.se.idms.cache.api;

import net.sf.ehcache.CacheManager;

public interface CacheManagerProvider {
	public abstract CacheManager getCacheManager();
}

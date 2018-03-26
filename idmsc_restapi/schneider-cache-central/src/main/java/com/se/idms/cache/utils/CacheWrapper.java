package com.se.idms.cache.utils;

import com.se.idms.cache.api.IDMSCache;

public class CacheWrapper {
	private IDMSCache idmsCache;

	public CacheWrapper(IDMSCache idmsCache) {
		this.idmsCache = idmsCache;
	}

	public String getString(Object key) {
		return (String) idmsCache.get(key);
	}

	public IDMSCache getIdmsache() {
		return idmsCache;
	}
}

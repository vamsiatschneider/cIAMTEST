package com.se.idms.cache;

/**
 * Interface for Cache Types
 * @author Aravindh Kumar
 *
 */
public interface CacheTypes {
	
	public Long SECONDS_PER_HOUR = new Long(3600);
	
	public String CACHE_CONFIGURATION_FILE ="ehcache.xml";
	
	public String APP_PROPERTIES_DIR = "C:\\idms\\config\\";
	
	public String EMAIL_TEMPLATE_DIR = "C:\\JsonRequestURLs\\EmailTemplates\\";
	
/*	public String APP_PROPERTIES_DIR = "/home/ec2-user/idms/";
	
	public String EMAIL_TEMPLATE_DIR = "/home/ec2-user/HOTP/EmailTemplates/";*/

}

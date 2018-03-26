package com.se.idms.properties;

import org.springframework.stereotype.Component;

@Component
public interface PropertiesAccess {

	public String getApplicationProperty(String key);

}

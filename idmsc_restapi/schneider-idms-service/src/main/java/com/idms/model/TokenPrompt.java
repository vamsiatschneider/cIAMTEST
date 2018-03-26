package com.idms.model;

import org.apache.commons.lang3.builder.ToStringBuilder;

/**
 * This class used to get the token oauth token information
 * */
public class TokenPrompt {
	
	/**
	 * Name of the token/prompt
	 * */
	private String name;
	
	/**
	 * Value of the token/prompt
	 * */
	private String value;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getValue() {
		return value;
	}

	public void setValue(String value) {
		this.value = value;
	}
	
	
	@Override
    public String toString() {
        return  ToStringBuilder.reflectionToString(this);
    }
	

}

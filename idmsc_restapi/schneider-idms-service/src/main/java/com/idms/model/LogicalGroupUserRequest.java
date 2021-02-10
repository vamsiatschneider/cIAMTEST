package com.idms.model;


import org.apache.commons.lang3.builder.ToStringBuilder;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class LogicalGroupUserRequest {

	@JsonInclude(Include.NON_NULL)
	private String firstName;

	@JsonInclude(Include.NON_NULL)
	private String lastName;
	
	@JsonInclude(Include.NON_NULL)
	private String email;
	
	@JsonInclude(Include.NON_NULL)
	private String appName;
	
	public String getFirstName() {
		return firstName;
	}

	public void setFirstName(String firstName) {
		this.firstName = firstName;
	}

	public String getLastName() {
		return lastName;
	}

	public void setLastName(String lastName) {
		this.lastName = lastName;
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public String getAppName() {
		return appName;
	}

	public void setAppName(String appName) {
		this.appName = appName;
	}

	public String getLanguage() {
		return language;
	}

	public void setLanguage(String language) {
		this.language = language;
	}

	public String getCountry() {
		return country;
	}

	public void setCountry(String country) {
		this.country = country;
	}

	@JsonProperty
	private String language;
	
	@JsonProperty
	private String country;

	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}

}

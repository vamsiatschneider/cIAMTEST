package com.idms.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

/**
 * 
 * @author Subbarao Maniam(SESA468450)
 *
 */
public class ResendRegEmailRequest {
	
	@JsonProperty
	private String email;
	
	@JsonProperty
	private String firstName;
	
	@JsonProperty
	private String lastName;
	
	@JsonProperty
	private String mobile;
	
	@JsonInclude(Include.NON_NULL)
	private String pathValue;

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

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

	public String getMobile() {
		return mobile;
	}

	public void setMobile(String mobile) {
		this.mobile = mobile;
	}

	public String getPathValue() {
		return pathValue;
	}

	public void setPathValue(String pathValue) {
		this.pathValue = pathValue;
	}
	
}

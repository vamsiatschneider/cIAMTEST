package com.idms.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class ResendEmailChangeRequest {

	@JsonProperty
	private String oldEmail;
	
	@JsonProperty
	private String newEmail;
	
	@JsonProperty
	private String firstName;
	
	@JsonProperty
	private String lastName;
	
	@JsonInclude(Include.NON_NULL)
	private String pathValue;

	@JsonProperty("OldEmail")
	public String getOldEmail() {
		return oldEmail;
	}

	@JsonProperty("OldEmail")
	public void setOldEmail(String oldEmail) {
		this.oldEmail = oldEmail;
	}

	@JsonProperty("NewEmail")
	public String getNewEmail() {
		return newEmail;
	}

	@JsonProperty("NewEmail")
	public void setNewEmail(String newEmail) {
		this.newEmail = newEmail;
	}

	@JsonProperty("Firstname")
	public String getFirstName() {
		return firstName;
	}

	@JsonProperty("Firstname")
	public void setFirstName(String firstName) {
		this.firstName = firstName;
	}

	@JsonProperty("Lastname")
	public String getLastName() {
		return lastName;
	}

	@JsonProperty("Lastname")
	public void setLastName(String lastName) {
		this.lastName = lastName;
	}

	public String getPathValue() {
		return pathValue;
	}

	public void setPathValue(String pathValue) {
		this.pathValue = pathValue;
	}
	
}

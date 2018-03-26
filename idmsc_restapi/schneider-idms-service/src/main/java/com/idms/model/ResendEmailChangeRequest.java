package com.idms.model;

import com.fasterxml.jackson.annotation.JsonProperty;

public class ResendEmailChangeRequest {

	@JsonProperty
	private String oldEmail;
	
	@JsonProperty
	private String newEmail;
	
	@JsonProperty
	private String firstName;
	
	@JsonProperty
	private String lastName;

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
	
}

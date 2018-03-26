package com.idms.model.digital;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class GoDigitalUser {

	@JsonProperty
	@JsonInclude(Include.NON_EMPTY)
	private String languageCode;

	@JsonProperty
	@JsonInclude(Include.NON_EMPTY)
	private String federatedId;

	@JsonProperty
	@JsonInclude(Include.NON_EMPTY)
	private String email;

	@JsonProperty
	@JsonInclude(Include.NON_EMPTY)
	private String firstName;

	@JsonProperty
	@JsonInclude(Include.NON_EMPTY)
	private String lastName;

	@JsonProperty
	@JsonInclude(Include.NON_EMPTY)
	private String countryCode;

	@JsonProperty
	@JsonInclude(Include.NON_EMPTY)
	private String jobDescription;

	@JsonProperty
	@JsonInclude(Include.NON_EMPTY)
	private String jobTitle;

	@JsonProperty
	@JsonInclude(Include.NON_EMPTY)
	private String jobFunction;

	@JsonProperty
	@JsonInclude(Include.NON_EMPTY)
	private String companyFederatedId;

	@JsonProperty
	@JsonInclude(Include.NON_EMPTY)
	private String adminCompanyFederatedId;

	@JsonProperty
	@JsonInclude(Include.NON_EMPTY)
	private String adminFederatedId;

	@JsonProperty
	@JsonInclude(Include.NON_EMPTY)
	private String companyName;

	@JsonProperty
	@JsonInclude(Include.NON_EMPTY)
	private String companyAddress;

	@JsonProperty
	@JsonInclude(Include.NON_EMPTY)
	private String companyCity;

	@JsonProperty
	@JsonInclude(Include.NON_EMPTY)
	private String companyPostalCode;

	@JsonProperty
	@JsonInclude(Include.NON_EMPTY)
	private String companyCountryCode;
	
	@JsonProperty
	@JsonInclude(Include.NON_EMPTY)
	private String userId;
	
	@JsonProperty
	@JsonInclude(Include.NON_EMPTY)
	private String adminBfoAccountId;

	@JsonProperty("LANGUAGE_CODE")
	public String getLanguageCode() {
		return languageCode;
	}

	@JsonProperty("LANGUAGE_CODE")
	public void setLanguageCode(String languageCode) {
		this.languageCode = languageCode;
	}

	@JsonProperty("FEDERATED_ID")
	public String getFederatedId() {
		return federatedId;
	}

	@JsonProperty("FEDERATED_ID")
	public void setFederatedId(String federatedId) {
		this.federatedId = federatedId;
	}

	@JsonProperty("EMAIL")
	public String getEmail() {
		return email;
	}

	@JsonProperty("EMAIL")
	public void setEmail(String email) {
		this.email = email;
	}

	@JsonProperty("FIRST_NAME")
	public String getFirstName() {
		return firstName;
	}

	@JsonProperty("FIRST_NAME")
	public void setFirstName(String firstName) {
		this.firstName = firstName;
	}

	@JsonProperty("LAST_NAME")
	public String getLastName() {
		return lastName;
	}

	@JsonProperty("LAST_NAME")
	public void setLastName(String lastName) {
		this.lastName = lastName;
	}

	@JsonProperty("COUNTRY_CODE")
	public String getCountryCode() {
		return countryCode;
	}

	@JsonProperty("COUNTRY_CODE")
	public void setCountryCode(String countryCode) {
		this.countryCode = countryCode;
	}

	@JsonProperty("JOB_DESCRIPTION")
	public String getJobDescription() {
		return jobDescription;
	}

	@JsonProperty("JOB_DESCRIPTION")
	public void setJobDescription(String jobDescription) {
		this.jobDescription = jobDescription;
	}

	@JsonProperty("JOB_TITLE")
	public String getJobTitle() {
		return jobTitle;
	}

	@JsonProperty("JOB_TITLE")
	public void setJobTitle(String jobTitle) {
		this.jobTitle = jobTitle;
	}

	@JsonProperty("JOB_FUNCTION")
	public String getJobFunction() {
		return jobFunction;
	}

	@JsonProperty("JOB_FUNCTION")
	public void setJobFunction(String jobFunction) {
		this.jobFunction = jobFunction;
	}

	@JsonProperty("COMPANY_FEDERATED_ID")
	public String getCompanyFederatedId() {
		return companyFederatedId;
	}

	@JsonProperty("COMPANY_FEDERATED_ID")
	public void setCompanyFederatedId(String companyFederatedId) {
		this.companyFederatedId = companyFederatedId;
	}

	@JsonProperty("COMPANY_NAME")
	public String getCompanyName() {
		return companyName;
	}

	@JsonProperty("COMPANY_NAME")
	public void setCompanyName(String companyName) {
		this.companyName = companyName;
	}

	@JsonProperty("COMPANY_ADDRESS")
	public String getCompanyAddress() {
		return companyAddress;
	}

	@JsonProperty("COMPANY_ADDRESS")
	public void setCompanyAddress(String companyAddress) {
		this.companyAddress = companyAddress;
	}

	@JsonProperty("COMPANY_CITY")
	public String getCompanyCity() {
		return companyCity;
	}

	@JsonProperty("COMPANY_CITY")
	public void setCompanyCity(String companyCity) {
		this.companyCity = companyCity;
	}

	@JsonProperty("COMPANY_POSTAL_CODE")
	public String getCompanyPostalCode() {
		return companyPostalCode;
	}

	@JsonProperty("COMPANY_POSTAL_CODE")
	public void setCompanyPostalCode(String companyPostalCode) {
		this.companyPostalCode = companyPostalCode;
	}

	@JsonProperty("COMPANY_COUNTRY_CODE")
	public String getCompanyCountryCode() {
		return companyCountryCode;
	}

	@JsonProperty("COMPANY_COUNTRY_CODE")
	public void setCompanyCountryCode(String companyCountryCode) {
		this.companyCountryCode = companyCountryCode;
	}

	@JsonProperty("ADMIN_COMPANY_FEDERATED_ID")
	public String getAdminCompanyFederatedId() {
		return adminCompanyFederatedId;
	}

	@JsonProperty("ADMIN_COMPANY_FEDERATED_ID")
	public void setAdminCompanyFederatedId(String adminCompanyFederatedId) {
		this.adminCompanyFederatedId = adminCompanyFederatedId;
	}

	@JsonProperty("ADMIN_FEDERATED_ID")
	public String getAdminFederatedId() {
		return adminFederatedId;
	}

	@JsonProperty("ADMIN_FEDERATED_ID")
	public void setAdminFederatedId(String adminFederatedId) {
		this.adminFederatedId = adminFederatedId;
	}

	@JsonProperty("USER_ID")
	public String getUserId() {
		return userId;
	}

	@JsonProperty("USER_ID")
	public void setUserId(String userId) {
		this.userId = userId;
	}
	
	
	@JsonProperty("ADMIN_BFO_ACCOUNT_ID")
	public String getAdminBfoAccountId() {
		return adminBfoAccountId;
	}

	@JsonProperty("ADMIN_BFO_ACCOUNT_ID")
	public void setAdminBfoAccountId(String adminBfoAccountId) {
		this.adminBfoAccountId = adminBfoAccountId;
	}

	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}

}

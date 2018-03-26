package com.idms.product.model;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class UimsUser {

	@JsonInclude(Include.NON_EMPTY)
	private String addInfoAddress;

	@JsonInclude(Include.NON_EMPTY)
	private String countrycode;

	@JsonInclude(Include.NON_EMPTY)
	private String jobDescription;

	@JsonInclude(Include.NON_EMPTY)
	private String jobFunction;

	@JsonInclude(Include.NON_EMPTY)
	private String jobTitle;

	@JsonInclude(Include.NON_EMPTY)
	private String localityName;

	@JsonInclude(Include.NON_EMPTY)
	private String middleName;

	@JsonInclude(Include.NON_EMPTY)
	private String postOfficeBox;

	@JsonInclude(Include.NON_EMPTY)
	private String salutation;

	@JsonInclude(Include.NON_EMPTY)
	private String street;

	@JsonInclude(Include.NON_EMPTY)
	private String companyId;

	@JsonInclude(Include.NON_EMPTY)
	private String postalCode;

	@JsonInclude(Include.NON_EMPTY)
	private String federatedID;

	@JsonInclude(Include.NON_EMPTY)
	private String firstName;

	@JsonInclude(Include.NON_EMPTY)
	private String email;

	@JsonInclude(Include.NON_EMPTY)
	private String cell;

	@JsonInclude(Include.NON_EMPTY)
	private String languageCode;

	@JsonInclude(Include.NON_EMPTY)
	private String lastName;

	@JsonInclude(Include.NON_EMPTY)
	private String state;

	@JsonInclude(Include.NON_EMPTY)
	private String phone;

	@JsonInclude(Include.NON_EMPTY)
	private String fax;

	@JsonInclude(Include.NON_EMPTY)
	private String county;

	@JsonInclude(Include.NON_EMPTY)
	private String primaryContact;

	@JsonInclude(Include.NON_EMPTY)
	private String acl;

	@JsonInclude(Include.NON_EMPTY)
	private String organizationName;

	@JsonInclude(Include.NON_EMPTY)
	private String countryCode;

	@JsonInclude(Include.NON_EMPTY)
	private String currencyCode;

	@JsonInclude(Include.NON_EMPTY)
	private String customerClass;

	@JsonInclude(Include.NON_EMPTY)
	private String marketSegment;

	@JsonInclude(Include.NON_EMPTY)
	private String st;

	@JsonInclude(Include.NON_EMPTY)
	private String headQuarter;

	@JsonInclude(Include.NON_EMPTY)
	private String webSite;

	@JsonInclude(Include.NON_EMPTY)
	private String marketServed;

	@JsonInclude(Include.NON_EMPTY)
	private String employeeSize;

	@JsonInclude(Include.NON_EMPTY)
	private String taxIdentificationNumber;

	public String getAddInfoAddress() {
		return addInfoAddress;
	}

	public void setAddInfoAddress(String addInfoAddress) {
		this.addInfoAddress = addInfoAddress;
	}

	public String getCountrycode() {
		return countrycode;
	}

	public void setCountrycode(String countrycode) {
		this.countrycode = countrycode;
	}

	public String getJobDescription() {
		return jobDescription;
	}

	public void setJobDescription(String jobDescription) {
		this.jobDescription = jobDescription;
	}

	public String getJobFunction() {
		return jobFunction;
	}

	public void setJobFunction(String jobFunction) {
		this.jobFunction = jobFunction;
	}

	public String getJobTitle() {
		return jobTitle;
	}

	public void setJobTitle(String jobTitle) {
		this.jobTitle = jobTitle;
	}

	public String getLocalityName() {
		return localityName;
	}

	public void setLocalityName(String localityName) {
		this.localityName = localityName;
	}

	public String getMiddleName() {
		return middleName;
	}

	public void setMiddleName(String middleName) {
		this.middleName = middleName;
	}

	public String getPostOfficeBox() {
		return postOfficeBox;
	}

	public void setPostOfficeBox(String postOfficeBox) {
		this.postOfficeBox = postOfficeBox;
	}

	public String getSalutation() {
		return salutation;
	}

	public void setSalutation(String salutation) {
		this.salutation = salutation;
	}

	public String getStreet() {
		return street;
	}

	public void setStreet(String street) {
		this.street = street;
	}

	public String getCompanyId() {
		return companyId;
	}

	public void setCompanyId(String companyId) {
		this.companyId = companyId;
	}

	public String getPostalCode() {
		return postalCode;
	}

	public void setPostalCode(String postalCode) {
		this.postalCode = postalCode;
	}

	public String getFederatedID() {
		return federatedID;
	}

	public void setFederatedID(String federatedID) {
		this.federatedID = federatedID;
	}

	public String getFirstName() {
		return firstName;
	}

	public void setFirstName(String firstName) {
		this.firstName = firstName;
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public String getCell() {
		return cell;
	}

	public void setCell(String cell) {
		this.cell = cell;
	}

	public String getLanguageCode() {
		return languageCode;
	}

	public void setLanguageCode(String languageCode) {
		this.languageCode = languageCode;
	}

	public String getLastName() {
		return lastName;
	}

	public void setLastName(String lastName) {
		this.lastName = lastName;
	}

	public String getState() {
		return state;
	}

	public void setState(String state) {
		this.state = state;
	}

	public String getPhone() {
		return phone;
	}

	public void setPhone(String phone) {
		this.phone = phone;
	}

	public String getFax() {
		return fax;
	}

	public void setFax(String fax) {
		this.fax = fax;
	}

	public String getCounty() {
		return county;
	}

	public void setCounty(String county) {
		this.county = county;
	}

	public String getPrimaryContact() {
		return primaryContact;
	}

	public void setPrimaryContact(String primaryContact) {
		this.primaryContact = primaryContact;
	}

	public String getAcl() {
		return acl;
	}

	public void setAcl(String acl) {
		this.acl = acl;
	}

	public String getOrganizationName() {
		return organizationName;
	}

	public void setOrganizationName(String organizationName) {
		this.organizationName = organizationName;
	}

	public String getCountryCode() {
		return countryCode;
	}

	public void setCountryCode(String countryCode) {
		this.countryCode = countryCode;
	}

	public String getCurrencyCode() {
		return currencyCode;
	}

	public void setCurrencyCode(String currencyCode) {
		this.currencyCode = currencyCode;
	}

	public String getCustomerClass() {
		return customerClass;
	}

	public void setCustomerClass(String customerClass) {
		this.customerClass = customerClass;
	}

	public String getMarketSegment() {
		return marketSegment;
	}

	public void setMarketSegment(String marketSegment) {
		this.marketSegment = marketSegment;
	}

	public String getSt() {
		return st;
	}

	public void setSt(String st) {
		this.st = st;
	}

	public String getHeadQuarter() {
		return headQuarter;
	}

	public void setHeadQuarter(String headQuarter) {
		this.headQuarter = headQuarter;
	}

	public String getWebSite() {
		return webSite;
	}

	public void setWebSite(String webSite) {
		this.webSite = webSite;
	}

	public String getMarketServed() {
		return marketServed;
	}

	public void setMarketServed(String marketServed) {
		this.marketServed = marketServed;
	}

	public String getEmployeeSize() {
		return employeeSize;
	}

	public void setEmployeeSize(String employeeSize) {
		this.employeeSize = employeeSize;
	}

	public String getTaxIdentificationNumber() {
		return taxIdentificationNumber;
	}

	public void setTaxIdentificationNumber(String taxIdentificationNumber) {
		this.taxIdentificationNumber = taxIdentificationNumber;
	}

	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}

}

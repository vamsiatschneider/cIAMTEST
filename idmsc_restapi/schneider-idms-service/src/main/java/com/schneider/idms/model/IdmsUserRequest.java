package com.schneider.idms.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class IdmsUserRequest {

	@JsonInclude(Include.NON_NULL)
	private String userContext;

	@JsonInclude(Include.NON_NULL)
	private String salutation;

	@JsonInclude(Include.NON_NULL)
	private String firstName;

	@JsonInclude(Include.NON_NULL)
	private String middleName;

	@JsonInclude(Include.NON_NULL)
	private String lastName;

	@JsonInclude(Include.NON_NULL)
	private String countryCode;

	@JsonInclude(Include.NON_NULL)
	private String email;

	@JsonInclude(Include.NON_NULL)
	private String mobilePhone;

	@JsonInclude(Include.NON_NULL)
	private String languageCode;

	@JsonInclude(Include.NON_NULL)
	private String emailOptIn;

	@JsonInclude(Include.NON_NULL)
	private String aboutMe;

	@JsonInclude(Include.NON_NULL)
	private String street;

	@JsonInclude(Include.NON_NULL)
	private String city;

	@JsonInclude(Include.NON_NULL)
	private String zipCode;

	@JsonInclude(Include.NON_NULL)
	private String stateOrProvinceCode;

	@JsonInclude(Include.NON_NULL)
	private String county;

	@JsonInclude(Include.NON_NULL)
	private String pOBox;

	@JsonInclude(Include.NON_NULL)
	private String additionalAddress;

	@JsonInclude(Include.NON_NULL)
	private String suffix;

	@JsonInclude(Include.NON_NULL)
	private String homePhone;

	@JsonInclude(Include.NON_NULL)
	private String fax;

	@JsonInclude(Include.NON_NULL)
	private String idmsFederatedId;

	@JsonInclude(Include.NON_NULL)
	private String registrationSource;

	@JsonInclude(Include.NON_NULL)
	private String currency;

	@JsonInclude(Include.NON_NULL)
	private String companyName;

	@JsonInclude(Include.NON_NULL)
	private String companyStreet;

	@JsonInclude(Include.NON_NULL)
	private String companyCity;

	@JsonInclude(Include.NON_NULL)
	private String companyZipCode;

	@JsonInclude(Include.NON_NULL)
	private String companyStateOrProvinceCode;

	@JsonInclude(Include.NON_NULL)
	private String companyPOBox;

	@JsonInclude(Include.NON_NULL)
	private String companyCounty;

	@JsonInclude(Include.NON_NULL)
	private String companyCountryCode;

	@JsonInclude(Include.NON_NULL)
	private String companyAdditionalAddress;

	@JsonInclude(Include.NON_NULL)
	private String companyWebsite;

	@JsonInclude(Include.NON_NULL)
	private String classLevel1;

	@JsonInclude(Include.NON_NULL)
	private String classLevel2;

	@JsonInclude(Include.NON_NULL)
	private String marketSegment;

	@JsonInclude(Include.NON_NULL)
	private String marketSubSegment;

	@JsonInclude(Include.NON_NULL)
	private String marketServed;

	@JsonInclude(Include.NON_NULL)
	private String employeeSize;

	@JsonInclude(Include.NON_NULL)
	private String department;

	@JsonInclude(Include.NON_NULL)
	private String headquarter;

	@JsonInclude(Include.NON_NULL)
	private String annualRevenue;

	@JsonInclude(Include.NON_NULL)
	private String taxIdentificationNumber;

	@JsonInclude(Include.NON_NULL)
	private String jobTitle;

	@JsonInclude(Include.NON_NULL)
	private String jobFunction;

	@JsonInclude(Include.NON_NULL)
	private String jobDescription;

	@JsonInclude(Include.NON_NULL)
	private String workPhone;

	@JsonInclude(Include.NON_NULL)
	private String companyFederatedId;
	
	@JsonInclude(Include.NON_NULL)
	private String trustedAdmin;

	@JsonInclude(Include.NON_NULL)
	private String adminCompanyFederatedId;

	@JsonInclude(Include.NON_NULL)
	private String hashedPin;

	@JsonInclude(Include.NON_NULL)
	private String profileLastUpdateSource;

	@JsonInclude(Include.NON_NULL)
	private String currencyCode;

	@JsonInclude(Include.NON_NULL)
	private String isActivated;

	@JsonInclude(Include.NON_NULL)
	private String accountId;

	@JsonInclude(Include.NON_NULL)
	private String primaryContact;

	@JsonInclude(Include.NON_NULL)
	private String identityType;

	@JsonInclude(Include.NON_NULL)
	private String delegatedIdp;

	@JsonInclude(Include.NON_NULL)
	private String federationId;

	@JsonInclude(Include.NON_NULL)
	private boolean isActive;

	public String getUserContext() {
		return userContext;
	}

	public void setUserContext(String userContext) {
		this.userContext = userContext;
	}

	public String getSalutation() {
		return salutation;
	}

	public void setSalutation(String salutation) {
		this.salutation = salutation;
	}

	public String getFirstName() {
		return firstName;
	}

	public void setFirstName(String firstName) {
		this.firstName = firstName;
	}

	public String getMiddleName() {
		return middleName;
	}

	public void setMiddleName(String middleName) {
		this.middleName = middleName;
	}

	public String getLastName() {
		return lastName;
	}

	public void setLastName(String lastName) {
		this.lastName = lastName;
	}

	public String getCountryCode() {
		return countryCode;
	}

	public void setCountryCode(String countryCode) {
		this.countryCode = countryCode;
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public String getMobilePhone() {
		return mobilePhone;
	}

	public void setMobilePhone(String mobilePhone) {
		this.mobilePhone = mobilePhone;
	}

	public String getLanguageCode() {
		return languageCode;
	}

	public void setLanguageCode(String languageCode) {
		this.languageCode = languageCode;
	}

	public String getEmailOptIn() {
		return emailOptIn;
	}

	public void setEmailOptIn(String emailOptIn) {
		this.emailOptIn = emailOptIn;
	}

	public String getAboutMe() {
		return aboutMe;
	}

	public void setAboutMe(String aboutMe) {
		this.aboutMe = aboutMe;
	}

	public String getStreet() {
		return street;
	}

	public void setStreet(String street) {
		this.street = street;
	}

	public String getCity() {
		return city;
	}

	public void setCity(String city) {
		this.city = city;
	}

	public String getZipCode() {
		return zipCode;
	}

	public void setZipCode(String zipCode) {
		this.zipCode = zipCode;
	}

	public String getStateOrProvinceCode() {
		return stateOrProvinceCode;
	}

	public void setStateOrProvinceCode(String stateOrProvinceCode) {
		this.stateOrProvinceCode = stateOrProvinceCode;
	}

	public String getCounty() {
		return county;
	}

	public void setCounty(String county) {
		this.county = county;
	}

	public String getpOBox() {
		return pOBox;
	}

	public void setpOBox(String pOBox) {
		this.pOBox = pOBox;
	}

	public String getAdditionalAddress() {
		return additionalAddress;
	}

	public void setAdditionalAddress(String additionalAddress) {
		this.additionalAddress = additionalAddress;
	}

	public String getSuffix() {
		return suffix;
	}

	public void setSuffix(String suffix) {
		this.suffix = suffix;
	}

	public String getHomePhone() {
		return homePhone;
	}

	public void setHomePhone(String homePhone) {
		this.homePhone = homePhone;
	}

	public String getFax() {
		return fax;
	}

	public void setFax(String fax) {
		this.fax = fax;
	}

	public String getIdmsFederatedId() {
		return idmsFederatedId;
	}

	public void setIdmsFederatedId(String idmsFederatedId) {
		this.idmsFederatedId = idmsFederatedId;
	}

	public String getRegistrationSource() {
		return registrationSource;
	}

	public void setRegistrationSource(String registrationSource) {
		this.registrationSource = registrationSource;
	}

	public String getCurrency() {
		return currency;
	}

	public void setCurrency(String currency) {
		this.currency = currency;
	}

	public String getCompanyName() {
		return companyName;
	}

	public void setCompanyName(String companyName) {
		this.companyName = companyName;
	}

	public String getCompanyStreet() {
		return companyStreet;
	}

	public void setCompanyStreet(String companyStreet) {
		this.companyStreet = companyStreet;
	}

	public String getCompanyCity() {
		return companyCity;
	}

	public void setCompanyCity(String companyCity) {
		this.companyCity = companyCity;
	}

	public String getCompanyZipCode() {
		return companyZipCode;
	}

	public void setCompanyZipCode(String companyZipCode) {
		this.companyZipCode = companyZipCode;
	}

	public String getCompanyStateOrProvinceCode() {
		return companyStateOrProvinceCode;
	}

	public void setCompanyStateOrProvinceCode(String companyStateOrProvinceCode) {
		this.companyStateOrProvinceCode = companyStateOrProvinceCode;
	}

	public String getCompanyPOBox() {
		return companyPOBox;
	}

	public void setCompanyPOBox(String companyPOBox) {
		this.companyPOBox = companyPOBox;
	}

	public String getCompanyCounty() {
		return companyCounty;
	}

	public void setCompanyCounty(String companyCounty) {
		this.companyCounty = companyCounty;
	}

	public String getCompanyCountryCode() {
		return companyCountryCode;
	}

	public void setCompanyCountryCode(String companyCountryCode) {
		this.companyCountryCode = companyCountryCode;
	}

	public String getCompanyAdditionalAddress() {
		return companyAdditionalAddress;
	}

	public void setCompanyAdditionalAddress(String companyAdditionalAddress) {
		this.companyAdditionalAddress = companyAdditionalAddress;
	}

	public String getCompanyWebsite() {
		return companyWebsite;
	}

	public void setCompanyWebsite(String companyWebsite) {
		this.companyWebsite = companyWebsite;
	}

	public String getClassLevel1() {
		return classLevel1;
	}

	public void setClassLevel1(String classLevel1) {
		this.classLevel1 = classLevel1;
	}

	public String getClassLevel2() {
		return classLevel2;
	}

	public void setClassLevel2(String classLevel2) {
		this.classLevel2 = classLevel2;
	}

	public String getMarketSegment() {
		return marketSegment;
	}

	public void setMarketSegment(String marketSegment) {
		this.marketSegment = marketSegment;
	}

	public String getMarketSubSegment() {
		return marketSubSegment;
	}

	public void setMarketSubSegment(String marketSubSegment) {
		this.marketSubSegment = marketSubSegment;
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

	public String getDepartment() {
		return department;
	}

	public void setDepartment(String department) {
		this.department = department;
	}

	public String getHeadquarter() {
		return headquarter;
	}

	public void setHeadquarter(String headquarter) {
		this.headquarter = headquarter;
	}

	public String getAnnualRevenue() {
		return annualRevenue;
	}

	public void setAnnualRevenue(String annualRevenue) {
		this.annualRevenue = annualRevenue;
	}

	public String getTaxIdentificationNumber() {
		return taxIdentificationNumber;
	}

	public void setTaxIdentificationNumber(String taxIdentificationNumber) {
		this.taxIdentificationNumber = taxIdentificationNumber;
	}

	public String getJobTitle() {
		return jobTitle;
	}

	public void setJobTitle(String jobTitle) {
		this.jobTitle = jobTitle;
	}

	public String getJobFunction() {
		return jobFunction;
	}

	public void setJobFunction(String jobFunction) {
		this.jobFunction = jobFunction;
	}

	public String getJobDescription() {
		return jobDescription;
	}

	public void setJobDescription(String jobDescription) {
		this.jobDescription = jobDescription;
	}

	public String getWorkPhone() {
		return workPhone;
	}

	public void setWorkPhone(String workPhone) {
		this.workPhone = workPhone;
	}

	public String getTrustedAdmin() {
		return trustedAdmin;
	}

	public void setTrustedAdmin(String trustedAdmin) {
		this.trustedAdmin = trustedAdmin;
	}

	public String getCompanyFederatedId() {
		return companyFederatedId;
	}

	public void setCompanyFederatedId(String companyFederatedId) {
		this.companyFederatedId = companyFederatedId;
	}

	public String getHashedPin() {
		return hashedPin;
	}

	public void setHashedPin(String hashedPin) {
		this.hashedPin = hashedPin;
	}

	public String getProfileLastUpdateSource() {
		return profileLastUpdateSource;
	}

	public void setProfileLastUpdateSource(String profileLastUpdateSource) {
		this.profileLastUpdateSource = profileLastUpdateSource;
	}

	public String getCurrencyCode() {
		return currencyCode;
	}

	public void setCurrencyCode(String currencyCode) {
		this.currencyCode = currencyCode;
	}

	public String getIsActivated() {
		return isActivated;
	}

	public void setIsActivated(String isActivated) {
		this.isActivated = isActivated;
	}

	public String getAccountId() {
		return accountId;
	}

	public void setAccountId(String accountId) {
		this.accountId = accountId;
	}

	public String getPrimaryContact() {
		return primaryContact;
	}

	public void setPrimaryContact(String primaryContact) {
		this.primaryContact = primaryContact;
	}

	public String getIdentityType() {
		return identityType;
	}

	public void setIdentityType(String identityType) {
		this.identityType = identityType;
	}

	public String getDelegatedIdp() {
		return delegatedIdp;
	}

	public void setDelegatedIdp(String delegatedIdp) {
		this.delegatedIdp = delegatedIdp;
	}

	public String getFederationId() {
		return federationId;
	}

	public void setFederationId(String federationId) {
		this.federationId = federationId;
	}

	public boolean isActive() {
		return isActive;
	}

	public void setActive(boolean isActive) {
		this.isActive = isActive;
	}

	public String getAdminCompanyFederatedId() {
		return adminCompanyFederatedId;
	}

	public void setAdminCompanyFederatedId(String adminCompanyFederatedId) {
		this.adminCompanyFederatedId = adminCompanyFederatedId;
	}

}

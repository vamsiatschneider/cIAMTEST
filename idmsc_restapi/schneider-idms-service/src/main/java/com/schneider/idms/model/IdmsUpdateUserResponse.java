package com.schneider.idms.model;

import com.fasterxml.jackson.annotation.JsonProperty;

public class IdmsUpdateUserResponse {

	@JsonProperty
	private String userId;

	@JsonProperty
	private String uimsFederatedId;

	@JsonProperty
	private String salutation;

	@JsonProperty
	private String firstName;

	@JsonProperty
	private String middleName;

	@JsonProperty
	private String lastName;

	@JsonProperty
	private String countryCode;

	@JsonProperty
	private String email;

	@JsonProperty
	private String mobilePhone;

	@JsonProperty
	private String languageCode;

	@JsonProperty
	private String emailOptIn;

	@JsonProperty
	private String aboutMe;

	@JsonProperty
	private String street;

	@JsonProperty
	private String city;

	@JsonProperty
	private String zipCode;

	@JsonProperty
	private String stateOrProvinceCode;

	@JsonProperty
	private String county;

	@JsonProperty
	private String pOBox;

	@JsonProperty
	private String additionalAddress;

	@JsonProperty
	private String suffix;

	@JsonProperty
	private String homePhone;

	@JsonProperty
	private String fax;

	@JsonProperty
	private String profileLastUpdateSource;

	@JsonProperty
	private String currency;

	@JsonProperty
	private String companyName;

	@JsonProperty
	private String companyStreet;

	@JsonProperty
	private String companyCity;

	@JsonProperty
	private String companyZipCode;

	@JsonProperty
	private String companyStateOrProvinceCode;

	@JsonProperty
	private String companyPOBox;

	@JsonProperty
	private String companyCounty;

	@JsonProperty
	private String companyCountryCode;

	@JsonProperty
	private String companyAdditionalAddress;

	@JsonProperty
	private String companyWebsite;

	@JsonProperty
	private String classLevel1;

	@JsonProperty
	private String classLevel2;

	@JsonProperty
	private String marketSegment;

	@JsonProperty
	private String marketSubSegment;

	@JsonProperty
	private String marketServed;

	@JsonProperty
	private String employeeSize;

	@JsonProperty
	private String department;

	@JsonProperty
	private String headquarter;

	@JsonProperty
	private String annualRevenue;

	@JsonProperty
	private String taxIdentificationNumber;

	@JsonProperty
	private String jobTitle;

	@JsonProperty
	private String jobFunction;

	@JsonProperty
	private String jobDescription;

	@JsonProperty
	private String workPhone;

	@JsonProperty
	private String userStatus;

	@JsonProperty
	private String socialProviders;

	@JsonProperty
	private String companyFederatedId;

	@JsonProperty
	private String trustedAdmin;

	@JsonProperty
	private String contactGoldenID;

	@JsonProperty
	private String accouontGoldenID;

	public String getUserId() {
		return userId;
	}

	public void setUserId(String userId) {
		this.userId = userId;
	}

	public String getUimsFederatedId() {
		return uimsFederatedId;
	}

	public void setUimsFederatedId(String uimsFederatedId) {
		this.uimsFederatedId = uimsFederatedId;
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

	public String getProfileLastUpdateSource() {
		return profileLastUpdateSource;
	}

	public void setProfileLastUpdateSource(String profileLastUpdateSource) {
		this.profileLastUpdateSource = profileLastUpdateSource;
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

	public String getUserStatus() {
		return userStatus;
	}

	public void setUserStatus(String userStatus) {
		this.userStatus = userStatus;
	}

	public String getSocialProviders() {
		return socialProviders;
	}

	public void setSocialProviders(String socialProviders) {
		this.socialProviders = socialProviders;
	}

	public String getCompanyFederatedId() {
		return companyFederatedId;
	}

	public void setCompanyFederatedId(String companyFederatedId) {
		this.companyFederatedId = companyFederatedId;
	}

	public String getTrustedAdmin() {
		return trustedAdmin;
	}

	public void setTrustedAdmin(String trustedAdmin) {
		this.trustedAdmin = trustedAdmin;
	}

	public String getContactGoldenID() {
		return contactGoldenID;
	}

	public void setContactGoldenID(String contactGoldenID) {
		this.contactGoldenID = contactGoldenID;
	}

	public String getAccouontGoldenID() {
		return accouontGoldenID;
	}

	public void setAccouontGoldenID(String accouontGoldenID) {
		this.accouontGoldenID = accouontGoldenID;
	}

}

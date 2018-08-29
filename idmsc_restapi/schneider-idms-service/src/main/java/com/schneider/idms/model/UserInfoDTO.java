/**
 * 
 */
package com.schneider.idms.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

/**
 * @author SESA508936
 * For Direct API Call
 */
public class UserInfoDTO {

	@JsonInclude(Include.NON_NULL)
	private String userId;
	
	@JsonInclude(Include.NON_NULL)
	private String contactId;
	
	@JsonInclude(Include.NON_NULL)
	private String accountId;
	
	@JsonInclude(Include.NON_NULL)
	private String federatedId;
	
	@JsonInclude(Include.NON_NULL)
	private String idmsFederatedId;
	
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
	private String emailOption;
	
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
	private String trustStatus;
	
	@JsonInclude(Include.NON_NULL)
	private String trustLevel;
	
	@JsonInclude(Include.NON_NULL)
	private String rejectionReason;
	
	@JsonInclude(Include.NON_NULL)
	private String rejectionComment;
	
	@JsonInclude(Include.NON_NULL)
	private String registrationSource;
	
	@JsonInclude(Include.NON_NULL)
	private String profileLastUpdateSource;
	
	@JsonInclude(Include.NON_NULL)
	private String delegatedIdp;
	
	@JsonInclude(Include.NON_NULL)
	private String identityType;
	
	@JsonInclude(Include.NON_NULL)
	private String isInternal;
	
	@JsonInclude(Include.NON_NULL)
	private String ail;
	
	@JsonInclude(Include.NON_NULL)
	private String ailApplications;
	
	@JsonInclude(Include.NON_NULL)
	private String ailFeatures;
	
	@JsonInclude(Include.NON_NULL)
	private String ailPrograms;
	
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
	private String division;
	
	@JsonInclude(Include.NON_NULL)
	private String title;
	
	@JsonInclude(Include.NON_NULL)
	private String businessUnit;
	
	@JsonInclude(Include.NON_NULL)
	private boolean headquarter;
	
	@JsonInclude(Include.NON_NULL)
	private float annualRevenue;
	
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
	private String userStatus;
	
	@JsonInclude(Include.NON_NULL)
	private String socialProviders;
	
	@JsonInclude(Include.NON_NULL)
	private String companyFederatedId;
	
	@JsonInclude(Include.NON_NULL)
	private boolean trustedAdmin;
	
	@JsonInclude(Include.NON_NULL)
	private String hashedPin;
	
	@JsonInclude(Include.NON_NULL)
	private String contactGoldenID;
	
	@JsonInclude(Include.NON_NULL)
	private String accountGoldenID;
	
	public String getUserId() {
		return userId;
	}
	public void setUserId(String userId) {
		this.userId = userId;
	}
	public String getContactId() {
		return contactId;
	}
	public void setContactId(String contactId) {
		this.contactId = contactId;
	}
	public String getAccountId() {
		return accountId;
	}
	public void setAccountId(String accountId) {
		this.accountId = accountId;
	}
	public String getFederatedId() {
		return federatedId;
	}
	public void setFederatedId(String federatedId) {
		this.federatedId = federatedId;
	}
	public String getIdmsFederatedId() {
		return idmsFederatedId;
	}
	public void setIdmsFederatedId(String idmsFederatedId) {
		this.idmsFederatedId = idmsFederatedId;
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
	public String getTrustStatus() {
		return trustStatus;
	}
	public void setTrustStatus(String trustStatus) {
		this.trustStatus = trustStatus;
	}
	public String getTrustLevel() {
		return trustLevel;
	}
	public void setTrustLevel(String trustLevel) {
		this.trustLevel = trustLevel;
	}
	public String getRejectionReason() {
		return rejectionReason;
	}
	public void setRejectionReason(String rejectionReason) {
		this.rejectionReason = rejectionReason;
	}
	public String getRejectionComment() {
		return rejectionComment;
	}
	public void setRejectionComment(String rejectionComment) {
		this.rejectionComment = rejectionComment;
	}
	public String getRegistrationSource() {
		return registrationSource;
	}
	public void setRegistrationSource(String registrationSource) {
		this.registrationSource = registrationSource;
	}
	public String getProfileLastUpdateSource() {
		return profileLastUpdateSource;
	}
	public void setProfileLastUpdateSource(String profileLastUpdateSource) {
		this.profileLastUpdateSource = profileLastUpdateSource;
	}
	public String getDelegatedIdp() {
		return delegatedIdp;
	}
	public void setDelegatedIdp(String delegatedIdp) {
		this.delegatedIdp = delegatedIdp;
	}
	public String getIdentityType() {
		return identityType;
	}
	public void setIdentityType(String identityType) {
		this.identityType = identityType;
	}
	public String getIsInternal() {
		return isInternal;
	}
	public void setIsInternal(String isInternal) {
		this.isInternal = isInternal;
	}
	public String getAil() {
		return ail;
	}
	public void setAil(String ail) {
		this.ail = ail;
	}
	public String getAilApplications() {
		return ailApplications;
	}
	public void setAilApplications(String ailApplications) {
		this.ailApplications = ailApplications;
	}
	public String getAilFeatures() {
		return ailFeatures;
	}
	public void setAilFeatures(String ailFeatures) {
		this.ailFeatures = ailFeatures;
	}
	public String getAilPrograms() {
		return ailPrograms;
	}
	public void setAilPrograms(String ailPrograms) {
		this.ailPrograms = ailPrograms;
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
	public String getDivision() {
		return division;
	}
	public void setDivision(String division) {
		this.division = division;
	}
	public String getTitle() {
		return title;
	}
	public void setTitle(String title) {
		this.title = title;
	}
	public String getBusinessUnit() {
		return businessUnit;
	}
	public void setBusinessUnit(String businessUnit) {
		this.businessUnit = businessUnit;
	}
	public boolean isHeadquarter() {
		return headquarter;
	}
	public void setHeadquarter(boolean headquarter) {
		this.headquarter = headquarter;
	}
	public float getAnnualRevenue() {
		return annualRevenue;
	}
	public void setAnnualRevenue(float annualRevenue) {
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
	public boolean isTrustedAdmin() {
		return trustedAdmin;
	}
	public void setTrustedAdmin(boolean trustedAdmin) {
		this.trustedAdmin = trustedAdmin;
	}
	public String getHashedPin() {
		return hashedPin;
	}
	public void setHashedPin(String hashedPin) {
		this.hashedPin = hashedPin;
	}
	public String getContactGoldenID() {
		return contactGoldenID;
	}
	public void setContactGoldenID(String contactGoldenID) {
		this.contactGoldenID = contactGoldenID;
	}
	public String getAccountGoldenID() {
		return accountGoldenID;
	}
	public void setAccountGoldenID(String accountGoldenID) {
		this.accountGoldenID = accountGoldenID;
	}
	public String getUserContext() {
		return userContext;
	}
	public void setUserContext(String userContext) {
		this.userContext = userContext;
	}
	public String getEmailOption() {
		return emailOption;
	}
	public void setEmailOption(String emailOption) {
		this.emailOption = emailOption;
	}
	
}

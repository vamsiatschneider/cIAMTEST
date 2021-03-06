/**
 * 
 */
package com.idms.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * @author SESA508936
 *
 */
public class GetUserRecordResponse {
	
	private String Id;	
	private String FirstName;
	private String LastName;
	private String Email;
	private String MobilePhone;	
	private String IDMSAil__c;	
	private String IDMSAIL_Applications__c;	
	private String IDMSAIL_Features__c;	
	private String IDMSAIL_Programs__c;
	private String country;
	private String postalCode;
	private String iam1;
	private String city;
	private String street;
	
	/*private String State;
	private String Country;
	private String City;
	private String Street;
	private String PostalCode;
	private String IDMSMarketSegment__c;
	private String IDMSMarketSubSegment__c;
	private String Job_Title__c;
	private String Job_Function__c;
	private String IDMSJobDescription__c;
	private String IDMSMiddleName__c;
	private String IDMSSalutation__c;
	private String Department;
	private String IDMSSuffix__c;
	private String IDMS_Company_Employee_Size__c;
	private String IDMSIdentityType__c;
	private String IDMS_Company_Name__c;
	private String IDMS_Company_Address1__c;
	private String IDMS_Company_City__c;
	private String IDMS_Company_Postal_Code__c;
	private String IDMS_Company_State__c;
	private String IDMSAnnualRevenue__c;
	private String IDMS_Company_Country__c;
	private String IDMS_Company_Address2__c;
	private String IDMS_Company_Address_PO_BOX__c;
	private String Fax;
	private String IDMS_Company_County__c;
	private String IDMS_Company_Website__c;
	private String IDMS_Company_My_Industry_is_a__c;
	private String IDMS_Company_Industries_I_serve__c;
	private String UserBusinessUnit__c;
	private String IDMS_Company_Headquarters__c;
	private String IDMS_Company_Tax_Identification_Number__c;
	private String Division;
	private String Title;
	private String AccountId;
	private String ContactId;
	private String IDMSCompanyFederationIdentifier__c;
	private String IDMSWorkPhone__c;*/
	//@JsonProperty("aboutMe")
	private String aboutMe;
	//@JsonProperty("additionalInfo")
	private String additionalInfo;
	//@JsonProperty("annualRevenue")
	private String annualRevenue;
	//@JsonProperty("bfoAccountId")
	private String bfoAccountId;
	//@JsonProperty("companyAdditionalInfo")
	private String companyAdditionalInfo;
	
	private String companyCity;
	private String companyCountry;
	private String companyCounty;
	private String companyFederatedID;
	private String companyName;
	private String companyPostalCode;
	private String companyPostOfficeBox;
	private String companyState;
	private String companyStreet;
	private String companyWebSite;
	private String contactId;
	private String context;
	private String county;
	private String currency;
	private String Division;
	private String emailOptIn;
	private String employeeSize;
	private String fax;
	private String federationID;
	private String headQuarters;
	private String homePhone;
	private String iam2;
	private String identityType;
	private String industrySegment;
	private String industrySubSegment;
	private String inetuserActive;
	private String isActivated;
	private String jobDescription;
	private String jobFunction;
	private String middleName;
	private String postOfficeBox;
	private String preferredLanguage;
	private String SEAccountNumber;
	private String stateOrProvinceName;
	private String suffix;
	private String taxID;
	private String telephoneNumber;
	private String title;
	
	

	public String getCompanyAdditionalInfo() {
		return companyAdditionalInfo;
	}
	@JsonProperty("companyAdditionalInfo")
	public void setCompanyAdditionalInfo(String companyAdditionalInfo) {
		this.companyAdditionalInfo = companyAdditionalInfo;
	}

	public String getBfoAccountId() {
		return bfoAccountId;
	}
	@JsonProperty("bfoAccountId")
	public void setBfoAccountId(String accountId) {
		bfoAccountId = accountId;
	}

	public String getAnnualRevenue() {
		return annualRevenue;
	}
	@JsonProperty("annualRevenue")
	public void setAnnualRevenue(String annualRevenue) {
		this.annualRevenue = annualRevenue;
	}

	public String getAdditionalInfo() {
		return additionalInfo;
	}
	@JsonProperty("additionalInfo")
	public void setAdditionalInfo(String additionalInfo) {
		this.additionalInfo = additionalInfo;
	}

	public String getAboutMe() {
		return aboutMe;
	}
	@JsonProperty("aboutMe")
	public void setAboutMe(String aboutMe) {
		this.aboutMe = aboutMe;
	}

	@JsonProperty("Id")
	public String getId() {
		return Id;
	}

	@JsonProperty("Id")
	public void setId(String id) {
		Id = id;
	}

	@JsonProperty("FirstName")
	public String getFirstName() {
		return FirstName;
	}

	@JsonProperty("FirstName")
	public void setFirstName(String firstName) {
		FirstName = firstName;
	}

	@JsonProperty("LastName")
	public String getLastName() {
		return LastName;
	}

	@JsonProperty("LastName")
	public void setLastName(String lastName) {
		LastName = lastName;
	}

	@JsonProperty("Email")
	public String getEmail() {
		return Email;
	}

	@JsonProperty("Email")
	public void setEmail(String email) {
		Email = email;
	}

	@JsonProperty("MobilePhone")
	public String getMobilePhone() {
		return MobilePhone;
	}

	@JsonProperty("MobilePhone")
	public void setMobilePhone(String mobilePhone) {
		MobilePhone = mobilePhone;
	}

	@JsonProperty("IDMSAil__c")
	public String getIDMSAil__c() {
		return IDMSAil__c;
	}

	@JsonProperty("IDMSAil__c")
	public void setIDMSAil__c(String iDMSAil__c) {
		IDMSAil__c = iDMSAil__c;
	}

	@JsonProperty("IDMSAIL_Applications__c")
	public String getIDMSAIL_Applications__c() {
		return IDMSAIL_Applications__c;
	}

	@JsonProperty("IDMSAIL_Applications__c")
	public void setIDMSAIL_Applications__c(String iDMSAIL_Applications__c) {
		IDMSAIL_Applications__c = iDMSAIL_Applications__c;
	}

	@JsonProperty("IDMSAIL_Features__c")
	public String getIDMSAIL_Features__c() {
		return IDMSAIL_Features__c;
	}

	@JsonProperty("IDMSAIL_Features__c")
	public void setIDMSAIL_Features__c(String iDMSAIL_Features__c) {
		IDMSAIL_Features__c = iDMSAIL_Features__c;
	}

	@JsonProperty("IDMSAIL_Programs__c")
	public String getIDMSAIL_Programs__c() {
		return IDMSAIL_Programs__c;
	}

	@JsonProperty("IDMSAIL_Programs__c")
	public void setIDMSAIL_Programs__c(String iDMSAIL_Programs__c) {
		IDMSAIL_Programs__c = iDMSAIL_Programs__c;
	}
	
	@JsonProperty("Country")
	public String getCountry() {
		return country;
	}

	public String getIam1() {
		return iam1;
	}
	@JsonProperty("iam1")
	public void setIam1(String iam1) {
		this.iam1 = iam1;
	}

	public String getCity() {
		return city;
	}
	@JsonProperty("city")
	public void setCity(String city) {
		this.city = city;
	}

	public String getStreet() {
		return street;
	}
	@JsonProperty("street")
	public void setStreet(String street) {
		this.street = street;
	}

	public String getCompanyCity() {
		return companyCity;
	}
	@JsonProperty("companyCity")
	public void setCompanyCity(String companyCity) {
		this.companyCity = companyCity;
	}

	public String getCompanyCountry() {
		return companyCountry;
	}
	@JsonProperty("companyCountry")
	public void setCompanyCountry(String companyCountry) {
		this.companyCountry = companyCountry;
	}

	public String getCompanyCounty() {
		return companyCounty;
	}
	@JsonProperty("companyCounty")
	public void setCompanyCounty(String companyCounty) {
		this.companyCounty = companyCounty;
	}

	public String getCompanyFederatedID() {
		return companyFederatedID;
	}
	@JsonProperty("companyFederatedID")
	public void setCompanyFederatedID(String companyFederatedID) {
		this.companyFederatedID = companyFederatedID;
	}

	public String getCompanyName() {
		return companyName;
	}
	@JsonProperty("companyName")
	public void setCompanyName(String companyName) {
		this.companyName = companyName;
	}

	public String getCompanyPostalCode() {
		return companyPostalCode;
	}
	@JsonProperty("companyPostalCode")
	public void setCompanyPostalCode(String companyPostalCode) {
		this.companyPostalCode = companyPostalCode;
	}

	public String getCompanyPostOfficeBox() {
		return companyPostOfficeBox;
	}
	@JsonProperty("companyPostOfficeBox")													//@JsonProperty
	public void setCompanyPostOfficeBox(String companyPostOfficeBox) {
		this.companyPostOfficeBox = companyPostOfficeBox;
	}

	public String getCompanyState() {
		return companyState;
	}
	@JsonProperty("companyState")
	public void setCompanyState(String companyState) {
		this.companyState = companyState;
	}

	public String getCompanyStreet() {
		return companyStreet;
	}
	@JsonProperty("companyStreet")
	public void setCompanyStreet(String companyStreet) {
		this.companyStreet = companyStreet;
	}

	public String getCompanyWebSite() {
		return companyWebSite;
	}
	@JsonProperty("companyWebSite")
	public void setCompanyWebSite(String companyWebSite) {
		this.companyWebSite = companyWebSite;
	}
	
	public String getContactId() {
		return contactId;
	}
	@JsonProperty("contactId")
	public void setContactId(String contactId) {
		this.contactId = contactId;
	}

	public String getContext() {
		return context;
	}
	@JsonProperty("context")
	public void setContext(String context) {
		this.context = context;
	}

	public String getCounty() {
		return county;
	}
	@JsonProperty("county")
	public void setCounty(String county) {
		this.county = county;
	}

	public String getCurrency() {
		return currency;
	}
	@JsonProperty("currency")
	public void setCurrency(String currency) {
		this.currency = currency;
	}

	public String getDivision() {
		return Division;
	}
	@JsonProperty("Division")
	public void setDivision(String division) {
		Division = division;
	}

	public String getEmailOptIn() {
		return emailOptIn;
	}
	@JsonProperty("emailOptIn")
	public void setEmailOptIn(String emailOptIn) {
		this.emailOptIn = emailOptIn;
	}

	public String getEmployeeSize() {
		return employeeSize;
	}
	@JsonProperty("employeesize")
	public void setEmployeeSize(String employeesize) {
		this.employeeSize = employeesize;
	}

	public String getFax() {
		return fax;
	}
	@JsonProperty("fax")
	public void setFax(String fax) {
		this.fax = fax;
	}

	public String getFederationID() {
		return federationID;
	}
	@JsonProperty("federationID")
	public void setFederationID(String federationID) {
		this.federationID = federationID;
	}

	public String getHeadQuarters() {
		return headQuarters;
	}
	@JsonProperty("headQuarters")
	public void setHeadQuarters(String headquarters) {
		this.headQuarters = headquarters;
	}

	public String getHomePhone() {
		return homePhone;
	}
	@JsonProperty("homePhone")
	public void setHomePhone(String homePhone) {
		this.homePhone = homePhone;
	}

	public String getIam2() {
		return iam2;
	}
	@JsonProperty("iam2")
	public void setIam2(String iam2) {
		this.iam2 = iam2;
	}

	public String getIdentityType() {
		return identityType;
	}
	@JsonProperty("identityType")
	public void setIdentityType(String identityType) {
		this.identityType = identityType;
	}

	public String getIndustrySegment() {
		return industrySegment;
	}
	@JsonProperty("industrySegment")	
	public void setIndustrySegment(String industrySegment) {
		this.industrySegment = industrySegment;
	}

	public String getIndustrySubSegment() {
		return industrySubSegment;
	}
	@JsonProperty("industrySubSegment")
	public void setIndustrySubSegment(String industrySubSegment) {
		this.industrySubSegment = industrySubSegment;
	}

	public String getInetuserActive() {
		return inetuserActive;
	}
	@JsonProperty("inetuserActive")
	public void setInetuserActive(String inetuserActive) {
		this.inetuserActive = inetuserActive;
	}

	public String getIsActivated() {
		return isActivated;
	}
	@JsonProperty("isActivated")
	public void setIsActivated(String isActivated) {
		this.isActivated = isActivated;
	}

	public String getJobDescription() {
		return jobDescription;
	}
	@JsonProperty("jobDescription")
	public void setJobDescription(String jobDescription) {
		this.jobDescription = jobDescription;
	}

	public String getJobFunction() {
		return jobFunction;
	}
	@JsonProperty("jobFunction")
	public void setJobFunction(String jobFunction) {
		this.jobFunction = jobFunction;
	}

	public String getMiddleName() {
		return middleName;
	}
	@JsonProperty("middleName")
	public void setMiddleName(String middleName) {
		this.middleName = middleName;
	}

	public String getPostOfficeBox() {
		return postOfficeBox;
	}
	@JsonProperty("postOfficeBox")
	public void setPostOfficeBox(String postOfficeBox) {
		this.postOfficeBox = postOfficeBox;
	}

	public String getPreferredLanguage() {
		return preferredLanguage;
	}
	@JsonProperty("preferredLanguage")
	public void setPreferredLanguage(String preferredLanguage) {
		this.preferredLanguage = preferredLanguage;
	}

	public String getSEAccountNumber() {
		return SEAccountNumber;
	}
	@JsonProperty("SEAccountNumber")
	public void setSEAccountNumber(String sEAccountNumber) {
		SEAccountNumber = sEAccountNumber;
	}

	public String getStateOrProvinceName() {
		return stateOrProvinceName;
	}
	@JsonProperty("stateOrProvinceName")
	public void setStateOrProvinceName(String stateOrProvinceName) {
		this.stateOrProvinceName = stateOrProvinceName;
	}

	public String getSuffix() {
		return suffix;
	}
	@JsonProperty("suffix")
	public void setSuffix(String suffix) {
		this.suffix = suffix;
	}

	public String getTaxID() {
		return taxID;
	}
	@JsonProperty("taxID")
	public void setTaxID(String taxID) {
		this.taxID = taxID;
	}

	public String getTelephoneNumber() {
		return telephoneNumber;
	}
	@JsonProperty("telephoneNumber")
	public void setTelephoneNumber(String telephoneNumber) {
		this.telephoneNumber = telephoneNumber;
	}

	public String getTitle() {
		return title;
	}
	@JsonProperty("title")
	public void setTitle(String title) {
		this.title = title;
	}

	@JsonProperty("Country")
	public void setCountry(String country) {
		this.country = country;
	}
	
	@JsonProperty("postalCode")
	public String getPostalCode() {
		return postalCode;
	}

	@JsonProperty("postalCode")
	public void setPostalCode(String postalCode) {
		this.postalCode = postalCode;
	}

	/*@JsonProperty("State")
	public String getState() {
		return State;
	}

	@JsonProperty("State")
	public void setState(String state) {
		State = state;
	}

	
	
	
	@JsonProperty("Country")
	public String getCountry() {
		return Country;
	}

	@JsonProperty("Country")
	public void setCountry(String country) {
		Country = country;
	}

	@JsonProperty("City")
	public String getCity() {
		return City;
	}

	@JsonProperty("City")
	public void setCity(String city) {
		City = city;
	}

	@JsonProperty("Street")
	public String getStreet() {
		return Street;
	}

	@JsonProperty("Street")
	public void setStreet(String street) {
		Street = street;
	}

	@JsonProperty("PostalCode")
	public String getPostalCode() {
		return PostalCode;
	}

	@JsonProperty("PostalCode")
	public void setPostalCode(String postalCode) {
		PostalCode = postalCode;
	}

	@JsonProperty("IDMSMarketSegment__c")
	public String getIDMSMarketSegment__c() {
		return IDMSMarketSegment__c;
	}

	@JsonProperty("IDMSMarketSegment__c")
	public void setIDMSMarketSegment__c(String iDMSMarketSegment__c) {
		IDMSMarketSegment__c = iDMSMarketSegment__c;
	}

	@JsonProperty("IDMSMarketSubSegment__c")
	public String getIDMSMarketSubSegment__c() {
		return IDMSMarketSubSegment__c;
	}

	@JsonProperty("IDMSMarketSubSegment__c")
	public void setIDMSMarketSubSegment__c(String iDMSMarketSubSegment__c) {
		IDMSMarketSubSegment__c = iDMSMarketSubSegment__c;
	}

	@JsonProperty("Job_Title__c")
	public String getJob_Title__c() {
		return Job_Title__c;
	}

	@JsonProperty("Job_Title__c")
	public void setJob_Title__c(String job_Title__c) {
		Job_Title__c = job_Title__c;
	}

	@JsonProperty("Job_Function__c")
	public String getJob_Function__c() {
		return Job_Function__c;
	}

	@JsonProperty("Job_Function__c")
	public void setJob_Function__c(String job_Function__c) {
		Job_Function__c = job_Function__c;
	}

	@JsonProperty("IDMSJobDescription__c")
	public String getIDMSJobDescription__c() {
		return IDMSJobDescription__c;
	}

	@JsonProperty("IDMSJobDescription__c")
	public void setIDMSJobDescription__c(String iDMSJobDescription__c) {
		IDMSJobDescription__c = iDMSJobDescription__c;
	}

	@JsonProperty("IDMSMiddleName__c")
	public String getIDMSMiddleName__c() {
		return IDMSMiddleName__c;
	}

	@JsonProperty("IDMSMiddleName__c")
	public void setIDMSMiddleName__c(String iDMSMiddleName__c) {
		IDMSMiddleName__c = iDMSMiddleName__c;
	}

	@JsonProperty("IDMSSalutation__c")
	public String getIDMSSalutation__c() {
		return IDMSSalutation__c;
	}

	@JsonProperty("IDMSSalutation__c")
	public void setIDMSSalutation__c(String iDMSSalutation__c) {
		IDMSSalutation__c = iDMSSalutation__c;
	}

	@JsonProperty("Department")
	public String getDepartment() {
		return Department;
	}

	@JsonProperty("Department")
	public void setDepartment(String department) {
		Department = department;
	}

	@JsonProperty("IDMSSuffix__c")
	public String getIDMSSuffix__c() {
		return IDMSSuffix__c;
	}

	@JsonProperty("IDMSSuffix__c")
	public void setIDMSSuffix__c(String iDMSSuffix__c) {
		IDMSSuffix__c = iDMSSuffix__c;
	}

	@JsonProperty("IDMS_Company_Employee_Size__c")
	public String getIDMS_Company_Employee_Size__c() {
		return IDMS_Company_Employee_Size__c;
	}

	@JsonProperty("IDMS_Company_Employee_Size__c")
	public void setIDMS_Company_Employee_Size__c(String iDMS_Company_Employee_Size__c) {
		IDMS_Company_Employee_Size__c = iDMS_Company_Employee_Size__c;
	}

	@JsonProperty("IDMSIdentityType__c")
	public String getIDMSIdentityType__c() {
		return IDMSIdentityType__c;
	}

	@JsonProperty("IDMSIdentityType__c")
	public void setIDMSIdentityType__c(String iDMSIdentityType__c) {
		IDMSIdentityType__c = iDMSIdentityType__c;
	}

	@JsonProperty("IDMS_Company_Name__c")
	public String getIDMS_Company_Name__c() {
		return IDMS_Company_Name__c;
	}

	@JsonProperty("IDMS_Company_Name__c")
	public void setIDMS_Company_Name__c(String iDMS_Company_Name__c) {
		IDMS_Company_Name__c = iDMS_Company_Name__c;
	}

	@JsonProperty("IDMS_Company_Address1__c")
	public String getIDMS_Company_Address1__c() {
		return IDMS_Company_Address1__c;
	}

	@JsonProperty("IDMS_Company_Address1__c")
	public void setIDMS_Company_Address1__c(String iDMS_Company_Address1__c) {
		IDMS_Company_Address1__c = iDMS_Company_Address1__c;
	}

	@JsonProperty("IDMS_Company_City__c")
	public String getIDMS_Company_City__c() {
		return IDMS_Company_City__c;
	}

	@JsonProperty("IDMS_Company_City__c")
	public void setIDMS_Company_City__c(String iDMS_Company_City__c) {
		IDMS_Company_City__c = iDMS_Company_City__c;
	}

	@JsonProperty("IDMS_Company_Postal_Code__c")
	public String getIDMS_Company_Postal_Code__c() {
		return IDMS_Company_Postal_Code__c;
	}

	@JsonProperty("IDMS_Company_Postal_Code__c")
	public void setIDMS_Company_Postal_Code__c(String iDMS_Company_Postal_Code__c) {
		IDMS_Company_Postal_Code__c = iDMS_Company_Postal_Code__c;
	}

	@JsonProperty("IDMS_Company_State__c")
	public String getIDMS_Company_State__c() {
		return IDMS_Company_State__c;
	}

	@JsonProperty("IDMS_Company_State__c")
	public void setIDMS_Company_State__c(String iDMS_Company_State__c) {
		IDMS_Company_State__c = iDMS_Company_State__c;
	}

	@JsonProperty("IDMSAnnualRevenue__c")
	public String getIDMSAnnualRevenue__c() {
		return IDMSAnnualRevenue__c;
	}

	@JsonProperty("IDMSAnnualRevenue__c")
	public void setIDMSAnnualRevenue__c(String iDMSAnnualRevenue__c) {
		IDMSAnnualRevenue__c = iDMSAnnualRevenue__c;
	}

	@JsonProperty("IDMS_Company_Country__c")
	public String getIDMS_Company_Country__c() {
		return IDMS_Company_Country__c;
	}

	@JsonProperty("IDMS_Company_Country__c")
	public void setIDMS_Company_Country__c(String iDMS_Company_Country__c) {
		IDMS_Company_Country__c = iDMS_Company_Country__c;
	}

	@JsonProperty("IDMS_Company_Address2__c")
	public String getIDMS_Company_Address2__c() {
		return IDMS_Company_Address2__c;
	}

	@JsonProperty("IDMS_Company_Address2__c")
	public void setIDMS_Company_Address2__c(String iDMS_Company_Address2__c) {
		IDMS_Company_Address2__c = iDMS_Company_Address2__c;
	}

	@JsonProperty("IDMS_Company_Address_PO_BOX__c")
	public String getIDMS_Company_Address_PO_BOX__c() {
		return IDMS_Company_Address_PO_BOX__c;
	}

	@JsonProperty("IDMS_Company_Address_PO_BOX__c")
	public void setIDMS_Company_Address_PO_BOX__c(String iDMS_Company_Address_PO_BOX__c) {
		IDMS_Company_Address_PO_BOX__c = iDMS_Company_Address_PO_BOX__c;
	}

	@JsonProperty("Fax")
	public String getFax() {
		return Fax;
	}

	@JsonProperty("Fax")
	public void setFax(String fax) {
		Fax = fax;
	}

	@JsonProperty("IDMS_Company_County__c")
	public String getIDMS_Company_County__c() {
		return IDMS_Company_County__c;
	}

	@JsonProperty("IDMS_Company_County__c")
	public void setIDMS_Company_County__c(String iDMS_Company_County__c) {
		IDMS_Company_County__c = iDMS_Company_County__c;
	}

	@JsonProperty("IDMS_Company_Website__c")
	public String getIDMS_Company_Website__c() {
		return IDMS_Company_Website__c;
	}

	@JsonProperty("IDMS_Company_Website__c")
	public void setIDMS_Company_Website__c(String iDMS_Company_Website__c) {
		IDMS_Company_Website__c = iDMS_Company_Website__c;
	}

	@JsonProperty("IDMS_Company_My_Industry_is_a__c")
	public String getIDMS_Company_My_Industry_is_a__c() {
		return IDMS_Company_My_Industry_is_a__c;
	}

	@JsonProperty("IDMS_Company_My_Industry_is_a__c")
	public void setIDMS_Company_My_Industry_is_a__c(String iDMS_Company_My_Industry_is_a__c) {
		IDMS_Company_My_Industry_is_a__c = iDMS_Company_My_Industry_is_a__c;
	}

	@JsonProperty("IDMS_Company_Industries_I_serve__c")
	public String getIDMS_Company_Industries_I_serve__c() {
		return IDMS_Company_Industries_I_serve__c;
	}

	@JsonProperty("IDMS_Company_Industries_I_serve__c")
	public void setIDMS_Company_Industries_I_serve__c(String iDMS_Company_Industries_I_serve__c) {
		IDMS_Company_Industries_I_serve__c = iDMS_Company_Industries_I_serve__c;
	}

	@JsonProperty("UserBusinessUnit__c")
	public String getUserBusinessUnit__c() {
		return UserBusinessUnit__c;
	}

	@JsonProperty("UserBusinessUnit__c")
	public void setUserBusinessUnit__c(String userBusinessUnit__c) {
		UserBusinessUnit__c = userBusinessUnit__c;
	}

	@JsonProperty("IDMS_Company_Headquarters__c")
	public String getIDMS_Company_Headquarters__c() {
		return IDMS_Company_Headquarters__c;
	}

	@JsonProperty("IDMS_Company_Headquarters__c")
	public void setIDMS_Company_Headquarters__c(String iDMS_Company_Headquarters__c) {
		IDMS_Company_Headquarters__c = iDMS_Company_Headquarters__c;
	}

	@JsonProperty("IDMS_Company_Tax_Identification_Number__c")
	public String getIDMS_Company_Tax_Identification_Number__c() {
		return IDMS_Company_Tax_Identification_Number__c;
	}

	@JsonProperty("IDMS_Company_Tax_Identification_Number__c")
	public void setIDMS_Company_Tax_Identification_Number__c(String iDMS_Company_Tax_Identification_Number__c) {
		IDMS_Company_Tax_Identification_Number__c = iDMS_Company_Tax_Identification_Number__c;
	}

	@JsonProperty("Division")
	public String getDivision() {
		return Division;
	}

	@JsonProperty("Division")
	public void setDivision(String division) {
		Division = division;
	}

	@JsonProperty("Title")
	public String getTitle() {
		return Title;
	}

	@JsonProperty("Title")
	public void setTitle(String title) {
		Title = title;
	}

	@JsonProperty("AccountId")
	public String getAccountId() {
		return AccountId;
	}

	@JsonProperty("AccountId")
	public void setAccountId(String accountId) {
		AccountId = accountId;
	}

	@JsonProperty("ContactId")
	public String getContactId() {
		return ContactId;
	}

	@JsonProperty("ContactId")
	public void setContactId(String contactId) {
		ContactId = contactId;
	}

	@JsonProperty("IDMSCompanyFederationIdentifier__c")
	public String getIDMSCompanyFederationIdentifier__c() {
		return IDMSCompanyFederationIdentifier__c;
	}

	@JsonProperty("IDMSCompanyFederationIdentifier__c")
	public void setIDMSCompanyFederationIdentifier__c(String iDMSCompanyFederationIdentifier__c) {
		IDMSCompanyFederationIdentifier__c = iDMSCompanyFederationIdentifier__c;
	}

	@JsonProperty("IDMSWorkPhone__c")
	public String getIDMSWorkPhone__c() {
		return IDMSWorkPhone__c;
	}

	@JsonProperty("IDMSWorkPhone__c")
	public void setIDMSWorkPhone__c(String iDMSWorkPhone__c) {
		IDMSWorkPhone__c = iDMSWorkPhone__c;
	}*/

}

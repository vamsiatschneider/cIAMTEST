package com.idms.product.model;

import javax.validation.constraints.Size;
import javax.xml.bind.annotation.XmlRootElement;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;

@XmlRootElement
public class OpenAMGetUserWorkResponse {

	@JsonInclude
	private Attributes Attributes;
	
	/**
	 * Attribute of the IFW which has the UserName for which this user is
	 * created.
	 */
	    
	@Size(min = 8, max = 12)
	private String Email;

	    
	private String MobilePhone;

	    
	private String Phone;

	    
	private String FirstName;

	    
	private String LastName;

	    
	private String IDMS_Email_opt_in__c;

	    
	private String IDMS_User_Context__c;

	    
	private String Country;

	    
	private String IDMS_PreferredLanguage__c;

	    
	private String IDMS_Company_Currency_ISO_Code__c;

	    
	private String Street;

	    
	private String City;

	    
	private String PostalCode;

	    
	private String State;

	    
	private String IDMS_County__c;

	    
	private String IDMS_POBox__c;

	    
	private String IDMS_Federated_ID__c;

	    
	private String IDMS_Registration_Source__c;

	    
	private String IDMS_Profile_update_source__c;
				   	
	    
	private String IDMS_AdditionalAddress__c;

	    
	private String IDMS_Company_Name__c;

	    
	private String IDMS_Company_Address1__c;

	    
	private String IDMS_Company_City__c;

	    
	private String IDMS_Company_Postal_Code__c;

	    
	private String IDMS_Company_State__c;

	    
	private String IDMS_Company_Address_PO_BOX__c;

	    
	private String IDMS_Company_Country__c;

	    
	private String IDMS_Company_Address2__c;

	    
	private String IDMS_Company_I_am_a__c;

	    
	private String IDMS_Company_My_Industry_is_a__c;
	
	private String IDMSClassLevel2_c;

	    
	private String IDMSMarketSegment__c;

	    
	private String IDMSMarketSubSegment__c;
	
	    
	private String Job_Title__c;

	    
	private String Job_Function__c;

	    
	private String IDMSJobDescription__c;
	
	    
	private String IDMS_Company_Industries_I_serve__c;
	
	    
	private String IDMS_Company_Employee_Size__c;
	
	    
	private String IDMS_Company_Headquarters__c;

	    
	private String IDMSAnnualRevenue__c;
	
	    
	private String IDMS_Company_Tax_Identification_Number__c;
	
	    
	private String IDMSMiddleName__c;
	
	    
	private String IDMS_Company_Website__c;
	
	    
	private String IDMSSalutation__c;
	
	    
	private String Department;

	    
	private String IDMSSuffix__c;

	    
	private String Fax;
	
	    
	private String IDMSDelegatedIdp__c;
	
	    
	private String IDMSIdentityType__c;
	
	    
	private String IDMS_Company_County__c;
	
	private String Id;
	
	private String tncFlag;
	
	private String Username;

	private String ContactId;
	
	private String AccountId;
	
	private String IDMS_federatedidentity__c;
	
	private String IDMSisInternal__c;
	
	private String IDMSAil__c;
	
	private String IDMSAIL_Applications__c;
	
	private String IDMSAIL_Features__c;
	
	private String IDMSAIL_Programs__c;
	
	private String IDMSPrimaryContact__c;
	
	private String companyFederatedID;
	
	private String AboutMe;

	//new fields
	private String DefaultCurrencyIsoCode;
	
	private String currencyCode;
	
	private String FederationIdentifier;
	
	private String IDMSClassLevel1__c;
	
	private String Title;
	
	private String UserStatus__c;
	
	private String Company_City__c;
	
	private String Company_Address2__c;
	
	private String IDMSCompanyFederationIdentifier__c;
	
	private String Company_Website__c;
	
	private String Company_Country__c;
	
	private String Company_State__c;
	
	private String Company_Postal_Code__c;
	
	private String IDMSTaxIdentificationNumber_c;
	
	private String IDMSCompanyHeadquarters__c;
	
	private String IDMSCompanyCounty__c;
	
	private String IDMSCompanyPoBox__c;
	
	private String CompanyName;
	
	private String IDMSCompanyNbrEmployees__c;
	
	private String trustedAdmin;
	
	private String isActivated;
	
	@JsonProperty("IDMSCompanyFederationIdentifier__c")
	public String getIDMSCompanyFederationIdentifier__c() {
		return IDMSCompanyFederationIdentifier__c;
	}

	public void setIDMSCompanyFederationIdentifier__c(String iDMSCompanyFederationIdentifier__c) {
		IDMSCompanyFederationIdentifier__c = iDMSCompanyFederationIdentifier__c;
	}
	
	@JsonProperty("IDMSCompanyHeadquarters__c")
	public String getIDMSCompanyHeadquarters_c() {
		return IDMSCompanyHeadquarters__c;
	}

	public void setIDMSCompanyHeadquarters_c(String iDMSCompanyHeadquarters__c) {
		IDMSCompanyHeadquarters__c = iDMSCompanyHeadquarters__c;
	}
	
	@JsonProperty("IDMSCompanyCounty__c")
	public String getIDMSCompanyCounty_c() {
		return IDMSCompanyCounty__c;
	}

	public void setIDMSCompanyCounty_c(String iDMSCompanyCounty__c) {
		IDMSCompanyCounty__c = iDMSCompanyCounty__c;
	}
	
	@JsonProperty("IDMSCompanyPoBox__c")
	public String getIDMSCompanyPoBox__c() {
		return IDMSCompanyPoBox__c;
	}

	public void setIDMSCompanyPoBox__c(String iDMSCompanyPoBox__c) {
		IDMSCompanyPoBox__c = iDMSCompanyPoBox__c;
	}

	
	@JsonProperty("CompanyName")
	public String getCompanyName_c() {
		return CompanyName;
	}

	public void setCompanyName_c(String companyName) {
		CompanyName = companyName;
	}
	
	@JsonProperty("IDMSCompanyNbrEmployees__c")
	public String getIDMSCompanyNbrEmployees_c() {
		return IDMSCompanyNbrEmployees__c;
	}

	public void setIDMSCompanyNbrEmployees_c(String iDMSCompanyNbrEmployees__c) {
		IDMSCompanyNbrEmployees__c = iDMSCompanyNbrEmployees__c;
	}
	
	
	@JsonProperty("Company_Postal_Code__c")
	public String getCompany_Postal_Code_c() {
		return Company_Postal_Code__c;
	}

	public void setCompany_Postal_Code_c(String company_Postal_Code__c) {
		Company_Postal_Code__c = company_Postal_Code__c;
	}
	
	
	@JsonProperty("Company_State__c")
	public String getCompany_State_c() {
		return Company_State__c;
	}

	public void setCompany_State_c(String company_State__c) {
		Company_State__c = company_State__c;
	}
	
	
	@JsonProperty("Company_Country__c")
	public String getCompany_Country_c() {
		return Company_Country__c;
	}

	public void setCompany_Country_c(String company_Country__c) {
		Company_Country__c = company_Country__c;
	}
	
	
	@JsonProperty("Company_Website__c")
	public String getCompany_Website_c() {
		return Company_Website__c;
	}

	public void setCompany_Website_c(String company_Website__c) {
		Company_Website__c = company_Website__c;
	}
	
	@JsonProperty("Company_Address2__c")
	public String getCompany_Address2_c() {
		return Company_Address2__c;
	}

	public void setCompany_Address2_c(String company_Address2__c) {
		Company_Address2__c = company_Address2__c;
	}
	
	@JsonProperty("Company_City__c")
	public String getCompany_City_c() {
		return Company_City__c;
	}

	public void setCompany_City_c(String company_City__c) {
		Company_City__c = company_City__c;
	}
	
	
	@JsonProperty("UserStatus__c")
	public String getUserStatus__c() {
		return UserStatus__c;
	}

	public void setUserStatus__c(String userStatus__c) {
		UserStatus__c = userStatus__c;
	}

	@JsonProperty("Title")
	public String getTitle() {
		return Title;
	}

	public void setTitle(String title) {
		Title = title;
	}

	@JsonProperty("IDMSClassLevel1__c")
	public String getIDMSClassLevel1_c() {
		return IDMSClassLevel1__c;
	}

	public void setIDMSClassLevel1_c(String iDMSClassLevel1__c) {
		IDMSClassLevel1__c = iDMSClassLevel1__c;
	}

	@JsonProperty("FederationIdentifier")
	public String getFederationIdentifier() {
		return FederationIdentifier;
	}

	public void setFederationIdentifier(String federationIdentifier) {
		FederationIdentifier = federationIdentifier;
	}

	@JsonProperty("DefaultCurrencyIsoCode")	
	public String getDefaultCurrencyIsoCode() {
		return DefaultCurrencyIsoCode;
	}

	public void setDefaultCurrencyIsoCode(String defaultCurrencyIsoCode) {
		DefaultCurrencyIsoCode = defaultCurrencyIsoCode;
	}
	
	@JsonProperty("Id")	
	public String getId() {
		return Id;
	}

	public void setId(String id) {
		Id = id;
	}

	public Attributes getAttributes() {
		return Attributes;
	}

	public void setAttributes(Attributes attributes) {
		Attributes = attributes;
	}

	@JsonProperty("Email")
	public String getEmail() {
		return Email;
	}

	public void setEmail(String email) {
		Email = email;
	}

	@JsonProperty("MobilePhone")
	public String getMobilePhone() {
		return MobilePhone;
	}

	public void setMobilePhone(String mobilePhone) {
		MobilePhone = mobilePhone;
	}

	@JsonProperty("Phone")
	public String getPhone() {
		return Phone;
	}

	public void setPhone(String phone) {
		Phone = phone;
	}

	@JsonProperty("FirstName")
	public String getFirstName() {
		return FirstName;
	}

	public void setFirstName(String firstName) {
		FirstName = firstName;
	}

	@JsonProperty("LastName")
	public String getLastName() {
		return LastName;
	}

	public void setLastName(String lastName) {
		LastName = lastName;
	}

	@JsonProperty("IDMS_Email_opt_in__c")
	public String getIDMS_Email_opt_in__c() {
		return IDMS_Email_opt_in__c;
	}

	public void setIDMS_Email_opt_in__c(String iDMS_Email_opt_in__c) {
		IDMS_Email_opt_in__c = iDMS_Email_opt_in__c;
	}

	@JsonProperty("IDMS_User_Context__c")
	public String getIDMS_User_Context__c() {
		return IDMS_User_Context__c;
	}

	public void setIDMS_User_Context__c(String iDMS_User_Context__c) {
		IDMS_User_Context__c = iDMS_User_Context__c;
	}

	@JsonProperty("Country")
	public String getCountry() {
		return Country;
	}

	public void setCountry(String country) {
		Country = country;
	}

	@JsonProperty("IDMS_PreferredLanguage__c")
	public String getIDMS_PreferredLanguage__c() {
		return IDMS_PreferredLanguage__c;
	}

	public void setIDMS_PreferredLanguage__c(String iDMS_PreferredLanguage__c) {
		IDMS_PreferredLanguage__c = iDMS_PreferredLanguage__c;
	}

	@JsonProperty("Street")
	public String getStreet() {
		return Street;
	}

	@JsonProperty("IDMS_Company_Currency_ISO_Code__c")
	public String getIDMS_Company_Currency_ISO_Code__c() {
		return IDMS_Company_Currency_ISO_Code__c;
	}

	public void setIDMS_Company_Currency_ISO_Code__c(String iDMS_Company_Currency_ISO_Code__c) {
		IDMS_Company_Currency_ISO_Code__c = iDMS_Company_Currency_ISO_Code__c;
	}

	public void setStreet(String street) {
		Street = street;
	}

	@JsonProperty("City")
	public String getCity() {
		return City;
	}

	public void setCity(String city) {
		City = city;
	}
	
	@JsonProperty("PostalCode")
	public String getPostalCode() {
		return PostalCode;
	}

	public void setPostalCode(String postalCode) {
		PostalCode = postalCode;
	}

	@JsonProperty("State")
	public String getState() {
		return State;
	}

	public void setState(String state) {
		State = state;
	}

	@JsonProperty("IDMS_County__c")
	public String getIDMS_County__c() {
		return IDMS_County__c;
	}

	public void setIDMS_County__c(String iDMS_County__c) {
		IDMS_County__c = iDMS_County__c;
	}

	@JsonProperty("IDMS_POBox__c")
	public String getIDMS_POBox__c() {
		return IDMS_POBox__c;
	}

	public void setIDMS_POBox__c(String iDMS_POBox__c) {
		IDMS_POBox__c = iDMS_POBox__c;
	}

	@JsonProperty("IDMS_Federated_ID__c")
	public String getIDMS_Federated_ID__c() {
		return IDMS_Federated_ID__c;
	}

	public void setIDMS_Federated_ID__c(String iDMS_Federated_ID__c) {
		IDMS_Federated_ID__c = iDMS_Federated_ID__c;
	}

	@JsonProperty("IDMS_Registration_Source__c")
	public String getIDMS_Registration_Source__c() {
		return IDMS_Registration_Source__c;
	}

	public void setIDMS_Registration_Source__c(String iDMS_Registration_Source__c) {
		IDMS_Registration_Source__c = iDMS_Registration_Source__c;
	}

	@JsonProperty("IDMS_Profile_update_source__c")
	public String getIDMS_Profile_update_source__c() {
		return IDMS_Profile_update_source__c;
	}

	public void setIDMS_Profile_update_source__c(String iDMS_Profile_update_source__c) {
		IDMS_Profile_update_source__c = iDMS_Profile_update_source__c;
	}
	
	@JsonProperty("IDMS_AdditionalAddress__c")
	public String getIDMS_AdditionalAddress__c() {
		return IDMS_AdditionalAddress__c;
	}

	public void setIDMS_AdditionalAddress__c(String iDMS_AdditionalAddress__c) {
		IDMS_AdditionalAddress__c = iDMS_AdditionalAddress__c;
	}

	@JsonProperty("IDMS_Company_Name__c")
	public String getCompanyName() {
		return IDMS_Company_Name__c;
	}

	public void setCompanyName(String companyName) {
		IDMS_Company_Name__c = companyName;
	}

	@JsonProperty("IDMS_Company_Address1__c")
	public String getCompany_Address1__c() {
		return IDMS_Company_Address1__c;
	}

	public void setCompany_Address1__c(String company_Address1__c) {
		IDMS_Company_Address1__c = company_Address1__c;
	}

	@JsonProperty("IDMS_Company_City__c")
	public String getCompany_City__c() {
		return IDMS_Company_City__c;
	}

	public void setCompany_City__c(String company_City__c) {
		IDMS_Company_City__c = company_City__c;
	}

	@JsonProperty("IDMS_Company_Postal_Code__c")
	public String getCompany_Postal_Code__c() {
		return IDMS_Company_Postal_Code__c;
	}

	public void setCompany_Postal_Code__c(String company_Postal_Code__c) {
		IDMS_Company_Postal_Code__c = company_Postal_Code__c;
	}

	@JsonProperty("IDMS_Company_State__c")
	public String getCompany_State__c() {
		return IDMS_Company_State__c;
	}

	public void setCompany_State__c(String company_State__c) {
		IDMS_Company_State__c = company_State__c;
	}

	
	@JsonProperty("IDMS_Company_Address_PO_BOX__c")
	public String getIDMS_Company_Address_PO_BOX__c() {
		return IDMS_Company_Address_PO_BOX__c;
	}

	@JsonProperty("IDMS_Company_Address_PO_BOX__c")
	public void setIDMS_Company_Address_PO_BOX__c(String iDMS_Company_Address_PO_BOX__c) {
		IDMS_Company_Address_PO_BOX__c = iDMS_Company_Address_PO_BOX__c;
	}

	@JsonProperty("IDMS_Company_Country__c")
	public String getCompany_Country__c() {
		return IDMS_Company_Country__c;
	}

	public void setCompany_Country__c(String company_Country__c) {
		IDMS_Company_Country__c = company_Country__c;
	}

	@JsonProperty("IDMS_Company_Address2__c")
	public String getCompany_Address2__c() {
		return IDMS_Company_Address2__c;
	}

	public void setCompany_Address2__c(String company_Address2__c) {
		IDMS_Company_Address2__c = company_Address2__c;
	}

	@JsonProperty("IDMS_Company_I_am_a__c")
	public String getIDMSClassLevel1__c() {
		return IDMS_Company_I_am_a__c;
	}

	public void setIDMSClassLevel1__c(String iDMSClassLevel1__c) {
		IDMS_Company_I_am_a__c = iDMSClassLevel1__c;
	}

	@JsonProperty("IDMS_Company_My_Industry_is_a__c")
	public String getIDMSClassLevel2__c() {
		return IDMS_Company_My_Industry_is_a__c;
	}

	public void setIDMSClassLevel2__c(String iDMSClassLevel2__c) {
		IDMS_Company_My_Industry_is_a__c = iDMSClassLevel2__c;
	}

	@JsonProperty("IDMSMarketSegment__c")
	public String getIDMSMarketSegment__c() {
		return IDMSMarketSegment__c;
	}

	public void setIDMSMarketSegment__c(String iDMSMarketSegment__c) {
		IDMSMarketSegment__c = iDMSMarketSegment__c;
	}

	@JsonProperty("IDMSMarketSubSegment__c")
	public String getIDMSMarketSubSegment__c() {
		return IDMSMarketSubSegment__c;
	}

	public void setIDMSMarketSubSegment__c(String iDMSMarketSubSegment__c) {
		IDMSMarketSubSegment__c = iDMSMarketSubSegment__c;
	}
	
	@JsonProperty("Job_Title__c")
	public String getJob_Title__c() {
		return Job_Title__c;
	}

	public void setJob_Title__c(String job_Title__c) {
		Job_Title__c = job_Title__c;
	}

	@JsonProperty("Job_Function__c")
	public String getJob_Function__c() {
		return Job_Function__c;
	}

	public void setJob_Function__c(String job_Function__c) {
		Job_Function__c = job_Function__c;
	}

	@JsonProperty("IDMSJobDescription__c")
	public String getIDMSJobDescription__c() {
		return IDMSJobDescription__c;
	}

	public void setIDMSJobDescription__c(String iDMSJobDescription__c) {
		IDMSJobDescription__c = iDMSJobDescription__c;
	}

	@JsonProperty("IDMS_Company_Industries_I_serve__c")
	public String getIDMSCompanyMarketServed__c() {
		return IDMS_Company_Industries_I_serve__c;
	}

	public void setIDMSCompanyMarketServed__c(String iDMSCompanyMarketServed__c) {
		IDMS_Company_Industries_I_serve__c = iDMSCompanyMarketServed__c;
	}
	
	@JsonProperty("IDMS_Company_Employee_Size__c")
	public String getIDMSCompanyNbrEmployees__c() {
		return IDMS_Company_Employee_Size__c;
	}

	public void setIDMSCompanyNbrEmployees__c(String iDMSCompanyNbrEmployees__c) {
		IDMS_Company_Employee_Size__c = iDMSCompanyNbrEmployees__c;
	}

	@JsonProperty("IDMS_Company_Headquarters__c")
	public String getIDMSCompanyHeadquarters__c() {
		return IDMS_Company_Headquarters__c;
	}

	public void setIDMSCompanyHeadquarters__c(String iDMSCompanyHeadquarters__c) {
		IDMS_Company_Headquarters__c = iDMSCompanyHeadquarters__c;
	}

	@JsonProperty("IDMSAnnualRevenue__c")
	public String getIDMSAnnualRevenue__c() {
		return IDMSAnnualRevenue__c;
	}

	public void setIDMSAnnualRevenue__c(String iDMSAnnualRevenue__c) {
		IDMSAnnualRevenue__c = iDMSAnnualRevenue__c;
	}

	@JsonProperty("IDMS_Company_Tax_Identification_Number__c")
	public String getIDMSTaxIdentificationNumber__c() {
		return IDMS_Company_Tax_Identification_Number__c;
	}

	public void setIDMSTaxIdentificationNumber__c(String iDMSTaxIdentificationNumber__c) {
		IDMS_Company_Tax_Identification_Number__c = iDMSTaxIdentificationNumber__c;
	}

	@JsonProperty("IDMSMiddleName__c")
	public String getIDMSMiddleName__c() {
		return IDMSMiddleName__c;
	}

	public void setIDMSMiddleName__c(String iDMSMiddleName__c) {
		IDMSMiddleName__c = iDMSMiddleName__c;
	}

	@JsonProperty("IDMS_Company_Website__c")
	public String getCompany_Website__c() {
		return IDMS_Company_Website__c;
	}

	public void setCompany_Website__c(String company_Website__c) {
		IDMS_Company_Website__c = company_Website__c;
	}

	@JsonProperty("IDMSSalutation__c")
	public String getIDMSSalutation__c() {
		return IDMSSalutation__c;
	}

	public void setIDMSSalutation__c(String iDMSSalutation__c) {
		IDMSSalutation__c = iDMSSalutation__c;
	}

	@JsonProperty("Department")
	public String getDepartment() {
		return Department;
	}

	public void setDepartment(String department) {
		Department = department;
	}

	@JsonProperty("IDMSSuffix__c")
	public String getIDMSSuffix__c() {
		return IDMSSuffix__c;
	}

	public void setIDMSSuffix__c(String iDMSSuffix__c) {
		IDMSSuffix__c = iDMSSuffix__c;
	}

	@JsonProperty("Fax")
	public String getFax() {
		return Fax;
	}

	public void setFax(String fax) {
		Fax = fax;
	}

	@JsonProperty("IDMSDelegatedIdp__c")
	public String getIDMSDelegatedIdp__c() {
		return IDMSDelegatedIdp__c;
	}

	public void setIDMSDelegatedIdp__c(String iDMSDelegatedIdp__c) {
		IDMSDelegatedIdp__c = iDMSDelegatedIdp__c;
	}

	@JsonProperty("IDMSIdentityType__c")
	public String getIDMSIdentityType__c() {
		return IDMSIdentityType__c;
	}

	public void setIDMSIdentityType__c(String iDMSIdentityType__c) {
		IDMSIdentityType__c = iDMSIdentityType__c;
	}

	@JsonProperty("IDMS_Company_County__c")
	public String getIDMSCompanyCounty__c() {
		return IDMS_Company_County__c;
	}

	public void setIDMSCompanyCounty__c(String iDMSCompanyCounty__c) {
		IDMS_Company_County__c = iDMSCompanyCounty__c;
	}

	public String getTncFlag() {
		return tncFlag;
	}

	public void setTncFlag(String tncFlag) {
		this.tncFlag = tncFlag;
	}

	@JsonProperty("Username")
	public String getUsername() {
		return Username;
	}

	@JsonProperty("Username")
	public void setUsername(String username) {
		Username = username;
	}

	@JsonProperty("ContactId")
	public String getContactId() {
		return ContactId;
	}

	@JsonProperty("ContactId")
	public void setContactId(String contactId) {
		ContactId = contactId;
	}

	@JsonProperty("AccountId")
	public String getAccountId() {
		return AccountId;
	}

	@JsonProperty("AccountId")
	public void setAccountId(String accountId) {
		AccountId = accountId;
	}

	@JsonProperty("IDMS_federatedidentity__c")
	public String getIDMS_federatedidentity__c() {
		return IDMS_federatedidentity__c;
	}

	@JsonProperty("IDMS_federatedidentity__c")
	public void setIDMS_federatedidentity__c(String iDMS_federatedidentity__c) {
		IDMS_federatedidentity__c = iDMS_federatedidentity__c;
	}

	@JsonProperty("IDMSisInternal__c")
	public String getIDMSisInternal__c() {
		return IDMSisInternal__c;
	}

	@JsonProperty("IDMSisInternal__c")
	public void setIDMSisInternal__c(String iDMSisInternal__c) {
		IDMSisInternal__c = iDMSisInternal__c;
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

	@JsonProperty("IDMSPrimaryContact__c")
	public String getIDMSPrimaryContact__c() {
		return IDMSPrimaryContact__c;
	}

	@JsonProperty("IDMSPrimaryContact__c")
	public void setIDMSPrimaryContact__c(String iDMSPrimaryContact__c) {
		IDMSPrimaryContact__c = iDMSPrimaryContact__c;
	}

	@JsonProperty("companyFederatedID")
	public String getCompanyFederatedID() {
		return companyFederatedID;
	}
	
	@JsonProperty("companyFederatedID")
	public void setCompanyFederatedID(String companyFederatedID) {
		this.companyFederatedID = companyFederatedID;
	}

	@JsonProperty("AboutMe")
	public String getAboutMe() {
		return AboutMe;
	}

	@JsonProperty("AboutMe")
	public void setAboutMe(String aboutMe) {
		AboutMe = aboutMe;
	}

	@JsonProperty("IDMSTaxIdentificationNumber__c")
	public String getIDMSTaxIdentificationNumber_c() {
		return IDMSTaxIdentificationNumber_c;
	}

	@JsonProperty("IDMSTaxIdentificationNumber__c")
	public void setIDMSTaxIdentificationNumber_c(String iDMSTaxIdentificationNumber_c) {
		IDMSTaxIdentificationNumber_c = iDMSTaxIdentificationNumber_c;
	}

	@JsonProperty("IDMSClassLevel2__c")
	public String getIDMSClassLevel2_c() {
		return IDMSClassLevel2_c;
	}

	@JsonProperty("IDMSClassLevel2__c")
	public void setIDMSClassLevel2_c(String iDMSClassLevel2_c) {
		IDMSClassLevel2_c = iDMSClassLevel2_c;
	}

	@JsonProperty("trustedAdmin")
	public String getTrustedAdmin() {
		return trustedAdmin;
	}

	@JsonProperty("trustedAdmin")
	public void setTrustedAdmin(String trustedAdmin) {
		this.trustedAdmin = trustedAdmin;
	}

	@JsonProperty("isActivated")
	public String getIsActivated() {
		return isActivated;
	}

	@JsonProperty("isActivated")
	public void setIsActivated(String isActivated) {
		this.isActivated = isActivated;
	}

	@JsonProperty("currencyCode")
	public String getCurrencyCode() {
		return currencyCode;
	}

	@JsonProperty("currencyCode")
	public void setCurrencyCode(String currencyCode) {
		this.currencyCode = currencyCode;
	}
	
	
}

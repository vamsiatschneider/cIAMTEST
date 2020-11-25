package com.idms.model;

import java.io.Serializable;

import org.springframework.stereotype.Component;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.idms.product.model.Attributes;

/*
 * @author:Subbarao Maniam(SESA468450)
 */

@Component
public class IDMSUserResponse implements Serializable {

	private static final long serialVersionUID = 1L;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private Attributes attributes;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String id;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMS_Federated_ID__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMS_User_Context__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String firstName;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String lastName;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String country;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String email;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String mobilePhone;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMS_PreferredLanguage__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMS_Email_opt_in__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String defaultCurrencyIsoCode;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String companyName;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String company_Address1__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String company_City__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String company_Postal_Code__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String company_State__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMSCompanyPoBox__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMSCompanyCounty__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String company_Country__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String company_Address2__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String company_Website__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMSClassLevel1__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMSClassLevel2__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMSMarketSegment__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMSMarketSubSegment__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMSCompanyMarketServed__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMSCompanyNbrEmployees__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String department;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMSCompanyHeadquarters__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMSAnnualRevenue__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMSTaxIdentificationNumber__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String job_Title__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String job_Function__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMSJobDescription__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String phone;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String street;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String city;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String postalCode;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String state;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMS_County__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMS_POBox__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMS_AdditionalAddress__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMSMiddleName__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMSSalutation__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMSSuffix__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String fax;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String accountId;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMS_Registration_Source__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMS_Profile_update_source__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMS_TrustStatus__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMS_RejectionReason__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMS_RejectionComments__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private boolean isActive;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String bFO_ACCOUNT_ID__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMSCompanyFederationIdentifier__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMSDelegatedIdp__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMSIdentityType__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String tncFlag;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMSPrimaryContact__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String companyFederatedId;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String adminFederatedId;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String adminCompanyFederatedId;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String aboutMe;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String federationIdentifier;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMS_Company_Name__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMS_Company_Address1__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMS_Company_City__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMS_Company_Postal_Code__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMS_Company_State__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMS_Company_Address_PO_BOX__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMS_Company_County__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMS_Company_Country__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMS_Company_Address2__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMS_Company_Website__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMS_Company_I_am_a__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMS_Company_My_Industry_is_a__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMS_Company_Industries_I_serve__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMS_Company_Employee_Size__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String userBusinessUnit__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMS_Company_Headquarters__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMS_Company_Tax_Identification_Number__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMS_TrustLevel__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMSIsInternal__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMSAil__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMSAIL_Applications__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMSAIL_Features__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMSAIL_Programs__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String title;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String division;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String iDMS_Company_Currency_ISO_Code__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String userStatus__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String socialProviders__c;

	@JsonProperty
	@JsonInclude(Include.NON_NULL)
	private String trustedAdmin;

	@JsonProperty
	private String Channel__c;

	@JsonProperty
	private String SubChannel__c;

	@JsonProperty
	private String contactId;
	
	@JsonProperty
	private String IDMSWorkPhone__c;
	
	/*@JsonProperty
	private List<RegistrationAttributes> attributes;*/

	/*
	 * @JsonProperty("attributes") public Attributes getAttributes() { return
	 * attributes; }
	 */

	@JsonProperty("attributes")
	public void setAttributes(Attributes attributes) {
		this.attributes = attributes;
	}

	@JsonProperty("Id")
	public String getId() {
		return id;
	}

	@JsonProperty("Id")
	public void setId(String id) {
		this.id = id;
	}

	@JsonProperty("IDMS_Federated_ID__c")
	public String getiDMS_Federated_ID__c() {
		return iDMS_Federated_ID__c;
	}

	@JsonProperty("IDMS_Federated_ID__c")
	public void setiDMS_Federated_ID__c(String iDMS_Federated_ID__c) {
		this.iDMS_Federated_ID__c = iDMS_Federated_ID__c;
	}

	@JsonProperty("IDMS_User_Context__c")
	public String getiDMS_User_Context__c() {
		return iDMS_User_Context__c;
	}

	@JsonProperty("IDMS_User_Context__c")
	public void setiDMS_User_Context__c(String iDMS_User_Context__c) {
		this.iDMS_User_Context__c = iDMS_User_Context__c;
	}

	@JsonProperty("FirstName")
	public String getFirstName() {
		return firstName;
	}

	@JsonProperty("FirstName")
	public void setFirstName(String firstName) {
		this.firstName = firstName;
	}

	@JsonProperty("LastName")
	public String getLastName() {
		return lastName;
	}

	@JsonProperty("LastName")
	public void setLastName(String lastName) {
		this.lastName = lastName;
	}

	@JsonProperty("Country")
	public String getCountry() {
		return country;
	}

	@JsonProperty("Country")
	public void setCountry(String country) {
		this.country = country;
	}

	@JsonProperty("Email")
	public String getEmail() {
		return email;
	}

	@JsonProperty("Email")
	public void setEmail(String email) {
		this.email = email;
	}

	@JsonProperty("MobilePhone")
	public String getMobilePhone() {
		return mobilePhone;
	}

	@JsonProperty("MobilePhone")
	public void setMobilePhone(String mobilePhone) {
		this.mobilePhone = mobilePhone;
	}

	@JsonProperty("IDMS_PreferredLanguage__c")
	public String getiDMS_PreferredLanguage__c() {
		return iDMS_PreferredLanguage__c;
	}

	@JsonProperty("IDMS_PreferredLanguage__c")
	public void setiDMS_PreferredLanguage__c(String iDMS_PreferredLanguage__c) {
		this.iDMS_PreferredLanguage__c = iDMS_PreferredLanguage__c;
	}

	@JsonProperty("IDMS_Email_opt_in__c")
	public String getiDMS_Email_opt_in__c() {
		return iDMS_Email_opt_in__c;
	}

	@JsonProperty("IDMS_Email_opt_in__c")
	public void setiDMS_Email_opt_in__c(String iDMS_Email_opt_in__c) {
		this.iDMS_Email_opt_in__c = iDMS_Email_opt_in__c;
	}

	@JsonProperty("DefaultCurrencyIsoCode")
	public String getDefaultCurrencyIsoCode() {
		return defaultCurrencyIsoCode;
	}

	@JsonProperty("DefaultCurrencyIsoCode")
	public void setDefaultCurrencyIsoCode(String defaultCurrencyIsoCode) {
		this.defaultCurrencyIsoCode = defaultCurrencyIsoCode;
	}

	@JsonProperty("CompanyName")
	public String getCompanyName() {
		return companyName;
	}

	@JsonProperty("CompanyName")
	public void setCompanyName(String companyName) {
		this.companyName = companyName;
	}

	@JsonProperty("Company_Address1__c")
	public String getCompany_Address1__c() {
		return company_Address1__c;
	}

	@JsonProperty("Company_Address1__c")
	public void setCompany_Address1__c(String company_Address1__c) {
		this.company_Address1__c = company_Address1__c;
	}

	@JsonProperty("Company_City__c")
	public String getCompany_City__c() {
		return company_City__c;
	}

	@JsonProperty("Company_City__c")
	public void setCompany_City__c(String company_City__c) {
		this.company_City__c = company_City__c;
	}

	@JsonProperty("Company_Postal_Code__c")
	public String getCompany_Postal_Code__c() {
		return company_Postal_Code__c;
	}

	@JsonProperty("Company_Postal_Code__c")
	public void setCompany_Postal_Code__c(String company_Postal_Code__c) {
		this.company_Postal_Code__c = company_Postal_Code__c;
	}

	@JsonProperty("Company_State__c")
	public String getCompany_State__c() {
		return company_State__c;
	}

	@JsonProperty("Company_State__c")
	public void setCompany_State__c(String company_State__c) {
		this.company_State__c = company_State__c;
	}

	@JsonProperty("IDMSCompanyPoBox__c")
	public String getiDMSCompanyPoBox__c() {
		return iDMSCompanyPoBox__c;
	}

	@JsonProperty("IDMSCompanyPoBox__c")
	public void setiDMSCompanyPoBox__c(String iDMSCompanyPoBox__c) {
		this.iDMSCompanyPoBox__c = iDMSCompanyPoBox__c;
	}

	@JsonProperty("IDMSCompanyCounty__c")
	public String getiDMSCompanyCounty__c() {
		return iDMSCompanyCounty__c;
	}

	@JsonProperty("IDMSCompanyCounty__c")
	public void setiDMSCompanyCounty__c(String iDMSCompanyCounty__c) {
		this.iDMSCompanyCounty__c = iDMSCompanyCounty__c;
	}

	@JsonProperty("Company_Country__c")
	public String getCompany_Country__c() {
		return company_Country__c;
	}

	@JsonProperty("Company_Country__c")
	public void setCompany_Country__c(String company_Country__c) {
		this.company_Country__c = company_Country__c;
	}

	@JsonProperty("Company_Address2__c")
	public String getCompany_Address2__c() {
		return company_Address2__c;
	}

	@JsonProperty("Company_Address2__c")
	public void setCompany_Address2__c(String company_Address2__c) {
		this.company_Address2__c = company_Address2__c;
	}

	@JsonProperty("Company_Website__c")
	public String getCompany_Website__c() {
		return company_Website__c;
	}

	@JsonProperty("Company_Website__c")
	public void setCompany_Website__c(String company_Website__c) {
		this.company_Website__c = company_Website__c;
	}

	@JsonProperty("IDMSClassLevel1__c")
	public String getiDMSClassLevel1__c() {
		return iDMSClassLevel1__c;
	}

	@JsonProperty("IDMSClassLevel1__c")
	public void setiDMSClassLevel1__c(String iDMSClassLevel1__c) {
		this.iDMSClassLevel1__c = iDMSClassLevel1__c;
	}

	@JsonProperty("IDMSClassLevel2__c")
	public String getiDMSClassLevel2__c() {
		return iDMSClassLevel2__c;
	}

	@JsonProperty("IDMSClassLevel2__c")
	public void setiDMSClassLevel2__c(String iDMSClassLevel2__c) {
		this.iDMSClassLevel2__c = iDMSClassLevel2__c;
	}

	@JsonProperty("IDMSMarketSegment__c")
	public String getiDMSMarketSegment__c() {
		return iDMSMarketSegment__c;
	}

	@JsonProperty("IDMSMarketSegment__c")
	public void setiDMSMarketSegment__c(String iDMSMarketSegment__c) {
		this.iDMSMarketSegment__c = iDMSMarketSegment__c;
	}

	@JsonProperty("IDMSMarketSubSegment__c")
	public String getiDMSMarketSubSegment__c() {
		return iDMSMarketSubSegment__c;
	}

	@JsonProperty("IDMSMarketSubSegment__c")
	public void setiDMSMarketSubSegment__c(String iDMSMarketSubSegment__c) {
		this.iDMSMarketSubSegment__c = iDMSMarketSubSegment__c;
	}

	@JsonProperty("IDMSCompanyMarketServed__c")
	public String getiDMSCompanyMarketServed__c() {
		return iDMSCompanyMarketServed__c;
	}

	@JsonProperty("IDMSCompanyMarketServed__c")
	public void setiDMSCompanyMarketServed__c(String iDMSCompanyMarketServed__c) {
		this.iDMSCompanyMarketServed__c = iDMSCompanyMarketServed__c;
	}

	@JsonProperty("IDMSCompanyNbrEmployees__c")
	public String getiDMSCompanyNbrEmployees__c() {
		return iDMSCompanyNbrEmployees__c;
	}

	@JsonProperty("IDMSCompanyNbrEmployees__c")
	public void setiDMSCompanyNbrEmployees__c(String iDMSCompanyNbrEmployees__c) {
		this.iDMSCompanyNbrEmployees__c = iDMSCompanyNbrEmployees__c;
	}

	@JsonProperty("Department")
	public String getDepartment() {
		return department;
	}

	@JsonProperty("Department")
	public void setDepartment(String department) {
		this.department = department;
	}

	@JsonProperty("IDMSCompanyHeadquarters__c")
	public String getiDMSCompanyHeadquarters__c() {
		return iDMSCompanyHeadquarters__c;
	}

	@JsonProperty("IDMSCompanyHeadquarters__c")
	public void setiDMSCompanyHeadquarters__c(String iDMSCompanyHeadquarters__c) {
		this.iDMSCompanyHeadquarters__c = iDMSCompanyHeadquarters__c;
	}

	@JsonProperty("IDMSAnnualRevenue__c")
	public String getiDMSAnnualRevenue__c() {
		return iDMSAnnualRevenue__c;
	}

	@JsonProperty("IDMSAnnualRevenue__c")
	public void setiDMSAnnualRevenue__c(String iDMSAnnualRevenue__c) {
		this.iDMSAnnualRevenue__c = iDMSAnnualRevenue__c;
	}

	@JsonProperty("IDMSTaxIdentificationNumber__c")
	public String getiDMSTaxIdentificationNumber__c() {
		return iDMSTaxIdentificationNumber__c;
	}

	@JsonProperty("IDMSTaxIdentificationNumber__c")
	public void setiDMSTaxIdentificationNumber__c(String iDMSTaxIdentificationNumber__c) {
		this.iDMSTaxIdentificationNumber__c = iDMSTaxIdentificationNumber__c;
	}

	@JsonProperty("Job_Title__c")
	public String getJob_Title__c() {
		return job_Title__c;
	}

	@JsonProperty("Job_Title__c")
	public void setJob_Title__c(String job_Title__c) {
		this.job_Title__c = job_Title__c;
	}

	@JsonProperty("Job_Function__c")
	public String getJob_Function__c() {
		return job_Function__c;
	}

	@JsonProperty("Job_Function__c")
	public void setJob_Function__c(String job_Function__c) {
		this.job_Function__c = job_Function__c;
	}

	@JsonProperty("IDMSJobDescription__c")
	public String getiDMSJobDescription__c() {
		return iDMSJobDescription__c;
	}

	@JsonProperty("IDMSJobDescription__c")
	public void setiDMSJobDescription__c(String iDMSJobDescription__c) {
		this.iDMSJobDescription__c = iDMSJobDescription__c;
	}

	@JsonProperty("Phone")
	public String getPhone() {
		return phone;
	}

	@JsonProperty("Phone")
	public void setPhone(String phone) {
		this.phone = phone;
	}

	@JsonProperty("Street")
	public String getStreet() {
		return street;
	}

	@JsonProperty("Street")
	public void setStreet(String street) {
		this.street = street;
	}

	@JsonProperty("City")
	public String getCity() {
		return city;
	}

	@JsonProperty("City")
	public void setCity(String city) {
		this.city = city;
	}

	@JsonProperty("PostalCode")
	public String getPostalCode() {
		return postalCode;
	}

	@JsonProperty("PostalCode")
	public void setPostalCode(String postalCode) {
		this.postalCode = postalCode;
	}

	@JsonProperty("State")
	public String getState() {
		return state;
	}

	@JsonProperty("State")
	public void setState(String state) {
		this.state = state;
	}

	@JsonProperty("IDMS_County__c")
	public String getiDMS_County__c() {
		return iDMS_County__c;
	}

	@JsonProperty("IDMS_County__c")
	public void setiDMS_County__c(String iDMS_County__c) {
		this.iDMS_County__c = iDMS_County__c;
	}

	@JsonProperty("IDMS_POBox__c")
	public String getiDMS_POBox__c() {
		return iDMS_POBox__c;
	}

	@JsonProperty("IDMS_POBox__c")
	public void setiDMS_POBox__c(String iDMS_POBox__c) {
		this.iDMS_POBox__c = iDMS_POBox__c;
	}

	@JsonProperty("IDMS_AdditionalAddress__c")
	public String getiDMS_AdditionalAddress__c() {
		return iDMS_AdditionalAddress__c;
	}

	@JsonProperty("IDMS_AdditionalAddress__c")
	public void setiDMS_AdditionalAddress__c(String iDMS_AdditionalAddress__c) {
		this.iDMS_AdditionalAddress__c = iDMS_AdditionalAddress__c;
	}

	@JsonProperty("IDMSMiddleName__c")
	public String getiDMSMiddleName__c() {
		return iDMSMiddleName__c;
	}

	@JsonProperty("IDMSMiddleName__c")
	public void setiDMSMiddleName__c(String iDMSMiddleName__c) {
		this.iDMSMiddleName__c = iDMSMiddleName__c;
	}

	@JsonProperty("IDMSSalutation__c")
	public String getiDMSSalutation__c() {
		return iDMSSalutation__c;
	}

	@JsonProperty("IDMSSalutation__c")
	public void setiDMSSalutation__c(String iDMSSalutation__c) {
		this.iDMSSalutation__c = iDMSSalutation__c;
	}

	@JsonProperty("IDMSSuffix__c")
	public String getiDMSSuffix__c() {
		return iDMSSuffix__c;
	}

	@JsonProperty("IDMSSuffix__c")
	public void setiDMSSuffix__c(String iDMSSuffix__c) {
		this.iDMSSuffix__c = iDMSSuffix__c;
	}

	@JsonProperty("Fax")
	public String getFax() {
		return fax;
	}

	@JsonProperty("Fax")
	public void setFax(String fax) {
		this.fax = fax;
	}

	@JsonProperty("AccountId")
	public String getAccountId() {
		return accountId;
	}

	@JsonProperty("AccountId")
	public void setAccountId(String accountId) {
		this.accountId = accountId;
	}

	@JsonProperty("IDMS_Registration_Source__c")
	public String getiDMS_Registration_Source__c() {
		return iDMS_Registration_Source__c;
	}

	@JsonProperty("IDMS_Registration_Source__c")
	public void setiDMS_Registration_Source__c(String iDMS_Registration_Source__c) {
		this.iDMS_Registration_Source__c = iDMS_Registration_Source__c;
	}

	@JsonProperty("IDMS_Profile_update_source__c")
	public String getiDMS_Profile_update_source__c() {
		return iDMS_Profile_update_source__c;
	}

	@JsonProperty("IDMS_Profile_update_source__c")
	public void setiDMS_Profile_update_source__c(String iDMS_Profile_update_source__c) {
		this.iDMS_Profile_update_source__c = iDMS_Profile_update_source__c;
	}

	@JsonProperty("IDMS_TrustStatus__c")
	public String getiDMS_TrustStatus__c() {
		return iDMS_TrustStatus__c;
	}

	@JsonProperty("IDMS_TrustStatus__c")
	public void setiDMS_TrustStatus__c(String iDMS_TrustStatus__c) {
		this.iDMS_TrustStatus__c = iDMS_TrustStatus__c;
	}

	@JsonProperty("IDMS_RejectionReason__c")
	public String getiDMS_RejectionReason__c() {
		return iDMS_RejectionReason__c;
	}

	@JsonProperty("IDMS_RejectionReason__c")
	public void setiDMS_RejectionReason__c(String iDMS_RejectionReason__c) {
		this.iDMS_RejectionReason__c = iDMS_RejectionReason__c;
	}

	@JsonProperty("IDMS_RejectionComments__c")
	public String getiDMS_RejectionComments__c() {
		return iDMS_RejectionComments__c;
	}

	@JsonProperty("IDMS_RejectionComments__c")
	public void setiDMS_RejectionComments__c(String iDMS_RejectionComments__c) {
		this.iDMS_RejectionComments__c = iDMS_RejectionComments__c;
	}

	@JsonProperty("isActive")
	public boolean isActive() {
		return isActive;
	}

	@JsonProperty("isActive")
	public void setActive(boolean isActive) {
		this.isActive = isActive;
	}

	@JsonProperty("BFO_ACCOUNT_ID__c")
	public String getbFO_ACCOUNT_ID__c() {
		return bFO_ACCOUNT_ID__c;
	}

	@JsonProperty("BFO_ACCOUNT_ID__c")
	public void setbFO_ACCOUNT_ID__c(String bFO_ACCOUNT_ID__c) {
		this.bFO_ACCOUNT_ID__c = bFO_ACCOUNT_ID__c;
	}

	@JsonProperty("IDMSCompanyFederationIdentifier__c")
	public String getiDMSCompanyFederationIdentifier__c() {
		return iDMSCompanyFederationIdentifier__c;
	}

	@JsonProperty("IDMSCompanyFederationIdentifier__c")
	public void setiDMSCompanyFederationIdentifier__c(String iDMSCompanyFederationIdentifier__c) {
		this.iDMSCompanyFederationIdentifier__c = iDMSCompanyFederationIdentifier__c;
	}

	@JsonProperty("IDMSDelegatedIdp__c")
	public String getiDMSDelegatedIdp__c() {
		return iDMSDelegatedIdp__c;
	}

	@JsonProperty("IDMSDelegatedIdp__c")
	public void setiDMSDelegatedIdp__c(String iDMSDelegatedIdp__c) {
		this.iDMSDelegatedIdp__c = iDMSDelegatedIdp__c;
	}

	@JsonProperty("IDMSIdentityType__c")
	public String getiDMSIdentityType__c() {
		return iDMSIdentityType__c;
	}

	@JsonProperty("IDMSIdentityType__c")
	public void setiDMSIdentityType__c(String iDMSIdentityType__c) {
		this.iDMSIdentityType__c = iDMSIdentityType__c;
	}

	@JsonProperty("tncFlag")
	public String getTncFlag() {
		return tncFlag;
	}

	@JsonProperty("tncFlag")
	public void setTncFlag(String tncFlag) {
		this.tncFlag = tncFlag;
	}

	@JsonProperty("IDMSPrimaryContact__c")
	public String getiDMSPrimaryContact__c() {
		return iDMSPrimaryContact__c;
	}

	@JsonProperty("IDMSPrimaryContact__c")
	public void setiDMSPrimaryContact__c(String iDMSPrimaryContact__c) {
		this.iDMSPrimaryContact__c = iDMSPrimaryContact__c;
	}

	@JsonProperty("companyFederatedId")
	public String getCompanyFederatedId() {
		return companyFederatedId;
	}

	@JsonProperty("companyFederatedId")
	public void setCompanyFederatedId(String companyFederatedId) {
		this.companyFederatedId = companyFederatedId;
	}

	@JsonProperty("adminFederatedId")
	public String getAdminFederatedId() {
		return adminFederatedId;
	}

	@JsonProperty("adminFederatedId")
	public void setAdminFederatedId(String adminFederatedId) {
		this.adminFederatedId = adminFederatedId;
	}

	@JsonProperty("adminCompanyFederatedId")
	public String getAdminCompanyFederatedId() {
		return adminCompanyFederatedId;
	}

	@JsonProperty("adminCompanyFederatedId")
	public void setAdminCompanyFederatedId(String adminCompanyFederatedId) {
		this.adminCompanyFederatedId = adminCompanyFederatedId;
	}

	@JsonProperty("AboutMe")
	public String getAboutMe() {
		return aboutMe;
	}

	@JsonProperty("AboutMe")
	public void setAboutMe(String aboutMe) {
		this.aboutMe = aboutMe;
	}

	////

	@JsonProperty("FederationIdentifier")
	public String getFederationIdentifier() {
		return federationIdentifier;
	}

	@JsonProperty("FederationIdentifier")
	public void setFederationIdentifier(String federationIdentifier) {
		this.federationIdentifier = federationIdentifier;
	}

	@JsonProperty("IDMS_Company_Name__c")
	public String getiDMS_Company_Name__c() {
		return iDMS_Company_Name__c;
	}

	@JsonProperty("IDMS_Company_Name__c")
	public void setiDMS_Company_Name__c(String iDMS_Company_Name__c) {
		this.iDMS_Company_Name__c = iDMS_Company_Name__c;
	}

	@JsonProperty("IDMS_Company_Address1__c")
	public String getiDMS_Company_Address1__c() {
		return iDMS_Company_Address1__c;
	}

	@JsonProperty("IDMS_Company_Address1__c")
	public void setiDMS_Company_Address1__c(String iDMS_Company_Address1__c) {
		this.iDMS_Company_Address1__c = iDMS_Company_Address1__c;
	}

	@JsonProperty("IDMS_Company_City__c")
	public String getiDMS_Company_City__c() {
		return iDMS_Company_City__c;
	}

	@JsonProperty("IDMS_Company_City__c")
	public void setiDMS_Company_City__c(String iDMS_Company_City__c) {
		this.iDMS_Company_City__c = iDMS_Company_City__c;
	}

	@JsonProperty("IDMS_Company_Postal_Code__c")
	public String getiDMS_Company_Postal_Code__c() {
		return iDMS_Company_Postal_Code__c;
	}

	@JsonProperty("IDMS_Company_Postal_Code__c")
	public void setiDMS_Company_Postal_Code__c(String iDMS_Company_Postal_Code__c) {
		this.iDMS_Company_Postal_Code__c = iDMS_Company_Postal_Code__c;
	}

	@JsonProperty("IDMS_Company_State__c")
	public String getiDMS_Company_State__c() {
		return iDMS_Company_State__c;
	}

	@JsonProperty("IDMS_Company_State__c")
	public void setiDMS_Company_State__c(String iDMS_Company_State__c) {
		this.iDMS_Company_State__c = iDMS_Company_State__c;
	}

	@JsonProperty("IDMS_Company_Address_PO_BOX__c")
	public String getiDMS_Company_Address_PO_BOX__c() {
		return iDMS_Company_Address_PO_BOX__c;
	}

	@JsonProperty("IDMS_Company_Address_PO_BOX__c")
	public void setiDMS_Company_Address_PO_BOX__c(String iDMS_Company_Address_PO_BOX__c) {
		this.iDMS_Company_Address_PO_BOX__c = iDMS_Company_Address_PO_BOX__c;
	}

	@JsonProperty("IDMS_Company_County__c")
	public String getiDMS_Company_County__c() {
		return iDMS_Company_County__c;
	}

	@JsonProperty("IDMS_Company_County__c")
	public void setiDMS_Company_County__c(String iDMS_Company_County__c) {
		this.iDMS_Company_County__c = iDMS_Company_County__c;
	}

	@JsonProperty("IDMS_Company_Country__c")
	public String getiDMS_Company_Country__c() {
		return iDMS_Company_Country__c;
	}

	@JsonProperty("IDMS_Company_Country__c")
	public void setiDMS_Company_Country__c(String iDMS_Company_Country__c) {
		this.iDMS_Company_Country__c = iDMS_Company_Country__c;
	}

	@JsonProperty("IDMS_Company_Address2__c")
	public String getiDMS_Company_Address2__c() {
		return iDMS_Company_Address2__c;
	}

	@JsonProperty("IDMS_Company_Address2__c")
	public void setiDMS_Company_Address2__c(String iDMS_Company_Address2__c) {
		this.iDMS_Company_Address2__c = iDMS_Company_Address2__c;
	}

	@JsonProperty("IDMS_Company_Website__c")
	public String getiDMS_Company_Website__c() {
		return iDMS_Company_Website__c;
	}

	@JsonProperty("IDMS_Company_Website__c")
	public void setiDMS_Company_Website__c(String iDMS_Company_Website__c) {
		this.iDMS_Company_Website__c = iDMS_Company_Website__c;
	}

	@JsonProperty("IDMS_Company_I_am_a__c")
	public String getiDMS_Company_I_am_a__c() {
		return iDMS_Company_I_am_a__c;
	}

	@JsonProperty("IDMS_Company_I_am_a__c")
	public void setiDMS_Company_I_am_a__c(String iDMS_Company_I_am_a__c) {
		this.iDMS_Company_I_am_a__c = iDMS_Company_I_am_a__c;
	}

	@JsonProperty("IDMS_Company_My_Industry_is_a__c")
	public String getiDMS_Company_My_Industry_is_a__c() {
		return iDMS_Company_My_Industry_is_a__c;
	}

	@JsonProperty("IDMS_Company_My_Industry_is_a__c")
	public void setiDMS_Company_My_Industry_is_a__c(String iDMS_Company_My_Industry_is_a__c) {
		this.iDMS_Company_My_Industry_is_a__c = iDMS_Company_My_Industry_is_a__c;
	}

	@JsonProperty("IDMS_Company_Industries_I_serve__c")
	public String getiDMS_Company_Industries_I_serve__c() {
		return iDMS_Company_Industries_I_serve__c;
	}

	@JsonProperty("IDMS_Company_Industries_I_serve__c")
	public void setiDMS_Company_Industries_I_serve__c(String iDMS_Company_Industries_I_serve__c) {
		this.iDMS_Company_Industries_I_serve__c = iDMS_Company_Industries_I_serve__c;
	}

	@JsonProperty("IDMS_Company_Employee_Size__c")
	public String getiDMS_Company_Employee_Size__c() {
		return iDMS_Company_Employee_Size__c;
	}

	@JsonProperty("IDMS_Company_Employee_Size__c")
	public void setiDMS_Company_Employee_Size__c(String iDMS_Company_Employee_Size__c) {
		this.iDMS_Company_Employee_Size__c = iDMS_Company_Employee_Size__c;
	}

	@JsonProperty("UserBusinessUnit__c")
	public String getUserBusinessUnit__c() {
		return userBusinessUnit__c;
	}

	@JsonProperty("UserBusinessUnit__c")
	public void setUserBusinessUnit__c(String userBusinessUnit__c) {
		this.userBusinessUnit__c = userBusinessUnit__c;
	}

	@JsonProperty("IDMS_Company_Headquarters__c")
	public String getiDMS_Company_Headquarters__c() {
		return iDMS_Company_Headquarters__c;
	}

	@JsonProperty("IDMS_Company_Headquarters__c")
	public void setiDMS_Company_Headquarters__c(String iDMS_Company_Headquarters__c) {
		this.iDMS_Company_Headquarters__c = iDMS_Company_Headquarters__c;
	}

	@JsonProperty("IDMS_Company_Tax_Identification_Number__c")
	public String getiDMS_Company_Tax_Identification_Number__c() {
		return iDMS_Company_Tax_Identification_Number__c;
	}

	@JsonProperty("IDMS_Company_Tax_Identification_Number__c")
	public void setiDMS_Company_Tax_Identification_Number__c(String iDMS_Company_Tax_Identification_Number__c) {
		this.iDMS_Company_Tax_Identification_Number__c = iDMS_Company_Tax_Identification_Number__c;
	}

	@JsonProperty("IDMS_TrustLevel__c")
	public String getiDMS_TrustLevel__c() {
		return iDMS_TrustLevel__c;
	}

	@JsonProperty("IDMS_TrustLevel__c")
	public void setiDMS_TrustLevel__c(String iDMS_TrustLevel__c) {
		this.iDMS_TrustLevel__c = iDMS_TrustLevel__c;
	}

	@JsonProperty("IDMSIsInternal__c")
	public String getiDMSIsInternal__c() {
		return iDMSIsInternal__c;
	}

	@JsonProperty("IDMSIsInternal__c")
	public void setiDMSIsInternal__c(String iDMSIsInternal__c) {
		this.iDMSIsInternal__c = iDMSIsInternal__c;
	}

	@JsonProperty("IDMSAil__c")
	public String getiDMSAil__c() {
		return iDMSAil__c;
	}

	@JsonProperty("IDMSAil__c")
	public void setiDMSAil__c(String iDMSAil__c) {
		this.iDMSAil__c = iDMSAil__c;
	}

	@JsonProperty("IDMSAIL_Applications__c")
	public String getiDMSAIL_Applications__c() {
		return iDMSAIL_Applications__c;
	}

	@JsonProperty("IDMSAIL_Applications__c")
	public void setiDMSAIL_Applications__c(String iDMSAIL_Applications__c) {
		this.iDMSAIL_Applications__c = iDMSAIL_Applications__c;
	}

	@JsonProperty("IDMSAIL_Features__c")
	public String getiDMSAIL_Features__c() {
		return iDMSAIL_Features__c;
	}

	@JsonProperty("IDMSAIL_Features__c")
	public void setiDMSAIL_Features__c(String iDMSAIL_Features__c) {
		this.iDMSAIL_Features__c = iDMSAIL_Features__c;
	}

	@JsonProperty("IDMSAIL_Programs__c")
	public String getiDMSAIL_Programs__c() {
		return iDMSAIL_Programs__c;
	}

	@JsonProperty("IDMSAIL_Programs__c")
	public void setiDMSAIL_Programs__c(String iDMSAIL_Programs__c) {
		this.iDMSAIL_Programs__c = iDMSAIL_Programs__c;
	}

	@JsonProperty("Title")
	public String getTitle() {
		return title;
	}

	@JsonProperty("Title")
	public void setTitle(String title) {
		this.title = title;
	}

	@JsonProperty("Division")
	public String getDivision() {
		return division;
	}

	@JsonProperty("Division")
	public void setDivision(String division) {
		this.division = division;
	}

	@JsonProperty("IDMS_Company_Currency_ISO_Code__c")
	public String getiDMS_Company_Currency_ISO_Code__c() {
		return iDMS_Company_Currency_ISO_Code__c;
	}

	@JsonProperty("IDMS_Company_Currency_ISO_Code__c")
	public void setiDMS_Company_Currency_ISO_Code__c(String iDMS_Company_Currency_ISO_Code__c) {
		this.iDMS_Company_Currency_ISO_Code__c = iDMS_Company_Currency_ISO_Code__c;
	}

	@JsonProperty("UserStatus__c")
	public String getUserStatus__c() {
		return userStatus__c;
	}

	@JsonProperty("UserStatus__c")
	public void setUserStatus__c(String userStatus__c) {
		this.userStatus__c = userStatus__c;
	}

	@JsonProperty("SocialProviders__c")
	public String getSocialProviders__c() {
		return socialProviders__c;
	}

	@JsonProperty("SocialProviders__c")
	public void setSocialProviders__c(String socialProviders__c) {
		this.socialProviders__c = socialProviders__c;
	}

	@JsonProperty("trustedAdmin")
	public String getTrustedAdmin() {
		return trustedAdmin;
	}

	@JsonProperty("trustedAdmin")
	public void setTrustedAdmin(String trustedAdmin) {
		this.trustedAdmin = trustedAdmin;
	}

	@JsonProperty("Channel__c")
	public String getChannel__c() {
		return Channel__c;
	}

	@JsonProperty("Channel__c")
	public void setChannel__c(String channel__c) {
		Channel__c = channel__c;
	}

	@JsonProperty("SubChannel__c")
	public String getSubChannel__c() {
		return SubChannel__c;
	}

	@JsonProperty("SubChannel__c")
	public void setSubChannel__c(String subChannel__c) {
		SubChannel__c = subChannel__c;
	}

	@JsonProperty("ContactId")
	public String getContactId() {
		return contactId;
	}

	@JsonProperty("ContactId")
	public void setContactId(String contactId) {
		this.contactId = contactId;
	}

	@JsonProperty("IDMSWorkPhone__c")
	public String getIDMSWorkPhone__c() {
		return IDMSWorkPhone__c;
	}

	@JsonProperty("IDMSWorkPhone__c")
	public void setIDMSWorkPhone__c(String iDMSWorkPhone__c) {
		IDMSWorkPhone__c = iDMSWorkPhone__c;
	}

	
}

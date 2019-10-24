package com.idms.model;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.idms.product.model.Attributes;

public class IFWUser extends BaseEntity {

	/**
	 * Attribute of the IFW which has the UserName for which this user is
	 * created.
	 * 
	 */
	@JsonProperty
	private Attributes attributes;

	@JsonProperty
	private String Id;

	@JsonProperty
	private String IDMS_Federated_ID__c;

	@JsonProperty
	private String IDMS_User_Context__c;

	@JsonProperty
	private String FirstName;

	@JsonProperty
	private String LastName;

	@JsonProperty
	private String Country;

	@JsonProperty
	private String Email;

	@JsonProperty
	private String MobilePhone;

	@JsonProperty
	private String IDMS_PreferredLanguage__c;

	@JsonProperty
	private String IDMS_Email_opt_in__c;

	@JsonProperty
	private String DefaultCurrencyIsoCode;

	@JsonProperty
	private String currencyCode;

	@JsonProperty
	private String CompanyName;

	@JsonProperty
	private String Company_Address1__c;

	@JsonProperty
	private String Company_City__c;

	@JsonProperty
	private String Company_Postal_Code__c;

	@JsonProperty
	private String Company_State__c;

	@JsonProperty
	private String IDMSCompanyPoBox__c;

	@JsonProperty
	private String IDMSCompanyCounty__c;

	@JsonProperty
	private String Company_Country__c;

	@JsonProperty
	private String Company_Address2__c;

	@JsonProperty
	private String Company_Website__c;

	@JsonProperty
	private String IDMSClassLevel1__c;

	@JsonProperty
	private String IDMSClassLevel2__c;

	@JsonProperty
	private String IDMSMarketSegment__c;

	@JsonProperty
	private String IDMSMarketSubSegment__c;

	@JsonProperty
	private String IDMSCompanyMarketServed__c;

	@JsonProperty
	private String IDMSCompanyNbrEmployees__c;

	@JsonProperty
	private String Department;

	@JsonProperty
	private String IDMSCompanyHeadquarters__c;

	@JsonProperty
	private String IDMSAnnualRevenue__c;

	@JsonProperty
	private String IDMSTaxIdentificationNumber__c;

	@JsonProperty
	private String Job_Title__c;

	@JsonProperty
	private String Job_Function__c;

	@JsonProperty
	private String IDMSJobDescription__c;

	@JsonProperty
	private String Phone;

	@JsonProperty
	private String Street;

	@JsonProperty
	private String City;

	@JsonProperty
	private String PostalCode;

	@JsonProperty
	private String State;

	@JsonProperty
	private String IDMS_County__c;

	@JsonProperty
	private String IDMS_POBox__c;

	@JsonProperty
	private String IDMS_AdditionalAddress__c;

	@JsonProperty
	private String IDMSMiddleName__c;

	@JsonProperty
	private String IDMSSalutation__c;

	@JsonProperty
	private String IDMSSuffix__c;

	@JsonProperty
	private String Fax;

	@JsonProperty
	private String AccountId;

	@JsonProperty
	private String IDMS_Registration_Source__c;

	@JsonProperty
	private String IDMS_Profile_update_source__c;

	@JsonProperty
	private String IDMS_TrustStatus__c;

	@JsonProperty
	private String IDMS_RejectionReason__c;

	@JsonProperty
	private String IDMS_RejectionComments__c;

	@JsonProperty
	private boolean isActive;

	@JsonProperty
	private String BFO_ACCOUNT_ID__c;

	@JsonProperty
	private String IDMSCompanyFederationIdentifier__c;

	@JsonProperty
	private String IDMSDelegatedIdp__c;

	@JsonProperty
	private String IDMSIdentityType__c;

	@JsonProperty
	private String tncFlag;

	@JsonProperty
	private String IDMSPrimaryContact__c;

	@JsonProperty
	private String companyFederatedId;

	@JsonProperty
	private String adminFederatedId;

	@JsonProperty
	private String adminCompanyFederatedId;

	@JsonProperty
	private String AboutMe;

	@JsonProperty
	private String invitationCode;

	@JsonProperty
	private String trustedAdmin;

	@JsonProperty
	private String idmsHashedToken;

	@JsonProperty
	private String isActivated;

	@JsonProperty
	private String Channel__c;

	@JsonProperty
	private String SubChannel__c;

	@JsonProperty
	private String contactId;
	
	@JsonProperty
	private String RegistrationAttributes__c;
	
	@JsonProperty
	private String IDMSWorkPhone__c;
	
	@JsonProperty
	private String login_mobile;
	
	@JsonProperty
	private String mobile_reg;
	
	@JsonProperty
	private String adminBFOAccoountID;
	
	//@JsonProperty
	//private String IDMSMarketServed__c;
	
	@JsonProperty("RegistrationAttributes__c")
	public String getRegistrationAttributes__c() {
		return RegistrationAttributes__c;
	}

	@JsonProperty("RegistrationAttributes__c")
	public void setRegistrationAttributes__c(String registrationAttributes__c) {
		RegistrationAttributes__c = registrationAttributes__c;
	}

	@JsonProperty("isActivated")
	public String getIsActivated() {
		return isActivated;
	}

	@JsonProperty("isActivated")
	public void setIsActivated(String isActivated) {
		this.isActivated = isActivated;
	}

	public Attributes getAttributes() {
		return attributes;
	}

	@JsonProperty("IDMSHashedToken__c")
	public String getIdmsHashedToken() {
		return idmsHashedToken;
	}

	@JsonProperty("IDMSHashedToken__c")
	public void setIdmsHashedToken(String idmsHashedToken) {
		this.idmsHashedToken = idmsHashedToken;
	}

	public void setAttributes(Attributes attributes) {
		this.attributes = attributes;
	}

	@JsonProperty("Id")
	public String getId() {
		return Id;
	}

	public void setId(String id) {
		Id = id;
	}

	public void setIDMS_Profile_update_source__c(String iDMS_Profile_update_source__c) {
		IDMS_Profile_update_source__c = iDMS_Profile_update_source__c;
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

	@JsonProperty("IDMS_PreferredLanguage__c")
	public String getIDMS_PreferredLanguage__c() {
		return IDMS_PreferredLanguage__c;
	}

	public void setIDMS_PreferredLanguage__c(String iDMS_PreferredLanguage__c) {
		IDMS_PreferredLanguage__c = iDMS_PreferredLanguage__c;
	}

	@JsonProperty("DefaultCurrencyIsoCode")
	public String getDefaultCurrencyIsoCode() {
		return DefaultCurrencyIsoCode;
	}

	public void setDefaultCurrencyIsoCode(String defaultCurrencyIsoCode) {
		DefaultCurrencyIsoCode = defaultCurrencyIsoCode;
	}

	@JsonProperty("Street")
	public String getStreet() {
		return Street;
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

	public void setIDMS_Profile_update_source_c(String iDMS_Profile_update_source__c) {
		IDMS_Profile_update_source__c = iDMS_Profile_update_source__c;
	}

	@JsonProperty("IDMS_AdditionalAddress__c")
	public String getIDMS_AdditionalAddress__c() {
		return IDMS_AdditionalAddress__c;
	}

	public void setIDMS_AdditionalAddress__c(String iDMS_AdditionalAddress__c) {
		IDMS_AdditionalAddress__c = iDMS_AdditionalAddress__c;
	}

	@JsonProperty("CompanyName")
	public String getCompanyName() {
		return CompanyName;
	}

	public void setCompanyName(String companyName) {
		CompanyName = companyName;
	}

	@JsonProperty("Company_Address1__c")
	public String getCompany_Address1__c() {
		return Company_Address1__c;
	}

	public void setCompany_Address1__c(String company_Address1__c) {
		Company_Address1__c = company_Address1__c;
	}

	@JsonProperty("Company_City__c")
	public String getCompany_City__c() {
		return Company_City__c;
	}

	public void setCompany_City__c(String company_City__c) {
		Company_City__c = company_City__c;
	}

	@JsonProperty("Company_Postal_Code__c")
	public String getCompany_Postal_Code__c() {
		return Company_Postal_Code__c;
	}

	public void setCompany_Postal_Code__c(String company_Postal_Code__c) {
		Company_Postal_Code__c = company_Postal_Code__c;
	}

	@JsonProperty("Company_State__c")
	public String getCompany_State__c() {
		return Company_State__c;
	}

	public void setCompany_State__c(String company_State__c) {
		Company_State__c = company_State__c;
	}

	@JsonProperty("IDMSCompanyPoBox__c")
	public String getIDMSCompanyPoBox__c() {
		return IDMSCompanyPoBox__c;
	}

	public void setIDMSCompanyPoBox__c(String iDMSCompanyPoBox__c) {
		IDMSCompanyPoBox__c = iDMSCompanyPoBox__c;
	}

	@JsonProperty("Company_Country__c")
	public String getCompany_Country__c() {
		return Company_Country__c;
	}

	public void setCompany_Country__c(String company_Country__c) {
		Company_Country__c = company_Country__c;
	}

	@JsonProperty("Company_Address2__c")
	public String getCompany_Address2__c() {
		return Company_Address2__c;
	}

	public void setCompany_Address2__c(String company_Address2__c) {
		Company_Address2__c = company_Address2__c;
	}

	@JsonProperty("IDMSClassLevel1__c")
	public String getIDMSClassLevel1__c() {
		return IDMSClassLevel1__c;
	}

	public void setIDMSClassLevel1__c(String iDMSClassLevel1__c) {
		IDMSClassLevel1__c = iDMSClassLevel1__c;
	}

	@JsonProperty("IDMSClassLevel2__c")
	public String getIDMSClassLevel2__c() {
		return IDMSClassLevel2__c;
	}

	public void setIDMSClassLevel2__c(String iDMSClassLevel2__c) {
		IDMSClassLevel2__c = iDMSClassLevel2__c;
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

	@JsonProperty("IDMSCompanyMarketServed__c")
	public String getIDMSCompanyMarketServed__c() {
		return IDMSCompanyMarketServed__c;
	}

	public void setIDMSCompanyMarketServed__c(String iDMSCompanyMarketServed__c) {
		IDMSCompanyMarketServed__c = iDMSCompanyMarketServed__c;
	}

	@JsonProperty("IDMSCompanyNbrEmployees__c")
	public String getIDMSCompanyNbrEmployees__c() {
		return IDMSCompanyNbrEmployees__c;
	}

	public void setIDMSCompanyNbrEmployees__c(String iDMSCompanyNbrEmployees__c) {
		IDMSCompanyNbrEmployees__c = iDMSCompanyNbrEmployees__c;
	}

	@JsonProperty("IDMSCompanyHeadquarters__c")
	public String getIDMSCompanyHeadquarters__c() {
		return IDMSCompanyHeadquarters__c;
	}

	public void setIDMSCompanyHeadquarters__c(String iDMSCompanyHeadquarters__c) {
		IDMSCompanyHeadquarters__c = iDMSCompanyHeadquarters__c;
	}

	@JsonProperty("IDMSAnnualRevenue__c")
	public String getIDMSAnnualRevenue__c() {
		return IDMSAnnualRevenue__c;
	}

	public void setIDMSAnnualRevenue__c(String iDMSAnnualRevenue__c) {
		IDMSAnnualRevenue__c = iDMSAnnualRevenue__c;
	}

	@JsonProperty("IDMSTaxIdentificationNumber__c")
	public String getIDMSTaxIdentificationNumber__c() {
		return IDMSTaxIdentificationNumber__c;
	}

	public void setIDMSTaxIdentificationNumber__c(String iDMSTaxIdentificationNumber__c) {
		IDMSTaxIdentificationNumber__c = iDMSTaxIdentificationNumber__c;
	}

	@JsonProperty("IDMSMiddleName__c")
	public String getIDMSMiddleName__c() {
		return IDMSMiddleName__c;
	}

	public void setIDMSMiddleName__c(String iDMSMiddleName__c) {
		IDMSMiddleName__c = iDMSMiddleName__c;
	}

	@JsonProperty("Company_Website__c")
	public String getCompany_Website__c() {
		return Company_Website__c;
	}

	public void setCompany_Website__c(String company_Website__c) {
		Company_Website__c = company_Website__c;
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

	@JsonProperty("IDMSCompanyFederationIdentifier__c")
	public String getIDMSCompanyFederationIdentifier__c() {
		return IDMSCompanyFederationIdentifier__c;
	}

	public void setIDMSCompanyFederationIdentifier__c(String iDMSCompanyFederationIdentifier__c) {
		IDMSCompanyFederationIdentifier__c = iDMSCompanyFederationIdentifier__c;
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

	@JsonProperty("IDMSCompanyCounty__c")
	public String getIDMSCompanyCounty__c() {
		return IDMSCompanyCounty__c;
	}

	public void setIDMSCompanyCounty__c(String iDMSCompanyCounty__c) {
		IDMSCompanyCounty__c = iDMSCompanyCounty__c;
	}

	@JsonProperty("Country")
	public String getCountry() {
		return Country;
	}

	public void setCountry(String country) {
		Country = country;
	}

	public String getTncFlag() {
		return tncFlag;
	}

	public void setTncFlag(String tncFlag) {
		this.tncFlag = tncFlag;
	}

	@JsonProperty("IDMS_TrustStatus__c")
	public String getIDMS_TrustStatus__c() {
		return IDMS_TrustStatus__c;
	}

	@JsonProperty("AccountId")
	public String getAccountId() {
		return AccountId;
	}

	public void setAccountId(String accountId) {
		AccountId = accountId;
	}

	public void setIDMS_TrustStatus__c(String iDMS_TrustStatus__c) {
		IDMS_TrustStatus__c = iDMS_TrustStatus__c;
	}

	@JsonProperty("IDMS_RejectionReason__c")
	public String getIDMS_RejectionReason__c() {
		return IDMS_RejectionReason__c;
	}

	public void setIDMS_RejectionReason__c(String iDMS_RejectionReason__c) {
		IDMS_RejectionReason__c = iDMS_RejectionReason__c;
	}

	@JsonProperty("IDMS_RejectionComments__c")
	public String getIDMS_RejectionComments__c() {
		return IDMS_RejectionComments__c;
	}

	public void setIDMS_RejectionComments__c(String iDMS_RejectionComments__c) {
		IDMS_RejectionComments__c = iDMS_RejectionComments__c;
	}

	@JsonProperty("IDMSPrimaryContact__c")
	public String getIDMSPrimaryContact__c() {
		return IDMSPrimaryContact__c;
	}

	public void setIDMSPrimaryContact__c(String iDMSPrimaryContact__c) {
		IDMSPrimaryContact__c = iDMSPrimaryContact__c;
	}

	public boolean isActive() {
		return isActive;
	}

	public void setActive(boolean isActive) {
		this.isActive = isActive;
	}

	@JsonProperty("BFO_ACCOUNT_ID__c")
	public String getBFO_ACCOUNT_ID__c() {
		return BFO_ACCOUNT_ID__c;
	}

	@JsonProperty("BFO_ACCOUNT_ID__c")
	public void setBFO_ACCOUNT_ID__c(String bFO_ACCOUNT_ID__c) {
		BFO_ACCOUNT_ID__c = bFO_ACCOUNT_ID__c;
	}

	@JsonProperty("COMPANY_FEDERATED_ID")
	public String getCompanyFederatedId() {
		return companyFederatedId;
	}

	@JsonProperty("COMPANY_FEDERATED_ID")
	public void setCompanyFederatedId(String companyFederatedId) {
		this.companyFederatedId = companyFederatedId;
	}

	@JsonProperty("ADMIN_FEDERATED_ID")
	public String getAdminFederatedId() {
		return adminFederatedId;
	}

	@JsonProperty("ADMIN_FEDERATED_ID")
	public void setAdminFederatedId(String adminFederatedId) {
		this.adminFederatedId = adminFederatedId;
	}

	@JsonProperty("ADMIN_COMPANY_FEDERATED_ID")
	public String getAdminCompanyFederatedId() {
		return adminCompanyFederatedId;
	}

	@JsonProperty("ADMIN_COMPANY_FEDERATED_ID")
	public void setAdminCompanyFederatedId(String adminCompanyFederatedId) {
		this.adminCompanyFederatedId = adminCompanyFederatedId;
	}

	@JsonProperty("AboutMe")
	public String getAboutMe() {
		return AboutMe;
	}

	@JsonProperty("AboutMe")
	public void setAboutMe(String aboutMe) {
		AboutMe = aboutMe;
	}

	@JsonProperty("InvitationCode")
	public String getInvitationCode() {
		return invitationCode;
	}

	@JsonProperty("InvitationCode")
	public void setInvitationCode(String invitationCode) {
		this.invitationCode = invitationCode;
	}

	public String getTrustedAdmin() {
		return trustedAdmin;
	}

	public void setTrustedAdmin(String trustedAdmin) {
		this.trustedAdmin = trustedAdmin;
	}

	@JsonProperty("currencyCode")
	public String getCurrencyCode() {
		return currencyCode;
	}

	@JsonProperty("currencyCode")
	public void setCurrencyCode(String currencyCode) {
		this.currencyCode = currencyCode;
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

	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}

	@JsonProperty("IDMSWorkPhone__c")
	public String getIDMSWorkPhone__c() {
		return IDMSWorkPhone__c;
	}

	@JsonProperty("IDMSWorkPhone__c")
	public void setIDMSWorkPhone__c(String iDMSWorkPhone__c) {
		IDMSWorkPhone__c = iDMSWorkPhone__c;
	}

	@JsonProperty("login_mobile")
	public String getLogin_mobile() {
		return login_mobile;
	}

	@JsonProperty("login_mobile")
	public void setLogin_mobile(String login_mobile) {
		this.login_mobile = login_mobile;
	}
	
	@JsonProperty("mobile_reg")
	public String getMobile_reg() {
		return mobile_reg;
	}
	
	@JsonProperty("mobile_reg")
	public void setMobile_reg(String mobile_reg) {
		this.mobile_reg = mobile_reg;
	}
	@JsonProperty("AdminBFOAccoountID")
	public String getAdminBFOAccoountID() {
		return adminBFOAccoountID;
	}
	
	@JsonProperty("AdminBFOAccoountID")
	public void setAdminBFOAccoountID(String adminBFOAccoountID) {
		this.adminBFOAccoountID = adminBFOAccoountID;
	}
	
	/*
	@JsonProperty("IDMSMarketServed__c")
	public List getIDMSMarketServed__c() {
		return IDMSMarketServed__c;
	}
	@JsonProperty("IDMSMarketServed__c")
	public void setIDMSMarketServed__c(List iDMSMarketServed__c) {
		IDMSMarketServed__c = iDMSMarketServed__c;
	}*/
	
}

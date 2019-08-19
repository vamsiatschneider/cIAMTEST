package com.idms.product.model;

import javax.validation.constraints.Size;
import javax.xml.bind.annotation.XmlRootElement;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;

@XmlRootElement
public class OpenAMGetUserHomeResponse {

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
	
	private String IDMS_Federated_ID__c;
	    
	private String IDMS_County__c;

	    
	private String IDMS_POBox__c;

	    
	private String IDMS_Registration_Source__c;

	    
	private String IDMS_Profile_update_source__c;
				   	
	    
	private String IDMS_AdditionalAddress__c;
	
	    
	private String IDMSMiddleName__c;
	
	    
	private String IDMSSalutation__c;

	    
	private String IDMSSuffix__c;

	    
	private String Fax;
	
	    
	private String IDMSDelegatedIdp__c;
	
	    
	private String IDMSIdentityType__c;
	
	private String IDMS_Company_I_am_a__c;
	
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
	
	private String AboutMe;

	private String DefaultCurrencyIsoCode;
	
	private String currencyCode;
	
	private String FederationIdentifier;
	
	private String IDMSClassLevel1__c;
	
	private String Title;
	
	private String UserStatus__c;
	
	private String IDMSTaxIdentificationNumber__c;
	
	private String trustedAdmin;
	
	private String isActivated;
	
	private String Loginid;

	// mobile_reg and login_mobile field is added for dual identifier feature
	@JsonProperty
	private String mobile_reg;

	@JsonProperty
	private String login_mobile;

	@JsonProperty("IDMSTaxIdentificationNumber__c")
	public String getIDMSTaxIdentificationNumber__c() {
		return IDMSTaxIdentificationNumber__c;
	}

	public void setIDMSTaxIdentificationNumber__c(String iDMSTaxIdentificationNumber__c) {
		IDMSTaxIdentificationNumber__c = iDMSTaxIdentificationNumber__c;
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

	@JsonProperty("IDMS_Company_I_am_a__c")
	public String getIDMSClassLevel1__c() {
		return IDMS_Company_I_am_a__c;
	}

	public void setIDMSClassLevel1__c(String iDMSClassLevel1__c) {
		IDMS_Company_I_am_a__c = iDMSClassLevel1__c;
	}

	public Attributes getAttributes() {
		return Attributes;
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

	@JsonProperty("IDMSMiddleName__c")
	public String getIDMSMiddleName__c() {
		return IDMSMiddleName__c;
	}

	public void setIDMSMiddleName__c(String iDMSMiddleName__c) {
		IDMSMiddleName__c = iDMSMiddleName__c;
	}

	@JsonProperty("IDMSSalutation__c")
	public String getIDMSSalutation__c() {
		return IDMSSalutation__c;
	}

	public void setIDMSSalutation__c(String iDMSSalutation__c) {
		IDMSSalutation__c = iDMSSalutation__c;
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

	public void setAttributes(Attributes attributes) {
		Attributes = attributes;
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

	@JsonProperty("IDMS_Federated_ID__c")
	public String getIDMS_Federated_ID__c() {
		return IDMS_Federated_ID__c;
	}

	@JsonProperty("IDMS_Federated_ID__c")
	public void setIDMS_Federated_ID__c(String iDMS_Federated_ID__c) {
		IDMS_Federated_ID__c = iDMS_Federated_ID__c;
	}

	public String getIDMS_Company_I_am_a__c() {
		return IDMS_Company_I_am_a__c;
	}

	public void setIDMS_Company_I_am_a__c(String iDMS_Company_I_am_a__c) {
		IDMS_Company_I_am_a__c = iDMS_Company_I_am_a__c;
	}

	@JsonProperty("IDMSPrimaryContact__c")
	public String getIDMSPrimaryContact__c() {
		return IDMSPrimaryContact__c;
	}

	@JsonProperty("IDMSPrimaryContact__c")
	public void setIDMSPrimaryContact__c(String iDMSPrimaryContact__c) {
		IDMSPrimaryContact__c = iDMSPrimaryContact__c;
	}

	@JsonProperty("AboutMe")
	public String getAboutMe() {
		return AboutMe;
	}

	@JsonProperty("AboutMe")
	public void setAboutMe(String aboutMe) {
		AboutMe = aboutMe;
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

	/**
	 * @return the mobile_reg
	 */
	@JsonProperty("mobile_reg")
	public String getMobile_reg() {
		return mobile_reg;
	}

	/**
	 * @param mobile_reg the mobile_reg to set
	 */
	@JsonProperty("mobile_reg")
	public void setMobile_reg(String mobile_reg) {
		this.mobile_reg = mobile_reg;
	}

	/**
	 * @return the login_mobile
	 */
	@JsonProperty("login_mobile")
	public String getLogin_mobile() {
		return login_mobile;
	}

	/**
	 * @param login_mobile the login_mobile to set
	 */
	@JsonProperty("login_mobile")
	public void setLogin_mobile(String login_mobile) {
		this.login_mobile = login_mobile;
	}
	
	@JsonProperty("Loginid")
	public String getLoginid() {
		return Loginid;
	}

	@JsonProperty("Loginid")
	public void setLoginid(String loginid) {
		Loginid = loginid;
	}
}

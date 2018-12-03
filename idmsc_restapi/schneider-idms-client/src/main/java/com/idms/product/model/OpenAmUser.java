package com.idms.product.model;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class OpenAmUser {

	@JsonInclude(Include.NON_NULL)
	private String mail;

	@JsonInclude(Include.NON_NULL)
	private String mobile;

	@JsonInclude(Include.NON_NULL)
	private String homePhone;

	@JsonInclude(Include.NON_NULL)
	private String givenName;

	@JsonInclude(Include.NON_EMPTY)
	private String sn;

	@JsonInclude(Include.NON_NULL)
	private String emailOptIn;

	@JsonInclude(Include.NON_NULL)
	private String employeeType;

	@JsonInclude(Include.NON_NULL)
	private String c;

	@JsonInclude(Include.NON_NULL)
	private String preferredlanguage;

	@JsonInclude(Include.NON_NULL)
	private String currency;

	@JsonInclude(Include.NON_NULL)
	private String street;

	@JsonInclude(Include.NON_NULL)
	private String l;// city

	@JsonInclude(Include.NON_NULL)
	private String postalCode;

	@JsonInclude(Include.NON_NULL)
	private String st;

	// county one more field is there

	@JsonInclude(Include.NON_NULL)
	private String county;

	@JsonInclude(Include.NON_NULL)
	private String postOfficeBox;

	@JsonInclude(Include.NON_NULL)
	private String federationID;

	@JsonInclude(Include.NON_NULL)
	private String registerationSource;

	@JsonInclude(Include.NON_NULL)
	private String updateSource;

	@JsonInclude(Include.NON_NULL)
	private String additionalInfo;

	@JsonInclude(Include.NON_NULL)
	private String companyName;

	@JsonInclude(Include.NON_NULL)
	private String companyStreet;

	@JsonInclude(Include.NON_NULL)
	private String companyCity;

	@JsonInclude(Include.NON_NULL)
	private String companyPostalCode;

	@JsonInclude(Include.NON_NULL)
	private String companyState;

	@JsonInclude(Include.NON_NULL)
	private String companyPostOfficeBox;

	@JsonInclude(Include.NON_NULL)
	private String companyCountry;

	@JsonInclude(Include.NON_NULL)
	private String companyAdditionalInfo;

	@JsonInclude(Include.NON_NULL)
	private String iam1;

	@JsonInclude(Include.NON_NULL)
	private String iam2;

	@JsonInclude(Include.NON_NULL)
	private String industrySegment;

	@JsonInclude(Include.NON_NULL)
	private String industrySubSegment;

	@JsonInclude(Include.NON_NULL)
	private String telephoneNumber;

	@JsonInclude(Include.NON_NULL)
	private String title;

	@JsonInclude(Include.NON_NULL)
	private String jobFunction;

	@JsonInclude(Include.NON_NULL)
	private String jobDescription;

	@JsonInclude(Include.NON_NULL)
	private String industries;

	@JsonInclude(Include.NON_NULL)
	private String employeeSize;

	@JsonInclude(Include.NON_NULL)
	private String headquarters;

	@JsonInclude(Include.NON_NULL)
	private String annualRevenue;

	@JsonInclude(Include.NON_NULL)
	private String taxID;

	@JsonInclude(Include.NON_NULL)
	private String middleName;

	@JsonInclude(Include.NON_NULL)
	private String companyWebSite;

	@JsonInclude(Include.NON_NULL)
	private String initials;

	@JsonInclude(Include.NON_NULL)
	private String departmentNumber;

	@JsonInclude(Include.NON_NULL)
	private String suffix;

	@JsonInclude(Include.NON_NULL)
	private String fax;

	@JsonInclude(Include.NON_NULL)
	private String companyFederatedID;

	@JsonInclude(Include.NON_NULL)
	private String delegatedIDP;

	@JsonInclude(Include.NON_NULL)
	private String identityType;

	@JsonInclude(Include.NON_NULL)
	private String primaryContact;

	@JsonInclude(Include.NON_NULL)
	private String companyCounty;

	@JsonInclude(Include.NON_NULL)
	private String userPassword;

	@JsonInclude(Include.NON_NULL)
	private String username;

	@JsonInclude(Include.NON_NULL)
	private String loginid;

	@JsonInclude(Include.NON_NULL)
	private String idmsuid;

	@JsonInclude(Include.NON_NULL)
	private String hotpEmailVerification;

	@JsonInclude(Include.NON_NULL)
	private String hotpMobileVerification;

	@JsonInclude(Include.NON_NULL)
	private String tncFlag;

	@JsonInclude(Include.NON_NULL)
	private String idmsail_c;

	@JsonInclude(Include.NON_NULL)
	private String tmp_password;

	@JsonInclude(Include.NON_NULL)
	private String IDMSisInternal__c;

	@JsonInclude(Include.NON_NULL)
	private String admin_company_id;

	@JsonInclude(Include.NON_NULL)
	private String admin_federated_id;

	@JsonInclude(Include.NON_NULL)
	private String CompanyID;

	@JsonInclude(Include.NON_NULL)
	private String AboutMe;

	@JsonInclude(Include.NON_NULL)
	private String invitationCode;

	@JsonInclude(Include.NON_NULL)
	private String trustedAdmin;

	@JsonInclude(Include.NON_NULL)
	private String isActivated;

	@JsonInclude(Include.NON_NULL)
	private String emailcount;

	@JsonInclude(Include.NON_NULL)
	private String channel;

	@JsonInclude(Include.NON_NULL)
	private String subchannel;

	@JsonInclude(Include.NON_NULL)
	private String contactId;
	
	@JsonInclude(Include.NON_NULL)
	private String RegistrationAttributes__c;
	
	@JsonInclude(Include.NON_NULL)
	private String IDMSWorkPhone__c;
	
	private String alink;
	
	@JsonProperty("IDMSWorkPhone__c")
	@JsonInclude(Include.NON_NULL)
	public String getIDMSWorkPhone__c() {
		return IDMSWorkPhone__c;
	}

	@JsonProperty("IDMSWorkPhone__c")
	@JsonInclude(Include.NON_NULL)
	public void setIDMSWorkPhone__c(String iDMSWorkPhone__c) {
		IDMSWorkPhone__c = iDMSWorkPhone__c;
	}

	@JsonProperty("RegistrationAttributes__c")
	@JsonInclude(Include.NON_NULL)
	public String getRegistrationAttributes__c() {
		return RegistrationAttributes__c;
	}

	@JsonProperty("RegistrationAttributes__c")
	@JsonInclude(Include.NON_NULL)
	public void setRegistrationAttributes__c(String registrationAttributes__c) {
		RegistrationAttributes__c = registrationAttributes__c;
	}

	@JsonInclude(Include.NON_NULL)
	private String bfoAccountId;

	@JsonProperty("emailcount")
	@JsonInclude(Include.NON_NULL)
	public String getEmailcount() {
		return emailcount;
	}

	@JsonProperty("emailcount")
	@JsonInclude(Include.NON_NULL)
	public void setEmailcount(String emailcount) {
		this.emailcount = emailcount;
	}

	@JsonProperty("isActivated")
	@JsonInclude(Include.NON_NULL)
	public String getIsActivated() {
		return isActivated;
	}

	@JsonProperty("isActivated")
	@JsonInclude(Include.NON_NULL)
	public void setIsActivated(String isActivated) {
		this.isActivated = isActivated;
	}

	@JsonProperty("AboutMe")
	@JsonInclude(Include.NON_NULL)
	public String getAboutMe() {
		return AboutMe;
	}

	@JsonProperty("AboutMe")
	@JsonInclude(Include.NON_NULL)
	public void setAboutMe(String aboutMe) {
		AboutMe = aboutMe;
	}

	@JsonProperty("CompanyID")
	@JsonInclude(Include.NON_NULL)
	public String getCompanyID() {
		return CompanyID;
	}

	@JsonProperty("CompanyID")
	@JsonInclude(Include.NON_NULL)
	public void setCompanyID(String companyID) {
		CompanyID = companyID;
	}

	public String getCn() {
		return cn;
	}

	public void setCn(String cn) {
		this.cn = cn;
	}

	@JsonInclude(Include.NON_NULL)
	private String cn;

	/*
	 * @JsonProperty("IDMSAil_c")
	 * 
	 * @JsonInclude(Include.NON_NULL) public String getIDMSAil_c() { return
	 * IDMSAil_c; }
	 * 
	 * public void setIDMSAil_c(String iDMSAil_c) { IDMSAil_c = iDMSAil_c; }
	 * 
	 * @JsonProperty("IDMSAIL_Applications_c")
	 * 
	 * @JsonInclude(Include.NON_NULL) public String getIDMSAIL_Applications_c()
	 * { return IDMSAIL_Applications_c; }
	 * 
	 * public void setIDMSAIL_Applications_c(String iDMSAIL_Applications_c) {
	 * IDMSAIL_Applications_c = iDMSAIL_Applications_c; }
	 * 
	 * @JsonProperty("IDMSAIL_Programs_c")
	 * 
	 * @JsonInclude(Include.NON_NULL) public String getIDMSAIL_Programs_c() {
	 * return IDMSAIL_Programs_c; }
	 * 
	 * public void setIDMSAIL_Programs_c(String iDMSAIL_Programs_c) {
	 * IDMSAIL_Programs_c = iDMSAIL_Programs_c; }
	 * 
	 * @JsonProperty("IDMSAIL_Features_c")
	 * 
	 * @JsonInclude(Include.NON_NULL) public String getIDMSAIL_Features_c() {
	 * return IDMSAIL_Features_c; }
	 * 
	 * public void setIDMSAIL_Features_c(String iDMSAIL_Features_c) {
	 * IDMSAIL_Features_c = iDMSAIL_Features_c; }
	 */
	@JsonInclude(Include.NON_NULL)
	private String idmsail_Applications_c;

	@JsonInclude(Include.NON_NULL)
	private String idmsail_Features_c;

	@JsonInclude(Include.NON_NULL)
	private String idmsail_Programs_c;

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public String getMail() {
		return mail;
	}

	public void setMail(String mail) {
		this.mail = mail;
	}

	public String getGivenName() {
		return givenName;
	}

	public void setGivenName(String givenName) {
		this.givenName = givenName;
	}

	public String getSn() {
		return sn;
	}

	public void setSn(String sn) {
		this.sn = sn;
	}

	public String getEmailOptIn() {
		return emailOptIn;
	}

	public void setEmailOptIn(String emailOptIn) {
		this.emailOptIn = emailOptIn;
	}

	public String getCurrency() {
		return currency;
	}

	public void setCurrency(String currency) {
		this.currency = currency;
	}

	public String getCounty() {
		return county;
	}

	public void setCounty(String county) {
		this.county = county;
	}

	public String getAdditionalInfo() {
		return additionalInfo;
	}

	public void setAdditionalInfo(String additionalInfo) {
		this.additionalInfo = additionalInfo;
	}

	public String getCompanyName() {
		return companyName;
	}

	public void setCompanyName(String companyName) {
		this.companyName = companyName;
	}

	public String getCompanyCity() {
		return companyCity;
	}

	public void setCompanyCity(String companyCity) {
		this.companyCity = companyCity;
	}

	public String getCompanyPostalCode() {
		return companyPostalCode;
	}

	public void setCompanyPostalCode(String companyPostalCode) {
		this.companyPostalCode = companyPostalCode;
	}

	public String getCompanyState() {
		return companyState;
	}

	public void setCompanyState(String companyState) {
		this.companyState = companyState;
	}

	public String getCompanyPostOfficeBox() {
		return companyPostOfficeBox;
	}

	public void setCompanyPostOfficeBox(String companyPostOfficeBox) {
		this.companyPostOfficeBox = companyPostOfficeBox;
	}

	public String getCompanyAdditionalInfo() {
		return companyAdditionalInfo;
	}

	public void setCompanyAdditionalInfo(String companyAdditionalInfo) {
		this.companyAdditionalInfo = companyAdditionalInfo;
	}

	public String getCompanyCountry() {
		return companyCountry;
	}

	public void setCompanyCountry(String companyCountry) {
		this.companyCountry = companyCountry;
	}

	public String getIam1() {
		return iam1;
	}

	public void setIam1(String iam1) {
		this.iam1 = iam1;
	}

	public String getIam2() {
		return iam2;
	}

	public void setIam2(String iam2) {
		this.iam2 = iam2;
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

	public String getIndustries() {
		return industries;
	}

	public void setIndustries(String industries) {
		this.industries = industries;
	}

	public String getEmployeeSize() {
		return employeeSize;
	}

	public void setEmployeeSize(String employeeSize) {
		this.employeeSize = employeeSize;
	}

	public String getHeadquarters() {
		return headquarters;
	}

	public void setHeadquarters(String headquarters) {
		this.headquarters = headquarters;
	}

	public String getAnnualRevenue() {
		return annualRevenue;
	}

	public void setAnnualRevenue(String annualRevenue) {
		this.annualRevenue = annualRevenue;
	}

	public String getTaxID() {
		return taxID;
	}

	public void setTaxID(String taxID) {
		this.taxID = taxID;
	}

	public String getMiddleName() {
		return middleName;
	}

	public void setMiddleName(String middleName) {
		this.middleName = middleName;
	}

	public String getCompanyWebSite() {
		return companyWebSite;
	}

	public void setCompanyWebSite(String companyWebSite) {
		this.companyWebSite = companyWebSite;
	}

	public String getSuffix() {
		return suffix;
	}

	public void setSuffix(String suffix) {
		this.suffix = suffix;
	}

	public String getFax() {
		return fax;
	}

	public void setFax(String fax) {
		this.fax = fax;
	}

	public String getCompanyFederatedID() {
		return companyFederatedID;
	}

	public void setCompanyFederatedID(String companyFederatedID) {
		this.companyFederatedID = companyFederatedID;
	}

	public String getDelegatedIDP() {
		return delegatedIDP;
	}

	public void setDelegatedIDP(String delegatedIDP) {
		this.delegatedIDP = delegatedIDP;
	}

	public String getIdentityType() {
		return identityType;
	}

	public void setIdentityType(String identityType) {
		this.identityType = identityType;
	}

	public String getRegisterationSource() {
		return registerationSource;
	}

	public void setRegisterationSource(String registerationSource) {
		this.registerationSource = registerationSource;
	}

	public String getUserPassword() {
		return userPassword;
	}

	public void setUserPassword(String userPassword) {
		this.userPassword = userPassword;
	}

	public String getLoginid() {
		return loginid;
	}

	public void setLoginid(String loginid) {
		this.loginid = loginid;
	}

	public String getIdmsuid() {
		return idmsuid;
	}

	public void setIdmsuid(String idmsuid) {
		this.idmsuid = idmsuid;
	}

	public String getMobile() {
		return mobile;
	}

	public void setMobile(String mobile) {
		this.mobile = mobile;
	}

	public String getHomePhone() {
		return homePhone;
	}

	public void setHomePhone(String homePhone) {
		this.homePhone = homePhone;
	}

	public String getEmployeeType() {
		return employeeType;
	}

	public void setEmployeeType(String employeeType) {
		this.employeeType = employeeType;
	}

	public String getPreferredlanguage() {
		return preferredlanguage;
	}

	public void setPreferredlanguage(String preferredlanguage) {
		this.preferredlanguage = preferredlanguage;
	}

	public String getStreet() {
		return street;
	}

	public void setStreet(String street) {
		this.street = street;
	}

	public String getL() {
		return l;
	}

	public void setL(String l) {
		this.l = l;
	}

	public String getPostalCode() {
		return postalCode;
	}

	public void setPostalCode(String postalCode) {
		this.postalCode = postalCode;
	}

	public String getC() {
		return c;
	}

	public void setC(String c) {
		this.c = c;
	}

	public String getSt() {
		return st;
	}

	public void setSt(String st) {
		this.st = st;
	}

	public String getPostOfficeBox() {
		return postOfficeBox;
	}

	public void setPostOfficeBox(String postOfficeBox) {
		this.postOfficeBox = postOfficeBox;
	}

	public String getFederationID() {
		return federationID;
	}

	public void setFederationID(String federationID) {
		this.federationID = federationID;
	}

	public String getUpdateSource() {
		return updateSource;
	}

	public void setUpdateSource(String updateSource) {
		this.updateSource = updateSource;
	}

	public String getCompanyStreet() {
		return companyStreet;
	}

	public void setCompanyStreet(String companyStreet) {
		this.companyStreet = companyStreet;
	}

	public String getIndustrySegment() {
		return industrySegment;
	}

	public void setIndustrySegment(String industrySegment) {
		this.industrySegment = industrySegment;
	}

	public String getIndustrySubSegment() {
		return industrySubSegment;
	}

	public void setIndustrySubSegment(String industrySubSegment) {
		this.industrySubSegment = industrySubSegment;
	}

	public String getTelephoneNumber() {
		return telephoneNumber;
	}

	public void setTelephoneNumber(String telephoneNumber) {
		this.telephoneNumber = telephoneNumber;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getInitials() {
		return initials;
	}

	public void setInitials(String initials) {
		this.initials = initials;
	}

	public String getDepartmentNumber() {
		return departmentNumber;
	}

	public void setDepartmentNumber(String departmentNumber) {
		this.departmentNumber = departmentNumber;
	}

	public String getPrimaryContact() {
		return primaryContact;
	}

	public void setPrimaryContact(String primaryContact) {
		this.primaryContact = primaryContact;
	}

	public String getCompanyCounty() {
		return companyCounty;
	}

	public void setCompanyCounty(String companyCounty) {
		this.companyCounty = companyCounty;
	}

	public String getHotpEmailVerification() {
		return hotpEmailVerification;
	}

	public void setHotpEmailVerification(String hotpEmailVerification) {
		this.hotpEmailVerification = hotpEmailVerification;
	}

	public String getHotpMobileVerification() {
		return hotpMobileVerification;
	}

	public void setHotpMobileVerification(String hotpMobileVerification) {
		this.hotpMobileVerification = hotpMobileVerification;
	}

	public String getTncFlag() {
		return tncFlag;
	}

	public void setTncFlag(String tncFlag) {
		this.tncFlag = tncFlag;
	}

	public String getIdmsail_c() {
		return idmsail_c;
	}

	public void setIdmsail_c(String idmsail_c) {
		this.idmsail_c = idmsail_c;
	}

	public String getIdmsail_Applications_c() {
		return idmsail_Applications_c;
	}

	public void setIdmsail_Applications_c(String idmsail_Applications_c) {
		this.idmsail_Applications_c = idmsail_Applications_c;
	}

	public String getIdmsail_Features_c() {
		return idmsail_Features_c;
	}

	public void setIdmsail_Features_c(String idmsail_Features_c) {
		this.idmsail_Features_c = idmsail_Features_c;
	}

	public String getIdmsail_Programs_c() {
		return idmsail_Programs_c;
	}

	public void setIdmsail_Programs_c(String idmsail_Programs_c) {
		this.idmsail_Programs_c = idmsail_Programs_c;
	}

	public String getTmp_password() {
		return tmp_password;
	}

	public void setTmp_password(String tmp_password) {
		this.tmp_password = tmp_password;
	}

	@JsonProperty("IDMSisInternal__c")
	@JsonInclude(Include.NON_NULL)
	public String getIDMSisInternal__c() {
		return IDMSisInternal__c;
	}

	@JsonProperty("IDMSisInternal__c")
	@JsonInclude(Include.NON_NULL)
	public void setIDMSisInternal__c(String iDMSisInternal__c) {
		IDMSisInternal__c = iDMSisInternal__c;
	}

	public String getAdmin_company_id() {
		return admin_company_id;
	}

	public void setAdmin_company_id(String admin_company_id) {
		this.admin_company_id = admin_company_id;
	}

	public String getAdmin_federated_id() {
		return admin_federated_id;
	}

	public void setAdmin_federated_id(String admin_federated_id) {
		this.admin_federated_id = admin_federated_id;
	}

	public String getInvitationCode() {
		return invitationCode;
	}

	public void setInvitationCode(String invitationCode) {
		this.invitationCode = invitationCode;
	}

	public String getTrustedAdmin() {
		return trustedAdmin;
	}

	public void setTrustedAdmin(String trustedAdmin) {
		this.trustedAdmin = trustedAdmin;
	}

	public String getChannel() {
		return channel;
	}

	public void setChannel(String channel) {
		this.channel = channel;
	}

	public String getSubchannel() {
		return subchannel;
	}

	public void setSubchannel(String subchannel) {
		this.subchannel = subchannel;
	}

	public String getContactId() {
		return contactId;
	}

	public void setContactId(String contactId) {
		this.contactId = contactId;
	}

	public String getBfoAccountId() {
		return bfoAccountId;
	}

	public void setBfoAccountId(String bfoAccountId) {
		this.bfoAccountId = bfoAccountId;
	}

	public String getAlink() {
		return alink;
	}

	public void setAlink(String alink) {
		this.alink = alink;
	}

	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}
}
package com.se.idms.dto;

import javax.inject.Inject;

import org.apache.commons.lang3.StringUtils;
import org.springframework.stereotype.Component;

import com.idms.model.GetUserRecordResponse;
import com.idms.product.model.OpenAMGetUserHomeResponse;
import com.idms.product.model.OpenAMGetUserWorkResponse;
import com.jayway.jsonpath.DocumentContext;
import com.se.idms.util.JsonConstants;
import com.se.idms.util.UserConstants;

/**
 * 
 * @author SESA468450(Subbarao Maniam)
 *
 */

@Component("parseValuesByOauthHomeWorkContext")
public class ParseValuesByOauthHomeWorkContextDto {
	//PMD:UnusedPrivateField
	//@Inject
	//private CompanyDetails companydetails;

	public void parseValuesByOauthWorkContext(GetUserWorkByOauthResponse userResponse,
			DocumentContext userProductDocCtx) {
		// TODO Auto-generated method stub

		userResponse.setUserId(null != userProductDocCtx.read("$.uid")
				? getValue(userProductDocCtx.read("$.uid").toString()) : getDelimeter());

		IFWCustomAttributesForWork customAttributes = new IFWCustomAttributesForWork();
		userResponse.setCustomAttributes(customAttributes);
		userResponse.getCustomAttributes().setAccountId(null != userProductDocCtx.read("$.uid")
				? getValue(userProductDocCtx.read("$.uid").toString()) : getDelimeter());	
		userResponse.getCustomAttributes().setAdditionalAddress(null != userProductDocCtx.read("$.additionalInfo")
				? getValue(userProductDocCtx.read("$.additionalInfo").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setAil(null != userProductDocCtx.read("$.IDMSAil_c")
				? getValue(userProductDocCtx.read("$.IDMSAil_c").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setAilApplications(null != userProductDocCtx.read("$.IDMSAIL_Applications_c")
				? getValue(userProductDocCtx.read("$.IDMSAIL_Applications_c").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setAilFeatures(null != userProductDocCtx.read("$.IDMSAIL_Features_c")
				? getValue(userProductDocCtx.read("$.IDMSAIL_Features_c").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setAilPrograms(null != userProductDocCtx.read("$.IDMSAIL_Programs_c")
				? getValue(userProductDocCtx.read("$.IDMSAIL_Programs_c").toString()) : getDelimeter());
//		userResponse.getCustomAttributes().setAboutMe(null != userProductDocCtx.read("$.")
//				? getValue(userProductDocCtx.read("$.").toString()) : getDelimeter());
		
		userResponse.getCustomAttributes().setCity(null != userProductDocCtx.read("$.l")
				? getValue(userProductDocCtx.read("$.l").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setContactId(null != userProductDocCtx.read("$.uid")
				? getValue(userProductDocCtx.read("$.uid").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setCountryCode(null != userProductDocCtx.read("$.c")
				? getValue(userProductDocCtx.read("$.c").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setCounty(null != userProductDocCtx.read("$.county")
				? getValue(userProductDocCtx.read("$.county").toString()) : getDelimeter());

		// companydetail start
		// userResponse.getCustomAttributes().setCompanyDetails(companydetails);

		userResponse.getCustomAttributes().setAnnualRevenue(null != userProductDocCtx.read("$.annualRevenue")
				? getValue(userProductDocCtx.read("$.annualRevenue").toString()) : getDelimeter());

		/*
		 * userResponse.getCustomAttributes().setBusinessUnit(null !=
		 * userProductDocCtx.read("$.") ?
		 * getValue(userProductDocCtx.read("$.").toString()) : getDelimeter());
		 */

		userResponse.getCustomAttributes().setClassLevel1(null != userProductDocCtx.read("$.iam1")
				? getValue(userProductDocCtx.read("$.iam1").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setClassLevel2(null != userProductDocCtx.read("$.iam2")
				? getValue(userProductDocCtx.read("$.iam2").toString()) : getDelimeter());
		userResponse.getCustomAttributes()
				.setCompanyAdditionalAddress(null != userProductDocCtx.read("$.companyAdditionalInfo")
						? getValue(userProductDocCtx.read("$.companyAdditionalInfo").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setCompanyCity(null != userProductDocCtx.read("$.companyCity")
				? getValue(userProductDocCtx.read("$.companyCity").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setCompanyCountryCode(null != userProductDocCtx.read("$.companyCountry")
				? getValue(userProductDocCtx.read("$.companyCountry").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setCompanyCounty(null != userProductDocCtx.read("$.companyCounty")
				? getValue(userProductDocCtx.read("$.companyCounty").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setCompanyName(null != userProductDocCtx.read("$.companyName")
				? getValue(userProductDocCtx.read("$.companyName").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setCompanyPOBox(null != userProductDocCtx.read("$.companyPostalCode")
				? getValue(userProductDocCtx.read("$.companyPostalCode").toString()) : getDelimeter());
		userResponse.getCustomAttributes()
				.setCompanyStateOrProvinceCode(null != userProductDocCtx.read("$.companyState")
						? getValue(userProductDocCtx.read("$.companyState").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setCompanyStreet(null != userProductDocCtx.read("$.companyStreet")
				? getValue(userProductDocCtx.read("$.companyStreet").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setCompanyWebsite(null != userProductDocCtx.read("$.companyWebSite")
				? getValue(userProductDocCtx.read("$.companyWebSite").toString()) : getDelimeter());

		userResponse.getCustomAttributes().setCompanyZipCode(null != userProductDocCtx.read("$.companyPostalCode")
				? getValue(userProductDocCtx.read("$.companyPostalCode").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setCurrency(null != userProductDocCtx.read("$.currency")
				? getValue(userProductDocCtx.read("$.currency").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setDepartment(null != userProductDocCtx.read("$.departmentNumber")
				? getValue(userProductDocCtx.read("$.departmentNumber").toString()) : getDelimeter());

		/*
		 * userResponse.getCustomAttributes().setDivision(null !=
		 * userProductDocCtx.read("$.annualRevenue") ?
		 * getValue(userProductDocCtx.read("$.annualRevenue").toString()) :
		 * getDelimeter());
		 */

		userResponse.getCustomAttributes().setEmployeeSize(null != userProductDocCtx.read("$.employeeSize")
				? getValue(userProductDocCtx.read("$.employeeSize").toString()) : getDelimeter());

		userResponse.getCustomAttributes().setHeadquarter(null != userProductDocCtx.read("$.headquarters")
				? getValue(userProductDocCtx.read("$.headquarters").toString()) : getDelimeter());

		userResponse.getCustomAttributes().setJobDescription(null != userProductDocCtx.read("$.jobDescription")
				? getValue(userProductDocCtx.read("$.jobDescription").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setJobFunction(null != userProductDocCtx.read("$.jobFunction")
				? getValue(userProductDocCtx.read("$.jobFunction").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setJobTitle(null != userProductDocCtx.read("$.title")
				? getValue(userProductDocCtx.read("$.title").toString()) : getDelimeter());

		userResponse.getCustomAttributes().setMarketSegment(null != userProductDocCtx.read("$.industrySegment")
				? getValue(userProductDocCtx.read("$.industrySegment").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setMarketServed(null != userProductDocCtx.read("$.industries")
				? getValue(userProductDocCtx.read("$.industries").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setMarketSubSegment(null != userProductDocCtx.read("$.industrySubSegment")
				? getValue(userProductDocCtx.read("$.industrySubSegment").toString()) : getDelimeter());

		/*
		 * userResponse.getCustomAttributes().setTaxIdentificationNumber(null !=
		 * userProductDocCtx.read("$.") ?
		 * getValue(userProductDocCtx.read("$.").toString()) : getDelimeter());
		 */

		// userResponse.getCustomAttributes().setTitle(null !=
		// userProductDocCtx.read("$.title")
		// ? getValue(userProductDocCtx.read("$.title").toString()) :
		// getDelimeter());

		userResponse.getCustomAttributes().setUserId(null != userProductDocCtx.read("$.uid")
				? getValue(userProductDocCtx.read("$.uid").toString()) : getDelimeter());

		userResponse.getCustomAttributes().setWorkPhone(null != userProductDocCtx.read("$.telephoneNumber")
				? getValue(userProductDocCtx.read("$.telephoneNumber").toString()) : getDelimeter());

		// companydetail end

		userResponse.getCustomAttributes().setDelegatedIdp(null != userProductDocCtx.read("$.delegatedIDP")
				? getValue(userProductDocCtx.read("$.delegatedIDP").toString()) : getDelimeter());

		userResponse.getCustomAttributes().setEmail(null != userProductDocCtx.read("$.mail")
				? getValue(userProductDocCtx.read("$.mail").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setEmailOptIn(null != userProductDocCtx.read("$.emailOptIn")
				? getValue(userProductDocCtx.read("$.emailOptIn").toString()) : getDelimeter());

		userResponse.getCustomAttributes().setFax(null != userProductDocCtx.read("$.fax")
				? getValue(userProductDocCtx.read("$.fax").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setFederatedId(null != userProductDocCtx.read("$.federationID")
				? getValue(userProductDocCtx.read("$.federationID").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setFirstName(null != userProductDocCtx.read("$.givenName")
				? getValue(userProductDocCtx.read("$.givenName").toString()) : getDelimeter());

		userResponse.getCustomAttributes().setHomePhone(null != userProductDocCtx.read("$.mobile")
				? getValue(userProductDocCtx.read("$.mobile").toString()) : getDelimeter());

		userResponse.getCustomAttributes().setIdentityType(null != userProductDocCtx.read("$.IDMSIdentityType__c")
				? getValue(userProductDocCtx.read("$.IDMSIdentityType__c").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setIdmsFederatedId(null != userProductDocCtx.read("$.federationID")
				? getValue(userProductDocCtx.read("$.federationID").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setIsInternal(null != userProductDocCtx.read("$.IDMSisInternal__c")
				? getValue(userProductDocCtx.read("$.IDMSisInternal__c").toString()) : getDelimeter());

		userResponse.getCustomAttributes().setLanguageCode(null != userProductDocCtx.read("$.preferredlanguage")
				? getValue(userProductDocCtx.read("$.preferredlanguage").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setLastName(null != userProductDocCtx.read("$.sn")
				? getValue(userProductDocCtx.read("$.sn").toString()) : getDelimeter());

		userResponse.getCustomAttributes().setMiddleName(null != userProductDocCtx.read("$.middleName")
				? getValue(userProductDocCtx.read("$.middleName").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setMobilePhone(null != userProductDocCtx.read("$.mobile")
				? getValue(userProductDocCtx.read("$.mobile").toString()) : getDelimeter());

		userResponse.getCustomAttributes().setProfileLastUpdateSource(null != userProductDocCtx.read("$.updateSource")
				? getValue(userProductDocCtx.read("$.updateSource").toString()) : getDelimeter());

		userResponse.getCustomAttributes().setRegistrationSource(null != userProductDocCtx.read("$.registerationSource")
				? getValue(userProductDocCtx.read("$.registerationSource").toString()) : getDelimeter());

		userResponse.getCustomAttributes().setSalutation(null != userProductDocCtx.read("$.initials")
				? getValue(userProductDocCtx.read("$.initials").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setStateOrProvinceCode(null != userProductDocCtx.read("$.st")
				? getValue(userProductDocCtx.read("$.st").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setStreet(null != userProductDocCtx.read("$.street")
				? getValue(userProductDocCtx.read("$.street").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setSuffix(null != userProductDocCtx.read("$.suffix")
				? getValue(userProductDocCtx.read("$.suffix").toString()) : getDelimeter());

		userResponse.getCustomAttributes().setUserContext(null != userProductDocCtx.read(JsonConstants.EMPLOYEE_TYPE)
				? getValue(userProductDocCtx.read(JsonConstants.EMPLOYEE_TYPE).toString()) : getDelimeter());

		userResponse.getCustomAttributes().setZipCode(null != userProductDocCtx.read("$.postalCode")
				? getValue(userProductDocCtx.read("$.postalCode").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setTrustedAdmin(null != userProductDocCtx.read("$.trustedAdmin")
				? getValue(userProductDocCtx.read("$.trustedAdmin").toString()) : getDelimeter());

		userResponse.getCustomAttributes().setTrustedAdmin(null != userProductDocCtx.read("$.isActivated")
				? getValue(userProductDocCtx.read("$.isActivated").toString()) : getDelimeter());

		userResponse.getCustomAttributes().setCompanyFederatedId(null != userProductDocCtx.read("$.companyFederatedID")
				? getValue(userProductDocCtx.read("$.companyFederatedID").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setCompanyPhone(null != userProductDocCtx.read("$.IDMSWorkPhone__c")
				? getValue(userProductDocCtx.read("$.IDMSWorkPhone__c").toString()) : getDelimeter());
		// mobile_reg and login_mobile field is set here for dual identifier
		// feature
		userResponse.getCustomAttributes().setLogin_mobile(null != userProductDocCtx.read("$.login_mobile")
				? getValue(userProductDocCtx.read("$.login_mobile").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setMobile_reg(null != userProductDocCtx.read("$.mobile_reg")
				? getValue(userProductDocCtx.read("$.mobile_reg").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setLoginid(null != userProductDocCtx.read("$.Loginid")
				? getValue(userProductDocCtx.read("$.Loginid").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setpOBox(null != userProductDocCtx.read("$.postOfficeBox")
				? getValue(userProductDocCtx.read("$.postOfficeBox").toString()) : getDelimeter());
		if(null == userResponse.getCustomAttributes().getLoginid() || userResponse.getCustomAttributes().getLoginid().isEmpty()){
			userResponse.getCustomAttributes().setLoginid(null != userProductDocCtx.read("$.loginid")
					? getValue(userProductDocCtx.read("$.loginid").toString()) : getDelimeter());
		}
	}

	public void parseValuesByOauthHomeContext(GetUserHomeByOauthResponse userResponse,
			DocumentContext userProductDocCtx) {

		userResponse.setUserId(null != userProductDocCtx.read("$.uid")
				? getValue(userProductDocCtx.read("$.uid").toString()) : getDelimeter());

		IFWCustomAttributesForHome customAttributes = new IFWCustomAttributesForHome();
		userResponse.setCustomAttributes(customAttributes);
		userResponse.getCustomAttributes().setAccountId(null != userProductDocCtx.read("$.uid")
				? getValue(userProductDocCtx.read("$.uid").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setAdditionalAddress(null != userProductDocCtx.read("$.additionalInfo")
				? getValue(userProductDocCtx.read("$.additionalInfo").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setAil(null != userProductDocCtx.read("$.IDMSAil_c")
				? getValue(userProductDocCtx.read("$.IDMSAil_c").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setAilApplications(null != userProductDocCtx.read("$.IDMSAIL_Applications_c")
				? getValue(userProductDocCtx.read("$.IDMSAIL_Applications_c").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setAilFeatures(null != userProductDocCtx.read("$.IDMSAIL_Features_c")
				? getValue(userProductDocCtx.read("$.IDMSAIL_Features_c").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setAilPrograms(null != userProductDocCtx.read("$.IDMSAIL_Programs_c")
				? getValue(userProductDocCtx.read("$.IDMSAIL_Programs_c").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setCity(null != userProductDocCtx.read("$.l")
				? getValue(userProductDocCtx.read("$.l").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setContactId(null != userProductDocCtx.read("$.uid")
				? getValue(userProductDocCtx.read("$.uid").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setCountryCode(null != userProductDocCtx.read("$.c")
				? getValue(userProductDocCtx.read("$.c").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setCounty(null != userProductDocCtx.read("$.county")
				? getValue(userProductDocCtx.read("$.county").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setDelegatedIdp(null != userProductDocCtx.read("$.delegatedIDP")
				? getValue(userProductDocCtx.read("$.delegatedIDP").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setEmail(null != userProductDocCtx.read("$.mail")
				? getValue(userProductDocCtx.read("$.mail").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setEmailOptIn(null != userProductDocCtx.read("$.emailOptIn")
				? getValue(userProductDocCtx.read("$.emailOptIn").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setFax(null != userProductDocCtx.read("$.fax")
				? getValue(userProductDocCtx.read("$.fax").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setFederatedId(null != userProductDocCtx.read("$.federationID")
				? getValue(userProductDocCtx.read("$.federationID").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setFirstName(null != userProductDocCtx.read("$.givenName")
				? getValue(userProductDocCtx.read("$.givenName").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setIdentityType(null != userProductDocCtx.read("$.IDMSIdentityType__c")
				? getValue(userProductDocCtx.read("$.IDMSIdentityType__c").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setIdmsFederatedId(null != userProductDocCtx.read("$.federationID")
				? getValue(userProductDocCtx.read("$.federationID").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setIsInternal(null != userProductDocCtx.read("$.IDMSisInternal__c")
				? getValue(userProductDocCtx.read("$.IDMSisInternal__c").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setLanguageCode(null != userProductDocCtx.read("$.preferredlanguage")
				? getValue(userProductDocCtx.read("$.preferredlanguage").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setLastName(null != userProductDocCtx.read("$.sn")
				? getValue(userProductDocCtx.read("$.sn").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setMiddleName(null != userProductDocCtx.read("$.middleName")
				? getValue(userProductDocCtx.read("$.middleName").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setMobilePhone(null != userProductDocCtx.read("$.mobile")
				? getValue(userProductDocCtx.read("$.mobile").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setHomePhone(null != userProductDocCtx.read("$.telephoneNumber")
				? getValue(userProductDocCtx.read("$.telephoneNumber").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setpOBox(null != userProductDocCtx.read("$.postOfficeBox")
				? getValue(userProductDocCtx.read("$.postOfficeBox").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setProfileLastUpdateSource(null != userProductDocCtx.read("$.updateSource")
				? getValue(userProductDocCtx.read("$.updateSource").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setRegistrationSource(null != userProductDocCtx.read("$.registerationSource")
				? getValue(userProductDocCtx.read("$.registerationSource").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setSalutation(null != userProductDocCtx.read("$.initials")
				? getValue(userProductDocCtx.read("$.initials").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setStateOrProvinceCode(null != userProductDocCtx.read("$.st")
				? getValue(userProductDocCtx.read("$.st").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setStreet(null != userProductDocCtx.read("$.st")
				? getValue(userProductDocCtx.read("$.st").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setSuffix(null != userProductDocCtx.read("$.suffix")
				? getValue(userProductDocCtx.read("$.suffix").toString()) : getDelimeter());

		/*
		 * userResponse.getCustomAttributes().setTrustLevel(null !=
		 * userProductDocCtx.read("$.") ?
		 * getValue(userProductDocCtx.read("$.").toString()) : getDelimeter());
		 * userResponse.getCustomAttributes().setTrustStatus(null !=
		 * userProductDocCtx.read("$.") ?
		 * getValue(userProductDocCtx.read("$.").toString()) : getDelimeter());
		 */

		userResponse.getCustomAttributes().setUserContext(null != userProductDocCtx.read(JsonConstants.EMPLOYEE_TYPE)
				? getValue(userProductDocCtx.read(JsonConstants.EMPLOYEE_TYPE).toString()) : getDelimeter());
		userResponse.getCustomAttributes().setUserId(null != userProductDocCtx.read("$.uid")
				? getValue(userProductDocCtx.read("$.uid").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setZipCode(null != userProductDocCtx.read("$.postalCode")
				? getValue(userProductDocCtx.read("$.postalCode").toString()) : getDelimeter());

		userResponse.getCustomAttributes().setTrustedAdmin(null != userProductDocCtx.read("$.trustedAdmin")
				? getValue(userProductDocCtx.read("$.trustedAdmin").toString()) : getDelimeter());

		userResponse.getCustomAttributes().setTrustedAdmin(null != userProductDocCtx.read("$.isActivated")
				? getValue(userProductDocCtx.read("$.isActivated").toString()) : getDelimeter());

		// mobile_reg and login_mobile field is set here for dual identifier
		// feature
		userResponse.getCustomAttributes().setLogin_mobile(null != userProductDocCtx.read("$.login_mobile")
				? getValue(userProductDocCtx.read("$.login_mobile").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setMobile_reg(null != userProductDocCtx.read("$.mobile_reg")
				? getValue(userProductDocCtx.read("$.mobile_reg").toString()) : getDelimeter());
		userResponse.getCustomAttributes().setLoginid(null != userProductDocCtx.read("$.Loginid")
				? getValue(userProductDocCtx.read("$.Loginid").toString()) : getDelimeter());
		if(null == userResponse.getCustomAttributes().getLoginid() || userResponse.getCustomAttributes().getLoginid().isEmpty()){
			userResponse.getCustomAttributes().setLoginid(null != userProductDocCtx.read("$.loginid")
					? getValue(userProductDocCtx.read("$.loginid").toString()) : getDelimeter());
		}
	}

	public void parseValuesHomeContext(OpenAMGetUserHomeResponse userResponse, DocumentContext userProductDocCtx) {

		userResponse.setUsername(null != userProductDocCtx.read("$.Loginid")
				? getValue(userProductDocCtx.read("$.Loginid").toString()) : getDelimeter());
		userResponse.setLoginid(null != userProductDocCtx.read("$.Loginid")
				? getValue(userProductDocCtx.read("$.Loginid").toString()) : getDelimeter());
		if(null == userResponse.getLoginid() || userResponse.getLoginid().isEmpty()){
			userResponse.setLoginid(null != userProductDocCtx.read("$.loginid")
					? getValue(userProductDocCtx.read("$.loginid").toString()) : getDelimeter());
		}
		// Changes done to get 2FA details
		if (StringUtils.isBlank(userResponse.getPrefCommnMethod())) {
			userResponse.setPrefCommnMethod(null != userProductDocCtx.read("$.preferredCommunication")
					? getValue(userProductDocCtx.read("$.preferredCommunication").toString())
					: getDelimeter());
		}
		if (StringUtils.isBlank(userResponse.getFirstTimeUser())) {
			userResponse.setFirstTimeUser(null != userProductDocCtx.read("$.isFirstTimeUser")
					? getValue(userProductDocCtx.read("$.isFirstTimeUser").toString())
					: getDelimeter());
		}
		if (StringUtils.isBlank(userResponse.get_2FAEnabled())) {
			userResponse.set_2FAEnabled(null != userProductDocCtx.read("$.is2FAEnabled")
					? getValue(userProductDocCtx.read("$.is2FAEnabled").toString())
					: getDelimeter());
		}
		// Changes end to get 2FA details
		// Changes done to set social ids in userinfo call
		if(StringUtils.isBlank(userResponse.getAppleid())){
			userResponse.setAppleid(null != userProductDocCtx.read("$.appleid")
					? getValue(userProductDocCtx.read("$.appleid").toString()) : getDelimeter());
		}
		if(StringUtils.isBlank(userResponse.getQqid())){
			userResponse.setQqid(null != userProductDocCtx.read("$.qqid")
					? getValue(userProductDocCtx.read("$.qqid").toString()) : getDelimeter());
		}
		if(StringUtils.isBlank(userResponse.getWeiboid())){
			userResponse.setWeiboid(null != userProductDocCtx.read("$.weiboid")
					? getValue(userProductDocCtx.read("$.weiboid").toString()) : getDelimeter());
		}
		if(StringUtils.isBlank(userResponse.getWechatpublicid())){
			userResponse.setWechatpublicid(null != userProductDocCtx.read("$.wechatpublicid")
					? getValue(userProductDocCtx.read("$.wechatpublicid").toString()) : getDelimeter());
		}
		if(StringUtils.isBlank(userResponse.getLinkedinID())){
			userResponse.setLinkedinID(null != userProductDocCtx.read("$.linkedinID")
					? getValue(userProductDocCtx.read("$.linkedinID").toString()) : getDelimeter());
		}
		// Changes end to set social ids in userinfo call

		String emailValue = null != userProductDocCtx.read("$.mail")
				? getValue(userProductDocCtx.read("$.mail").toString()) : getDelimeter();
		/*
		 * String newmail= null != userProductDocCtx.read("$.newmail") ?
		 * getValue(userProductDocCtx.read("$.newmail").toString()) :
		 * getDelimeter(); if(null != newmail && !newmail.isEmpty()){
		 * userResponse.setEmail(newmail); }else{
		 * userResponse.setEmail(emailValue); }
		 */
		userResponse.setEmail(emailValue);
		String firstNameValue = null != userProductDocCtx.read("$.givenName")
				? getValue(userProductDocCtx.read("$.givenName").toString()) : getDelimeter();
		userResponse.setFirstName(firstNameValue);
		String lastNameValue = null != userProductDocCtx.read("$.sn")
				? getValue(userProductDocCtx.read("$.sn").toString()) : getDelimeter();
		userResponse.setLastName(lastNameValue);
		String mobileValue = null != userProductDocCtx.read("$.mobile")
				? getValue(userProductDocCtx.read("$.mobile").toString()) : getDelimeter();
		userResponse.setMobilePhone(mobileValue);
		userResponse.setIDMSTaxIdentificationNumber__c(null != userProductDocCtx.read("$.taxID")
				? getValue(userProductDocCtx.read("$.taxID").toString()) : getDelimeter());

		userResponse.setFederationIdentifier(null != userProductDocCtx.read("$.federationID")
				? getValue(userProductDocCtx.read("$.federationID").toString()) : getDelimeter());

		userResponse.setTitle(null != userProductDocCtx.read("$.title")
				? getValue(userProductDocCtx.read("$.title").toString()) : getDelimeter());
		userResponse.setIDMSClassLevel1__c(null != userProductDocCtx.read("$.iam1")
				? getValue(userProductDocCtx.read("$.iam1").toString()) : getDelimeter());
		String emailOptInValue = null != userProductDocCtx.read("$.emailOptIn")
				? getValue(userProductDocCtx.read("$.emailOptIn").toString()) : getDelimeter();
		userResponse.setIDMS_Email_opt_in__c(emailOptInValue);
		String employeeTypeValue = null != userProductDocCtx.read(JsonConstants.EMPLOYEE_TYPE)
				? getValue(userProductDocCtx.read(JsonConstants.EMPLOYEE_TYPE).toString()) : getDelimeter();
		userResponse.setIDMS_User_Context__c(employeeTypeValue);
		String countryValue = null != userProductDocCtx.read("$.c") ? getValue(userProductDocCtx.read("$.c").toString())
				: getDelimeter();
		userResponse.setCountry(countryValue);
		userResponse.setIDMS_PreferredLanguage__c(null != userProductDocCtx.read("$.preferredlanguage")
				? getValue(userProductDocCtx.read("$.preferredlanguage").toString()) : getDelimeter());
		userResponse.setIDMS_Company_Currency_ISO_Code__c(null != userProductDocCtx.read("$.currency")
				? getValue(userProductDocCtx.read("$.currency").toString()) : getDelimeter());

		userResponse.setStreet(null != userProductDocCtx.read("$.street")
				? getValue(userProductDocCtx.read("$.street").toString()) : getDelimeter());
		userResponse.setCity(null != userProductDocCtx.read("$.l") ? getValue(userProductDocCtx.read("$.l").toString())
				: getDelimeter());
		userResponse.setPostalCode(null != userProductDocCtx.read("$.postalCode")
				? getValue(userProductDocCtx.read("$.postalCode").toString()) : getDelimeter());

		userResponse.setState(null != userProductDocCtx.read("$.st")
				? getValue(userProductDocCtx.read("$.st").toString()) : getDelimeter());
		userResponse.setIDMS_POBox__c(null != userProductDocCtx.read("$.postOfficeBox")
				? getValue(userProductDocCtx.read("$.postOfficeBox").toString()) : getDelimeter());
		userResponse.setIDMS_Registration_Source__c(null != userProductDocCtx.read("$.registerationSource")
				? getValue(userProductDocCtx.read("$.registerationSource").toString()) : getDelimeter());
		userResponse.setIDMS_AdditionalAddress__c(null != userProductDocCtx.read("$.additionalInfo")
				? getValue(userProductDocCtx.read("$.additionalInfo").toString()) : getDelimeter());
		userResponse.setIDMSMiddleName__c(null != userProductDocCtx.read("$.middleName")
				? getValue(userProductDocCtx.read("$.middleName").toString()) : getDelimeter());
		userResponse.setIDMSSalutation__c(null != userProductDocCtx.read("$.initials")
				? getValue(userProductDocCtx.read("$.initials").toString()) : getDelimeter());
		userResponse.setIDMSSuffix__c(null != userProductDocCtx.read("$.suffix")
				? getValue(userProductDocCtx.read("$.suffix").toString()) : getDelimeter());
		userResponse.setFax(null != userProductDocCtx.read("$.fax")
				? getValue(userProductDocCtx.read("$.fax").toString()) : getDelimeter());
		userResponse.setIDMSDelegatedIdp__c(null != userProductDocCtx.read("$.delegatedIDP")
				? getValue(userProductDocCtx.read("$.delegatedIDP").toString()) : getDelimeter());
		userResponse.setId(null != userProductDocCtx.read(JsonConstants.USER_NAME)
				? getValue(userProductDocCtx.read(JsonConstants.USER_NAME).toString()) : getDelimeter());

		userResponse.setDefaultCurrencyIsoCode(null != userProductDocCtx.read("$.currency")
				? getValue(userProductDocCtx.read("$.currency").toString()) : getDelimeter());
		userResponse.setCurrencyCode(null != userProductDocCtx.read("$.currency")
				? getValue(userProductDocCtx.read("$.currency").toString()) : getDelimeter());

		userResponse.setCurrencyCode(null != userProductDocCtx.read("$.currency")
				? getValue(userProductDocCtx.read("$.currency").toString()) : getDelimeter());

		userResponse.setIDMS_Registration_Source__c(null != userProductDocCtx.read("$.registerationSource")
				? getValue(userProductDocCtx.read("$.registerationSource").toString()) : getDelimeter());
		userResponse.setIDMS_Profile_update_source__c(null != userProductDocCtx.read("$.updateSource")
				? getValue(userProductDocCtx.read("$.updateSource").toString()) : getDelimeter());

		// common code ends here

		String homePhoneValue = null != userProductDocCtx.read("$.telephoneNumber")
				? getValue(userProductDocCtx.read("$.telephoneNumber").toString()) : null;
		userResponse.setPhone(homePhoneValue);
		userResponse.setIDMS_County__c(null != userProductDocCtx.read("$.county")
				? getValue(userProductDocCtx.read("$.county").toString()) : getDelimeter());
		userResponse.setIDMSClassLevel1__c(null != userProductDocCtx.read("$.iam1")
				? getValue(userProductDocCtx.read("$.iam1").toString()) : getDelimeter());

		userResponse.setTncFlag(null != userProductDocCtx.read("$.tncFlag")
				? getValue(userProductDocCtx.read("$.tncFlag").toString()) : getDelimeter());

		// ail update properties

		userResponse.setContactId(null != userProductDocCtx.read("$.contactId")
				? getValue(userProductDocCtx.read("$.contactId").toString()) : getDelimeter());

		userResponse.setAccountId(null != userProductDocCtx.read("$.bfoAccountId")
				? getValue(userProductDocCtx.read("$.bfoAccountId").toString()) : getDelimeter());

		userResponse.setIDMS_federatedidentity__c(null != userProductDocCtx.read("$.uid")
				? getValue(userProductDocCtx.read("$.uid").toString()) : getDelimeter());

		userResponse.setIDMSDelegatedIdp__c(null != userProductDocCtx.read("$.IDMSDelegatedIdp__c")
				? getValue(userProductDocCtx.read("$.IDMSDelegatedIdp__c").toString()) : getDelimeter());

		userResponse.setIDMSIdentityType__c(null != userProductDocCtx.read("$.IDMSIdentityType__c")
				? getValue(userProductDocCtx.read("$.IDMSIdentityType__c").toString()) : getDelimeter());

		userResponse.setIDMSisInternal__c(null != userProductDocCtx.read("$.IDMSisInternal__c")
				? getValue(userProductDocCtx.read("$.IDMSisInternal__c").toString()) : getDelimeter());

		userResponse.setIDMSAil__c(null != userProductDocCtx.read("$.IDMSAil_c")
				? getValue(userProductDocCtx.read("$.IDMSAil_c").toString()) : getDelimeter());

		userResponse.setIDMSAIL_Applications__c(null != userProductDocCtx.read("$.IDMSAIL_Applications_c")
				? getValue(userProductDocCtx.read("$.IDMSAIL_Applications_c").toString()) : getDelimeter());

		userResponse.setIDMSAIL_Features__c(null != userProductDocCtx.read("$.IDMSAIL_Features_c")
				? getValue(userProductDocCtx.read("$.IDMSAIL_Features_c").toString()) : getDelimeter());

		userResponse.setIDMSAIL_Programs__c(null != userProductDocCtx.read("$.IDMSAIL_Programs_c")
				? getValue(userProductDocCtx.read("$.IDMSAIL_Programs_c").toString()) : getDelimeter());

		userResponse.setIDMSPrimaryContact__c(null != userProductDocCtx.read("$.primaryContact")
				? getValue(userProductDocCtx.read("$.primaryContact").toString()) : getDelimeter());

		userResponse.setAboutMe(null != userProductDocCtx.read("$.AboutMe")
				? getValue(userProductDocCtx.read("$.AboutMe").toString()) : getDelimeter());

		userResponse.setTrustedAdmin(null != userProductDocCtx.read("$.trustedAdmin")
				? getValue(userProductDocCtx.read("$.trustedAdmin").toString()) : getDelimeter());

		userResponse.setIsActivated(null != userProductDocCtx.read("$.isActivated")
				? getValue(userProductDocCtx.read("$.isActivated").toString()) : getDelimeter());

		userResponse.setIDMS_Federated_ID__c(null != userProductDocCtx.read("$.federationID")
				? getValue(userProductDocCtx.read("$.federationID").toString()) : getDelimeter());

		// mobile_reg and login_mobile field is set here for dual identifier
		// feature
		userResponse.setLogin_mobile(null != userProductDocCtx.read("$.login_mobile")
				? getValue(userProductDocCtx.read("$.login_mobile").toString()) : getDelimeter());
		userResponse.setMobile_reg(null != userProductDocCtx.read("$.mobile_reg")
				? getValue(userProductDocCtx.read("$.mobile_reg").toString()) : getDelimeter());

	}

	public void parseValuesWorkContext(OpenAMGetUserWorkResponse userResponse, DocumentContext userProductDocCtx) {

		userResponse.setCompanyFederatedID(null != userProductDocCtx.read("$.companyFederatedID")
				? getValue(userProductDocCtx.read("$.companyFederatedID").toString()) : getDelimeter());
		userResponse.setUsername(null != userProductDocCtx.read("$.loginid")
				? getValue(userProductDocCtx.read("$.loginid").toString()) : getDelimeter());
		userResponse.setLoginid(null != userProductDocCtx.read("$.Loginid")
				? getValue(userProductDocCtx.read("$.Loginid").toString()) : getDelimeter());
		if(null == userResponse.getLoginid() || userResponse.getLoginid().isEmpty()){
			userResponse.setLoginid(null != userProductDocCtx.read("$.loginid")
					? getValue(userProductDocCtx.read("$.loginid").toString()) : getDelimeter());
		}
		// Changes done to get 2FA details
		if(StringUtils.isBlank(userResponse.getPrefCommnMethod())){
			userResponse.setPrefCommnMethod(null != userProductDocCtx.read("$.preferredCommunication")
							? getValue(userProductDocCtx.read("$.preferredCommunication").toString()) : getDelimeter());
		}
		if(StringUtils.isBlank(userResponse.getFirstTimeUser())){
			userResponse.setFirstTimeUser(null != userProductDocCtx.read("$.isFirstTimeUser")
							? getValue(userProductDocCtx.read("$.isFirstTimeUser").toString()) : getDelimeter());
		}
		if(StringUtils.isBlank(userResponse.get_2FAEnabled())){
			userResponse.set_2FAEnabled(null != userProductDocCtx.read("$.is2FAEnabled")
							? getValue(userProductDocCtx.read("$.is2FAEnabled").toString()) : getDelimeter());
		}
		// Changes end to get 2FA details

		// Changes done to set social ids in userinfo call
		if (StringUtils.isBlank(userResponse.getAppleid())) {
			userResponse.setAppleid(null != userProductDocCtx.read("$.appleid")
					? getValue(userProductDocCtx.read("$.appleid").toString()) : getDelimeter());
		}
		if (StringUtils.isBlank(userResponse.getQqid())) {
			userResponse.setQqid(null != userProductDocCtx.read("$.qqid")
					? getValue(userProductDocCtx.read("$.qqid").toString()) : getDelimeter());
		}
		if (StringUtils.isBlank(userResponse.getWeiboid())) {
			userResponse.setWeiboid(null != userProductDocCtx.read("$.weiboid")
					? getValue(userProductDocCtx.read("$.weiboid").toString()) : getDelimeter());
		}
		if (StringUtils.isBlank(userResponse.getWechatpublicid())) {
			userResponse.setWechatpublicid(null != userProductDocCtx.read("$.wechatpublicid")
					? getValue(userProductDocCtx.read("$.wechatpublicid").toString()) : getDelimeter());
		}
		if (StringUtils.isBlank(userResponse.getLinkedinID())) {
			userResponse.setLinkedinID(null != userProductDocCtx.read("$.linkedinID")
					? getValue(userProductDocCtx.read("$.linkedinID").toString()) : getDelimeter());
		}
		// Changes end to set social ids in userinfo call
		String emailValue = null != userProductDocCtx.read("$.mail")
				? getValue(userProductDocCtx.read("$.mail").toString()) : getDelimeter();

		/*
		 * String newmail = null != userProductDocCtx.read("$.newmail") ?
		 * getValue(userProductDocCtx.read("$.newmail").toString()) :
		 * getDelimeter(); if (null != newmail && !newmail.isEmpty()) {
		 * userResponse.setEmail(newmail); } else {
		 * userResponse.setEmail(emailValue); }
		 */
		userResponse.setEmail(emailValue);
		String firstNameValue = null != userProductDocCtx.read("$.givenName")
				? getValue(userProductDocCtx.read("$.givenName").toString()) : getDelimeter();
		userResponse.setFirstName(firstNameValue);
		String lastNameValue = null != userProductDocCtx.read("$.sn")
				? getValue(userProductDocCtx.read("$.sn").toString()) : getDelimeter();
		userResponse.setLastName(lastNameValue);
		String mobileValue = null != userProductDocCtx.read("$.mobile")
				? getValue(userProductDocCtx.read("$.mobile").toString()) : getDelimeter();
		userResponse.setMobilePhone(mobileValue);
		String workPhoneValue = null != userProductDocCtx.read("$.telephoneNumber")
				? getValue(userProductDocCtx.read("$.telephoneNumber").toString()) : null;
		userResponse.setPhone(workPhoneValue);
		String emailOptInValue = null != userProductDocCtx.read("$.emailOptIn")
				? getValue(userProductDocCtx.read("$.emailOptIn").toString()) : getDelimeter();
		userResponse.setIDMS_Email_opt_in__c(emailOptInValue);
		String employeeTypeValue = null != userProductDocCtx.read(JsonConstants.EMPLOYEE_TYPE)
				? getValue(userProductDocCtx.read(JsonConstants.EMPLOYEE_TYPE).toString()) : getDelimeter();
		userResponse.setIDMS_User_Context__c(employeeTypeValue);
		String countryValue = null != userProductDocCtx.read("$.c") ? getValue(userProductDocCtx.read("$.c").toString())
				: getDelimeter();
		userResponse.setCountry(countryValue);
		userResponse.setIDMS_PreferredLanguage__c(null != userProductDocCtx.read("$.preferredlanguage")
				? getValue(userProductDocCtx.read("$.preferredlanguage").toString()) : getDelimeter());

		userResponse.setDefaultCurrencyIsoCode(null != userProductDocCtx.read("$.currency")
				? getValue(userProductDocCtx.read("$.currency").toString()) : getDelimeter());
		userResponse.setCurrencyCode(null != userProductDocCtx.read("$.currency")
				? getValue(userProductDocCtx.read("$.currency").toString()) : getDelimeter());

		userResponse.setIDMS_Company_Currency_ISO_Code__c(null != userProductDocCtx.read("$.currency")
				? getValue(userProductDocCtx.read("$.currency").toString()) : getDelimeter());
		userResponse.setStreet(null != userProductDocCtx.read("$.street")
				? getValue(userProductDocCtx.read("$.street").toString()) : getDelimeter());
		userResponse.setCity(null != userProductDocCtx.read("$.l") ? getValue(userProductDocCtx.read("$.l").toString())
				: getDelimeter());
		userResponse.setPostalCode(null != userProductDocCtx.read("$.postalCode")
				? getValue(userProductDocCtx.read("$.postalCode").toString()) : getDelimeter());
		userResponse.setState(null != userProductDocCtx.read("$.st")
				? getValue(userProductDocCtx.read("$.st").toString()) : getDelimeter());
		userResponse.setCompany_State__c(null != userProductDocCtx.read("$.st")
				? getValue(userProductDocCtx.read("$.st").toString()) : getDelimeter());
		userResponse.setIDMS_POBox__c(null != userProductDocCtx.read("$.postOfficeBox")
				? getValue(userProductDocCtx.read("$.postOfficeBox").toString()) : getDelimeter());
		userResponse.setIDMS_Registration_Source__c(null != userProductDocCtx.read("$.registerationSource")
				? getValue(userProductDocCtx.read("$.registerationSource").toString()) : getDelimeter());
		userResponse.setIDMS_Profile_update_source__c(null != userProductDocCtx.read("$.updateSource")
				? getValue(userProductDocCtx.read("$.updateSource").toString()) : getDelimeter());
		userResponse.setIDMS_AdditionalAddress__c(null != userProductDocCtx.read("$.additionalInfo")
				? getValue(userProductDocCtx.read("$.additionalInfo").toString()) : getDelimeter());
		userResponse.setIDMSMiddleName__c(null != userProductDocCtx.read("$.middleName")
				? getValue(userProductDocCtx.read("$.middleName").toString()) : getDelimeter());
		userResponse.setIDMSSalutation__c(null != userProductDocCtx.read("$.initials")
				? getValue(userProductDocCtx.read("$.initials").toString()) : getDelimeter());
		userResponse.setIDMSSuffix__c(null != userProductDocCtx.read("$.suffix")
				? getValue(userProductDocCtx.read("$.suffix").toString()) : getDelimeter());
		userResponse.setFax(null != userProductDocCtx.read("$.fax")
				? getValue(userProductDocCtx.read("$.fax").toString()) : getDelimeter());
		userResponse.setIDMSDelegatedIdp__c(null != userProductDocCtx.read("$.delegatedIDP")
				? getValue(userProductDocCtx.read("$.delegatedIDP").toString()) : getDelimeter());

		// common code ends here

		userResponse.setCompanyName(null != userProductDocCtx.read("$.companyName")
				? getValue(userProductDocCtx.read("$.companyName").toString()) : getDelimeter());

		userResponse.setCompanyName_c(null != userProductDocCtx.read("$.companyName")
				? getValue(userProductDocCtx.read("$.companyName").toString()) : getDelimeter());
		userResponse.setCompany_Address1__c(null != userProductDocCtx.read("$.companyStreet")
				? getValue(userProductDocCtx.read("$.companyStreet").toString()) : getDelimeter());
		userResponse.setCompany_City__c(null != userProductDocCtx.read("$.companyCity")
				? getValue(userProductDocCtx.read("$.companyCity").toString()) : getDelimeter());
		userResponse.setCompany_City_c(null != userProductDocCtx.read("$.companyCity")
				? getValue(userProductDocCtx.read("$.companyCity").toString()) : getDelimeter());
		userResponse.setCompany_Postal_Code__c(null != userProductDocCtx.read("$.companyPostalCode")
				? getValue(userProductDocCtx.read("$.companyPostalCode").toString()) : getDelimeter());
		userResponse.setCompany_Postal_Code_c(null != userProductDocCtx.read("$.companyPostalCode")
				? getValue(userProductDocCtx.read("$.companyPostalCode").toString()) : getDelimeter());
		userResponse.setCompany_State__c(null != userProductDocCtx.read("$.companyState")
				? getValue(userProductDocCtx.read("$.companyState").toString()) : getDelimeter());
		userResponse.setCompany_State_c(null != userProductDocCtx.read("$.companyState")
				? getValue(userProductDocCtx.read("$.companyState").toString()) : getDelimeter());

		userResponse.setIDMS_Company_Address_PO_BOX__c(null != userProductDocCtx.read("$.companyPostOfficeBox")
				? getValue(userProductDocCtx.read("$.companyPostOfficeBox").toString()) : getDelimeter());
		userResponse.setCompany_Country__c(null != userProductDocCtx.read("$.companyCountry")
				? getValue(userProductDocCtx.read("$.companyCountry").toString()) : getDelimeter());
		userResponse.setCompany_Country_c(null != userProductDocCtx.read("$.companyCountry")
				? getValue(userProductDocCtx.read("$.companyCountry").toString()) : getDelimeter());
		userResponse.setCompany_Address2__c(null != userProductDocCtx.read("$.companyAdditionalInfo")
				? getValue(userProductDocCtx.read("$.companyAdditionalInfo").toString()) : getDelimeter());
		userResponse.setCompany_Address2_c(null != userProductDocCtx.read("$.companyAdditionalInfo")
				? getValue(userProductDocCtx.read("$.companyAdditionalInfo").toString()) : getDelimeter());
		
		userResponse.setIDMSClassLevel1__c(null != userProductDocCtx.read("$.iam1")
				? getValue(userProductDocCtx.read("$.iam1").toString()) : getDelimeter());
		userResponse.setIDMSClassLevel1_c(null != userProductDocCtx.read("$.iam1")
				? getValue(userProductDocCtx.read("$.iam1").toString()) : getDelimeter());
		userResponse.setIDMSClassLevel2__c(null != userProductDocCtx.read("$.iam2")
				? getValue(userProductDocCtx.read("$.iam2").toString()) : getDelimeter());
		userResponse.setIDMSClassLevel2_c(null != userProductDocCtx.read("$.iam2")
				? getValue(userProductDocCtx.read("$.iam2").toString()) : getDelimeter());
		userResponse.setIDMSMarketSegment__c(null != userProductDocCtx.read("$.industrySegment")
				? getValue(userProductDocCtx.read("$.industrySegment").toString()) : getDelimeter());
		userResponse.setIDMSMarketSubSegment__c(null != userProductDocCtx.read("$.industrySubSegment")
				? getValue(userProductDocCtx.read("$.industrySubSegment").toString()) : getDelimeter());
		userResponse.setJob_Title__c(null != userProductDocCtx.read("$.title")
				? getValue(userProductDocCtx.read("$.title").toString()) : getDelimeter());

		userResponse.setTitle(null != userProductDocCtx.read("$.title")
				? getValue(userProductDocCtx.read("$.title").toString()) : getDelimeter());

		userResponse.setJob_Function__c(null != userProductDocCtx.read("$.jobFunction")
				? getValue(userProductDocCtx.read("$.jobFunction").toString()) : getDelimeter());
		userResponse.setIDMSJobDescription__c(null != userProductDocCtx.read("$.jobDescription")
				? getValue(userProductDocCtx.read("$.jobDescription").toString()) : getDelimeter());
		userResponse.setIDMSCompanyMarketServed__c(null != userProductDocCtx.read("$.industries")
				? getValue(userProductDocCtx.read("$.industries").toString()) : getDelimeter());
		userResponse.setIDMSCompanyNbrEmployees__c(null != userProductDocCtx.read("$.employeeSize")
				? getValue(userProductDocCtx.read("$.employeeSize").toString()) : getDelimeter());
		userResponse.setIDMSCompanyNbrEmployees_c(null != userProductDocCtx.read("$.employeeSize")
				? getValue(userProductDocCtx.read("$.employeeSize").toString()) : getDelimeter());
		userResponse.setIDMSCompanyHeadquarters__c(null != userProductDocCtx.read("$.headquarters")
				? getValue(userProductDocCtx.read("$.headquarters").toString()) : getDelimeter());
		userResponse.setIDMSCompanyHeadquarters_c(null != userProductDocCtx.read("$.headquarters")
				? getValue(userProductDocCtx.read("$.headquarters").toString()) : getDelimeter());
		userResponse.setIDMSAnnualRevenue__c(null != userProductDocCtx.read("$.annualRevenue")
				? getValue(userProductDocCtx.read("$.annualRevenue").toString()) : getDelimeter());
		userResponse.setIDMSTaxIdentificationNumber__c(null != userProductDocCtx.read("$.taxID")
				? getValue(userProductDocCtx.read("$.taxID").toString()) : getDelimeter());
		userResponse.setIDMSTaxIdentificationNumber_c(null != userProductDocCtx.read("$.taxID")
				? getValue(userProductDocCtx.read("$.taxID").toString()) : getDelimeter());
		userResponse.setCompany_Website__c(null != userProductDocCtx.read("$.companyWebSite")
				? getValue(userProductDocCtx.read("$.companyWebSite").toString()) : getDelimeter());
		userResponse.setCompany_Website_c(null != userProductDocCtx.read("$.companyWebSite")
				? getValue(userProductDocCtx.read("$.companyWebSite").toString()) : getDelimeter());

		userResponse.setDepartment(null != userProductDocCtx.read("$.departmentNumber")
				? getValue(userProductDocCtx.read("$.departmentNumber").toString()) : getDelimeter());
		userResponse.setIDMSCompanyCounty__c(null != userProductDocCtx.read("$.companyCounty")
				? getValue(userProductDocCtx.read("$.companyCounty").toString()) : getDelimeter());
		userResponse.setIDMSCompanyCounty_c(null != userProductDocCtx.read("$.companyCounty")
				? getValue(userProductDocCtx.read("$.companyCounty").toString()) : getDelimeter());

		userResponse.setIDMS_County__c(null != userProductDocCtx.read("$.county")
				? getValue(userProductDocCtx.read("$.county").toString()) : getDelimeter());
		userResponse.setId(null != userProductDocCtx.read(JsonConstants.USER_NAME)
				? getValue(userProductDocCtx.read(JsonConstants.USER_NAME).toString()) : getDelimeter());

		userResponse.setTncFlag(null != userProductDocCtx.read("$.tncFlag")
				? getValue(userProductDocCtx.read("$.tncFlag").toString()) : getDelimeter());
		userResponse.setIDMSIdentityType__c(null != userProductDocCtx.read("$.identityType")
				? getValue(userProductDocCtx.read("$.identityType").toString()) : getDelimeter());

		userResponse.setIDMSCompanyPoBox__c(null != userProductDocCtx.read("$.postOfficeBox")
				? getValue(userProductDocCtx.read("$.postOfficeBox").toString()) : getDelimeter());

		// ail update properties

		userResponse.setContactId(null != userProductDocCtx.read("$.contactId")
				? getValue(userProductDocCtx.read("$.contactId").toString()) : getDelimeter());

		/*
		 * userResponse.setAccountId(null != userProductDocCtx.read("$.uid") ?
		 * getValue(userProductDocCtx.read("$.uid").toString()) :
		 * getDelimeter());
		 */

		userResponse.setIDMS_Federated_ID__c(null != userProductDocCtx.read("$.federationID")
				? getValue(userProductDocCtx.read("$.federationID").toString()) : getDelimeter());

		userResponse.setIDMS_federatedidentity__c(null != userProductDocCtx.read("$.uid")
				? getValue(userProductDocCtx.read("$.uid").toString()) : getDelimeter());

		userResponse.setIDMSDelegatedIdp__c(null != userProductDocCtx.read("$.IDMSDelegatedIdp__c")
				? getValue(userProductDocCtx.read("$.IDMSDelegatedIdp__c").toString()) : getDelimeter());

		userResponse.setIDMSIdentityType__c(null != userProductDocCtx.read("$.IDMSIdentityType__c")
				? getValue(userProductDocCtx.read("$.IDMSIdentityType__c").toString()) : getDelimeter());

		userResponse.setIDMSisInternal__c(null != userProductDocCtx.read("$.IDMSisInternal__c")
				? getValue(userProductDocCtx.read("$.IDMSisInternal__c").toString()) : getDelimeter());

		userResponse.setIDMSAil__c(null != userProductDocCtx.read("$.IDMSAil_c")
				? getValue(userProductDocCtx.read("$.IDMSAil_c").toString()) : getDelimeter());

		userResponse.setIDMSAIL_Applications__c(null != userProductDocCtx.read("$.IDMSAIL_Applications_c")
				? getValue(userProductDocCtx.read("$.IDMSAIL_Applications_c").toString()) : getDelimeter());

		userResponse.setIDMSAIL_Features__c(null != userProductDocCtx.read("$.IDMSAIL_Features_c")
				? getValue(userProductDocCtx.read("$.IDMSAIL_Features_c").toString()) : getDelimeter());

		userResponse.setIDMSAIL_Programs__c(null != userProductDocCtx.read("$.IDMSAIL_Programs_c")
				? getValue(userProductDocCtx.read("$.IDMSAIL_Programs_c").toString()) : getDelimeter());

		userResponse.setIDMSPrimaryContact__c(null != userProductDocCtx.read("$.primaryContact")
				? getValue(userProductDocCtx.read("$.primaryContact").toString()) : getDelimeter());

		userResponse.setIDMSCompanyFederationIdentifier__c(null != userProductDocCtx.read("$.companyFederatedID")
				? getValue(userProductDocCtx.read("$.companyFederatedID").toString()) : getDelimeter());

		userResponse.setFederationIdentifier(null != userProductDocCtx.read("$.federationID")
				? getValue(userProductDocCtx.read("$.federationID").toString()) : getDelimeter());

		userResponse.setCompanyFederatedID(null != userProductDocCtx.read("$.companyFederatedID")
				? getValue(userProductDocCtx.read("$.companyFederatedID").toString()) : getDelimeter());
		userResponse.setAboutMe(null != userProductDocCtx.read("$.AboutMe")
				? getValue(userProductDocCtx.read("$.AboutMe").toString()) : getDelimeter());

		userResponse.setTrustedAdmin(null != userProductDocCtx.read("$.trustedAdmin")
				? getValue(userProductDocCtx.read("$.trustedAdmin").toString()) : getDelimeter());

		userResponse.setIsActivated(null != userProductDocCtx.read("$.isActivated")
				? getValue(userProductDocCtx.read("$.isActivated").toString()) : getDelimeter());

		userResponse.setChannel__c(null != userProductDocCtx.read("$.channel")
				? getValue(userProductDocCtx.read("$.channel").toString()) : getDelimeter());

		userResponse.setSubChannel__c(null != userProductDocCtx.read("$.subchannel")
				? getValue(userProductDocCtx.read("$.subchannel").toString()) : getDelimeter());

		userResponse.setContactId(null != userProductDocCtx.read("$.contactId")
				? getValue(userProductDocCtx.read("$.contactId").toString()) : getDelimeter());
		userResponse.setAccountId(null != userProductDocCtx.read("$.bfoAccountId")
				? getValue(userProductDocCtx.read("$.bfoAccountId").toString()) : getDelimeter());
		userResponse.setIDMSWorkPhone__c(null != userProductDocCtx.read("$.IDMSWorkPhone__c")
				? getValue(userProductDocCtx.read("$.IDMSWorkPhone__c").toString()) : getDelimeter());

		// mobile_reg and login_mobile field is set here for dual identifier
		// feature
		userResponse.setLogin_mobile(null != userProductDocCtx.read("$.login_mobile")
				? getValue(userProductDocCtx.read("$.login_mobile").toString()) : getDelimeter());
		userResponse.setMobile_reg(null != userProductDocCtx.read("$.mobile_reg")
				? getValue(userProductDocCtx.read("$.mobile_reg").toString()) : getDelimeter());

	}
	
	public void parseValuesForGetUserByApplication(GetUserRecordResponse userResponse, DocumentContext userProductDocCtx) {
		
		userResponse.setId(null != userProductDocCtx.read("$.result[0].federationID[0]")
				? getValue(userProductDocCtx.read("$.result[0].federationID[0]").toString()) : getDelimeter());
		userResponse.setFirstName(null != userProductDocCtx.read("$.result[0].givenName[0]")
				? getValue(userProductDocCtx.read("$.result[0].givenName[0]").toString()) : getDelimeter());
		userResponse.setLastName(null != userProductDocCtx.read("$.result[0].sn[0]")
				? getValue(userProductDocCtx.read("$.result[0].sn[0]").toString()) : getDelimeter());
		userResponse.setEmail(null != userProductDocCtx.read("$.result[0].mail[0]")
				? getValue(userProductDocCtx.read("$.result[0].mail[0]").toString()) : getDelimeter());
		userResponse.setMobilePhone(null != userProductDocCtx.read("$.result[0].mobile[0]")
				? getValue(userProductDocCtx.read("$.result[0].mobile[0]").toString()) : getDelimeter());
		userResponse.setCountry(null != userProductDocCtx.read("$.result[0].c[0]")
				? getValue(userProductDocCtx.read("$.result[0].c[0]").toString()) : getDelimeter());
		userResponse.setPostalCode(null != userProductDocCtx.read("$.result[0].postalCode[0]")
				? getValue(userProductDocCtx.read("$.result[0].postalCode[0]").toString()) : getDelimeter());
		userResponse.setAboutMe(null != userProductDocCtx.read("$.result[0].AboutMe[0]")
				? getValue(userProductDocCtx.read("$.result[0].AboutMe[0]").toString()) : getDelimeter());
		userResponse.setAdditionalInfo(null != userProductDocCtx.read("$.result[0].additionalInfo[0]")
				? getValue(userProductDocCtx.read("$.result[0].additionalInfo[0]").toString()) : getDelimeter());
		userResponse.setAnnualRevenue(null != userProductDocCtx.read("$.result[0].annualRevenue[0]")
				? getValue(userProductDocCtx.read("$.result[0].annualRevenue[0]").toString()) : getDelimeter());
		userResponse.setBfoAccountId(null != userProductDocCtx.read("$.result[0].bfoAccountId[0]")
				? getValue(userProductDocCtx.read("$.result[0].bfoAccountId[0]").toString()) : getDelimeter());
		userResponse.setCompanyCity(null != userProductDocCtx.read("$.result[0].companyCity[0]")
				? getValue(userProductDocCtx.read("$.result[0].companyCity[0]").toString()) : getDelimeter());
		userResponse.setCompanyCountry(null != userProductDocCtx.read("$.result[0].companyCountry[0]")
				? getValue(userProductDocCtx.read("$.result[0].companyCountry[0]").toString()) : getDelimeter());
		userResponse.setCompanyCounty(null != userProductDocCtx.read("$.result[0].companyCounty[0]")
				? getValue(userProductDocCtx.read("$.result[0].companyCounty[0]").toString()) : getDelimeter());
		userResponse.setCompanyFederatedID(null != userProductDocCtx.read("$.result[0].companyFederatedID[0]")
				? getValue(userProductDocCtx.read("$.result[0].companyFederatedID[0]").toString()) : getDelimeter());
		
		userResponse.setCompanyName(null != userProductDocCtx.read("$.result[0].companyName[0]")
				? getValue(userProductDocCtx.read("$.result[0].companyName[0]").toString()) : getDelimeter());
		userResponse.setCompanyPostalCode(null != userProductDocCtx.read("$.result[0].companyPostalCode[0]")
				? getValue(userProductDocCtx.read("$.result[0].companyPostalCode[0]").toString()) : getDelimeter());
		userResponse.setCompanyPostOfficeBox(null != userProductDocCtx.read("$.result[0].companyPostOfficeBox[0]")
				? getValue(userProductDocCtx.read("$.result[0].companyPostOfficeBox[0]").toString()) : getDelimeter());
		userResponse.setCompanyState(null != userProductDocCtx.read("$.result[0].companyState[0]")
				? getValue(userProductDocCtx.read("$.result[0].companyState[0]").toString()) : getDelimeter());
		
		userResponse.setCompanyStreet(null != userProductDocCtx.read("$.result[0].companyStreet[0]")
				? getValue(userProductDocCtx.read("$.result[0].companyStreet[0]").toString()) : getDelimeter());
		userResponse.setCompanyWebSite(null != userProductDocCtx.read("$.result[0].companyWebSite[0]")
				? getValue(userProductDocCtx.read("$.result[0].companyWebSite[0]").toString()) : getDelimeter());
		
		userResponse.setContactId(null != userProductDocCtx.read("$.result[0].contactId[0]")
				? getValue(userProductDocCtx.read("$.result[0].contactId[0]").toString()) : getDelimeter());
		userResponse.setContext(null != userProductDocCtx.read("$.result[0].context[0]")
				? getValue(userProductDocCtx.read("$.result[0].context[0]").toString()) : getDelimeter());
		userResponse.setCounty(null != userProductDocCtx.read("$.result[0].county[0]")
				? getValue(userProductDocCtx.read("$.result[0].county[0]").toString()) : getDelimeter());
		userResponse.setCurrency(null != userProductDocCtx.read("$.result[0].currency[0]")
				? getValue(userProductDocCtx.read("$.result[0].currency[0]").toString()) : getDelimeter());
		
		userResponse.setDivision(null != userProductDocCtx.read("$.result[0].Division[0]")
				? getValue(userProductDocCtx.read("$.result[0].Division[0]").toString()) : getDelimeter());
		userResponse.setEmailOptIn(null != userProductDocCtx.read("$.result[0].emailOptInt[0]")
				? getValue(userProductDocCtx.read("$.result[0].emailOptIn[0]").toString()) : getDelimeter());
		userResponse.setEmployeeSize(null != userProductDocCtx.read("$.result[0].employeesize[0]")
				? getValue(userProductDocCtx.read("$.result[0].employeesize[0]").toString()) : getDelimeter());
		userResponse.setFax(null != userProductDocCtx.read("$.result[0].fax[0]")
				? getValue(userProductDocCtx.read("$.result[0].fax[0]").toString()) : getDelimeter());
		userResponse.setFederationID(null != userProductDocCtx.read("$.result[0].federationID[0]")
				? getValue(userProductDocCtx.read("$.result[0].federationID[0]").toString()) : getDelimeter());
		
		userResponse.setHeadQuarters(null != userProductDocCtx.read("$.result[0].headquarters[0]")
				? getValue(userProductDocCtx.read("$.result[0].headquarters[0]").toString()) : getDelimeter());
		userResponse.setHomePhone(null != userProductDocCtx.read("$.result[0].homePhone[0]")
				? getValue(userProductDocCtx.read("$.result[0].homePhone[0]").toString()) : getDelimeter());
		
		userResponse.setIam1(null != userProductDocCtx.read("$.result[0].iam1[0]")
				? getValue(userProductDocCtx.read("$.result[0].iam1[0]").toString()) : getDelimeter());
		userResponse.setIam2(null != userProductDocCtx.read("$.result[0].iam2[0]")
				? getValue(userProductDocCtx.read("$.result[0].iam2[0]").toString()) : getDelimeter());
		userResponse.setIdentityType(null != userProductDocCtx.read("$.result[0].identityType[0]")
				? getValue(userProductDocCtx.read("$.result[0].identityType[0]").toString()) : getDelimeter());
		
		userResponse.setIndustrySegment(null != userProductDocCtx.read("$.result[0].industrySegment[0]")
				? getValue(userProductDocCtx.read("$.result[0].industrySegment[0]").toString()) : getDelimeter());
		userResponse.setIndustrySubSegment(null != userProductDocCtx.read("$.result[0].industrySubSegment[0]")
				? getValue(userProductDocCtx.read("$.result[0].industrySubSegment[0]").toString()) : getDelimeter());
		userResponse.setInetuserActive(null != userProductDocCtx.read("$.result[0].inetUserStatus[0]")
				? getValue(userProductDocCtx.read("$.result[0].inetUserStatus[0]").toString()) : getDelimeter());
		userResponse.setIsActivated(null != userProductDocCtx.read("$.result[0].isActivated[0]")
				? getValue(userProductDocCtx.read("$.result[0].isActivated[0]").toString()) : getDelimeter());
		
		userResponse.setJobDescription(null != userProductDocCtx.read("$.result[0].jobDescription[0]")
				? getValue(userProductDocCtx.read("$.result[0].jobDescription[0]").toString()) : getDelimeter());
		userResponse.setJobFunction(null != userProductDocCtx.read("$.result[0].jobFunction[0]")
				? getValue(userProductDocCtx.read("$.result[0].jobFunction[0]").toString()) : getDelimeter());
		
		userResponse.setCity(null != userProductDocCtx.read("$.result[0].l[0]")
				? getValue(userProductDocCtx.read("$.result[0].l[0]").toString()) : getDelimeter());
		
		userResponse.setMiddleName(null != userProductDocCtx.read("$.result[0].middleName[0]")
				? getValue(userProductDocCtx.read("$.result[0].middleName[0]").toString()) : getDelimeter());
		userResponse.setPostOfficeBox(null != userProductDocCtx.read("$.result[0].postOfficeBox[0]")
				? getValue(userProductDocCtx.read("$.result[0].postOfficeBox[0]").toString()) : getDelimeter());
		
		userResponse.setPreferredLanguage(null != userProductDocCtx.read("$.result[0].preferredLanguage[0]")
				? getValue(userProductDocCtx.read("$.result[0].preferredLanguage[0]").toString()) : getDelimeter());
		userResponse.setSEAccountNumber(null != userProductDocCtx.read("$.result[0].SEAccountNumber[0]")
				? getValue(userProductDocCtx.read("$.result[0].SEAccountNumber[0]").toString()) : getDelimeter());
		
		userResponse.setStateOrProvinceName(null != userProductDocCtx.read("$.result[0].stateOrProvinceName[0]")
				? getValue(userProductDocCtx.read("$.result[0].stateOrProvinceName[0]").toString()) : getDelimeter());
		userResponse.setStreet(null != userProductDocCtx.read("$.result[0].street[0]")
				? getValue(userProductDocCtx.read("$.result[0].street[0]").toString()) : getDelimeter());
		userResponse.setSuffix(null != userProductDocCtx.read("$.result[0].suffix[0]")
				? getValue(userProductDocCtx.read("$.result[0].suffix[0]").toString()) : getDelimeter());
		
		userResponse.setTaxID(null != userProductDocCtx.read("$.result[0].taxID[0]")
				? getValue(userProductDocCtx.read("$.result[0].taxID[0]").toString()) : getDelimeter());
		userResponse.setTelephoneNumber(null != userProductDocCtx.read("$.result[0].telephoneNumber[0]")
				? getValue(userProductDocCtx.read("$.result[0].telephoneNumber[0]").toString()) : getDelimeter());
		userResponse.setTitle(null != userProductDocCtx.read("$.result[0].title[0]")
				? getValue(userProductDocCtx.read("$.result[0].title[0]").toString()) : getDelimeter());
		
		userResponse.setIDMSAil__c(null != userProductDocCtx.read("$.result[0].IDMSAil_c[0]")
				? getValue(userProductDocCtx.read("$.result[0].IDMSAil_c[0]").toString()) : getDelimeter());

		userResponse.setIDMSAIL_Applications__c(null != userProductDocCtx.read("$.result[0].IDMSAIL_Applications_c[0]")
				? getValue(userProductDocCtx.read("$.result[0].IDMSAIL_Applications_c[0]").toString()) : getDelimeter());

		userResponse.setIDMSAIL_Features__c(null != userProductDocCtx.read("$.result[0].IDMSAIL_Features_c[0]")
				? getValue(userProductDocCtx.read("$.result[0].IDMSAIL_Features_c[0]").toString()) : getDelimeter());

		userResponse.setIDMSAIL_Programs__c(null != userProductDocCtx.read("$.result[0].IDMSAIL_Programs_c[0]")
				? getValue(userProductDocCtx.read("$.result[0].IDMSAIL_Programs_c[0]").toString()) : getDelimeter());
	}
	
	private String getDelimeter() {
		return UserConstants.USER_DELIMETER;
	}

	public static String getValue(String key) {
		if (null != key) {

			if (null != key && key.equals("[]")) {
				return key;
			}

			if (!key.contains("[")) {
				return key;
			}
			if (key.contains("[\"[]")) {
				return null;
			}
			if (key.contains("[\"[(") || key.contains("[\"[nul,(") || key.contains("[\"[null,")) {
				return key.substring(key.indexOf("[\"[") + 3, key.lastIndexOf("]\""));
			}

			if (key.contains("[\"[")) {
				return key.substring(key.indexOf("[\"[") + 3, key.lastIndexOf("]\""));
			}
			int beginIndex = key.indexOf('[') + 1;
			int endIndex = key.indexOf(']');
			String preValue = key.substring(beginIndex, endIndex);
			return preValue.substring(preValue.indexOf('\"') + 1, preValue.lastIndexOf('\"'));
		}
		return "";
	}

	public static String getValues(String key) {
		if (null != key) {
			if (!key.contains("[" + '"' + "[")) {
				return key;
			}
			int beginIndex = key.indexOf('[') + 1;
			int endIndex = key.indexOf(']');
			String preValue = key.substring(beginIndex, endIndex);
			return preValue.substring(preValue.indexOf('\"') + 1, preValue.lastIndexOf('\"'));
		}
		return "";
	}

}

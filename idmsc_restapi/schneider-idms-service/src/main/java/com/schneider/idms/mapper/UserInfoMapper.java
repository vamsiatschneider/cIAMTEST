/**
 * 
 */
package com.schneider.idms.mapper;

import com.jayway.jsonpath.DocumentContext;
import com.schneider.idms.model.UserInfoDTO;
import com.se.idms.util.UserConstants;

/**
 * @author SESA508936
 * For Direct API Call
 */
public class UserInfoMapper {
	
	public void parseGetUserResponse(UserInfoDTO userInfoDTO,DocumentContext userProductDocCtx){
		
		userInfoDTO.setAboutMe(null != userProductDocCtx.read("$.AboutMe")
				? getValue(userProductDocCtx.read("$.AboutMe").toString()) : getDelimeter());
		//userInfoDTO.setAccountGoldenID(accountGoldenID);
		userInfoDTO.setAccountId(null != userProductDocCtx.read("$.uid")
				? getValue(userProductDocCtx.read("$.uid").toString()) : getDelimeter());
		userInfoDTO.setAdditionalAddress(null != userProductDocCtx.read("$.additionalInfo")
				? getValue(userProductDocCtx.read("$.additionalInfo").toString()) : getDelimeter());
		userInfoDTO.setAil(null != userProductDocCtx.read("$.ail")
				? getValue(userProductDocCtx.read("$.ail").toString()) : getDelimeter());
		userInfoDTO.setAilApplications(null != userProductDocCtx.read("$.ail_applications")
				? getValue(userProductDocCtx.read("$.ail_applications").toString()) : getDelimeter());
		userInfoDTO.setAilFeatures(null != userProductDocCtx.read("$.ail_features")
				? getValue(userProductDocCtx.read("$.ail_features").toString()) : getDelimeter());
		userInfoDTO.setAilPrograms(null != userProductDocCtx.read("$.ail_programs")
				? getValue(userProductDocCtx.read("$.ail_programs").toString()) : getDelimeter());
		userInfoDTO.setAnnualRevenue(null != userProductDocCtx.read("$.annualRevenue")
				? Float.parseFloat(getValue(userProductDocCtx.read("$.annualRevenue").toString())) : 0.0f);
		//userInfoDTO.setBusinessUnit(businessUnit);
		userInfoDTO.setCity(null != userProductDocCtx.read("$.l")
				? getValue(userProductDocCtx.read("$.l").toString()) : getDelimeter());
		userInfoDTO.setClassLevel1(null != userProductDocCtx.read("$.classLevel1")
				? getValue(userProductDocCtx.read("$.classLevel1").toString()) : getDelimeter());
		userInfoDTO.setClassLevel2(null != userProductDocCtx.read("$.iam2")
				? getValue(userProductDocCtx.read("$.iam2").toString()) : getDelimeter());
		userInfoDTO.setCompanyAdditionalAddress(null != userProductDocCtx.read("$.companyAdditionalInfo")
				? getValue(userProductDocCtx.read("$.companyAdditionalInfo").toString()) : getDelimeter());
		userInfoDTO.setCompanyCity(null != userProductDocCtx.read("$.companyCity")
				? getValue(userProductDocCtx.read("$.companyCity").toString()) : getDelimeter());
		userInfoDTO.setCompanyCountryCode(null != userProductDocCtx.read("$.companyCountry")
				? getValue(userProductDocCtx.read("$.companyCountry").toString()) : getDelimeter());
		userInfoDTO.setCompanyCounty(null != userProductDocCtx.read("$.companyCounty")
				? getValue(userProductDocCtx.read("$.companyCounty").toString()) : getDelimeter());
		userInfoDTO.setCompanyFederatedId(null != userProductDocCtx.read("$.companyFederatedID")
				? getValue(userProductDocCtx.read("$.companyFederatedID").toString()) : getDelimeter());
		userInfoDTO.setCompanyName(null != userProductDocCtx.read("$.CompanyName")
				? getValue(userProductDocCtx.read("$.CompanyName").toString()) : getDelimeter());
		userInfoDTO.setCompanyPOBox(null != userProductDocCtx.read("$.companyPostalCode")
				? getValue(userProductDocCtx.read("$.companyPostalCode").toString()) : getDelimeter());
		userInfoDTO.setCompanyStateOrProvinceCode(null != userProductDocCtx.read("$.companyState")
				? getValue(userProductDocCtx.read("$.companyState").toString()) : getDelimeter());
		userInfoDTO.setCompanyStreet(null != userProductDocCtx.read("$.companyStreet")
				? getValue(userProductDocCtx.read("$.companyStreet").toString()) : getDelimeter());
		userInfoDTO.setCompanyWebsite(null != userProductDocCtx.read("$.companyWebSite")
				? getValue(userProductDocCtx.read("$.companyWebSite").toString()) : getDelimeter());
		userInfoDTO.setCompanyZipCode(null != userProductDocCtx.read("$.companyPostalCode")
				? getValue(userProductDocCtx.read("$.companyPostalCode").toString()) : getDelimeter());
		//userInfoDTO.setContactGoldenID(contactGoldenID);
		userInfoDTO.setContactId(null != userProductDocCtx.read("$.uid")
				? getValue(userProductDocCtx.read("$.uid").toString()) : getDelimeter());
		userInfoDTO.setCountryCode(null != userProductDocCtx.read("$.c")
				? getValue(userProductDocCtx.read("$.c").toString()) : getDelimeter());
		userInfoDTO.setCounty(null != userProductDocCtx.read("$.county")
				? getValue(userProductDocCtx.read("$.county").toString()) : getDelimeter());
		userInfoDTO.setCurrency(null != userProductDocCtx.read("$.currency")
				? getValue(userProductDocCtx.read("$.currency").toString()) : getDelimeter());
		userInfoDTO.setDelegatedIdp(null != userProductDocCtx.read("$.delegatedIDP")
				? getValue(userProductDocCtx.read("$.delegatedIDP").toString()) : getDelimeter());
		userInfoDTO.setDepartment(null != userProductDocCtx.read("$.departmentNumber")
				? getValue(userProductDocCtx.read("$.departmentNumber").toString()) : getDelimeter());
		//userInfoDTO.setDivision(division);
		userInfoDTO.setEmail(null != userProductDocCtx.read("$.email")
				? getValue(userProductDocCtx.read("$.email").toString()) : getDelimeter());
		userInfoDTO.setEmployeeSize(null != userProductDocCtx.read("$.employeeSize")
				? getValue(userProductDocCtx.read("$.employeeSize").toString()) : getDelimeter());
		userInfoDTO.setFax(null != userProductDocCtx.read("$.fax")
				? getValue(userProductDocCtx.read("$.fax").toString()) : getDelimeter());
		userInfoDTO.setFederatedId(null != userProductDocCtx.read("$.federationID")
				? getValue(userProductDocCtx.read("$.federationID").toString()) : getDelimeter());
		userInfoDTO.setFirstName(null != userProductDocCtx.read("$.firstName")
				? getValue(userProductDocCtx.read("$.firstName").toString()) : getDelimeter());
		//userInfoDTO.setHashedPin(hashedPin);
		userInfoDTO.setHeadquarter(null != userProductDocCtx.read("$.headquarter")
				? Boolean.parseBoolean(getValue(userProductDocCtx.read("$.headquarter").toString())) : false);
		userInfoDTO.setHomePhone(null != userProductDocCtx.read("$.homePhone")
				? getValue(userProductDocCtx.read("$.homePhone").toString()) : getDelimeter());
		userInfoDTO.setIdentityType(null != userProductDocCtx.read("$.identityType")
				? getValue(userProductDocCtx.read("$.identityType").toString()) : getDelimeter());
		userInfoDTO.setIdmsFederatedId(null != userProductDocCtx.read("$.federationID")
				? getValue(userProductDocCtx.read("$.federationID").toString()) : getDelimeter());
		userInfoDTO.setIsInternal(null != userProductDocCtx.read("$.IDMSisInternal__c")
				? getValue(userProductDocCtx.read("$.IDMSisInternal__c").toString()) : getDelimeter());
		userInfoDTO.setJobDescription(null != userProductDocCtx.read("$.jobDescription")
				? getValue(userProductDocCtx.read("$.jobDescription").toString()) : getDelimeter());
		userInfoDTO.setJobFunction(null != userProductDocCtx.read("$.jobFunction")
				? getValue(userProductDocCtx.read("$.jobFunction").toString()) : getDelimeter());
		userInfoDTO.setJobTitle(null != userProductDocCtx.read("$.job_Title__c")
				? getValue(userProductDocCtx.read("$.job_Title__c").toString()) : getDelimeter());
		userInfoDTO.setLanguageCode(null != userProductDocCtx.read("$.preferredlanguage")
				? getValue(userProductDocCtx.read("$.preferredlanguage").toString()) : getDelimeter());
		userInfoDTO.setLastName(null != userProductDocCtx.read("$.LastName")
				? getValue(userProductDocCtx.read("$.LastName").toString()) : getDelimeter());
		userInfoDTO.setMarketSegment(null != userProductDocCtx.read("$.industrySegment")
				? getValue(userProductDocCtx.read("$.industrySegment").toString()) : getDelimeter());
		userInfoDTO.setMarketServed(null != userProductDocCtx.read("$.industries")
				? getValue(userProductDocCtx.read("$.industries").toString()) : getDelimeter());
		userInfoDTO.setMarketSubSegment(null != userProductDocCtx.read("$.industrySubSegment")
				? getValue(userProductDocCtx.read("$.industrySubSegment").toString()) : getDelimeter());
		userInfoDTO.setMiddleName(null != userProductDocCtx.read("$.middleName")
				? getValue(userProductDocCtx.read("$.middleName").toString()) : getDelimeter());
		userInfoDTO.setMobilePhone(null != userProductDocCtx.read("$.mobile")
				? getValue(userProductDocCtx.read("$.mobile").toString()) : getDelimeter());
		userInfoDTO.setpOBox(null != userProductDocCtx.read("$.postOfficeBox")
				? getValue(userProductDocCtx.read("$.postOfficeBox").toString()) : getDelimeter());
		userInfoDTO.setProfileLastUpdateSource(null != userProductDocCtx.read("$.updateSource")
				? getValue(userProductDocCtx.read("$.updateSource").toString()) : getDelimeter());
		userInfoDTO.setRegistrationSource(null != userProductDocCtx.read("$.registerationSource")
				? getValue(userProductDocCtx.read("$.registerationSource").toString()) : getDelimeter());
		userInfoDTO.setRejectionComment(null != userProductDocCtx.read("$.IDMS_RejectionComments__c")
		? getValue(userProductDocCtx.read("$.IDMS_RejectionComments__c").toString()) : getDelimeter());
		userInfoDTO.setRejectionReason(null != userProductDocCtx.read("$.IDMS_RejectionReason__c")
		? getValue(userProductDocCtx.read("$.IDMS_RejectionReason__c").toString()) : getDelimeter());
		userInfoDTO.setSalutation(null != userProductDocCtx.read("$.initials")
				? getValue(userProductDocCtx.read("$.initials").toString()) : getDelimeter());
		//userInfoDTO.setSocialProviders(socialProviders);
		userInfoDTO.setStateOrProvinceCode(null != userProductDocCtx.read("$.State")
				? getValue(userProductDocCtx.read("$.State").toString()) : getDelimeter());
		userInfoDTO.setStreet(null != userProductDocCtx.read("$.street")
				? getValue(userProductDocCtx.read("$.street").toString()) : getDelimeter());
		userInfoDTO.setSuffix(null != userProductDocCtx.read("$.suffix")
				? getValue(userProductDocCtx.read("$.suffix").toString()) : getDelimeter());
		userInfoDTO.setTaxIdentificationNumber(null != userProductDocCtx.read("$.taxID")
				? getValue(userProductDocCtx.read("$.taxID").toString()) : getDelimeter());
		userInfoDTO.setTitle(null != userProductDocCtx.read("$.title")
				? getValue(userProductDocCtx.read("$.title").toString()) : getDelimeter());
		userInfoDTO.setTrustedAdmin(null != userProductDocCtx.read("$.trustedAdmin")
				? Boolean.parseBoolean(getValue(userProductDocCtx.read("$.trustedAdmin").toString())) : false);
		userInfoDTO.setTrustLevel(null != userProductDocCtx.read("$.IDMS_TrustLevel__c")
		? getValue(userProductDocCtx.read("$.IDMS_TrustLevel__c").toString()) : getDelimeter());
		userInfoDTO.setTrustStatus(null != userProductDocCtx.read("$.IDMS_TrustStatus__c")
		? getValue(userProductDocCtx.read("$.IDMS_TrustStatus__c").toString()) : getDelimeter());
		userInfoDTO.setUserId(null != userProductDocCtx.read("$.IDMSUID")
				? getValue(userProductDocCtx.read("$.IDMSUID").toString()) : getDelimeter());
		userInfoDTO.setUserStatus(null != userProductDocCtx.read("$.inetUserStatus")
		? getValue(userProductDocCtx.read("$.inetUserStatus").toString()) : getDelimeter());
		userInfoDTO.setWorkPhone(null != userProductDocCtx.read("$.telephoneNumber")
				? getValue(userProductDocCtx.read("$.telephoneNumber").toString()) : getDelimeter());
		userInfoDTO.setZipCode(null != userProductDocCtx.read("$.PostalCode")
				? getValue(userProductDocCtx.read("$.PostalCode").toString()) : getDelimeter());
		
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

}

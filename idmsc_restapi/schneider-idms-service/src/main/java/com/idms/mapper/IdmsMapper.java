package com.idms.mapper;

import javax.inject.Named;

import com.idms.model.CreateUserRequest;
import com.idms.model.IDMSUserResponse;
import com.idms.model.IFWUser;
import com.idms.model.PasswordRecoveryRequest;
import com.idms.model.UIMSCompanyRequest;
import com.idms.model.UpdateUserRequest;
import com.idms.model.UserRegistrationInfoRequest;
import com.idms.product.model.OpenAMPasswordRecoveryInput;
import com.idms.product.model.OpenAmUserRequest;
import com.idms.product.model.UimsUserRequest;
import com.schneider.ims.service.uimsv2.CompanyV3;
import com.se.idms.dto.IFWCustomAttributesForWork;
import com.se.idms.util.UserConstants;
import com.uims.authenticatedUsermanager.UserV6;

import ma.glasnost.orika.MapperFactory;
import ma.glasnost.orika.impl.ConfigurableMapper;

@Named
public class IdmsMapper extends ConfigurableMapper{


    @Override
    protected void configure(MapperFactory mapperFactory) {
    	configureIFWUserRequestToOpenAmUserRequest(mapperFactory);
    	configureUpdateUserRequest(mapperFactory);
    	configurePasswordRecoveryRequest(mapperFactory);
    	configureIFWUserRequestToUimsUserRequest(mapperFactory);
    	configureIFWUserRequestToUserV5Request(mapperFactory);
    	configureIFWUserRequestToCompanyV3Request(mapperFactory);
    	configureUpdateUserToCompanyV3Request(mapperFactory);
    	configureUpdateUserRequestToUserV5Request(mapperFactory);
    	configureIFWUserRequestToGoDigitalUserRequest(mapperFactory);
    	
    	configureIFWUserRequestToIDMSUserRecord(mapperFactory);
    	configureIFWGetUserResponseToIDMSCreateUserRequest(mapperFactory);
    }
    
   
	/**
	 * User Registration
	 * @param mapperFactory
	 */
	private void configureIFWUserRequestToOpenAmUserRequest(MapperFactory mapperFactory) {
        mapperFactory.classMap(CreateUserRequest.class,OpenAmUserRequest.class)
        .field("userRecord.email", "input.user.mail")
        .field("userRecord.mobilePhone", "input.user.mobile")
        //.field("userRecord.phone", "input.user.homePhone")
        .field("userRecord.firstName", "input.user.givenName")
        .field("userRecord.lastName", "input.user.sn")
        .field("userRecord.country", "input.user.c")
        .field("userRecord.IDMS_Email_opt_in__c", "input.user.emailOptIn")
        .field("userRecord.IDMS_User_Context__c", "input.user.employeeType")
        .field("userRecord.IDMS_PreferredLanguage__c", "input.user.preferredlanguage")
        .field("userRecord.defaultCurrencyIsoCode", "input.user.currency")
        .field("userRecord.street", "input.user.street")
        .field("userRecord.city", "input.user.l")
        .field("userRecord.postalCode", "input.user.postalCode")
        .field("userRecord.state", "input.user.st")
        .field("userRecord.IDMS_County__c", "input.user.county")
        .field("userRecord.IDMS_POBox__c", "input.user.postOfficeBox")
        .field("userRecord.IDMS_Federated_ID__c", "input.user.federationID")
        .field("userRecord.IDMS_Registration_Source__c", "input.user.registerationSource")
        .field("userRecord.IDMS_Profile_update_source__c", "input.user.updateSource")
        .field("userRecord.IDMS_AdditionalAddress__c", "input.user.additionalInfo")
        .field("userRecord.companyName", "input.user.companyName")
        .field("userRecord.company_Address1__c", "input.user.companyStreet")
        .field("userRecord.company_City__c", "input.user.companyCity")
        .field("userRecord.company_Postal_Code__c", "input.user.companyPostalCode")
        .field("userRecord.company_State__c", "input.user.companyState")
        .field("userRecord.IDMSCompanyPoBox__c", "input.user.companyPostOfficeBox")
        .field("userRecord.company_Country__c", "input.user.companyCountry")
        .field("userRecord.company_Address2__c", "input.user.companyAdditionalInfo")
        .field("userRecord.IDMSClassLevel1__c", "input.user.iam1")
        .field("userRecord.IDMSClassLevel2__c", "input.user.iam2")
        .field("userRecord.IDMSMarketSegment__c", "input.user.industrySegment")
        .field("userRecord.IDMSMarketSubSegment__c", "input.user.industrySubSegment")
        .field("userRecord.phone", "input.user.telephoneNumber")
        .field("userRecord.job_Title__c", "input.user.title")
        .field("userRecord.job_Function__c", "input.user.jobFunction")
        .field("userRecord.IDMSJobDescription__c", "input.user.jobDescription")
        .field("userRecord.IDMSCompanyMarketServed__c", "input.user.industries")
        .field("userRecord.IDMSCompanyNbrEmployees__c", "input.user.employeeSize")
        .field("userRecord.IDMSCompanyHeadquarters__c", "input.user.headquarters")
        .field("userRecord.IDMSAnnualRevenue__c", "input.user.annualRevenue")
        .field("userRecord.IDMSTaxIdentificationNumber__c", "input.user.taxID")
        .field("userRecord.IDMSMiddleName__c", "input.user.middleName")
        .field("userRecord.company_Website__c", "input.user.companyWebSite")
        .field("userRecord.IDMSSalutation__c", "input.user.initials")
        .field("userRecord.department", "input.user.departmentNumber")
        .field("userRecord.IDMSSuffix__c", "input.user.suffix")
        .field("userRecord.fax", "input.user.fax")
        .field("userRecord.IDMSCompanyFederationIdentifier__c", "input.user.companyFederatedID")
        .field("userRecord.IDMSDelegatedIdp__c", "input.user.delegatedIDP")
        .field("userRecord.IDMSIdentityType__c", "input.user.identityType")
        .field("userRecord.IDMSCompanyCounty__c", "input.user.companyCounty")
        .field("userRecord.tncFlag", "input.user.tncFlag")
        .field("userRecord.IDMSPrimaryContact__c", "input.user.primaryContact")
        .field(UserConstants.MAPPER_CREATE_USER_PR_REQUEST,UserConstants.MAPPER_OPENAM_USER_PR_REQUEST)
        .field("userRecord.adminCompanyFederatedId", "input.user.admin_company_id")
        .field("userRecord.adminFederatedId", "input.user.admin_federated_id")
        .field("userRecord.BFO_ACCOUNT_ID__c", "input.user.bfoAccountId")
        .field("userRecord.companyFederatedId", "input.user.companyID")
        .field("userRecord.aboutMe", "input.user.aboutMe")
        .field("userRecord.invitationCode", "input.user.invitationCode")
        .field("userRecord.trustedAdmin", "input.user.trustedAdmin")
        .field("userRecord.isActivated", "input.user.isActivated")
        .field("userRecord.channel__c", "input.user.channel")
        .field("userRecord.subChannel__c", "input.user.subchannel")
        .field("userRecord.contactId", "input.user.contactId")
        .field("userRecord.registrationAttributes__c", "input.user.registrationAttributes__c")
        .field("userRecord.IDMSWorkPhone__c", "input.user.IDMSWorkPhone__c")
        .byDefault()
        .register();
    }
	
	/*
	 * Update User
	 */
	private void configureUpdateUserRequest(MapperFactory mapperFactory) {
        mapperFactory.classMap(UpdateUserRequest.class,OpenAmUserRequest.class)
        .field("userRecord.email", "input.user.mail")
        .field("userRecord.mobilePhone", "input.user.mobile")
        //.field("userRecord.phone", "input.user.homePhone")
        .field("userRecord.firstName", "input.user.givenName")
        .field("userRecord.lastName", "input.user.sn")
        .field("userRecord.country", "input.user.c")
        .field("userRecord.IDMS_Email_opt_in__c", "input.user.emailOptIn")
        .field("userRecord.IDMS_User_Context__c", "input.user.employeeType")
        .field("userRecord.IDMS_PreferredLanguage__c", "input.user.preferredlanguage")
        .field("userRecord.defaultCurrencyIsoCode", "input.user.currency")
        .field("userRecord.street", "input.user.street")
        .field("userRecord.city", "input.user.l")
        .field("userRecord.postalCode", "input.user.postalCode")
        .field("userRecord.state", "input.user.st")
        .field("userRecord.IDMS_County__c", "input.user.county")
        .field("userRecord.IDMS_POBox__c", "input.user.postOfficeBox")
        .field("userRecord.IDMS_Federated_ID__c", "input.user.federationID")
        .field("userRecord.IDMS_Registration_Source__c", "input.user.registerationSource")
        .field("userRecord.IDMS_Profile_update_source__c", "input.user.updateSource")
        .field("userRecord.IDMS_AdditionalAddress__c", "input.user.additionalInfo")
        .field("userRecord.companyName", "input.user.companyName")
        .field("userRecord.company_Address1__c", "input.user.companyStreet")
        .field("userRecord.company_City__c", "input.user.companyCity")
        .field("userRecord.company_Postal_Code__c", "input.user.companyPostalCode")
        .field("userRecord.company_State__c", "input.user.companyState")
        .field("userRecord.IDMSCompanyPoBox__c", "input.user.companyPostOfficeBox")
        .field("userRecord.company_Country__c", "input.user.companyCountry")
        .field("userRecord.company_Address2__c", "input.user.companyAdditionalInfo")
        .field("userRecord.IDMSClassLevel1__c", "input.user.iam1")
        .field("userRecord.IDMSClassLevel2__c", "input.user.iam2")
        .field("userRecord.IDMSMarketSegment__c", "input.user.industrySegment")
        .field("userRecord.IDMSMarketSubSegment__c", "input.user.industrySubSegment")
        .field("userRecord.phone", "input.user.telephoneNumber")
        .field("userRecord.job_Title__c", "input.user.title")
        .field("userRecord.job_Function__c", "input.user.jobFunction")
        .field("userRecord.IDMSJobDescription__c", "input.user.jobDescription")
        .field("userRecord.IDMSCompanyMarketServed__c", "input.user.industries")
        .field("userRecord.IDMSCompanyNbrEmployees__c", "input.user.employeeSize")
        .field("userRecord.IDMSCompanyHeadquarters__c", "input.user.headquarters")
        .field("userRecord.IDMSAnnualRevenue__c", "input.user.annualRevenue")
        .field("userRecord.IDMSTaxIdentificationNumber__c", "input.user.taxID")
        .field("userRecord.IDMSMiddleName__c", "input.user.middleName")
        .field("userRecord.company_Website__c", "input.user.companyWebSite")
        .field("userRecord.IDMSSalutation__c", "input.user.initials")
        .field("userRecord.department", "input.user.departmentNumber")
        .field("userRecord.IDMSSuffix__c", "input.user.suffix")
        .field("userRecord.fax", "input.user.fax")
        .field("userRecord.IDMSCompanyFederationIdentifier__c", "input.user.companyFederatedID")
        .field("userRecord.IDMSDelegatedIdp__c", "input.user.delegatedIDP")
        .field("userRecord.IDMSIdentityType__c", "input.user.identityType")
        .field("userRecord.IDMSCompanyCounty__c", "input.user.companyCounty")
        .field("userRecord.tncFlag", "input.user.tncFlag")
        .field("userRecord.adminCompanyFederatedId", "input.user.admin_company_id")
        .field("userRecord.adminFederatedId", "input.user.admin_federated_id")
        .field("userRecord.aboutMe", "input.user.aboutMe")
        .field("userRecord.IDMSPrimaryContact__c", "input.user.primaryContact")
        .field("userRecord.trustedAdmin", "input.user.trustedAdmin")
        .field("userRecord.channel__c", "input.user.channel")
        .field("userRecord.subChannel__c", "input.user.subchannel")
        .field("userRecord.BFO_ACCOUNT_ID__c", "input.user.bfoAccountId")
        .field("userRecord.contactId", "input.user.contactId")
        .field("userRecord.IDMSWorkPhone__c", "input.user.IDMSWorkPhone__c")
        .field("userRecord.registrationAttributes__c", "input.user.registrationAttributes__c")
        .byDefault()
        .register();
    }
	
	/*
	 * PasswordRecoveryRequest
	 */
	private void configurePasswordRecoveryRequest(MapperFactory mapperFactory) {
        mapperFactory.classMap(PasswordRecoveryRequest.class,OpenAMPasswordRecoveryInput.class)
        .field("userRecord.email", "userRecord.email")
        .field("userRecord.mobilePhone", "userRecord.mobile")
        .field("userRecord.IDMS_Profile_update_source__c", "userRecord.profileLastUpdateSource")
        .byDefault()
        .register();
    }
	
	/**
	 * Currently not in use
	 * @param mapperFactory
	 */
	private void configureIFWUserRequestToUimsUserRequest(MapperFactory mapperFactory) {
        mapperFactory.classMap(CreateUserRequest.class,UimsUserRequest.class)
        .field("userRecord.email", "input.email") 
        .field("userRecord.mobilePhone", "input.cell")
        ////.field("userRecord.phone", "input.homePhone")
        .field("userRecord.firstName", "input.firstName")
        .field("userRecord.lastName", "input.lastName")
        //.field("userRecord.country", "input.countrycode")
        ////.field("userRecord.IDMS_Email_opt_in__c", "input.emailOptIn")
        //.field("userRecord.IDMS_User_Context__c", "input.employeeType")
        .field("userRecord.IDMS_PreferredLanguage__c", "input.languageCode")
        .field("userRecord.defaultCurrencyIsoCode", "input.currencyCode")
        .field("userRecord.street", "input.street")
        .field("userRecord.city", "input.localityName")
        //.field("userRecord.postalCode", "input.postalCode")
        .field("userRecord.state", "input.state")
        //.field("userRecord.IDMS_County__c", "input.county")
        .field("userRecord.IDMS_POBox__c", "input.postOfficeBox")
        .field("userRecord.IDMS_Federated_ID__c", "input.federatedID")
        //.field("userRecord.IDMS_Registration_Source__c", "input.registerationSource")
        //.field("userRecord.IDMS_Profile_update_source__c", "input.updateSource")
        .field("userRecord.IDMS_AdditionalAddress__c", "input.addInfoAddress")
        .field("userRecord.companyName", "input.organizationName")
        //.field("userRecord.company_Address1__c", "input.companyStreet")
        //.field("userRecord.company_City__c", "input.localityName")
        .field("userRecord.company_Postal_Code__c", "input.postalCode")
        .field("userRecord.company_State__c", "input.st")
        //.field("userRecord.IDMSCompanyPoBox__c", "input.companyPostOfficeBox")
        .field("userRecord.company_Country__c", "input.countrycode")
        //.field("userRecord.company_Address2__c", "input.companyAdditionalInfo")
        //.field("userRecord.IDMSClassLevel1__c", "input.iam1")
        .field("userRecord.IDMSClassLevel2__c", "input.customerClass")
        .field("userRecord.IDMSMarketSegment__c", "input.marketSegment")
        //.field("userRecord.IDMSMarketSubSegment__c", "input.industrySubSegment")
        .field("userRecord.phone", "input.phone")
        .field("userRecord.job_Title__c", "input.jobTitle")
        .field("userRecord.job_Function__c", "input.jobFunction")
        .field("userRecord.IDMSJobDescription__c", "input.jobDescription")
        .field("userRecord.IDMSCompanyMarketServed__c", "input.marketServed")
        .field("userRecord.IDMSCompanyNbrEmployees__c", "input.employeeSize")
        .field("userRecord.IDMSCompanyHeadquarters__c", "input.headQuarter")
        //.field("userRecord.IDMSAnnualRevenue__c", "input.annualRevenue")
        .field("userRecord.IDMSTaxIdentificationNumber__c", "input.taxIdentificationNumber")
        .field("userRecord.IDMSMiddleName__c", "input.middleName")
        .field("userRecord.company_Website__c", "input.webSite")
        .field("userRecord.IDMSSalutation__c", "input.salutation")
        //.field("userRecord.department", "input.departmentNumber")
        //.field("userRecord.IDMSSuffix__c", "input.suffix")
        .field("userRecord.fax", "input.fax")
        //.field("userRecord.IDMSCompanyFederationIdentifier__c", "input.companyFederatedID")
        //.field("userRecord.IDMSDelegatedIdp__c", "input.delegatedIDP")
        //.field("userRecord.IDMSIdentityType__c", "input.identityType")
        //.field("userRecord.IDMSCompanyCounty__c", "input.countrycode")
        //.field("userRecord.tncFlag", "input.tncFlag")
        .byDefault()
        .register();
    }	
	
	/**
	 * UIMS Create User
	 * @param mapperFactory
	 */
	private void configureIFWUserRequestToUserV5Request(MapperFactory mapperFactory) {
        mapperFactory.classMap(CreateUserRequest.class,UserV6.class)
        .field("userRecord.email", "email") 
        .field("userRecord.mobilePhone", "cell")
        .field("userRecord.firstName", "firstName")
        .field("userRecord.lastName", "lastName")
        .field("userRecord.IDMS_Federated_ID__c", "federatedID")
        .field("userRecord.IDMS_PreferredLanguage__c", "languageCode")
        .field("userRecord.country", "countryCode")
        .field("userRecord.IDMS_AdditionalAddress__c", "addInfoAddress")
        .field("userRecord.IDMSJobDescription__c", "jobDescription")
        .field("userRecord.job_Function__c", "jobFunction")
        .field("userRecord.job_Title__c", "jobTitle")
        .field("userRecord.city", "localityName")
        .field("userRecord.IDMSMiddleName__c", "middleName")
        .field("userRecord.IDMS_POBox__c", "postOfficeBox")
        .field("userRecord.IDMSSalutation__c", "salutation")
        .field("userRecord.street", "street")
        .field("userRecord.postalCode", "postalCode")
        .field("userRecord.state", "state")
        .field("userRecord.phone", "phone")
        .field("userRecord.fax", "fax")
        .field("userRecord.IDMS_County__c", "county")
        //.field("userRecord.mobilePhone", "phoneId")
        .field("userRecord.channel__c", "channel")
        .field("userRecord.subChannel__c", "subChannel")
        .field("userRecord.IDMSPrimaryContact__c", "primaryContact")
        .field("userRecord.contactId", "bfoId")
        

        .byDefault()
        .register();
    }
	
	/**
	 * UpdateUser for UIMS User
	 * @param mapperFactory
	 */
	private void configureUpdateUserRequestToUserV5Request(MapperFactory mapperFactory) {
        mapperFactory.classMap(UpdateUserRequest.class,com.se.uims.usermanager.UserV5.class)
        .field("userRecord.email", "email") 
        .field("userRecord.mobilePhone", "cell")
        .field("userRecord.firstName", "firstName")
        .field("userRecord.lastName", "lastName")
        .field("userRecord.IDMS_Federated_ID__c", "federatedID")
        .field("userRecord.IDMS_PreferredLanguage__c", "languageCode")
        .field("userRecord.country", "countryCode")
        .field("userRecord.IDMS_AdditionalAddress__c", "addInfoAddress")
        .field("userRecord.IDMSJobDescription__c", "jobDescription")
        .field("userRecord.job_Function__c", "jobFunction")
        .field("userRecord.job_Title__c", "jobTitle")
        .field("userRecord.city", "localityName")
        .field("userRecord.IDMSMiddleName__c", "middleName")
        .field("userRecord.IDMS_POBox__c", "postOfficeBox")
        .field("userRecord.IDMSSalutation__c", "salutation")
        .field("userRecord.street", "street")
        .field("userRecord.postalCode", "postalCode")
        .field("userRecord.state", "state")
        .field("userRecord.phone", "phone")
        .field("userRecord.fax", "fax")
        .field("userRecord.IDMS_County__c", "county")
        .field("userRecord.channel__c", "channel")
        .field("userRecord.subChannel__c", "subChannel")
        .field("userRecord.IDMSPrimaryContact__c", "primaryContact")
        .field("userRecord.contactId", "bfoId")
        .byDefault()
        .register();
    }
	/**
	 * UserRegitstation to UIMS Create Company
	 * @param mapperFactory
	 */
	private void configureIFWUserRequestToCompanyV3Request(MapperFactory mapperFactory) {
        mapperFactory.classMap(CreateUserRequest.class,CompanyV3.class)
       // .field("userRecord.IDMSCompanyFederationIdentifier__c", "federatedId") 
        .field("userRecord.companyName", "organizationName")
        .field("userRecord.company_Country__c", "countryCode")
        .field("userRecord.defaultCurrencyIsoCode", "currencyCode")
        .field("userRecord.IDMSClassLevel2__c", "customerClass")
        .field("userRecord.company_City__c", "localityName")
        .field("userRecord.IDMSMarketSegment__c", "marketSegment")
        .field("userRecord.company_Postal_Code__c", "postalCode")
        .field("userRecord.IDMSCompanyPoBox__c", "postOfficeBox")
        .field("userRecord.company_State__c", "state")
        .field("userRecord.company_Address1__c", "street")
        .field("userRecord.IDMSCompanyHeadquarters__c", "headQuarter")
        .field("userRecord.company_Address2__c", "addInfoAddress")
        .field("userRecord.company_Address2__c", "postalAddress")
        .field("userRecord.IDMSCompanyCounty__c", "county")
        .field("userRecord.company_Website__c", "webSite")
        .field("userRecord.IDMSCompanyMarketServed__c", "marketServed")
        .field("userRecord.IDMSCompanyNbrEmployees__c", "employeeSize")
        .field("userRecord.IDMSCompanyNbrEmployees__c", "numberEmployees")
        .field("userRecord.IDMSTaxIdentificationNumber__c", "taxIdentificationNumber")
        .field("userRecord.IDMS_PreferredLanguage__c", "languageCode")
        //adding new mappings
        .field("userRecord.IDMSCompanyPoBox__c", "postOfficeBoxCode")
        .field("userRecord.IDMSClassLevel1__c", "businessType")
        .field("userRecord.IDMSCompanyMarketServed__c", "marketSegment")
        .field("userRecord.IDMSAnnualRevenue__c", "annualSales")
        .field("userRecord.BFO_ACCOUNT_ID__c", "bfoId")
        .field("userRecord.IDMSWorkPhone__c", "telephoneNumber")
        .byDefault()
        .register();
    }
	
	/**
	 * UpdateUser to UIMS Company
	 * @param mapperFactory
	 */
	private void configureUpdateUserToCompanyV3Request(MapperFactory mapperFactory) {
        mapperFactory.classMap(UpdateUserRequest.class,CompanyV3.class)
       // .field("userRecord.IDMSCompanyFederationIdentifier__c", "federatedId") 
        .field("userRecord.companyName", "organizationName")
        .field("userRecord.company_Country__c", "countryCode")
        .field("userRecord.defaultCurrencyIsoCode", "currencyCode")
        .field("userRecord.IDMSClassLevel2__c", "customerClass")
        .field("userRecord.company_City__c", "localityName")
        .field("userRecord.IDMSMarketSegment__c", "marketSegment")
        .field("userRecord.company_Postal_Code__c", "postalCode")
        .field("userRecord.IDMSCompanyPoBox__c", "postOfficeBox")
        .field("userRecord.company_State__c", "state")
        .field("userRecord.company_Address1__c", "street")
        .field("userRecord.IDMSCompanyHeadquarters__c", "headQuarter")
        .field("userRecord.company_Address2__c", "addInfoAddress")
        .field("userRecord.company_Address2__c", "postalAddress")
        .field("userRecord.IDMSCompanyCounty__c", "county")
        .field("userRecord.company_Website__c", "webSite")
        .field("userRecord.IDMSCompanyMarketServed__c", "marketServed")
        .field("userRecord.IDMSCompanyNbrEmployees__c", "employeeSize")
        .field("userRecord.IDMSCompanyNbrEmployees__c", "numberEmployees")
        .field("userRecord.IDMSTaxIdentificationNumber__c", "taxIdentificationNumber")
        .field("userRecord.IDMS_PreferredLanguage__c", "languageCode")
        //adding new mappings
        .field("userRecord.IDMSCompanyPoBox__c", "postOfficeBoxCode")
        .field("userRecord.IDMSClassLevel1__c", "businessType")
        .field("userRecord.IDMSCompanyMarketServed__c", "marketSegment")
        .field("userRecord.IDMSAnnualRevenue__c", "annualSales")
        .field("userRecord.BFO_ACCOUNT_ID__c", "bfoId")
        .field("userRecord.IDMSWorkPhone__c", "telephoneNumber")
        .byDefault()
        .register();
    }
	
	private void configureOpenAmUserRequestToUimsUserRequest(MapperFactory mapperFactory) {
        mapperFactory.classMap(OpenAmUserRequest.class,UimsUserRequest.class)
        .field("input.user.mail","input.email")
        .field("input.user.mobile","input.cell")
        .field("input.user.homePhone","input.phone")
        .field("input.user.givenName", "input.firstName")
        .field("input.user.sn", "input.lastName")
        
        .field("input.user.emailOptIn", "input.")
        .field("input.user.employeeType","input.")
        
        .field("input.user.c", "input.localityName")
        .field("input.user.preferredlanguage", "input.languageCode")
        
        .field("input.user.currency", "input.currencyCode")
        
        .field("input.user.street", "input.street")
        
        .field("input.user.l", "input.l")
        
        .field("input.user.postalCode", "input.postalCode")
        
        .field("input.user.st", "input.st")
        
        .field("input.user.county", "input.county")
        
        .field("input.user.postOfficeBox", "input.")
        
        .field("input.user.federationID", "input.federatedID")
        
        .field("input.user.registerationSource", "input.")
        .field("input.user.updateSource", "input.")
        
        .field("input.user.additionalInfo", "input.addInfoAddress")
        .field("input.user.companyName", "input.organizationName")
        .field("input.user.companyStreet", "input.street")
        .field("input.user.companyPostalCode", "input.postalCode")
        .field("input.user.companyState", "input.")
        .field("input.user.companyPostOfficeBox", "input.postOfficeBox")
        .field("input.user.companyCountry", "input.countryCode")
        
        .field("input.user.companyAdditionalInfo", "input.")
        .field("input.user.iam1", "input.")
        .field("input.user.iam2", "input.")
        .field("input.user.industrySegment", "input.")
        .field("input.user.industrySubSegment", "input.")
        .field("input.user.telephoneNumber", "input.")
        
        .field("input.user.title", "input.jobTitle")
        .field("input.user.jobFunction", "input.jobFunction")
        .field("input.user.jobDescription", "input.jobDescription")
        
        .field("input.user.industries", "input.")
        .field("input.user.employeeSize", "input.")
        
        .field("input.user.headquarters", "input.headQuarter")
        
        .field("input.user.annualRevenue", "input.")
        
        .field("input.user.taxID", "input.taxIdentificationNumber")
        .field("input.user.middleName", "input.middleName")
        .field("input.user.companyWebSite", "input.webSite")
        
        .field("input.user.initials", "input.")
        .field("input.user.departmentNumber", "input.")
        .field("input.user.suffix", "input.")
        
        .field("input.user.fax", "input.fax")
        .field("input.user.companyFederatedID", "input.federatedID")
        
        .field("input.user.delegatedIDP", "input.")
        .field("input.user.identityType", "input.")
        
        .field("input.user.primaryContact", "input.primaryContact")
        .field("input.user.companyCounty", "input.")
        
        .field("input.user.userPassword", "input.")
        .field("input.user.username", "input.")
        .field("input.user.loginid", "input.")
        .field("input.user.idmsuid", "input.")
        .field("input.user.hotpEmailVerification", "input.")
        .field("input.user.hotpMobileVerification", "input.")
        .field("input.user.tncFlag", "input.")
        .field("input.user.idmsail_c", "input.")
        .field("input.user.tmp_password", "input.")
        .byDefault()
        .register();
    }	
	
	
	private void configureIFWUserRequestToUIMSCompanyRequest(MapperFactory mapperFactory) {
        mapperFactory.classMap(CreateUserRequest.class,UIMSCompanyRequest.class)
        .field("userRecord.IDMSCompanyFederationIdentifier__c", "uimscompany.federatedID")
        .field("userRecord.companyName", "uimscompany.organizationName")
        .field("userRecord.IDMSCompanyNbrEmployees__c", "uimscompany.employeeSize")
        .field("userRecord.defaultCurrencyIsoCode", "uimscompany.currencyCode")
        .field("userRecord.IDMSClassLevel2__c", "uimscompany.customerClass")
        .field("userRecord.company_City__c", "uimscompany.localityName")
        .field("userRecord.IDMSMarketSegment__c", "uimscompany.marketSegment")
        .field("userRecord.company_Postal_Code__c", "uimscompany.postalCode")
        .field("userRecord.IDMSCompanyPoBox__c", "uimscompany.postOfficeBox")
        .field("userRecord.company_State__c", "uimscompany.st")
        .field("userRecord.company_Address1__c", "uimscompany.street")
        .field("userRecord.IDMSCompanyHeadquarters__c", "uimscompany.headQuarter")
        .field("userRecord.company_Address2__c", "uimscompany.addInfoAddress")
        .field("userRecord.IDMSCompanyCounty__c", "uimscompany.county")
        .field("userRecord.company_Website__c", "uimscompany.webSite")
        .field("userRecord.IDMSCompanyMarketServed__c", "uimscompany.marketServed")
        .field("userRecord.IDMSTaxIdentificationNumber__c", "uimscompany.taxIdentificationNumber")
        .byDefault()
        .register();
    }
	
	/**
	 * GoDigital Registration
	 * @param mapperFactory
	 */
	private void configureIFWUserRequestToGoDigitalUserRequest(MapperFactory mapperFactory) {
        mapperFactory.classMap(CreateUserRequest.class,UserRegistrationInfoRequest.class)
        
        .field("userRecord.email", "userRegistrationInfoRequest.userDetails.email")
        .field("userRecord.firstName", "userRegistrationInfoRequest.userDetails.firstName")
        .field("userRecord.lastName", "userRegistrationInfoRequest.userDetails.lastName")
        
        .field("userRecord.IDMS_PreferredLanguage__c", "userRegistrationInfoRequest.userDetails.languageCode")
        .field("userRecord.country", "userRegistrationInfoRequest.userDetails.countryCode")
        
        .field("userRecord.IDMSJobDescription__c", "userRegistrationInfoRequest.userDetails.jobDescription")
        .field("userRecord.job_Title__c", "userRegistrationInfoRequest.userDetails.jobTitle")
        .field("userRecord.job_Function__c", "userRegistrationInfoRequest.userDetails.jobFunction")
                
        .field("userRecord.IDMS_Federated_ID__c", "userRegistrationInfoRequest.userDetails.federatedId")
        .field("userRecord.companyFederatedId", "userRegistrationInfoRequest.userDetails.companyFederatedId")
        
        .field("userRecord.companyName", "userRegistrationInfoRequest.userDetails.companyName")
        .field("userRecord.company_Address1__c", "userRegistrationInfoRequest.userDetails.companyAddress")
        .field("userRecord.company_City__c", "userRegistrationInfoRequest.userDetails.companyCity")
        .field("userRecord.company_Postal_Code__c", "userRegistrationInfoRequest.userDetails.companyPostalCode")
        .field("userRecord.company_Country__c", "userRegistrationInfoRequest.userDetails.companyCountryCode")
        
        .field("userRecord.adminCompanyFederatedId", "userRegistrationInfoRequest.userDetails.adminCompanyFederatedId")
        .field("userRecord.adminCompanyFederatedId", "userRegistrationInfoRequest.userDetails.adminBfoAccountId")
        .field("userRecord.adminFederatedId", "userRegistrationInfoRequest.userDetails.adminFederatedId")
        .field("userRecord.IDMS_Federated_ID__c", "userRegistrationInfoRequest.userDetails.userId")
       
        
        
        .byDefault()
        .register();
    }
	
	/**
	 * User Registration Response
	 * @param mapperFactory
	 */
	private void configureIFWUserRequestToIDMSUserRecord(MapperFactory mapperFactory) {
        mapperFactory.classMap(CreateUserRequest.class,IDMSUserResponse.class)
        
        .field("userRecord.attributes", "attributes")
        .field("userRecord.id", "id")
        .field("userRecord.IDMS_Federated_ID__c", "iDMS_Federated_ID__c")
        .field("userRecord.IDMS_User_Context__c", "iDMS_User_Context__c")
        .field("userRecord.firstName", "firstName")
        .field("userRecord.lastName", "lastName")
        .field("userRecord.country", "country")
        .field("userRecord.email", "email")
        .field("userRecord.mobilePhone", "mobilePhone")
        .field("userRecord.IDMS_PreferredLanguage__c", "iDMS_PreferredLanguage__c")
        .field("userRecord.IDMS_Email_opt_in__c", "iDMS_Email_opt_in__c")
        .field("userRecord.defaultCurrencyIsoCode", "defaultCurrencyIsoCode")
        .field("userRecord.companyName", "companyName")
        .field("userRecord.company_Address1__c", "company_Address1__c")
        .field("userRecord.company_City__c", "company_City__c")
        .field("userRecord.company_Postal_Code__c", "company_Postal_Code__c")
        .field("userRecord.company_State__c", "company_State__c")
        .field("userRecord.IDMSCompanyPoBox__c", "iDMSCompanyPoBox__c")
        .field("userRecord.IDMSCompanyCounty__c", "iDMSCompanyCounty__c")
        .field("userRecord.company_Country__c", "company_Country__c")
        .field("userRecord.company_Address2__c", "company_Address2__c")
        .field("userRecord.company_Website__c", "company_Website__c")
        .field("userRecord.IDMSClassLevel1__c", "iDMSClassLevel1__c")
        .field("userRecord.IDMSClassLevel2__c", "iDMSClassLevel2__c")
        .field("userRecord.IDMSMarketSegment__c", "iDMSMarketSegment__c")
        .field("userRecord.IDMSMarketSubSegment__c", "iDMSMarketSubSegment__c")
        .field("userRecord.IDMSCompanyMarketServed__c", "iDMSCompanyMarketServed__c")
        .field("userRecord.IDMSCompanyNbrEmployees__c", "iDMSCompanyNbrEmployees__c")
        .field("userRecord.department", "department")
        .field("userRecord.IDMSCompanyHeadquarters__c", "iDMSCompanyHeadquarters__c")
        .field("userRecord.IDMSAnnualRevenue__c", "iDMSAnnualRevenue__c")
        .field("userRecord.IDMSTaxIdentificationNumber__c", "iDMSTaxIdentificationNumber__c")
        .field("userRecord.job_Title__c", "job_Title__c")
        .field("userRecord.job_Function__c", "job_Function__c")
        .field("userRecord.IDMSJobDescription__c", "iDMSJobDescription__c")
        .field("userRecord.phone", "phone")
        .field("userRecord.street", "street")
        .field("userRecord.city", "city")
        .field("userRecord.postalCode", "postalCode")
        .field("userRecord.state", "state")
        .field("userRecord.IDMS_County__c", "iDMS_County__c")
        .field("userRecord.IDMS_POBox__c", "iDMS_POBox__c")
        .field("userRecord.IDMS_AdditionalAddress__c", "iDMS_AdditionalAddress__c")
        .field("userRecord.IDMSMiddleName__c", "iDMSMiddleName__c")
        .field("userRecord.IDMSSalutation__c", "iDMSSalutation__c")
        .field("userRecord.IDMSSuffix__c", "iDMSSuffix__c")
        .field("userRecord.fax", "fax")
        .field("userRecord.accountId", "accountId")
        .field("userRecord.IDMS_Registration_Source__c", "iDMS_Registration_Source__c")
        .field("userRecord.IDMS_Profile_update_source__c", "iDMS_Profile_update_source__c")
        .field("userRecord.IDMS_TrustStatus__c", "iDMS_TrustStatus__c")
        .field("userRecord.IDMS_RejectionReason__c", "iDMS_RejectionReason__c")
        .field("userRecord.IDMS_RejectionComments__c", "iDMS_RejectionComments__c")
        //.field("userRecord.isActive", "isActive")
        .field("userRecord.BFO_ACCOUNT_ID__c", "bFO_ACCOUNT_ID__c")
        .field("userRecord.IDMSCompanyFederationIdentifier__c", "iDMSCompanyFederationIdentifier__c")
        .field("userRecord.IDMSDelegatedIdp__c", "iDMSDelegatedIdp__c")
        .field("userRecord.IDMSIdentityType__c", "iDMSIdentityType__c")
        .field("userRecord.tncFlag", "tncFlag")
        .field("userRecord.IDMSPrimaryContact__c", "iDMSPrimaryContact__c")
        .field("userRecord.companyFederatedId", "companyFederatedId")
        .field("userRecord.adminFederatedId", "adminFederatedId")
        .field("userRecord.adminCompanyFederatedId", "adminCompanyFederatedId")
        .field("userRecord.aboutMe", "aboutMe")
        .field("userRecord.trustedAdmin", "trustedAdmin")
        .field("userRecord.IDMS_Federated_ID__c", "federationIdentifier")
        .field("userRecord.companyName", "iDMS_Company_Name__c")
        .field("userRecord.company_Address1__c", "iDMS_Company_Address1__c")
        .field("userRecord.company_City__c", "iDMS_Company_City__c")
        .field("userRecord.company_Postal_Code__c", "iDMS_Company_Postal_Code__c")
        .field("userRecord.company_State__c", "iDMS_Company_State__c")
        .field("userRecord.IDMSCompanyPoBox__c", "iDMS_Company_Address_PO_BOX__c")
        .field("userRecord.IDMSCompanyCounty__c", "iDMS_Company_County__c")
        .field("userRecord.company_Country__c", "iDMS_Company_Country__c")
        .field("userRecord.company_Address2__c", "iDMS_Company_Address2__c")
        .field("userRecord.company_Website__c", "iDMS_Company_Website__c")
        .field("userRecord.IDMSClassLevel1__c", "iDMS_Company_I_am_a__c")
        .field("userRecord.IDMSClassLevel2__c", "iDMS_Company_My_Industry_is_a__c")
        .field("userRecord.IDMSCompanyMarketServed__c", "iDMS_Company_Industries_I_serve__c")
        .field("userRecord.IDMSCompanyNbrEmployees__c", "iDMS_Company_Employee_Size__c")
        .field("userRecord.IDMSCompanyHeadquarters__c", "iDMS_Company_Headquarters__c")
        .field("userRecord.IDMSTaxIdentificationNumber__c", "iDMS_Company_Tax_Identification_Number__c")
        
        .field("userRecord.channel__c", "channel__c")
        .field("userRecord.subChannel__c", "subChannel__c")
        .field("userRecord.contactId", "contactId")
        /*.field("userRecord.", "iDMSIsInternal__c")
        .field("userRecord.", "iDMSAil__c")
        .field("userRecord.", "iDMSAIL_Applications__c")
        .field("userRecord.", "iDMSAIL_Features__c")
        .field("userRecord.", "iDMSAIL_Programs__c")
        .field("userRecord.", "title")
        .field("userRecord.", "division")*/
        .field("userRecord.defaultCurrencyIsoCode", "iDMS_Company_Currency_ISO_Code__c")
        /*.field("", "userStatus__c")
        .field("", "socialProviders__c")*/
        .field("userRecord.IDMSWorkPhone__c", "IDMSWorkPhone__c")
        
        .byDefault()
        .register();
    }
	
	/**
	 * From global fail to create user then in confirm pin user not found then get the user from global and create it in china
	 * @param mapperFactory
	 */
	private void configureIFWGetUserResponseToIDMSCreateUserRequest(MapperFactory mapperFactory){
		mapperFactory.classMap(IFWCustomAttributesForWork.class, IFWUser.class)
		 .field("userId", "id")
		 .field("contactId", "id")
		 .field("accountId", "accountId")
		.field("uimsFederatedId", "IDMS_Federated_ID__c")
		 .field("idmsFederatedId", "IDMS_Federated_ID__c")
		 .field("userContext", "IDMS_User_Context__c")
		 .field("salutation", "IDMSSalutation__c")
		 .field("firstName", "firstName")
		 .field("middleName", "IDMSMiddleName__c")
		 .field("lastName", "lastName")
		 .field("countryCode", "country")
		 .field("email", "email")
		 .field("mobilePhone", "mobilePhone")
		 .field("languageCode", "IDMS_PreferredLanguage__c")
		 .field("emailOptIn", "IDMS_Email_opt_in__c")
		 .field("aboutMe", "aboutMe")
		 
		   .field("street", "street")
		 .field("city", "city")
		 .field("zipCode", "postalCode")
		 .field("stateOrProvinceCode", "state")
		 .field("county", "IDMS_County__c")
		 .field("pOBox", "IDMS_POBox__c")
		 .field("additionalAddress", "IDMS_AdditionalAddress__c")
		 .field("suffix", "IDMSSuffix__c")
		 .field("homePhone", "phone")
		 .field("fax", "fax")
		  .field("trustStatus", "IDMS_TrustStatus__c")
//		 .field("trustLevel", "")
		 .field("rejectionReason", "IDMS_RejectionReason__c")
		 .field("rejectionComment", "IDMS_RejectionComments__c")
		 .field("registrationSource", "IDMS_Registration_Source__c")
		 .field("profileLastUpdateSource", "IDMS_Profile_update_source__c")
		 .field("delegatedIdp", "IDMSDelegatedIdp__c")
		 .field("identityType", "IDMSIdentityType__c")
//		 .field("isInternal", "")
//		 .field("ail", "")
//		 .field("ailApplications", "")		 
//		 .field("ailFeatures", "")
//		 .field("ailPrograms", "")
		 .field("currency", "defaultCurrencyIsoCode")
		 .field("companyName", "companyName")
		 .field("companyStreet", "street")
		 .field("companyCity", "company_City__c")
		 .field("companyZipCode", "company_Postal_Code__c")
		 .field("companyStateOrProvinceCode", "company_State__c")
		 .field("companyPOBox", "IDMSCompanyPoBox__c")
		 .field("companyCounty", "IDMSCompanyCounty__c")
		 .field("companyCountryCode", "company_Country__c")
		 .field("companyAdditionalAddress", "company_Address2__c")
		 .field("companyWebsite", "company_Website__c")
		 .field("classLevel1", "IDMSClassLevel1__c")
		 .field("classLevel2", "IDMSClassLevel2__c")
		 .field("marketSegment", "IDMSMarketSegment__c")
		 
		 .field("marketSubSegment", "IDMSMarketSubSegment__c")
		 .field("marketServed", "IDMSCompanyMarketServed__c")
		 .field("employeeSize", "IDMSCompanyNbrEmployees__c")
		 .field("department", "department")
//		 .field("division", "")
//		 .field("title", "")
//		 .field("businessUnit", "")
		 .field("headquarter", "IDMSCompanyHeadquarters__c")
		 .field("annualRevenue", "IDMSAnnualRevenue__c")
		 .field("taxIdentificationNumber", "IDMSTaxIdentificationNumber__c")
		 .field("jobTitle", "job_Title__c")
		 .field("jobFunction", "job_Function__c")
		 .field("jobDescription", "IDMSJobDescription__c")
		 .field("workPhone", "phone")
		 .field("idmsHashedToken", "idmsHashedToken")
	        .byDefault()
	        .register();
	}
	
	
	
	
	
	
}
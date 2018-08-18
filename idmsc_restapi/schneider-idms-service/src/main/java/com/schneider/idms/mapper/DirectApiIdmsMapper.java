package com.schneider.idms.mapper;

import javax.inject.Named;

import com.idms.product.model.OpenAmUserRequest;
import com.schneider.idms.model.IdmsCreateUserResponse;
import com.schneider.idms.model.IdmsUpdateUserRequest;
import com.schneider.idms.model.IdmsUpdateUserResponse;
import com.schneider.idms.model.IdmsUserRequest;
import com.uims.authenticatedUsermanager.UserV6;
import com.uims.companymanager.CompanyV3;

import ma.glasnost.orika.MapperFactory;
import ma.glasnost.orika.impl.ConfigurableMapper;

@Named
public class DirectApiIdmsMapper extends ConfigurableMapper{
	



    @Override
    protected void configure(MapperFactory mapperFactory) {
    	configureIdmsUserRequestToOpenAmUserRequest(mapperFactory);
    	configureIdmsUserRequestToUimsCompanyV3(mapperFactory);
    	configureIdmsUserRequestToUimsUserV6(mapperFactory);
    	configureIdmsUserRequestToIdmsUserResponse(mapperFactory);
    	
    	configureIdmsUpdateUserRequestToOpenAmUserRequest(mapperFactory);
    	configureIdmsUpdateUserRequestToUimsCompanyV3(mapperFactory);
    	configureIdmsUpdateUserRequestToUimsUserV6(mapperFactory);
    	configureIdmsUpdareUserRequestToIdmsUpdateUserResponse(mapperFactory);
    }
    
   
	
	private void configureIdmsUserRequestToOpenAmUserRequest(MapperFactory mapperFactory) {
        mapperFactory.classMap(IdmsUserRequest.class,OpenAmUserRequest.class)
        
        .field("userContext", "input.user.employeeType")
        .field("salutation", "input.user.initials")
        .field("firstName", "input.user.givenName")
        .field("middleName", "input.user.middleName")
        .field("lastName", "input.user.sn")
        .field("countryCode", "input.user.c")
        .field("email", "input.user.mail")
        .field("mobilePhone", "input.user.mobile")
        .field("languageCode", "input.user.preferredlanguage")
        .field("emailOptIn", "input.user.emailOptIn")
        .field("aboutMe", "input.user.aboutMe")
        .field("street", "input.user.street")
        .field("city", "input.user.l")
        .field("zipCode", "input.user.postalCode")
        .field("stateOrProvinceCode", "input.user.st")
        .field("county", "input.user.county")
        .field("pOBox", "input.user.postOfficeBox")
        .field("additionalAddress", "input.user.additionalInfo")
        .field("suffix", "input.user.suffix")
        .field("homePhone", "input.user.homePhone")
        .field("fax", "input.user.fax")
        .field("idmsFederatedId", "input.user.federationID")
        .field("registrationSource", "input.user.registerationSource")
        .field("currency", "input.user.currency")
        .field("companyName", "input.user.companyName")
        .field("companyStreet", "input.user.companyStreet")
        .field("companyCity", "input.user.companyCity")
        .field("companyZipCode", "input.user.companyPostalCode")
        .field("companyStateOrProvinceCode", "input.user.companyState")
        .field("companyPOBox", "input.user.companyPostOfficeBox")
        .field("companyCounty", "input.user.companyCounty")
        .field("companyCountryCode", "input.user.companyCountry")
        .field("companyAdditionalAddress", "input.user.companyAdditionalInfo")
        .field("companyWebsite", "input.user.companyWebSite")
        .field("classLevel1", "input.user.iam1")
        .field("classLevel2", "input.user.iam2")
        .field("marketSegment", "input.user.industrySegment")
        .field("marketSubSegment", "input.user.industrySubSegment")
        .field("marketServed", "input.user.industries")
        .field("employeeSize", "input.user.employeeSize")
        .field("department", "input.user.departmentNumber")
        .field("headquarter", "input.user.headquarters")
        .field("annualRevenue", "input.user.annualRevenue")
        .field("taxIdentificationNumber", "input.user.taxID")
        .field("jobTitle", "input.user.title")
        .field("jobFunction", "input.user.jobFunction")
        .field("jobDescription", "input.user.jobDescription")
        .field("workPhone", "input.user.telephoneNumber")
        .field("trustedAdmin", "input.user.trustedAdmin")
        .field("companyFederatedId", "input.user.companyFederatedID")
        .field("adminCompanyFederatedId", "input.user.admin_company_id")
        .field("profileLastUpdateSource", "input.user.updateSource")
        .field("currencyCode", "input.user.currency") 
        .field("isActivated", "input.user.isActivated")
        .field("accountId", "input.user.companyID")
        .field("primaryContact", "input.user.primaryContact")
        .field("identityType", "input.user.identityType")
        .field("delegatedIdp", "input.user.delegatedIDP")
        
        .field("channel", "input.user.channel")
        .field("subchannel", "input.user.subchannel")
        /*.field("userRecord.tncFlag", "input.user.tncFlag")
        .field(UserConstants.MAPPER_CREATE_USER_PR_REQUEST,UserConstants.MAPPER_OPENAM_USER_PR_REQUEST)
        .field("userRecord.adminFederatedId", "input.user.admin_federated_id")
        .field("userRecord.invitationCode", "input.user.invitationCode")*/
        .byDefault()
        .register();
    }
	
	private void configureIdmsUserRequestToIdmsUserResponse(MapperFactory mapperFactory) {
        mapperFactory.classMap(IdmsUserRequest.class,IdmsCreateUserResponse.class)
        
        .field("userContext", "userContext")
        .field("salutation", "salutation")
        .field("firstName", "firstName")
        .field("middleName", "middleName")
        .field("lastName", "lastName")
        .field("countryCode", "countryCode")
        .field("email", "email")
        .field("mobilePhone", "mobilePhone")
        .field("languageCode", "languageCode")
        .field("emailOptIn", "emailOptIn")
        .field("aboutMe", "aboutMe")
        .field("street", "street")
        .field("city", "city")
        .field("zipCode", "zipCode")
        .field("stateOrProvinceCode", "stateOrProvinceCode")
        .field("county", "county")
        .field("pOBox", "pOBox")
        .field("additionalAddress", "additionalAddress")
        .field("suffix", "suffix")
        .field("homePhone", "homePhone")
        .field("fax", "fax")
        .field("accountId", "accountId")
        .field("registrationSource", "registrationSource")
        .field("currency", "currency")
        .field("companyName", "companyName")
        .field("companyStreet", "companyStreet")
        .field("companyCity", "companyCity")
        .field("companyZipCode", "companyZipCode")
        .field("companyStateOrProvinceCode", "companyStateOrProvinceCode")
        .field("companyPOBox", "companyPOBox")
        .field("companyCounty", "companyCounty")
        .field("companyCountryCode", "companyCountryCode")
        .field("companyAdditionalAddress", "companyAdditionalAddress")
        .field("companyWebsite", "companyWebsite")
        .field("classLevel1", "classLevel1")
        .field("classLevel2", "classLevel2")
        .field("marketSegment", "marketSegment")
        .field("marketSubSegment", "marketSubSegment")
        .field("marketServed", "marketServed")
        .field("employeeSize", "employeeSize")
        .field("department", "department")
        .field("headquarter", "headquarter")
        .field("annualRevenue", "annualRevenue")
        .field("taxIdentificationNumber", "taxIdentificationNumber")
        .field("jobTitle", "jobTitle")
        .field("jobFunction", "jobFunction")
        .field("jobDescription", "jobDescription")
        .field("workPhone", "workPhone")
        .field("trustedAdmin", "trustedAdmin")
        .field("companyFederatedId", "companyFederatedId")
        .field("currencyCode", "currencyCode") 
        .byDefault()
        .register();
    }
	
	
	private void configureIdmsUpdareUserRequestToIdmsUpdateUserResponse(MapperFactory mapperFactory) {
        mapperFactory.classMap(IdmsUpdateUserRequest.class,IdmsUpdateUserResponse.class)
        
        .field("salutation", "salutation")
        .field("firstName", "firstName")
        .field("middleName", "middleName")
        .field("lastName", "lastName")
        .field("countryCode", "countryCode")
        .field("email", "email")
        .field("mobilePhone", "mobilePhone")
        .field("languageCode", "languageCode")
        .field("emailOptIn", "emailOptIn")
        .field("aboutMe", "aboutMe")
        .field("street", "street")
        .field("city", "city")
        .field("zipCode", "zipCode")
        .field("stateOrProvinceCode", "stateOrProvinceCode")
        .field("county", "county")
        .field("pOBox", "pOBox")
        .field("additionalAddress", "additionalAddress")
        .field("suffix", "suffix")
        .field("homePhone", "homePhone")
        .field("fax", "fax")
        .field("profileLastUpdateSource", "profileLastUpdateSource")
        .field("currency", "currency")
        .field("companyName", "companyName")
        .field("companyStreet", "companyStreet")
        .field("companyCity", "companyCity")
        .field("companyZipCode", "companyZipCode")
        .field("companyStateOrProvinceCode", "companyStateOrProvinceCode")
        .field("companyPOBox", "companyPOBox")
        .field("companyCounty", "companyCounty")
        .field("companyCountryCode", "companyCountryCode")
        .field("companyAdditionalAddress", "companyAdditionalAddress")
        .field("companyWebsite", "companyWebsite")
        .field("classLevel1", "classLevel1")
        .field("classLevel2", "classLevel2")
        .field("marketSegment", "marketSegment")
        .field("marketSubSegment", "marketSubSegment")
        .field("marketServed", "marketServed")
        .field("employeeSize", "employeeSize")
        .field("department", "department")
        .field("headquarter", "headquarter")
        .field("annualRevenue", "annualRevenue")
        .field("taxIdentificationNumber", "taxIdentificationNumber")
        .field("jobTitle", "jobTitle")
        .field("jobFunction", "jobFunction")
        .field("jobDescription", "jobDescription")
        .field("workPhone", "workPhone")
        .field("trustedAdmin", "trustedAdmin")
        .byDefault()
        .register();
    }
	
	private void configureIdmsUserRequestToUimsCompanyV3(MapperFactory mapperFactory) {
        mapperFactory.classMap(IdmsUserRequest.class,CompanyV3.class)
        .field("idmsFederatedId", "federatedId") 
        .field("companyName", "organizationName")
        .field("countryCode", "countryCode")
        .field("currencyCode", "currencyCode")
        .field("classLevel2", "customerClass")
        .field("companyCity", "localityName")
        .field("marketSegment", "marketSegment")
        .field("zipCode", "postalCode")
        .field("pOBox", "postOfficeBox")
        .field("companyStateOrProvinceCode", "state")
        .field("companyStreet", "street")
        .field("headquarter", "headQuarter")
        .field("companyAdditionalAddress", "addInfoAddress")
        .field("companyCounty", "county")
        .field("companyWebsite", "webSite")
        .field("marketServed", "marketServed")
        .field("employeeSize", "employeeSize")
        //.field("userRecord.IDMSCompanyNbrEmployees__c", "numberEmployees")
        .field("taxIdentificationNumber", "taxIdentificationNumber")
        .field("languageCode", "languageCode")
        //adding new mappings
        .field("companyPOBox", "postOfficeBoxCode")
        .field("classLevel1", "businessType")
        .field("marketSegment", "marketSegment")
        .field("annualRevenue", "annualSales")
        .byDefault()
        .register();
    }
	
	private void configureIdmsUpdateUserRequestToUimsCompanyV3(MapperFactory mapperFactory) {
        mapperFactory.classMap(IdmsUpdateUserRequest.class,CompanyV3.class)
       /* .field("idmsFederatedId", "federatedId") */
        .field("companyName", "organizationName")
        .field("countryCode", "countryCode")
        .field("currencyCode", "currencyCode")
        .field("classLevel2", "customerClass")
        .field("companyCity", "localityName")
        .field("marketSegment", "marketSegment")
        .field("zipCode", "postalCode")
        .field("pOBox", "postOfficeBox")
        .field("companyStateOrProvinceCode", "state")
        .field("companyStreet", "street")
        .field("headquarter", "headQuarter")
        .field("companyAdditionalAddress", "addInfoAddress")
        .field("companyCounty", "county")
        .field("companyWebsite", "webSite")
        .field("marketServed", "marketServed")
        .field("employeeSize", "employeeSize")
        //.field("userRecord.IDMSCompanyNbrEmployees__c", "numberEmployees")
        .field("taxIdentificationNumber", "taxIdentificationNumber")
        .field("languageCode", "languageCode")
        //adding new mappings
        .field("companyPOBox", "postOfficeBoxCode")
        .field("classLevel1", "businessType")
        .field("marketSegment", "marketSegment")
        .field("annualRevenue", "annualSales")
        .byDefault()
        .register();
    }
	
	
	private void configureIdmsUserRequestToUimsUserV6(MapperFactory mapperFactory) {
        mapperFactory.classMap(IdmsUserRequest.class,UserV6.class)
        .field("email", "email") 
        .field("mobilePhone", "cell")
        .field("firstName", "firstName")
        .field("lastName", "lastName")
        .field("idmsFederatedId", "federatedID")
        .field("languageCode", "languageCode")
        .field("countryCode", "countryCode")
        .field("additionalAddress", "addInfoAddress")
        .field("jobDescription", "jobDescription")
        .field("jobFunction", "jobFunction")
        .field("jobTitle", "jobTitle")
        .field("city", "localityName")
        .field("middleName", "middleName")
        .field("pOBox", "postOfficeBox")
        .field("salutation", "salutation")
        .field("street", "street")
        .field("zipCode", "postalCode")
        .field("stateOrProvinceCode", "state")
        .field("homePhone", "phone")
        .field("fax", "fax")
        .field("county", "county")
        .field("mobilePhone", "phoneId")
        //.field("userRecord.mobilePhone", "phoneId")
        //.field("userRecord.country", "input.countrycode")
        ////.field("userRecord.IDMS_Email_opt_in__c", "input.emailOptIn")

        .byDefault()
        .register();
    }
	
	private void configureIdmsUpdateUserRequestToUimsUserV6(MapperFactory mapperFactory) {
        mapperFactory.classMap(IdmsUpdateUserRequest.class,UserV6.class)
        .field("email", "email") 
        .field("mobilePhone", "cell")
        .field("firstName", "firstName")
        .field("lastName", "lastName")
        /*.field("idmsFederatedId", "federatedID")*/
        .field("languageCode", "languageCode")
        .field("countryCode", "countryCode")
        .field("additionalAddress", "addInfoAddress")
        .field("jobDescription", "jobDescription")
        .field("jobFunction", "jobFunction")
        .field("jobTitle", "jobTitle")
        .field("city", "localityName")
        .field("middleName", "middleName")
        .field("pOBox", "postOfficeBox")
        .field("salutation", "salutation")
        .field("street", "street")
        .field("zipCode", "postalCode")
        .field("stateOrProvinceCode", "state")
        .field("homePhone", "phone")
        .field("fax", "fax")
        .field("county", "county")
        .field("mobilePhone", "phoneId")
        //.field("userRecord.mobilePhone", "phoneId")
        //.field("userRecord.country", "input.countrycode")
        ////.field("userRecord.IDMS_Email_opt_in__c", "input.emailOptIn")

        .byDefault()
        .register();
    }
	
	private void configureIdmsUpdateUserRequestToOpenAmUserRequest(MapperFactory mapperFactory) {
        mapperFactory.classMap(IdmsUserRequest.class,OpenAmUserRequest.class)
        
        .field("salutation", "input.user.initials")
        .field("firstName", "input.user.givenName")
        .field("middleName", "input.user.middleName")
        .field("lastName", "input.user.sn")
        .field("countryCode", "input.user.c")
        .field("email", "input.user.mail")
        .field("mobilePhone", "input.user.mobile")
        .field("languageCode", "input.user.preferredlanguage")
        .field("emailOptIn", "input.user.emailOptIn")
        .field("aboutMe", "input.user.aboutMe")
        .field("street", "input.user.street")
        .field("city", "input.user.l")
        .field("zipCode", "input.user.postalCode")
        .field("stateOrProvinceCode", "input.user.st")
        .field("county", "input.user.county")
        .field("pOBox", "input.user.postOfficeBox")
        .field("additionalAddress", "input.user.additionalInfo")
        .field("suffix", "input.user.suffix")
        .field("homePhone", "input.user.homePhone")
        .field("fax", "input.user.fax")
        .field("profileLastUpdateSource", "input.user.updateSource")
        .field("currency", "input.user.currency")
        .field("companyName", "input.user.companyName")
        .field("companyStreet", "input.user.companyStreet")
        .field("companyCity", "input.user.companyCity")
        .field("companyZipCode", "input.user.companyPostalCode")
        .field("companyStateOrProvinceCode", "input.user.companyState")
        .field("companyPOBox", "input.user.companyPostOfficeBox")
        .field("companyCounty", "input.user.companyCounty")
        .field("companyCountryCode", "input.user.companyCountry")
        .field("companyAdditionalAddress", "input.user.companyAdditionalInfo")
        .field("companyWebsite", "input.user.companyWebSite")
        .field("classLevel1", "input.user.iam1")
        .field("classLevel2", "input.user.iam2")
        .field("marketSegment", "input.user.industrySegment")
        .field("marketSubSegment", "input.user.industrySubSegment")
        .field("marketServed", "input.user.industries")
        .field("employeeSize", "input.user.employeeSize")
        .field("department", "input.user.departmentNumber")
        .field("headquarter", "input.user.headquarters")
        .field("annualRevenue", "input.user.annualRevenue")
        .field("taxIdentificationNumber", "input.user.taxID")
        .field("jobTitle", "input.user.title")
        .field("jobFunction", "input.user.jobFunction")
        .field("jobDescription", "input.user.jobDescription")
        .field("workPhone", "input.user.telephoneNumber")
        .field("trustedAdmin", "input.user.trustedAdmin")
        /*.field("companyFederatedId", "input.user.companyFederatedID")
        .field("adminCompanyFederatedId", "input.user.admin_company_id")
        .field("profileLastUpdateSource", "input.user.updateSource")
        .field("currencyCode", "input.user.currency") 
        .field("isActivated", "input.user.isActivated")
        .field("accountId", "input.user.companyID")
        .field("primaryContact", "input.user.primaryContact")
        .field("identityType", "input.user.identityType")
        .field("delegatedIdp", "input.user.delegatedIDP")*/
        
        /*.field("userRecord.tncFlag", "input.user.tncFlag")
        .field(UserConstants.MAPPER_CREATE_USER_PR_REQUEST,UserConstants.MAPPER_OPENAM_USER_PR_REQUEST)
        .field("userRecord.adminFederatedId", "input.user.admin_federated_id")
        .field("userRecord.invitationCode", "input.user.invitationCode")*/
        .byDefault()
        .register();
    }
}

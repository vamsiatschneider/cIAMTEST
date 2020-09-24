package com.idms.service;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

import javax.ws.rs.core.Response;

import org.json.simple.JSONObject;

import com.idms.model.IFWUser;
import com.idms.product.model.OpenAmUser;

/**
 * Prepare mock data for domain classes. Note: Do NOT use Mock Data class for
 * Integration Tests. All values here are dummy.
 */
public class DomainMockData {

	// Note: No Java doc necessary for constants UNLESS it is not
	// self-documenting. If in doubt, provide the Java doc.

	
	public static final String EMAIL = "test@mailinator.com";

	public static final String MOBILE_PHONE = "09739200404";

	public static final String PHONE = "123-456-7890";

	public static final String FIRST_NAME = "FIRST_NAME";

	public static final String LAST_NAME = "LAST_NAME";

	public static final String IDMS_EMAIL_OPT_IN__C = "Y";

	public static final String IDMS_USER_CONTEXT__C = "@Home";

	public static final String COUNTRY = "IN";

	public static final String IDMS_PREFERREDLANGUAGE__C = "en";

	public static final String DEFAULTCURRENCYISOCODE = "DZD";

	public static final String STREET = "STREET";

	public static final String CITY = "CITY";

	public static final String POSTALCODE = "POSTALCODE";

	public static final String STATE = "STATE";

	public static final String IDMS_COUNTY__C = "IDMS_COUNTY__C";

	public static final String IDMS_POBOX__C = "IDMS_POBOX__C";

	public static final String IDMS_Federated_ID__c = "IDMS_Federated_ID__c";

	public static final String IDMS_Registration_Source__c = "IDMS_Registration_Source__c";

	public static final String IDMS_Profile_update_source__c = "IDMS_Profile_update_source__c";

	public static final String IDMS_AdditionalAddress__c = "IDMS_AdditionalAddress__c";

	public static final String CompanyName = "CompanyName";

	public static final String Company_Address1__c = "Company_Address1__c";

	public static final String Company_City__c = "Company_City__c";

	public static final String Company_Postal_Code__c = "Company_Postal_Code__c";

	public static final String Company_State__c = "Company_State__c";

	public static final String IDMSCompanyPoBox__c = "IDMSCompanyPoBox__c";

	public static final String Company_Country__c = "Company_Country__c";

	public static final String Company_Address2__c = "Company_Address2__c";

	public static final String IDMSClassLevel1__c = "IDMSClassLevel1__c";

	public static final String IDMSClassLevel2__c = "IDMSClassLevel2__c";

	public static final String IDMSMarketSegment__c = "IDMSMarketSegment__c";

	public static final String IDMSMarketSubSegment__c = "IDMSMarketSubSegment__c";

	public static final String Job_Title__c = "Job_Title__c";

	public static final String Job_Function__c = "Job_Function__c";

	public static final String IDMSJobDescription__c = "IDMSJobDescription__c";

	public static final String IDMSCompanyMarketServed__c = "IDMSCompanyMarketServed__c";

	public static final String IDMSCompanyNbrEmployees__c = "1,2 to 5";

	public static final String IDMSCompanyHeadquarters__c = "TRUE";

	public static final String IDMSAnnualRevenue__c = "2.0";

	public static final String IDMSTaxIdentificationNumber__c = "IDMSTaxIdentificationNumber__c";

	public static final String IDMSMiddleName__c = "IDMSMiddleName__c";

	public static final String Company_Website__c = "Company_Website__c";

	public static final String IDMSSalutation__c = "IDMSSalutation__c";

	public static final String Department = "Department";

	public static final String IDMSSuffix__c = "IDMSSuffix__c";

	public static final String Fax = "Fax";

	public static final String IDMSCompanyFederationIdentifier__c = "IDMSCompanyFederationIdentifier__c";

	public static final String IDMSDelegatedIdp__c = "IDMSDelegatedIdp__c";

	public static final String IDMSIdentityType__c = "IDMSIdentityType__c";

	public static final String IDMSPrimaryContact__c = "1234567890";

	public static final String IDMSCOMPANYCOUNTY__C = "IDMSCOMPANYCOUNTY__C";
	
	
	
	public static final String AUTHENTICATION_JSON = "{\"tokenId\":\"AQIC5wM2LY4SfcysPQTDKWWZ0R9pEMb68DRr1cZASWa3VmA.*AAJTSQACMDEAAlNLABQtNTc0NjUwNzAxODcxMTc0OTkzMgACUzEAAA..*\",\"successUrl\":\"/accessmanager/console\"}";
	
	public static final String USER_EXISTS = "{\"result\":[],\"resultCount\":0,\"pagedResultsCookie\":null,\"totalPagedResultsPolicy\":\"NONE\",\"totalPagedResults\":-1,\"remainingPagedResults\":-1}";
	
	public static final String USER_EXISTS_TRUE = "{\"result\":[{\"username\":\"cn00297b214c-d97c-4136-98f6-9ac8c1745a4c\",\"realm\":\"/se\",\"mail\":[\"godtest33@mailinator.com\"],\"Loginid\":[\"godtest33@mailinator.com\"],\"companyName\":[\"Accenture\"],\"postalCode\":[\"1100092\"],\"companyState\":[\"010\"],\"county\":[\"test\"],\"dn\":[\"uid=cn00297b214c-d97c-4136-98f6-9ac8c1745a4c,ou=people,dc=schneider-electric,dc=com\"],\"companyStreet\":[\"Sector 21, Gurgaon\"],\"createTimestamp\":[\"20171122104904Z\"],\"companyPostOfficeBox\":[\"122001\"],\"registerationSource\":[\"idms\"],\"postOfficeBox\":[\"110092\"],\"additionalInfo\":[\"testing\"],\"companyCountry\":[\"CN\"],\"IDMSUID\":[\"godtest33@mailinator.com\"],\"companyPostalCode\":[\"122001\"],\"fax\":[\"123456789\"],\"companyCity\":[\"Gurgaon\"],\"initials\":[\"Z003\"],\"givenName\":[\"godigital\"],\"objectClass\":[\"iplanet-am-managed-person\",\"inetuser\",\"sunFederationManagerDataStore\",\"sunFMSAML2NameIdentifier\",\"inetorgperson\",\"devicePrintProfilesContainer\",\"sunIdentityServerLibertyPPService\",\"pushDeviceProfilesContainer\",\"iplanet-am-user-service\",\"iPlanetPreferences\",\"forgerock-am-dashboard-service\",\"organizationalperson\",\"top\",\"kbaInfoContainer\",\"oathDeviceProfilesContainer\",\"sunAMAuthAccountLockout\",\"person\",\"iplanet-am-auth-configuration-service\",\"chinaIdmsPerson\"],\"IDMSAIL_Programs_c\":[\"[]\"],\"tmp_password\":[\"V2VsY29tZTE=\"],\"headquarters\":[\"true\"],\"departmentNumber\":[\"Security\"],\"preferredlanguage\":[\"fr\"],\"hotpEmailVerification\":[\"godtest33@mailinator.com:cn00297b214c-d97c-4136-98f6-9ac8c1745a4c:fr:idms:godigital china\"],\"jobDescription\":[\"Technical\"],\"federationID\":[\"cn00297b214c-d97c-4136-98f6-9ac8c1745a4c\"],\"IDMSisInternal__c\":[\"FALSE\"],\"annualRevenue\":[\"10978686\"],\"emailOptIn\":[\"Y\"],\"iam1\":[\"EU\"],\"companyFederatedID\":[\"6372c85d-c436-4ebe-994b-199082800ba6\"],\"modifyTimestamp\":[\"20171122104956Z\"],\"uid\":[\"cn00297b214c-d97c-4136-98f6-9ac8c1745a4c\"],\"IDMSAIL_Applications_c\":[\"[]\"],\"street\":[\"Delhi\"],\"currency\":[\"XOF\"],\"sn\":[\"china\"],\"IDMSAil_c\":[\"[]\"],\"st\":[\"010\"],\"c\":[\"CN\"],\"IDMSAIL_Features_c\":[\"[]\"],\"companyWebSite\":[\"www.schneiderelectric.com\"],\"cn\":[\"godigital china\"],\"l\":[\"Delhi\"],\"AuthID\":[\"9b247a9995b4244414746f9893d6a4c222a3428ed952f0af26133f3bdb91bc1e:1511347744602\"],\"employeeType\":[\"@Work\"],\"updateSource\":[\"Uims\"],\"universalid\":[\"id=cn00297b214c-d97c-4136-98f6-9ac8c1745a4c,ou=user,o=se,ou=services,dc=schneider-electric,dc=com\"],\"V_Old\":[\"2\"],\"companyAdditionalInfo\":[\"Tower-B,Unitech Infospace\"],\"primaryContact\":[\"true\"],\"hotpMobileVerification\":[\"null:cn00297b214c-d97c-4136-98f6-9ac8c1745a4c:fr:idms:godigital china\"],\"inetUserStatus\":[\"Active\"],\"V_New\":[\"2\"]}],\"resultCount\":1,\"pagedResultsCookie\":null,\"totalPagedResultsPolicy\":\"NONE\",\"totalPagedResults\":-1,\"remainingPagedResults\":-1}";
	
	public static final String USER_EXISTS_WITHOUT_LOGINID = "{\"result\":[{\"username\":\"cn00297b214c-d97c-4136-98f6-9ac8c1745a4c\",\"realm\":\"/se\",\"mail\":[\"godtest33@mailinator.com\"],\"companyName\":[\"Accenture\"],\"postalCode\":[\"1100092\"],\"companyState\":[\"010\"],\"county\":[\"test\"],\"dn\":[\"uid=cn00297b214c-d97c-4136-98f6-9ac8c1745a4c,ou=people,dc=schneider-electric,dc=com\"],\"companyStreet\":[\"Sector 21, Gurgaon\"],\"createTimestamp\":[\"20171122104904Z\"],\"companyPostOfficeBox\":[\"122001\"],\"registerationSource\":[\"idms\"],\"postOfficeBox\":[\"110092\"],\"additionalInfo\":[\"testing\"],\"companyCountry\":[\"CN\"],\"IDMSUID\":[\"godtest33@mailinator.com\"],\"companyPostalCode\":[\"122001\"],\"fax\":[\"123456789\"],\"companyCity\":[\"Gurgaon\"],\"initials\":[\"Z003\"],\"givenName\":[\"godigital\"],\"objectClass\":[\"iplanet-am-managed-person\",\"inetuser\",\"sunFederationManagerDataStore\",\"sunFMSAML2NameIdentifier\",\"inetorgperson\",\"devicePrintProfilesContainer\",\"sunIdentityServerLibertyPPService\",\"pushDeviceProfilesContainer\",\"iplanet-am-user-service\",\"iPlanetPreferences\",\"forgerock-am-dashboard-service\",\"organizationalperson\",\"top\",\"kbaInfoContainer\",\"oathDeviceProfilesContainer\",\"sunAMAuthAccountLockout\",\"person\",\"iplanet-am-auth-configuration-service\",\"chinaIdmsPerson\"],\"IDMSAIL_Programs_c\":[\"[]\"],\"tmp_password\":[\"V2VsY29tZTE=\"],\"headquarters\":[\"true\"],\"departmentNumber\":[\"Security\"],\"preferredlanguage\":[\"fr\"],\"hotpEmailVerification\":[\"godtest33@mailinator.com:cn00297b214c-d97c-4136-98f6-9ac8c1745a4c:fr:idms:godigital china\"],\"jobDescription\":[\"Technical\"],\"federationID\":[\"cn00297b214c-d97c-4136-98f6-9ac8c1745a4c\"],\"IDMSisInternal__c\":[\"FALSE\"],\"annualRevenue\":[\"10978686\"],\"emailOptIn\":[\"Y\"],\"iam1\":[\"EU\"],\"companyFederatedID\":[\"6372c85d-c436-4ebe-994b-199082800ba6\"],\"modifyTimestamp\":[\"20171122104956Z\"],\"uid\":[\"cn00297b214c-d97c-4136-98f6-9ac8c1745a4c\"],\"IDMSAIL_Applications_c\":[\"[]\"],\"street\":[\"Delhi\"],\"currency\":[\"XOF\"],\"sn\":[\"china\"],\"IDMSAil_c\":[\"[]\"],\"st\":[\"010\"],\"c\":[\"CN\"],\"IDMSAIL_Features_c\":[\"[]\"],\"companyWebSite\":[\"www.schneiderelectric.com\"],\"cn\":[\"godigital china\"],\"l\":[\"Delhi\"],\"AuthID\":[\"9b247a9995b4244414746f9893d6a4c222a3428ed952f0af26133f3bdb91bc1e:1511347744602\"],\"employeeType\":[\"@Work\"],\"updateSource\":[\"Uims\"],\"universalid\":[\"id=cn00297b214c-d97c-4136-98f6-9ac8c1745a4c,ou=user,o=se,ou=services,dc=schneider-electric,dc=com\"],\"V_Old\":[\"2\"],\"companyAdditionalInfo\":[\"Tower-B,Unitech Infospace\"],\"primaryContact\":[\"true\"],\"hotpMobileVerification\":[\"null:cn00297b214c-d97c-4136-98f6-9ac8c1745a4c:fr:idms:godigital china\"],\"inetUserStatus\":[\"Active\"],\"V_New\":[\"2\"]}],\"resultCount\":1,\"pagedResultsCookie\":null,\"totalPagedResultsPolicy\":\"NONE\",\"totalPagedResults\":-1,\"remainingPagedResults\":-1}";
	
	public static final String USER_EXISTS_TRUE_WITH_NEWEMAIL = "{\"result\":[{\"username\":\"cn00297b214c-d97c-4136-98f6-9ac8c1745a4c\",\"realm\":\"/se\",\"mail\":[\"godtest33@mailinator.com\"],\"newmail\":[\"godtestNew@mailinator.com\"],\"Loginid\":[\"godtest33@mailinator.com\"],\"companyName\":[\"Accenture\"],\"postalCode\":[\"1100092\"],\"companyState\":[\"010\"],\"county\":[\"test\"],\"dn\":[\"uid=cn00297b214c-d97c-4136-98f6-9ac8c1745a4c,ou=people,dc=schneider-electric,dc=com\"],\"companyStreet\":[\"Sector 21, Gurgaon\"],\"createTimestamp\":[\"20171122104904Z\"],\"companyPostOfficeBox\":[\"122001\"],\"registerationSource\":[\"idms\"],\"postOfficeBox\":[\"110092\"],\"additionalInfo\":[\"testing\"],\"companyCountry\":[\"CN\"],\"IDMSUID\":[\"godtest33@mailinator.com\"],\"companyPostalCode\":[\"122001\"],\"fax\":[\"123456789\"],\"companyCity\":[\"Gurgaon\"],\"initials\":[\"Z003\"],\"givenName\":[\"godigital\"],\"objectClass\":[\"iplanet-am-managed-person\",\"inetuser\",\"sunFederationManagerDataStore\",\"sunFMSAML2NameIdentifier\",\"inetorgperson\",\"devicePrintProfilesContainer\",\"sunIdentityServerLibertyPPService\",\"pushDeviceProfilesContainer\",\"iplanet-am-user-service\",\"iPlanetPreferences\",\"forgerock-am-dashboard-service\",\"organizationalperson\",\"top\",\"kbaInfoContainer\",\"oathDeviceProfilesContainer\",\"sunAMAuthAccountLockout\",\"person\",\"iplanet-am-auth-configuration-service\",\"chinaIdmsPerson\"],\"IDMSAIL_Programs_c\":[\"[]\"],\"tmp_password\":[\"V2VsY29tZTE=\"],\"headquarters\":[\"true\"],\"departmentNumber\":[\"Security\"],\"preferredlanguage\":[\"fr\"],\"hotpEmailVerification\":[\"godtest33@mailinator.com:cn00297b214c-d97c-4136-98f6-9ac8c1745a4c:fr:idms:godigital china\"],\"jobDescription\":[\"Technical\"],\"federationID\":[\"cn00297b214c-d97c-4136-98f6-9ac8c1745a4c\"],\"IDMSisInternal__c\":[\"FALSE\"],\"annualRevenue\":[\"10978686\"],\"emailOptIn\":[\"Y\"],\"iam1\":[\"EU\"],\"companyFederatedID\":[\"6372c85d-c436-4ebe-994b-199082800ba6\"],\"modifyTimestamp\":[\"20171122104956Z\"],\"uid\":[\"cn00297b214c-d97c-4136-98f6-9ac8c1745a4c\"],\"IDMSAIL_Applications_c\":[\"[]\"],\"street\":[\"Delhi\"],\"currency\":[\"XOF\"],\"sn\":[\"china\"],\"IDMSAil_c\":[\"[]\"],\"st\":[\"010\"],\"c\":[\"CN\"],\"IDMSAIL_Features_c\":[\"[]\"],\"companyWebSite\":[\"www.schneiderelectric.com\"],\"cn\":[\"godigital china\"],\"l\":[\"Delhi\"],\"AuthID\":[\"9b247a9995b4244414746f9893d6a4c222a3428ed952f0af26133f3bdb91bc1e:1511347744602\"],\"employeeType\":[\"@Work\"],\"updateSource\":[\"Uims\"],\"universalid\":[\"id=cn00297b214c-d97c-4136-98f6-9ac8c1745a4c,ou=user,o=se,ou=services,dc=schneider-electric,dc=com\"],\"V_Old\":[\"2\"],\"companyAdditionalInfo\":[\"Tower-B,Unitech Infospace\"],\"primaryContact\":[\"true\"],\"hotpMobileVerification\":[\"null:cn00297b214c-d97c-4136-98f6-9ac8c1745a4c:fr:idms:godigital china\"],\"inetUserStatus\":[\"Active\"],\"V_New\":[\"2\"]}],\"resultCount\":1,\"pagedResultsCookie\":null,\"totalPagedResultsPolicy\":\"NONE\",\"totalPagedResults\":-1,\"remainingPagedResults\":-1}";
	
	public static final String USER_REGISTRATION = "{\"type\":\"selfRegistration\",\"tag\":\"end\",\"status\":{\"success\":true},\"additions\":{}}";
	
	public static final String EMAIL_OTP = "{authId=eyAidHlwIjogIkpXVCIsICJhbGciOiAiSFMyNTYiIH0.eyAiYXV0aEluZGV4VmFsdWUiOiAiSE9UUF9FbWFpbCIsICJvdGsiOiAidjZnZDdmazhqazFlaG01cWdhdW4xaDdiNjQiLCAiYXV0aEluZGV4VHlwZSI6ICJzZXJ2aWNlIiwgInJlYWxtIjogIm89c2Usb3U9c2VydmljZXMsZGM9c2NobmVpZGVyLWVsZWN0cmljLGRjPWNvbSIsICJzZXNzaW9uSWQiOiAiQVFJQzV3TTJMWTRTZmN6UkNjcjZCVTJRdVlka1oyd2FvUjcyTkdyeVJCeHB3dTAuKkFBSlRTUUFDTURFQUFsTkxBQk14TkRrNE5Ua3pNak0wT1RJM05EazBPVGs1QUFKVE1RQUEqIiB9.W3lfIX-JNBYjupx4foE6RZ70eLeVJXkfROnBGeCN7B8, template=, stage=HOTP2, header=Please enter your One Time Password, or request a new one, callbacks=[{\"type\":\"PasswordCallback\",\"output\":[{\"name\":\"prompt\",\"value\":\"Enter OTP\"}],\"input\":[{\"name\":\"IDToken1\",\"value\":\"\"}]},{\"type\":\"ConfirmationCallback\",\"output\":[{\"name\":\"prompt\",\"value\":\"\"},{\"name\":\"messageType\",\"value\":0},{\"name\":\"options\",\"value\":[\"Submit OTP\",\"Request OTP\"]},{\"name\":\"optionType\",\"value\":-1},{\"name\":\"defaultOption\",\"value\":0}],\"input\":[{\"name\":\"IDToken2\",\"value\":1}]}]}";
	
	public static final String MOBILE_OTP ="{authId=eyAidHlwIjogIkpXVCIsICJhbGciOiAiSFMyNTYiIH0.eyAiYXV0aEluZGV4VmFsdWUiOiAiSE9UUF9Nb2JpbGVfQ3JlYXRlX1BXRFJFUSIsICJvdGsiOiAidXEza2w5MWVzdGVrdHYydGZnbGc2bDM5anMiLCAiYXV0aEluZGV4VHlwZSI6ICJzZXJ2aWNlIiwgInJlYWxtIjogIm89c2Usb3U9c2VydmljZXMsZGM9c2NobmVpZGVyLWVsZWN0cmljLGRjPWNvbSIsICJzZXNzaW9uSWQiOiAiQVFJQzV3TTJMWTRTZmN5Y3loSEY4U2g5LVlUcWpNblpVNFBRd2pUd250Z1NtN2cuKkFBSlRTUUFDTURFQUFsTkxBQlF0TnpVME56WXdOamd6T0RZeE5EWXdPVGc1TmdBQ1V6RUFBQS4uKiIgfQ.i6Zhhv14RYQLMrumMseGNvWW4dJII293zfSwudw72no, template=, stage=HOTP2, header=Please enter your One Time Password, or request a new one, callbacks=[{\"type\":\"PasswordCallback\",\"output\":[{\"name\":\"prompt\",\"value\":\"Enter OTP\"}],\"input\":[{\"name\":\"IDToken1\",\"value\":\"\"}]},{\"type\":\"ConfirmationCallback\",\"output\":[{\"name\":\"prompt\",\"value\":\"\"},{\"name\":\"messageType\",\"value\":0},{\"name\":\"options\",\"value\":[\"Submit OTP\",\"Request OTP\"]},{\"name\":\"optionType\",\"value\":-1},{\"name\":\"defaultOption\",\"value\":0}],\"input\":[{\"name\":\"IDToken2\",\"value\":1}]}]}";

	public static final String GET_USER ="{\"username\":\"37260d8c-78ac-4023-a7c1-f04c43100f0e\",\"realm\":\"/se\",\"Loginid\":[\"13655207991\"],\"companyName\":[\"Accenture\"],\"postalCode\":[\"560100\"],\"companyState\":[\"01\"],\"county\":[\"test\"],\"dn\":[\"uid=37260d8c-78ac-4023-a7c1-f04c43100f0e,ou=people,dc=schneider-electric,dc=com\"],\"createTimestamp\":[\"20170406125730Z\"],\"companyStreet\":[\"OuterRingRoad, Bangalore\"],\"companyPostOfficeBox\":[\"122001\"],\"registerationSource\":[\"IDMSWork\"],\"postOfficeBox\":[\"560100\"],\"additionalInfo\":[\"testing\"],\"companyCountry\":[\"IN\"],\"IDMSUID\":[\"44444444442bridge-fo.com\"],\"companyPostalCode\":[\"122001\"],\"fax\":[\"123456789\"],\"companyCity\":[\"Bangalore\"],\"initials\":[\"Z003\"],\"givenName\":[\"SURESH\"],\"objectClass\":[\"iplanet-am-managed-person\",\"inetuser\",\"sunFederationManagerDataStore\",\"sunFMSAML2NameIdentifier\",\"inetorgperson\",\"devicePrintProfilesContainer\",\"sunIdentityServerLibertyPPService\",\"pushDeviceProfilesContainer\",\"iplanet-am-user-service\",\"iPlanetPreferences\",\"forgerock-am-dashboard-service\",\"organizationalperson\",\"top\",\"kbaInfoContainer\",\"oathDeviceProfilesContainer\",\"sunAMAuthAccountLockout\",\"person\",\"iplanet-am-auth-configuration-service\",\"chinaIdmsPerson\"],\"headquarters\":[\"true\"],\"departmentNumber\":[\"Security\"],\"preferredlanguage\":\"fr\",\"jobDescription\":[\"Technical\"],\"hotpEmailVerification\":[\"null:37260d8c-78ac-4023-a7c1-f04c43100f0e:fr:IDMSWork\"],\"annualRevenue\":[\"10978686\"],\"telephoneNumber\":[\"0124-490-3000\"],\"emailOptIn\":[\"Y\"],\"iam1\":[\"EU\"],\"modifyTimestamp\":[\"20170406125854Z\"],\"uid\":[\"37260d8c-78ac-4023-a7c1-f04c43100f0e\"],\"street\":[\"BANGALORE\"],\"currency\":[\"XOF\"],\"sn\":[\"BACHU\"],\"st\":[\"01\"],\"c\":[\"IN\"],\"mobile\":[\"44444444442\"],\"companyWebSite\":[\"www.accenture.com\"],\"cn\":\"37260d8c-78ac-4023-a7c1-f04c43100f0e\",\"l\":[\"BANGALORE\"],\"AuthID\":[\"eyAidHlwIjogIkpXVCIsICJhbGciOiAiSFMyNTYiIH0.eyAiYXV0aEluZGV4VmFsdWUiOiAiSE9UUF9Nb2JpbGVfQ3JlYXRlX1BXRFJFUSIsICJvdGsiOiAiNHEwdTJlNWFqazNiOHF0MWlqMmRnZ29qaWIiLCAiYXV0aEluZGV4VHlwZSI6ICJzZXJ2aWNlIiwgInJlYWxtIjogIm89c2Usb3U9c2VydmljZXMsZGM9c2NobmVpZGVyLWVsZWN0cmljLGRjPWNvbSIsICJzZXNzaW9uSWQiOiAiQVFJQzV3TTJMWTRTZmN5cWdIaXkxaHZOUlVKOGNWV1A0SHNwTlZBbjNrdi1JV1UuKkFBSlRTUUFDTURFQUFsTkxBQk16TWpNM05qWTFPVGMxTkRNeU5EQTJNekE1QUFKVE1RQUEqIiB9.r8tWEDgjdn1cf1Ce5ir4Fe0r1ImMskLwinoOg_AEHeI\"],\"employeeType\":[\"@Work\"],\"updateSource\":[\"Heroku\"],\"universalid\":[\"id=37260d8c-78ac-4023-a7c1-f04c43100f0e,ou=user,o=se,ou=services,dc=schneider-electric,dc=com\"],\"hotpMobileVerification\":[\"44444444442:37260d8c-78ac-4023-a7c1-f04c43100f0e:fr:IDMSWork\"],\"primaryContact\":[\"1234567890\"],\"companyAdditionalInfo\":[\"Tower-B,Unitech Infospace\"],\"inetUserStatus\":[\"Active\"]}";
	
	public static final String GET_INACTIVE_USER ="{\"username\":\"37260d8c-78ac-4023-a7c1-f04c43100f0e\",\"realm\":\"/se\",\"companyName\":[\"Accenture\"],\"postalCode\":[\"560100\"],\"companyState\":[\"01\"],\"county\":[\"test\"],\"dn\":[\"uid=37260d8c-78ac-4023-a7c1-f04c43100f0e,ou=people,dc=schneider-electric,dc=com\"],\"createTimestamp\":[\"20170406125730Z\"],\"companyStreet\":[\"OuterRingRoad, Bangalore\"],\"companyPostOfficeBox\":[\"122001\"],\"registerationSource\":[\"IDMSWork\"],\"postOfficeBox\":[\"560100\"],\"additionalInfo\":[\"testing\"],\"companyCountry\":[\"IN\"],\"IDMSUID\":[\"44444444442bridge-fo.com\"],\"companyPostalCode\":[\"122001\"],\"fax\":[\"123456789\"],\"companyCity\":[\"Bangalore\"],\"initials\":[\"Z003\"],\"givenName\":[\"SURESH\"],\"objectClass\":[\"iplanet-am-managed-person\",\"inetuser\",\"sunFederationManagerDataStore\",\"sunFMSAML2NameIdentifier\",\"inetorgperson\",\"devicePrintProfilesContainer\",\"sunIdentityServerLibertyPPService\",\"pushDeviceProfilesContainer\",\"iplanet-am-user-service\",\"iPlanetPreferences\",\"forgerock-am-dashboard-service\",\"organizationalperson\",\"top\",\"kbaInfoContainer\",\"oathDeviceProfilesContainer\",\"sunAMAuthAccountLockout\",\"person\",\"iplanet-am-auth-configuration-service\",\"chinaIdmsPerson\"],\"headquarters\":[\"true\"],\"departmentNumber\":[\"Security\"],\"preferredlanguage\":\"fr\",\"jobDescription\":[\"Technical\"],\"hotpEmailVerification\":[\"null:37260d8c-78ac-4023-a7c1-f04c43100f0e:fr:IDMSWork\"],\"annualRevenue\":[\"10978686\"],\"telephoneNumber\":[\"0124-490-3000\"],\"emailOptIn\":[\"Y\"],\"iam1\":[\"EU\"],\"modifyTimestamp\":[\"20170406125854Z\"],\"uid\":[\"37260d8c-78ac-4023-a7c1-f04c43100f0e\"],\"street\":[\"BANGALORE\"],\"currency\":[\"XOF\"],\"sn\":[\"BACHU\"],\"st\":[\"01\"],\"c\":[\"IN\"],\"mobile\":[\"44444444442\"],\"companyWebSite\":[\"www.accenture.com\"],\"cn\":\"37260d8c-78ac-4023-a7c1-f04c43100f0e\",\"l\":[\"BANGALORE\"],\"AuthID\":[\"eyAidHlwIjogIkpXVCIsICJhbGciOiAiSFMyNTYiIH0.eyAiYXV0aEluZGV4VmFsdWUiOiAiSE9UUF9Nb2JpbGVfQ3JlYXRlX1BXRFJFUSIsICJvdGsiOiAiNHEwdTJlNWFqazNiOHF0MWlqMmRnZ29qaWIiLCAiYXV0aEluZGV4VHlwZSI6ICJzZXJ2aWNlIiwgInJlYWxtIjogIm89c2Usb3U9c2VydmljZXMsZGM9c2NobmVpZGVyLWVsZWN0cmljLGRjPWNvbSIsICJzZXNzaW9uSWQiOiAiQVFJQzV3TTJMWTRTZmN5cWdIaXkxaHZOUlVKOGNWV1A0SHNwTlZBbjNrdi1JV1UuKkFBSlRTUUFDTURFQUFsTkxBQk16TWpNM05qWTFPVGMxTkRNeU5EQTJNekE1QUFKVE1RQUEqIiB9.r8tWEDgjdn1cf1Ce5ir4Fe0r1ImMskLwinoOg_AEHeI\"],\"employeeType\":[\"@Work\"],\"updateSource\":[\"Heroku\"],\"universalid\":[\"id=37260d8c-78ac-4023-a7c1-f04c43100f0e,ou=user,o=se,ou=services,dc=schneider-electric,dc=com\"],\"hotpMobileVerification\":[\"44444444442:37260d8c-78ac-4023-a7c1-f04c43100f0e:fr:IDMSWork\"],\"primaryContact\":[\"1234567890\"],\"companyAdditionalInfo\":[\"Tower-B,Unitech Infospace\"],\"inetUserStatus\":[\"Active\"]}";
	public static final String GET_USER_AIL ="{\"username\":\"cn00c5c336a7-5cda-4392-9071-38567edfb15b\",\"realm\":\"/se\",\"mail\":[\"perftes9t5127@mailinator.com\"],\"CompanyID\":[\"1234567122\"],\"companyName\":[\"Accenture\"],\"postalCode\":[\"1100092\"],\"companyState\":[\"010\"],\"county\":[\"test\"],\"dn\":[\"uid=cn00c5c336a7-5cda-4392-9071-38567edfb15b,ou=people,dc=schneider-electric,dc=com\"],\"createTimestamp\":[\"20171128054958Z\"],\"companyStreet\":[\"Sector 21, Gurgaon\"],\"companyPostOfficeBox\":[\"122001\"],\"registerationSource\":[\"idms\"],\"postOfficeBox\":[\"110092\"],\"additionalInfo\":[\"testing\"],\"companyCountry\":[\"CN\"],\"IDMSUID\":[\"perftes9t5127@mailinator.com\"],\"companyPostalCode\":[\"122001\"],\"fax\":[\"123456789\"],\"companyCity\":[\"Gurgaon\"],\"initials\":[\"Z003\"],\"givenName\":[\"Nitin\"],\"objectClass\":[\"iplanet-am-managed-person\",\"inetuser\",\"sunFederationManagerDataStore\",\"sunFMSAML2NameIdentifier\",\"inetorgperson\",\"devicePrintProfilesContainer\",\"sunIdentityServerLibertyPPService\",\"pushDeviceProfilesContainer\",\"iplanet-am-user-service\",\"iPlanetPreferences\",\"forgerock-am-dashboard-service\",\"organizationalperson\",\"top\",\"kbaInfoContainer\",\"oathDeviceProfilesContainer\",\"sunAMAuthAccountLockout\",\"person\",\"iplanet-am-auth-configuration-service\",\"chinaIdmsPerson\"],\"IDMSAIL_Programs_c\":[\"[]\"],\"tmp_password\":[\"V2VsY29tZTE=\"],\"headquarters\":[\"true\"],\"departmentNumber\":[\"Security\"],\"preferredlanguage\":[\"fr\"],\"hotpEmailVerification\":[\"perftes9t5127@mailinator.com:cn00c5c336a7-5cda-4392-9071-38567edfb15b:fr:idms:Nitin Jain\"],\"jobDescription\":[\"Technical\"],\"IDMSisInternal__c\":[\"FALSE\"],\"annualRevenue\":[\"10978686\"],\"emailOptIn\":[\"Y\"],\"iam1\":[\"EU\"],\"authId\":[\"11c06f3372bafe4ed0ba9220e3cba4806f31cd9dbfdb91eb11dfaf5a25751e15:1511848198604\"],\"modifyTimestamp\":[\"20171128054958Z\"],\"uid\":[\"cn00c5c336a7-5cda-4392-9071-38567edfb15b\"],\"IDMSAIL_Applications_c\":[\"[]\"],\"street\":[\"Delhi\"],\"currency\":[\"XOF\"],\"sn\":[\"Jain\"],\"IDMSAil_c\":[\"[]\"],\"st\":[\"010\"],\"c\":[\"CN\"],\"IDMSAIL_Features_c\":[\"[]\"],\"companyWebSite\":[\"www.accenture.com\"],\"cn\":[\"Nitin Jain\"],\"l\":[\"Delhi\"],\"employeeType\":[\"@Work\"],\"universalid\":[\"id=cn00c5c336a7-5cda-4392-9071-38567edfb15b,ou=user,o=se,ou=services,dc=schneider-electric,dc=com\"],\"V_Old\":[\"0\"],\"companyAdditionalInfo\":[\"Tower-B,Unitech Infospace\"],\"primaryContact\":[\"true\"],\"hotpMobileVerification\":[\"null:cn00c5c336a7-5cda-4392-9071-38567edfb15b:fr:idms:Nitin Jain\"],\"inetUserStatus\":[\"Active\"],\"V_New\":[\"1\"]}";
	
	public static final String USER_INFO="{\"sub\":\"7165e429-8224-48d4-b1c2-de98a1a72924\",\"name\":\"7165e429-8224-48d4-b1c2-de98a1a72924.tmp\",\"given_name\":\"SURESH\",\"family_name\":\"BACHU\"}";
	
	public static final String PROVISIONAL_USER_REGISTRATION="{\"type\":\"selfRegistration\",\"tag\":\"end\",\"status\":{\"success\":true},\"additions\":{}}";
	
	public static final String ENTITY = "{\"authId\":\"eyAidHlwIjogIkpXVCIsICJhbGciOiAiSFMyNTYiIH0.eyAiYXV0aEluZGV4VmFsdWUiOiAiSE9UUF9FbWFpbCIsICJvdGsiOiAibHNwODYwM3BmMW9kcTMzbnZrczlnNGJsMDEiLCAiYXV0aEluZGV4VHlwZSI6ICJzZXJ2aWNlIiwgInJlYWxtIjogIm89c2Usb3U9c2VydmljZXMsZGM9c2NobmVpZGVyLWVsZWN0cmljLGRjPWNvbSIsICJzZXNzaW9uSWQiOiAiQVFJQzV3TTJMWTRTZmN5dlRRVG5PdlBBYlBmMUdTY1JqT3E5LUw5bTM1OTkxdlUuKkFBSlRTUUFDTURFQUFsTkxBQk0yT1RFd05qSXlPRFE0TlRNM09EYzFOelF5QUFKVE1RQUEqIiB9.xogyhwFSrFNqlxsIf_OUCR6sKe4pIZ6ulNiwRhj3NW0\",\"template\":\"\",\"stage\":\"LDAP1\",\"header\":\"Sign in to OpenAM\",\"callbacks\":[{\"type\":\"NameCallback\",\"output\":[{\"name\":\"prompt\",\"value\":\"User Name:\"}],\"input\":[{\"name\":\"IDToken1\",\"value\":\"\"}]},{\"type\":\"PasswordCallback\",\"output\":[{\"name\":\"prompt\",\"value\":\"Password:\"}],\"input\":[{\"name\":\"IDToken2\",\"value\":\"\"}]}]}";
	
	public static final String ID = "78d58fc3-6cf1-43b6-939a-444eb02e3550";
	
	public static final String FEDERATION_ID = "78d58fc3-6cf1-43b6-939a-444eb02e3550";
	
	public static final String REGISTRATION_PIN = "343671";
	
	public static final String USER_REGISTRATION_OPERATION = "userRegistration";
	
	public static final String PROFILE_UPDATE_SOURCE = "oauthSampleApp";
	
	public static final String JSON_REQUEST= "{\"userPassword\":\"iSBcxcm*`s\",\"username\":\"cn006d6c9bfd-2128-45c0-a331-686fe1e7ddef\",\"idmsuid\":\"09739200404bridge-fo.com\",\"hotpEmailVerification\":\"test@mailinator.com:37260d8c-78ac-4023-a7c1-f04c43100f0e:fr:IDMS_Profile_update_source__c:37260d8c-78ac-4023-a7c1-f04c43100f0e\"}";
	
	public static final String JSON_RESPONSE = "";
	
	public static final String PASSWORD = "Welcome@123";
	
	public static final String IFW_USER = "{\n    \"scope\": \"am_application_scope default\",\n    \"token_type\": \"Bearer\",\n    \"expires_in\": 43200,\n    \"access_token\": \"95d408eea1c69d7920c9c115338796dc\"\n}";
	
	public static final String SALESFORCE_USER = "{\n    \"access_token\": \"00D11000000Doh7!AR8AQNhFaNkFVe.iPtV5cbK27PYYqMgoWFusvNsrFFKad8OmQj5m04sAchJhFNLegYyWeCgvLoATjo6KKfff85dkSlWedhCN\",\n    \"instance_url\": \"https://se--DEVMERGE.cs18.my.salesforce.com\",\n    \"id\": \"https://test.salesforce.com/id/00D11000000Doh7EAC/00511000004ZRFKAA4\",\n    \"token_type\": \"Bearer\",\n    \"issued_at\": \"1512545632582\",\n    \"signature\": \"ldsh22179/Jj9bfe/TMn6L8Clb+tmYqBkW3ntvHYCds=\"\n}";
	
	public static final String TECHNICAL_USER = "{\"userGroup\": \"technicaluser\"}";

	public static final String GET_UNAUTHORIZED_ERROR = "{\"errorCode\": \"Unauthorized\",\"message\":\"OpenAM issue of Authorization for cn00Nfbt-XRzP-zytK-VE68-THyztdBHxvh5\"}";
	public static final String GET_INVALID_USER = "{\"errorCode\": \"NOT_FOUND\",\"message\":\"Provided external ID field does not exist or is  not accessible: null\"}";
	/**
	 * Private default constructor to prevent external instantiation.
	 */
	private DomainMockData() {
	}

	/**
	 * Build User instance with basic default values.
	 *
	 * @return built IssueType instance
	 */
	public static IFWUser buildUser() {
		IFWUser user = new IFWUser();
		user.setEmail(EMAIL);
		user.setMobilePhone(MOBILE_PHONE);
		user.setPhone(PHONE);
		user.setFirstName(FIRST_NAME);
		user.setLastName(LAST_NAME);
		user.setIDMS_Email_opt_in__c(IDMS_EMAIL_OPT_IN__C);
		user.setIDMS_User_Context__c(IDMS_USER_CONTEXT__C);
		user.setCountry(COUNTRY);
		user.setIDMS_PreferredLanguage__c(IDMS_PREFERREDLANGUAGE__C);
		user.setDefaultCurrencyIsoCode(DEFAULTCURRENCYISOCODE);
		user.setStreet(STREET);
		user.setPostalCode(POSTALCODE);
		user.setState(STATE);
		user.setCity(CITY);
		user.setIDMS_County__c(IDMS_COUNTY__C);
		user.setIDMS_POBox__c(IDMS_POBOX__C);
		user.setIDMS_Federated_ID__c(IDMS_Federated_ID__c);
		user.setIDMS_Registration_Source__c(IDMS_Registration_Source__c);
		user.setIDMS_Profile_update_source__c(IDMS_Profile_update_source__c);
		user.setIDMS_AdditionalAddress__c(IDMS_AdditionalAddress__c);
		user.setCompanyName(CompanyName);
		user.setCompany_Address1__c(Company_Address1__c);
		user.setCompany_Address2__c(Company_Address2__c);
		user.setCompany_City__c(Company_City__c);
		user.setCompany_Postal_Code__c(Company_Postal_Code__c);
		user.setCompany_State__c(Company_State__c);
		user.setIDMSCompanyPoBox__c(IDMSCompanyPoBox__c);
		user.setCompany_Country__c(Company_Country__c);
		user.setCompany_Address2__c(Company_Address2__c);
		user.setIDMSClassLevel1__c(IDMSClassLevel1__c);
		user.setIDMSClassLevel2__c(IDMSClassLevel2__c);
		user.setIDMSMarketSegment__c(IDMSMarketSegment__c);
		user.setIDMSMarketSubSegment__c(IDMSMarketSubSegment__c);
		user.setJob_Title__c(Job_Title__c);
		user.setJob_Function__c(Job_Function__c);
		user.setIDMSJobDescription__c(IDMSJobDescription__c);
		user.setIDMSCompanyMarketServed__c(IDMSCompanyMarketServed__c);
		user.setIDMSCompanyNbrEmployees__c(IDMSCompanyNbrEmployees__c);
		user.setIDMSCompanyHeadquarters__c(IDMSCompanyHeadquarters__c);
		user.setIDMSAnnualRevenue__c(IDMSAnnualRevenue__c.toString());
		user.setIDMSTaxIdentificationNumber__c(IDMSTaxIdentificationNumber__c);
		user.setIDMSMiddleName__c(IDMSMiddleName__c);
		user.setCompany_Website__c(Company_Website__c);
		user.setIDMSSalutation__c(IDMSSalutation__c);
		user.setDepartment(Department);
		user.setIDMSSuffix__c(IDMSSuffix__c);
		user.setFax(Fax);
		user.setIDMSCompanyFederationIdentifier__c(IDMSCompanyFederationIdentifier__c);
		user.setIDMSDelegatedIdp__c(IDMSDelegatedIdp__c);
		user.setIDMSIdentityType__c(IDMSIdentityType__c);
		
		
		
		return user;
	}

	/**
	 * Build User instance with basic default values.
	 *
	 * @return built IssueType instance
	 */
	public static OpenAmUser buildOpenAmUser() {
		OpenAmUser user = new OpenAmUser();
		user.setMail(EMAIL);
		user.setMobile(MOBILE_PHONE);
		user.setHomePhone(PHONE);
		user.setGivenName(FIRST_NAME);
		user.setSn(LAST_NAME);
		user.setEmailOptIn(IDMS_EMAIL_OPT_IN__C);
		user.setEmployeeType(IDMS_USER_CONTEXT__C);
		user.setC(COUNTRY);
		user.setPreferredlanguage(IDMS_PREFERREDLANGUAGE__C);
		user.setCurrency(DEFAULTCURRENCYISOCODE);
		user.setStreet(STREET);
		user.setL(CITY);
		user.setPostalCode(POSTALCODE);
		user.setSt(STATE);
		
		user.setCounty(IDMS_COUNTY__C);
		user.setPostOfficeBox(IDMS_POBOX__C);
		user.setFederationID(IDMS_Federated_ID__c);
		user.setRegisterationSource(IDMS_Registration_Source__c);
		user.setUpdateSource(IDMS_Profile_update_source__c);
		user.setAdditionalInfo(IDMS_AdditionalAddress__c);
		user.setCompanyName(CompanyName);
		user.setCompanyStreet(Company_Address1__c);
		user.setCompanyAdditionalInfo(Company_Address2__c);
		user.setCompanyCity(Company_City__c);
		user.setCompanyPostalCode(Company_Postal_Code__c);
		user.setCompanyState(Company_State__c);
		user.setCompanyPostOfficeBox(IDMSCompanyPoBox__c);
		user.setCompanyCountry(Company_Country__c);
		user.setCompanyAdditionalInfo(Company_Address2__c);
		user.setIam1(IDMSClassLevel1__c);
		user.setIam2(IDMSClassLevel2__c);
		user.setIndustrySegment(IDMSMarketSegment__c);
		user.setIndustrySubSegment(IDMSMarketSubSegment__c);
		user.setTitle(Job_Title__c);
		user.setJobFunction(Job_Function__c);
		user.setJobDescription(IDMSJobDescription__c);
		user.setIndustries(IDMSCompanyMarketServed__c);
		user.setEmployeeSize(IDMSCompanyNbrEmployees__c);
		user.setHeadquarters(IDMSCompanyHeadquarters__c);
		user.setAnnualRevenue(IDMSAnnualRevenue__c);
		user.setTaxID(IDMSTaxIdentificationNumber__c);
		user.setMiddleName(IDMSMiddleName__c);
		user.setCompanyWebSite(Company_Website__c);
		user.setInitials(IDMSSalutation__c);
		user.setDepartmentNumber(Department);
		user.setSuffix(IDMSSuffix__c);
		user.setFax(Fax);
		user.setCompanyFederatedID(IDMSCompanyFederationIdentifier__c);
		user.setDelegatedIDP(IDMSDelegatedIdp__c);
		user.setIdentityType(IDMSIdentityType__c);
		user.setPrimaryContact(IDMSPrimaryContact__c);
		
		
		
		return user;
	}

	public static Response getOkIfwResponse() {
		return Response.ok().build();
	}
	public static Response get404IfwResponse() {
		return Response.noContent().build();
	}

	public static Response getAppDetails() {
		JSONObject jsonObj = new JSONObject();
		jsonObj.put("_isOTPEnabled", "false");
		final InputStream inputStream = new ByteArrayInputStream(jsonObj.toJSONString().getBytes(StandardCharsets.UTF_8));
		Response response = Response.ok(inputStream).build();
		return response;
	}
}

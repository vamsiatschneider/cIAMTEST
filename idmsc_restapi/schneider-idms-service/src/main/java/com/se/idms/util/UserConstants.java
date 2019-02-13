package com.se.idms.util;

public class UserConstants {
	
	/**
	 * Randon Password Characters
	 * */
	
	public static final String RANDOM_PR_CHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789~`!@#$%^&*()-_";
	
	public static final String RANDOM_CHARS = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
	
	public static final String RANDOM_PIN_CHARS = "0123456789";
	/**
	 * Password Policy Regular Expression
	 * */
	
	public static final String PR_REGEX1 = "^(?=.*?[A-Z]+)(?=.*?[a-z])(?=.*?[0-9]{1,2})(?=.*?[#?!@$%^&*-]).{8,16}$";
	
	public static final String PASSWORD_REGEX = "^((?=.*?[A-Z]+)(?=.*?[a-z])(?=.*?[0-9])|(?=.*?[a-z]+)(?=.*?[A-Z])(?=.*?[^\\w])|(?=.*?[0-9]+)(?=.*?[a-z])(?=.*?[^\\w])|(?=.*?[^\\w]+)(?=.*?[A-Z]+)(?=.*?[0-9])).{8,532}$";
	
	public static final String PR_REGEX = "^((([^<>()\\[\\];:\\s@]([^<>()\\[\\]\\.;:\\s@])*)|(\".+\"))+@(([0-9]{1,3}+\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3})|([a-zA-Z&#45;0-9]+\\.[a-zA-Z]{2,})))$";
	/**
	 * Token constanst 
	 * 
	 * */
	public static final String EMAIL ="email";
	
	public static final String MOBILE ="mobile";
	
	public static final String REALM ="/";
	
	public static final String SE_REALM ="/se";
	
	public static final String SET_INVITATION_SUCCESS_MESSAGE="Sent invitation successfully";
	
	public static final String RESEND_REGEMAIL_SUCCESS_MESSAGE="Resend Registration Email successfully";
	
	public static final String IDMSIDPCHANING_SUCCESS_MESSAGE="Idms Idp Chaning completed successfully";
	
	public static final String CREATE_USER_SUCCESS_MESSAGE="User created successfully";
	
	public static final String PIN_VALIDATED_SUCCESS="PIN validated Successfully";
	
	public static final String UPDATE_USER_SUCCESS_MESSAGE="user profile update successfully";
	
	public static final String PIN_SEND_SUCCESS= "Pin Code has been sent successfully";
	
	public static final String CHINA_IDMS_TOKEN = "iPlanetDirectoryPro=";
	
	//public static final String CHINA_IDMS_TOKEN = "chinaIDMSToken=";
	
	public static final String COUNTRY_KEY ="Country";
	
	public static final String USER_EXISTS ="User Already exists";
	
	public static final String NEW_USER_EXISTS ="New username already present";
	
	public static final String USER_NOT_EXISTS ="User does not exists";
	
	public static final String REQUIRED_FIELDS_MISSING = "Required Fields Missing - ";
	
	public static final String INCORRECT_FIELDS_LENGTH = "Fields with incorrect length -";
	
	public static final String INCORRECT_FIELDS_LENGTH_PREFERED_LANGUAGE = " Field is mandatory- " ;
	
	public static final String INVALID_VALUE_IDMS = "Invalid Value: ";
	
	public static final String INVALID_VALUE = "Invalid Value ";
	
	public static final String COUNTRY_FIELDS_MISSING = "Field(s) not in correct format- ";
	
	public static final String INVALID_VALUE_HEADQUARTER = "Invalid value ";
	
	public static final String FIELD_NOT_IN_LIST = "Field(s) Not in the List - ";
	
	public static final String ERROR_CREATE_USER="Error in creating user.";
	
	public static final String ATTRIBUTE_NOT_AVAILABELE ="Attribute or Property not available in the properties file.";
	
	public static final String ERROR_UPDATE_USER="Error in Updating User.  ";
	
	public static final String ERROR_RESEND_PIN="Error in Resend user pin.";
	
	public static final String USER_CONTEXT_HOME = "@Home";
	
	public static final String USER_CONTEXT_HOME_1 = "Home";
	
	public static final String USER_CONTEXT_WORK = "@Work";
	
	public static final String USER_CONTEXT_WORK_1 = "Work";
	
	public static final String MOBILE_VALIDATION = "Mobile should be 11 Numberic Digits - ";
	
	public static final String EMAIL_VALIDATION = "Email error - ";
	
	public static final String EMAIL_VALIDATION_INTERNALUSER = "Employees are restricted from User registration";
	
	public static final String MOBILE_CHINA_CODE = "86";
	
	
	/**
	 * Mandatory fields for create user
	 * */
	
	public static final String FIRST_NAME ="FirstName";
	
	public static final String LAST_NAME ="LastName";
	
	public static final String MOBILE_PHONE = "MobilePhone";
	
	
	
	public static final String COUNTRY ="Country";
	
	public static final String PREFERRED_LANGUAGE ="IDMS_PreferredLanguage__c";

	public static final String CURRENCY ="DefaultCurrencyIsoCode";
	
	public static final String REGISTRATION_SOURCE ="IDMS_Registration_Source__c";
	
	public static final String UPDATE_SOURCE ="IDMS_Profile_update_source__c";
	
	public static final String COMPANY_COUNTRY_C ="Company_Country__c";
	
	public static final String IAM_A1 ="Iam_a1";
	
	public static final String IDMS_CLASS_LEVEL_C ="IDMSClassLevel1__c";
	
	public static final String PRIMARY_CONTACT = "Primary_Contact";
	
	public static final String IDMS_PRIMARY_CONTACT_C = "IDMSPrimaryContact__c";
	
	public static final String PHONE = "Phone";
	
	public static final String IDMS_JOB_DESCRIPTION_C = "IDMSJobDescription__c";
	
	public static final String IDMS_COMPANY_MARKET_SERVED_C ="IDMSCompanyMarketServed__c";
	
	public static final String IDMS_COMPANY_NBR_EMPLOYEES_C ="IDMSCompanyNbrEmployees__c";
	
	public static final String IDMS_COMPANY_HEAD_QUARTERS_C = "IDMSCompanyHeadquarters__c";
	
	public static final String IDMS_ANNUAL_REVENUE_C = "IDMSAnnualRevenue__c";
	
	public static final String IDMS_TAX_IDENTIFICATION_NUMBER_C = "IDMSTaxIdentificationNumber__c";
	
	public static final String IDMS_MIDDLE_NAME_C ="IDMSMiddleName__c";
	
	public static final String COMPANY_WEBSITE_C ="Company_Website__c";
	
	public static final String DEPARTMENT ="Department";
	
	public static final String IDMS_SUFFIX_C ="IDMSSuffix__c";
	
	public static final String FAX ="Fax";
	
	public static final String IDMS_COMAPNY_FED_IDENTIFIER_C ="IDMSCompanyFederationIdentifier__c";
	
	/**
	 * Pick List properties
	 * */
	
	public static final String APPLICATIONS = "Applications";
	
	public static final String EMLAIL_OPT_IN = "emailOptIn";
	
	public static final String IDMS_Email_opt_in__c="IDMS_Email_opt_in__c";
	
	public static final String EMLAIL_OPT_IN_DOC = "Email opt-in";
	
	public static final String IDMS_USER_CONTEXT_C ="IDMS_User_Context__c";
	
	public static final String COMPANY_STATE_C ="Company_State__c";
	
	public static final String STATE = "State";
	
	public static final String IAM_A2 ="Iam_a2";
	
	public static final String IDMS_CLASS_LEVEL2_C ="IDMSClassLevel2__c";
	
	public static final String IDMS_MARKET_SEGMENT_C ="IDMSMarketSegment__c";
	
	public static final String MY_INDUSTRY_SEGMENT ="My_Industry_segment";
	
	public static final String MY_INDUSTRY_SUB_SEGMENT ="My_Industry_sub_segment";
	
	public static final String IDMS_MARKET_SUB_SEGMENT_C ="IDMSMarketSubSegment__c";
	
	public static final String JOB_TITLE ="Job_Title";
	
	public static final String JOB_TITLE_C ="Job_Title__c";
	
	public static final String JOB_FUNCTION ="Job_Function";
	
	public static final String JOB_FUNCTION_C ="Job_Function__c";
	
	public static final String SALUTATION ="Salutation";
	
	public static final String IDMS_SALUTATION_C ="IDMSSalutation__c";
	
	public static final String DELEGATED_IDP ="DeletegatedIdP";
	
	public static final String IDMS_DELEGATED_IDP_C ="IDMSDelegatedIdp__c";
	
	public static final String IDENTITY_TYPE ="Identiy_Type";
	
	public static final String IDMS_IDENTITY_TYPE_C ="IDMSIdentityType__c";
	
	public static final String IDMS_COMPANY_PO_BOX_C = "IDMSCompanyPoBox__c";
	
	public static final String COMPANY_ADDRESS2_C = "Company_Address2__c";
	
	public static final String IDMSCompanyCounty__c ="IDMSCompanyCounty__c";
	
	public static final String IDMS_ACL_TYPE_C="IDMSAclType__c";
	
	public static final String IDMS_OPERATION_C="IDMSOperation__c";
	
	
	/**
	 * Length Properties
	 * */
	
	public static final String STREET ="Street";
	
	public static final String CITY ="City";
	
	public static final String POSTAL_CODE= "PostalCode";
	
	public static final String IDMS_COUNTY_C = "IDMS_County__c";
	
	public static final String IDMS_PO_BOX_C = "IDMS_POBox__c";
	
	public static final String FEDERATION_IDENTIFIER = "IDMS_Federated_ID__c"; 
	
	public static final String IDMS_REGISTRATION_SOURCE_C = "IDMS_Registration_Source__c";
	
	public static final String IDMS_ADDITIONAL_ADDRESS_C = "IDMS_AdditionalAddress__c";
	
	public static final String COMPANY_NAME  = "CompanyName";
	
	public static final String COMPANY_ADDRESS1_C = "Company_Address1__c"; 
	
	public static final String COMPANY_CITY_C = "Company_City__c";
	
	public static final String COMPANY_POSTAL_CODE_C = "Company_Postal_Code__c";
	
	public static final String ADMIN_FEDERATED_ID ="ADMIN_FEDERATED_ID";
	
	public static final String ADMIN_COMPANY_FEDERATED_ID ="ADMIN_COMPANY_FEDERATED_ID";
	
	public static final String ABOUT_ME ="AboutMe";
	
	public static final String ACCOUNT_ID ="ACCOUNT_ID";
	
	public static final String BFO_ACCOUNT_ID ="BFO_ACCOUNT_ID";
	
	public static final String OTP_VALIDATED_SUCCESS ="OTP validated Successfully";
	
	public static final String OTP_EXPIRED = "Expired verification code";
	
	public static final String OTP_INVALID = "OTP is not valid";
	
	public static final String OTP_EMPTY = "OTP is null or empty";
	
	public static final String MOBILE_EMPTY = "mobile is null or empty";
	
	public static final String EMAIL_EMPTY = "email is null or empty";
	
	public static final String FEDID_EMPTY = "federation ID is null or empty";
	
	/**
	 * HOTP Query parametersConstants
	 * 
	 * */
	
	public static final String HOTP_EMAIL ="HOTP_Email";
	
	public static final String HOTP_MOBILE_USER_REGISTRATION ="HOTP_Mobile_Create_PWDREQ";
	
	public static final String HOTP_EMAIL_UPDATE ="HOTP_Email_Update";
	
	public static final String HOTP_MOBILE_UPDATE ="HOTP_Mobile_Update";
	
	public static final String HOTP_EMAIL_ADD ="HOTP_Email_Add";
	
	public static final String HOTP_MOBILE_ADD ="HOTP_Mobile_Add";
	
	public static final String HOTP_SERVICE ="service";
	
	public static final String HOTP_EMAIL_RESET_PR = "HOTP_Email_ResetPassword";
	
	public static final String HOTP_MOBILE_RESET_PR = "HOTP_Mobile_ResetPassword";
	
	public static final String USER_DELIMETER = null;
	
	public static final String OPT_SUBMIT_REQUEST = "{ \"authId\": \"\", \n\"template\": \"\", \n\"stage\": \"HOTP2\", \n\"callbacks\": [\n { \"type\": \"PasswordCallback\", \n\t\"output\": [ { \"name\": \"prompt\", \"value\": \" Enter OTP \" } ], \n\t\"input\": [ { \"name\": \"IDToken1\", \"value\": \"575165\" } ] }, \n { \"type\": \"ConfirmationCallback\", \n   \"output\": [ { \"name\": \"prompt\", \"value\": \"\" }, \n\t\t\t   { \"name\": \"messageType\", \"value\": 0 }, \n\t\t\t   { \"name\": \"options\", \"value\": [ \" Submit OTP \", \" Request OTP \" ] }, \n\t\t\t   { \"name\": \"optionType\", \"value\": -1}, \n\t\t\t   { \"name\": \"defaultOption\", \"value\": 0 } ], \n\t\"input\": [ { \"name\": \"IDToken2\", \"value\": 0 } ] } ] }";


	/**
	 * Confirm PIN Constants
	 * */
	
	public static final String USER_REGISTRATION= "userRegistration";
	
	public static final String SET_USER_PR= "SetUserPwd"; 
	
	public static final String MANDATORY_PINCODE = "Pin code is Mandatory";
	
	public static final String INVALID_UPDATE_SOURCE = "Invalid update source";
	
	public static final String MANDATORY_ID = "Id is Mandatory";
	
	public static final String PASSWORD_NOT_ALLOWED = "Operation blocked. Set password is not allowed.";
	
	public static final String PROFILE_UPDATE_SOURCE = "Profile update source is Mandatory.";
	
	public static final String INVALID_PINCODE = "PIN Entered is not correct";
	
	public static final String MANDATORY_FEDERATION_ID = "Missing mandatory Federation Id and it should not be null";
	
	public static final String OPERATION_MISMATCH = "Invalid Idms Operation";
	
	public static final String MANDATORY_PR = "Password is Mandatory when IDMS Operation is SetUserPwd.";
	
	public static final String MANDATORY_OPERATION = "Missing mandatory Operation";
	
	public static final String PR_POLICY = "Password does not match with password policy.";
	
	public static final String INCORRECT_REVENUE = "Annual revenue should not contain characters.";
	
	public static final String TOKEN_INVALID = "iPlanetDirectoryKey Should not be null or empty";
	
	public static final String PIN_INVALID = "Pin is  not  valid";
	
	public static final String PINCODE_VALIDATED = "PIN validated Successfully";
	
	public static final String SERVER_ERROR = "Internal Server errror";
	
	public static final String MANDATORY_ADD_EMAIL_OPT_TYPE = "Operation Type is mandatory for add email";
	
	public static final String TRUE ="true";
	
	public static final String FALSE ="false";
	
	public static final String UPDATE_USER_RECORD= "UpdateUserRecord";
	public static final String ADD_EMAIL_USER_RECORD= "AddEmailUserRecord";
	
	public static final String INVALID_ACL_TYPE="Invalid IDMSAclType__c";
	
	public static final String INVALID_OPERATION="Invalid Operation";
	
	public static final String MANDATORY_ACL="Acl is Mandatory";
	
	public static final String MANDATORY_PROFILE_UPDATE_SOURCE="IDMS_Profile_update_source__c is Mandatory";
	
	/**
	 * */
	public static final String CREATE_USER_SERVICE = "CREATE_USER";
	
	public static final String UPDATE_USER_SERVICE = "UPDATE_USER";
	
	public static final String ADD_EM_USER_SERVICE = "ADD_EM_USER";
	public static final String IDMS_PROFILE_UPDATE_SOURCE = "IDMS_Profile_update_source__c";
	
	public static final String CONTENT_TYPE="application/x-www-form-urlencoded";
	public static final String REDIRECT_URI="https://identity-int.schneider-electric.com/openid/cb-basic.html";
	public static final String SCOPE="openid%20profile";
	public static final String BEARER_TOKEN="Basic bXljbGllbnRJRDpwYXNzd29yZA==";
	public static final String CACHE="no-cache";
	public static final String GRANT_TYPE="authorization_code";
	public static final String RESPONSE_TYPE="code";
	public static final String CLIENT_ID="myClientID";
	public static final String CODE_FIELD="code=";
	
	public static final String AUDIT_REQUESTING_USER = "Audit: {requestingUser: ";
	public static final String AUDIT_TECHNICAL_USER = "Technical User";
	public static final String AUDIT_IMPERSONATING_USER = ", impersonatingUser: ";
	public static final String AUDIT_API_ADMIN  = "apiAdmin";
	public static final String AUDIT_OPENAM_API = ", openAMApi: ";
	public static final String AUDIT_LOG_CLOSURE = "}”";
	
	public static final String AUDIT_OPENAM_GET_CALL = "GET /accessmanager/json/se/users/";	
	public static final String AUDIT_OPENAM_AUTHENTICATE_CALL = "POST /accessmanager/json/authenticate/";
	public static final String AUDIT_OPENAM_USER_EXISTS_CALL = "GET /accessmanager/json/se/users?_queryFilter=";
	public static final String AUDIT_OPENAM_USER_REGISTRATION_CALL = "POST /accessmanager/json/se/selfservice/userRegistration?_action=";
	public static final String AUDIT_OPENAM_USER_REGISTRATION_PROVISIONAL_CALL = "POST /accessmanager/json/ProvisionalRealm/selfservice/userRegistration";
	public static final String AUDIT_OPENAM_UPDATE_CALL = "PUT /accessmanager/json/se/users/";
	public static final String AUDIT_OPENAM_UPDATE_PROVISIONAL_CALL = "PUT /accessmanager/json/ProvisionalRealm/users/";
	public static final String AUDIT_OPENAM_AUTHORIZE_CALL = "GET /accessmanager/oauth2/authorize";
	public static final String AUDIT_OPENAM_AUTHORIZE_POST_CALL = "POST /accessmanager/oauth2/authorize";
	public static final String AUDIT_OPENAM_USER_INFO_CALL = "POST /accessmanager/oauth2/userinfo?realm=";
	
	public static final String ID = "Id";
	public static final String STATUS = "Status";
	public static final String MESSAGE = "Message";
	public static final String STATUS_FAILD = "Failed";
	public static final String STATUS_ERROR = "Error";
	public static final String RESPONSE_MESSAGE = "User ID and Federated Id doesn’t match";
	public static final String RESEND_ONLYMOBILE_ERROR_MESSAGE = "Resend pin only for mobile users";
	public static final String RESEND_UPDATEOPTTYPE_ERROR = "Operation is not permitted";
	
	public static final String RESPONSE_MESSAGE_NULL = "User ID and Federated Id should not be null";
	public static final long TIME_IN_MILLI_SECONDS = System.currentTimeMillis();
	public static final String GET_USER_TIME_LOG = "Time taken by UserServiceImpl.getUser() : ";
	public static final String GET_USER_BY_TOKEN_TIME_LOG = "Time taken by UserServiceImpl.getUserbyToken() : ";
	public static final String USER_REGISTRATION_TIME_LOG = "Time taken by UserServiceImpl.userRegistration() : ";
	public static final String MAPPER_CREATE_USER_PR_REQUEST  = "password";
	public static final String MAPPER_OPENAM_USER_PR_REQUEST = "input.user.userPassword";
	public static final String UPDATE_PR_EQUAL = "New Password and Existing Password are the same";
	
	public static final String ACLTYPE_APPLICATION = "Application";
	public static final String ACLTYPE_PROGRAM = "Program";
	public static final String ACLTYPE_FEATURE = "Feature";
	
	public static final String ACLTYPE_APPLICATIONS = "Applications";
	public static final String ACLTYPE_PROGRAMS = "Programs";
	public static final String ACLTYPE_FEATURES = "Features";
	
	public static final String USER_NOT_FOUND = "User not found based on user Id";
	public static final String USER_ACTIVATED = "User has been activated successfully";
	public static final String REGISTRATION_SOURCE_MISSING = "Registration source is missing";
	public static final String REGISTRATION_SOURCE_NOT_MATCHING = "Registration source not matching with openAM";
	public static final String EMAIL_OR_MOBILE_NOT_MATCHING = "Wrong Email or Mobile passed by user";
	
	//IFW Constants
	
	public static final String CONTENT_TYPE_URL_FROM = "application/x-www-form-urlencoded";
	public static final String CONTENT_TYPE_APP_XML = "application/xml";
	public static final String ACCEPT_TYPE_APP_JSON = "application/json";
	public static final String IFW_GRANT_TYPE = "client_credentials";
	public static final String IFW_CLIENT_ID = "N2s0I7Ifmxyf5gIDUUihdinmrRUa";
	public static final String CLIENT_SECRET = "Qy5VK5oQgquECwhXIugBY1aamQca";
	
	public static final String APPLICATION_NAME = "China IDMS";
	public static final String COUNTRY_CODE = "FR";
	public static final String CHINA_CODE = "CN";
	public static final String LANGUAGE_CODE = "EN";
	public static final String REQUEST_ID = "10001";
	public static final String PR_GRANT_TYPE = "password";
	public static final String RESET_PR_SUCCESS = "Reset Password Done successfully.";
	public static final String USER_NOT_FOUND_EMAIL_MOBILE ="User not found based on Email/Mobile";
	public static final String ERROR_RESET_PR= "Error in Reset Password.";
	public static final String GLOBAL_USER_BOOLEAN = "Globaluser value should be boolean";
	public static final String UIMS = "UIMS";
	public static final String PRM = "PRM";
	public static final String PACE = "PACE";
	public static final String AMLB_COOKIE = "AWSELB=";
	public static final String UIMS_VERSION = "1";
	public static final String EMAIL_BODY ="You have requested to change your email address, this address will not be used after confirmation for using Schneider Electric applications. If you didn't request the email change or if you have questions, please contact support immediately @ Secure_Identity-L2@schneider-electric.com";
	public static final String LANGUAGE_CHINA="ZH";
	public static final String UPDATE_EMAIL_NOTIFICATION ="UPDATE EMAIL NOTIFICATION";
	public static final String UPDATE_EMAIL_NOTIFICATION_ZH ="更新电子邮件通知";
	public static final String UID_PREFIX ="cn00";
	public static final String INVALID_UIMS_CREDENTIALS ="Uims ClientId and ClientSecret are not matching with the system ";
	public static final String UIMS_CLIENTID_SECRET ="Uims ClientId and ClientSecret are mandatory";
	
	public static final String V_OLD = "0";
	public static final String V_NEW = "1";
	
	public static final String GO_DIGITAL = "UIMS";
	public static final String AUTH_FAILED="auth.failed";
	public static final String ERROR_MESSAGE="&errorMessage=";
	public static final String GOTO ="&uimsgoto=";
	public static final String SUNQUERY_PARAM_STRING ="&uimsSunQueryParamsString=";
	
	public static final String IDBUTTON ="&IDButton=";
	public static final String GOTO_ONFAIL ="&gotoOnFail=";
	public static final String ENCODED ="&encoded=";
	public static final String GX_CHARSET ="&gx_charset=";
	
	
	public static final String PROXY_URL = "PROXY_URL=";
	public static final String ORIG_URL = "ORIG_URL=";
	public static final String NTID = "NTID=";
	public static final String AMLBCOOKIE = "amlbcookie=";
	
	public static final String AUTH_ID_EMPTY = "AuthId is empty";
	public static final String STATE_EMPTY = "State is empty";
	public static final String PROXY_URL_EMPTY = "Porxy Url is empty";
	public static final String ORIG_URL_EMPTY = "Orig Url is empty";
	public static final String NTID_EMPTY = "Ntid is empty";
	public static final String AMLBCOOKIE_EMPTY = "AMLB Cookie is empty";
	public static final String SOCIAL_LOGIN_SERVICE ="WeChatSocialAuthChain";
	public static final String SOCIAL_LOGIN_SERVICE_NAME ="Service is not matching";
	
	
	public static final String SE_TRUSTED_ADMIN = "SE_TRUSTED_ADMIN";
	
	public static final String SOCIAL_LOGIN_PREFIX="cn01";
	
	public static final String redirectUrl_Option3 = "https://10.194.157.89/login.jsp";
	
	public static final String ACCOUNT_BLOCKED = "Your account has been locked.";
	
	public static final String OPERATION_BLCOKED = "Operation blocked. Set password is not allowed.";
	
	public static final String PASSWORD_WITH_USER_REG_BLCOKED = "Operation blocked. User registration with password is not allowed.";
	
	public static final String UPDATE_USER_REC_BLCOKED = "Operation blocked. Updating password is not allwed";

	public static final String IDMS_BFO_profile = "IDMS_BFO_profile";
	
	public static final String TOKEN_TYPE = "contact activation";
	
	public static final String PRMPORTAL = "PRMPortal";
	
	public static final String UIMSPasswordSync = "UIMSPasswordSync";
	
	public static final String USER_TYPE_L1 = "L1";
	
	public static final String USER_TYPE_L2 = "L2";
	
	public static final String USER_TYPE_L3 = "L3";

	public static final String TECHNICAL_USER = "TechnicalUser";
	
	public static final String UIMSCreateUserSync = "UIMSCreateUserSync";
	
	public static final String CHANNEL = "Channel__c";
	
	public static final String SUBCHANNEL = "SubChannel__c";
	
	public static final String SE_MAIL = "@se.com";
	
	public static final String NON_SE_MAIL = "@non.se.com";
	
	public static final String SCHNEIDER_MAIL = "@schneider-electric.com";
	
	public static final String NON_SCHNEIDER_MAIL = "@non.schneider-electric.com";
	
	public static final String ADD_MOBILE_IDENTIFIER = "Mobile is added successfully";
	
	public static final String ADD_EMAIL_PROFILE = "Email is added, verification is pending";
	
	public static final String ADD_EMAIL_PROFILE_SUCCESS = "Email is verified and added as identifier";
	
	public static final String PIN_NOT_VERIFIED = "PinNotVerified";
	
	public static final String PIN_VERIFIED = "PinVerified";
	
	public static final String BAD_REQUEST = "Bad Request";
	
	public static final String SERVER_ERROR_IFW = "Internal Server errror from IFW";
	
	public static final String AUTHENTICATION_ERROR_IFW = "Authentication error from IFW";
	
	public static final String UNKNOWN_ERROR = "Currently Service Unavailable";
	
	public static final String LOGZ_IO_DEFAULT_APP = "Miscellaneous";
	
	public static final String PRM_DEFAULT_SP_LOGIN = "mySchneiderPartnerPortal.SP";
	
	public static final String INCORRECT_PASSWORD ="Incorrect password";
	
	public static final String LOGIN_ERROR ="Problem in secure login";
	
	public static final String LOGIN_SUCCESS ="Login Successful";
	
	public static final String SESSION_TIME_OUT= "900";
	
	
}

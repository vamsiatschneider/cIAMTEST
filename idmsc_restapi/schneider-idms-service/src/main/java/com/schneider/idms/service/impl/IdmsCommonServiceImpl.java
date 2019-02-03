package com.schneider.idms.service.impl;

import static com.se.idms.util.UserConstants.AUDIT_API_ADMIN;
import static com.se.idms.util.UserConstants.AUDIT_IMPERSONATING_USER;
import static com.se.idms.util.UserConstants.AUDIT_LOG_CLOSURE;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_API;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_AUTHENTICATE_CALL;
import static com.se.idms.util.UserConstants.AUDIT_REQUESTING_USER;
import static com.se.idms.util.UserConstants.AUDIT_TECHNICAL_USER;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.text.SimpleDateFormat;

import javax.annotation.Resource;
import javax.inject.Inject;
import javax.ws.rs.core.Response;

import org.apache.commons.lang3.RandomStringUtils;
import org.apache.cxf.helpers.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.ehcache.EhCacheCache;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import com.idms.product.client.IFWService;
import com.idms.product.client.OpenAMService;
import com.idms.product.client.OpenAMTokenService;
import com.idms.product.client.OpenDjService;
import com.idms.product.client.SalesForceService;
import com.idms.service.SendEmail;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Option;
import com.schneider.idms.common.DirectApiConstants;
import com.schneider.idms.common.EmailOption;
import com.schneider.idms.common.ErrorResponseCode;
import com.schneider.idms.common.UserContext;
import com.schneider.idms.model.IdmsUserRequest;
import com.schneider.idms.salesforce.service.SalesforceSyncServiceImpl;
import com.schneider.uims.service.DirectUIMSUserManagerSoapService;
import com.se.idms.cache.CacheTypes;
import com.se.idms.cache.validate.IValidator;
import com.se.idms.dto.ParseValuesByOauthHomeWorkContextDto;
import com.se.idms.dto.UserServiceResponse;
import com.se.idms.util.EmailValidator;
import com.se.idms.util.JsonConstants;
import com.se.idms.util.PhoneValidator;
import com.se.idms.util.UserConstants;

@Service("commonService")
public class IdmsCommonServiceImpl {

	/**
	 * Logger instance.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(IdmsCommonServiceImpl.class);

	/**
	 * Service to fetch information about {@link Product}s.
	 */

	@Inject
	protected OpenAMService productService;

	@Inject
	protected OpenDjService openDJService;

	@Inject
	public OpenAMTokenService openAMTokenService;

	@Inject
	protected IFWService ifwService;

	@Inject
	protected SalesForceService salesForceService;

	@Inject
	@Qualifier("pickListValidator")
	protected IValidator pickListValidator;

	@Inject
	@Qualifier("multiPickListValidator")
	protected IValidator multiPickListValidator;

	@Inject
	@Qualifier("legthValidator")
	protected IValidator legthValidator;

	@Autowired
	protected ParseValuesByOauthHomeWorkContextDto valuesByOauthHomeWorkContext;

	@Inject
	@Qualifier("phoneValidator")
	protected PhoneValidator phoneValidator;

	@Inject
	@Qualifier("emailService")
	@Lazy
	protected SendEmail sendEmail;

	@Inject
	public DirectUIMSUserManagerSoapService directUIMSUserManagerSoapService;
	
	@Inject
	private SalesforceSyncServiceImpl sfSyncServiceImpl;
	
	@Autowired
	protected static UserServiceResponse userResponse; 

	@Value("${authCsvPath}")
	protected String authCsvPath;

	@Value("${registrationCsvPath}")
	protected String registrationCsvPath;

	@Value("${adminUserName}")
	protected String adminUserName;

	@Value("${adminPassword}")
	protected String adminPassword;

	@Value("${ifwClientId}")
	protected String ifwClientId;

	@Value("${ifwClientSecret}")
	protected String ifwClientSecret;

	@Value("${salesForceClientId}")
	protected String salesForceClientId;

	@Value("${salesForceClientSecret}")
	protected String salesForceClientSecret;

	@Value("${salesForceUserName}")
	protected String salesForceUserName;

	@Value("${salesForcePassword}")
	protected String salesForcePassword;

	@Value("${ha_mode}")
	protected String ha_mode;

	@Value("${fromUserName}")
	protected String fromUserName;

	@Value("${goDitalToken}")
	protected String goDitalToken;

	@Value("${goDigitalValue}")
	protected String goDigitalValue;

	@Value("${uimsClientId}")
	protected String uimsClientId;

	@Value("${uimsClientSecret}")
	protected String uimsClientSecret;

	@Value("${redirect.uri}")
	protected String redirectUri;

	@Value("${openAMService.url}")
	protected String prefixStartUrl;

	@Value("${openDJUserName}")
	protected String openDJUserName;

	@Value("${openDJUserPassword}")
	protected String openDJUserPassword;
	
	@Value("${directApiSecretToken}")
	protected String directApiSecretToken;
	
	//CODE-RE-STRUCTURING
	@Value("${email.template.dir}")
	private String EMAIL_TEMPLATE_DIR;

	protected static String userAction = "submitRequirements";

	protected static String errorStatus = "Error";

	protected static String successStatus = "Success";

	protected static EmailValidator emailValidator = null;

	protected static SimpleDateFormat formatter;

	protected static EhCacheCache cache = null;

	@Resource(name = "cacheManager")
	protected org.springframework.cache.ehcache.EhCacheCacheManager cacheManager;

	Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();

	static {
		emailValidator = EmailValidator.getInstance();
		formatter = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");
		userResponse = new UserServiceResponse();
	}

	protected boolean checkMandatoryFieldsForDirectAPIRequest(IdmsUserRequest userRequest, ErrorResponseCode errorResponse,
			boolean checkMandatoryFields) throws IOException {

		LOGGER.info("Entered checkMandatoryFieldsFromRequest() -> Start");
		LOGGER.info("Parameter userRequest -> " + userRequest);
		LOGGER.info("Parameter userResponse -> " + errorResponse);
		LOGGER.info("Parameter checkMandatoryFields -> " + checkMandatoryFields);

		

		String userType = null;

		/**
		 * Validating Application configured or not
		 */

		if (checkMandatoryFields) {
			userType = getUserType(userRequest.getRegistrationSource());
		} else {
			userType = getUserType(userRequest.getProfileLastUpdateSource());
		}

		/***
		 * HomeContext Mandatory checks start
		 */
		
		
		/**
		 * userContext validation and length check Mandatory
		 */
		if ((checkMandatoryFields)
				&& (!UserConstants.UIMS.equalsIgnoreCase(userRequest.getRegistrationSource()))
				&& (null == userRequest.getUserContext() || userRequest.getUserContext().isEmpty())) {
			errorResponse
					.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.USERCONTEXT);
			return true;
		} else if (null != userRequest.getUserContext() && !userRequest.getUserContext().isEmpty()) {

			if (UserContext.valueOf(userRequest.getUserContext()).equals(userRequest.getUserContext()))  {
				errorResponse.setMessage(
						UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.USERCONTEXT);
				return true;

			}
		}
		
		
		/**
		 * salutation Length Validation check
		 */
		if ((null != userRequest.getSalutation() && !userRequest.getSalutation().isEmpty())
				&& (!pickListValidator.validate(UserConstants.SALUTATION.toString(),
						userRequest.getSalutation()))) {
			errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.SALUTATION);
			return true;
		}

		/**
		 * firstName Mandatory validation and length check
		 */
		if ((checkMandatoryFields) && (null == userRequest.getFirstName() || userRequest.getFirstName().isEmpty())) {
			errorResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.FIRSTNAME);
			return true;
		} else if ((null != userRequest.getFirstName() && !userRequest.getFirstName().isEmpty())
				&& (!legthValidator.validate(UserConstants.FIRST_NAME, userRequest.getFirstName()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.FIRSTNAME);
			return true;
		}
		
		/**
		 * middleName Length Validation check
		 */
		if ((null != userRequest.getMiddleName() && !userRequest.getMiddleName().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_MIDDLE_NAME_C, userRequest.getMiddleName()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.MIDDLENAME);
			return true;
		}

		/**
		 * LastName validation and length check
		 */
		if ((checkMandatoryFields) && (null == userRequest.getLastName() || userRequest.getLastName().isEmpty())) {
			errorResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.LASTNAME);
			return true;
		} else if ((null != userRequest.getLastName() && !userRequest.getLastName().isEmpty())
				&& (!legthValidator.validate(UserConstants.LAST_NAME, userRequest.getLastName()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.LASTNAME);
			return true;
		}
		
		/**
		 * validate Country Code attribute values should be present
		 */
		if ((checkMandatoryFields) && (null == userRequest.getCountryCode() || userRequest.getCountryCode().isEmpty())) {
			errorResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.COUNTRYCODE);
			return true;
		}else if(null != userRequest.getCountryCode() && !userRequest.getCountryCode().isEmpty()){
			
			if (!legthValidator.validate(UserConstants.COUNTRY, userRequest.getCountryCode())) {
				errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.COUNTRYCODE);
				return true;

			} else if (!pickListValidator.validate(UserConstants.COUNTRY, userRequest.getCountryCode())) {
				errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.COUNTRYCODE);
				return true;
			}
		}
		
		
		/**
		 * Email or Mobile is mandatory for user creation
		 */

		if ((checkMandatoryFields) && (null == userRequest.getEmail() || userRequest.getEmail().isEmpty())
				&& (null == userRequest.getMobilePhone() || userRequest.getMobilePhone().isEmpty())) {
			errorResponse.setMessage(
					UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.EMAIL + " OR " + DirectApiConstants.MOBILEPHONE);
			return true;
		}

		if ((null != userRequest.getEmail() && !userRequest.getEmail().isEmpty())
				&& (userRequest.getEmail().length() > 65)) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.EMAIL);
			return true;
		}

		if ((null != userRequest.getEmail()) && (!userRequest.getEmail().isEmpty())) {
			if (!emailValidator.validate(userRequest.getEmail())) {
				errorResponse.setMessage(UserConstants.EMAIL_VALIDATION + userRequest.getEmail());
				return true;
			}
			
			if (userRequest.getEmail().contains(UserConstants.SE_MAIL)
					|| userRequest.getEmail().contains(UserConstants.NON_SE_MAIL)
					|| userRequest.getEmail().contains(UserConstants.SCHNEIDER_MAIL)
					|| userRequest.getEmail().contains(UserConstants.NON_SCHNEIDER_MAIL)) {

				errorResponse.setMessage(UserConstants.EMAIL_VALIDATION + userRequest.getEmail());
				return true;
			}
		}
		
		/**
		 * mobilePhone is mandatory when user not provided email
		 */
		
		if (null != userRequest.getRegistrationSource()
				&& !UserConstants.UIMS.equalsIgnoreCase(userRequest.getRegistrationSource())) {

			if ((null != userRequest.getMobilePhone()) && (!userRequest.getMobilePhone().isEmpty())) {
				if (!legthValidator.validate(UserConstants.MOBILE_PHONE, userRequest.getMobilePhone())) {

					errorResponse.setMessage("Field(s) not in correct format -" + DirectApiConstants.MOBILEPHONE);
					return true;
				}
			}
		}
		
		
		/**
		 * languageCode validation and length check Mandatory
		 */

		if ((checkMandatoryFields) && (null == userRequest.getLanguageCode() || userRequest.getLanguageCode().isEmpty())) {
			errorResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.LANGUAGECODE);
			return true;
		} else if ((null != userRequest.getLanguageCode()&& !userRequest.getLanguageCode().isEmpty())
				&& !pickListValidator.validate(UserConstants.PREFERRED_LANGUAGE,
						userRequest.getLanguageCode().toLowerCase())) {
			errorResponse.setMessage(UserConstants.INVALID_VALUE_IDMS + DirectApiConstants.LANGUAGECODE);
			return true;
		}

		
		/**
		 * emailOptIn validation check it should allow 'Y' or 'N'
		 */

		if (null != userRequest.getEmailOptIn() && !userRequest.getEmailOptIn().isEmpty()) {

			if (EmailOption.valueOf(userRequest.getEmailOptIn()).equals(userRequest.getEmailOptIn())){
				errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.EMAILOPTIN);
				return true;
			}
		}
		
		/**
		 * aboutMe validation check 
		 */
		
		if ((null != userRequest.getAboutMe() && !userRequest.getAboutMe().isEmpty())
				&& (!legthValidator.validate(UserConstants.ABOUT_ME, userRequest.getAboutMe()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.ABOUTME);
			return true;
		}
		
		/**
		 * street Length Validation check
		 */

		if ((null != userRequest.getStreet() && !userRequest.getStreet().isEmpty())
				&& (!legthValidator.validate(UserConstants.STREET, userRequest.getStreet()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.STREET);
			return true;
		}
		
		/**
		 * city Length Validation check
		 */
		if ((null != userRequest.getCity() && !userRequest.getCity().isEmpty())
				&& (!legthValidator.validate(UserConstants.CITY, userRequest.getCity()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.CITY);
			return true;
		}

		
		/**
		 * zipCode Length Validation check
		 */

		if ((null != userRequest.getZipCode() && !userRequest.getZipCode().isEmpty())
				&& (!legthValidator.validate(UserConstants.POSTAL_CODE, userRequest.getZipCode()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.ZIPCODE);
			return true;
		}
		
		/**
		 * stateOrProvinceCode Length & PickList Validation check
		 */
		
		if ((null != userRequest.getStateOrProvinceCode() && !userRequest.getStateOrProvinceCode().isEmpty())) {

			if (!legthValidator.validate(UserConstants.STATE, userRequest.getStateOrProvinceCode())) {
				errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.STATEORPROVINCECODE);
				return true;

			} else if (!pickListValidator.validate(UserConstants.STATE, userRequest.getStateOrProvinceCode())) {
				errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.STATEORPROVINCECODE);
				return true;
			}
		}
		
		/**
		 * county length validation check
		 */

		if ((null != userRequest.getCounty() && !userRequest.getCounty().isEmpty())) {

			if (!legthValidator.validate(UserConstants.IDMS_COUNTY_C, userRequest.getCounty())) {
				errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.COUNTY);
				return true;

			} 
		}

		/**
		 * pOBox Length Validation check
		 */
		if ((null != userRequest.getpOBox() && !userRequest.getpOBox().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_PO_BOX_C, userRequest.getpOBox()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.POBOX);
			return true;
		}
		
		
		/**
		 * additionalAddress Length Validation check
		 */

		if ((null != userRequest.getAdditionalAddress()	&& !userRequest.getAdditionalAddress().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_ADDITIONAL_ADDRESS_C,userRequest.getAdditionalAddress()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.ADDITIONALADDRESS);
			return true;
		}
		
		/**
		 * suffix Length Validation check
		 */
		if ((null != userRequest.getSuffix() && !userRequest.getSuffix().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_SUFFIX_C, userRequest.getSuffix()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.SUFFIX);
			return true;
		}

		/**
		 * homePhone Length Validation check
		 */
		if ((null != userRequest.getHomePhone() && !userRequest.getHomePhone().isEmpty())
				&& (!legthValidator.validate(UserConstants.PHONE, userRequest.getHomePhone()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.HOMEPHONE);
			return true;
		}
		
		/**
		 * fax Length Validation check
		 */
		if ((null != userRequest.getFax() && !userRequest.getFax().isEmpty())
				&& (!legthValidator.validate(UserConstants.FAX, userRequest.getFax()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.FAX);
			return true;
		}
		
		/**
		 * idmsFederatedId Length Validation check
		 */
		if ((null != userRequest.getIdmsFederatedId() && !userRequest.getIdmsFederatedId().isEmpty())
				&& (userRequest.getIdmsFederatedId().length() > 0 && userRequest.getIdmsFederatedId().length() > 40)) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.IDMSFEDERATEDID);
			return true;
		}
		
		
		/**
		 * registrationSource validation and length check Mandatory
		 */

		if ((checkMandatoryFields) && (null == userRequest.getRegistrationSource()	|| userRequest.getRegistrationSource().isEmpty())) {
			errorResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.REGISTRATIONSOURCE);
			return true;
		}else if ((null != userRequest.getRegistrationSource()&& !userRequest.getRegistrationSource().isEmpty()) 
				&&(pickListValidator.validate(UserConstants.APPLICATIONS, userRequest.getRegistrationSource().toUpperCase()))
				) {
			errorResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.REGISTRATIONSOURCE);
			return true;
		}

		
		/**
		 * currency validation check
		 */
		if ((null != userRequest.getCurrency() && !userRequest.getCurrency().isEmpty())) {

			if (!pickListValidator.validate(UserConstants.CURRENCY, userRequest.getCurrency())) {
				errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.CURRENCY);
				return true;
			}
		}
		
		
		/**
		 * currencyCode validation check
		 */
		if ((null != userRequest.getCurrencyCode() && !userRequest.getCurrencyCode().isEmpty())) {

			if (!pickListValidator.validate(UserConstants.CURRENCY, userRequest.getCurrencyCode())) {
				errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.CURRENCYCODE);
				return true;
			}
		}
		
		if ((UserConstants.USER_TYPE_L2.equalsIgnoreCase(userType)
				|| UserConstants.USER_TYPE_L3.equalsIgnoreCase(userType))) {
			
			/**
			 * companyName Length Validation check
			 */
			
			if ((checkMandatoryFields)	& (null == userRequest.getCompanyName() || userRequest.getCompanyName().isEmpty())) {
				errorResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.COMPANYNAME);
				return true;
			} else if ((null != userRequest.getCompanyName() && !userRequest.getCompanyName().isEmpty())
					&& (!legthValidator.validate(UserConstants.COMPANY_NAME, userRequest.getCompanyName()))) {
				errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.COMPANYNAME);
				return true;
			}
			
			
		}
		
		if (UserConstants.USER_TYPE_L3.equalsIgnoreCase(userType)) {
			
			/**
			 * companyStreet Length Validation check
			 */
			if ((checkMandatoryFields)&& (null == userRequest.getCompanyStreet() || userRequest.getCompanyStreet().isEmpty())) {
				errorResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.COMPANYSTREET);
				return true;
			} else if ((null != userRequest.getCompanyStreet() && !userRequest.getCompanyStreet().isEmpty())
					&& (!legthValidator.validate(UserConstants.COMPANY_ADDRESS1_C,userRequest.getCompanyStreet()))) {
				errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.COMPANYSTREET);
				return true;
			}
			
			/**
			 * companyCity Length Validation check
			 */

			if ((checkMandatoryFields)&& (null == userRequest.getCompanyCity() || userRequest.getCompanyCity().isEmpty())) {
				errorResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.COMPANYCITY);
				return true;
			} else if ((null != userRequest.getCompanyCity() && !userRequest.getCompanyCity().isEmpty())
					&& (!legthValidator.validate(UserConstants.COMPANY_CITY_C, userRequest.getCompanyCity()))) {
				errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.COMPANYCITY);
				return true;
			}
			
			
			/**
			 * companyZipCode Length Validation check
			 */

			if ((checkMandatoryFields)&& (null == userRequest.getCompanyZipCode() || userRequest.getCompanyZipCode().isEmpty())) {
				errorResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.COMPANYZIPCODE);
				return true;
			} else if ((null != userRequest.getCompanyZipCode() && !userRequest.getCompanyZipCode().isEmpty())
					&& (!legthValidator.validate(UserConstants.COMPANY_POSTAL_CODE_C,userRequest.getCompanyZipCode()))) {
				errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.COMPANYZIPCODE);
				return true;
			}
			
		}
		
		
		/**
		 * companyStateOrProvinceCode Pick List Validation check
		 */

		if ((null != userRequest.getCompanyStateOrProvinceCode() && !userRequest.getCompanyStateOrProvinceCode().isEmpty())) {

			if (!pickListValidator.validate(UserConstants.COMPANY_STATE_C, userRequest.getCompanyStateOrProvinceCode())) {
				errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.COMPANYSTATEORPROVINCECODE);
				return true;
			}
		}
		
		/**
		 * companyPOBox Length Validation check
		 */

		if ((null != userRequest.getCompanyPOBox() && !userRequest.getCompanyPOBox().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_COMPANY_PO_BOX_C,userRequest.getCompanyPOBox()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.COMPANYPOBOX);
			return true;
		}
		
		
		/**
		 * companyCounty Length Validation check
		 */

		if (UserConstants.USER_TYPE_L3.equalsIgnoreCase(userType)) {
			if ((checkMandatoryFields)&& (null == userRequest.getCompanyCounty()) || userRequest.getCompanyCounty().isEmpty()) {
				errorResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.COMPANYCOUNTY);
				return true;
			} else if ((null != userRequest.getCompanyCounty() && !userRequest.getCompanyCounty().isEmpty())
					&& (!pickListValidator.validate(UserConstants.COUNTRY, userRequest.getCompanyCounty()))) {
				/*
				 * TODO Need to check list of company county values
				 */
				errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.COMPANYCOUNTY);
				return true;
			}

		}
		/**
		 * companyCountryCode validation and length check
		 */

		if ((null != userRequest.getCompanyCountryCode() && !userRequest.getCompanyCountryCode().isEmpty())) {
			if (!legthValidator.validate(UserConstants.COUNTRY, userRequest.getCompanyCountryCode())) {
				errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.COMPANYCOUNTRYCODE);
				return true;

			} else if (!pickListValidator.validate(UserConstants.COUNTRY, userRequest.getCompanyCountryCode())) {
				errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.COMPANYCOUNTRYCODE);
				return true;
			}
		}
		
		
		/**
		 * companyAdditionalAddress Length Validation check
		 */

		if ((null != userRequest.getCompanyAdditionalAddress()&& !userRequest.getCompanyAdditionalAddress().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_ADDITIONAL_ADDRESS_C,userRequest.getCompanyAdditionalAddress()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.COMPANYADDITIONALADDRESS);
			return true;
		}
		
		/**
		 * companyWebsite Length Validation check
		 */

		if ((null != userRequest.getCompanyWebsite() && !userRequest.getCompanyWebsite().isEmpty())
				&& (!legthValidator.validate(UserConstants.COMPANY_WEBSITE_C, userRequest.getCompanyWebsite()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.COMPANYWEBSITE);
			return true;
		}
		
		if ((UserConstants.USER_TYPE_L2.equalsIgnoreCase(userType)
				|| UserConstants.USER_TYPE_L3.equalsIgnoreCase(userType))) {

			/**
			 * classLevel1 validation and length check
			 */
			if ((checkMandatoryFields) && (null == userRequest.getClassLevel1() || userRequest.getClassLevel1().isEmpty())) {
				errorResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.CLASSLEVEL1);
				return true;
			} else if ((null != userRequest.getClassLevel1()&& !userRequest.getClassLevel1().isEmpty())) {
				if (!pickListValidator.validate(UserConstants.IAM_A1, userRequest.getClassLevel1())) {
					errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.CLASSLEVEL1);
					return true;
				}
			}
		}

		if (UserConstants.USER_TYPE_L3.equalsIgnoreCase(userType)) {
			
			/**
			 * classLevel2 Length Validation check
			 */

			if ((checkMandatoryFields)&& (null == userRequest.getClassLevel2() || userRequest.getClassLevel2().isEmpty())) {
				errorResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.CLASSLEVEL2);
				return true;
			} else if ((null != userRequest.getClassLevel2() && !userRequest.getClassLevel2().isEmpty())) {

				if (!pickListValidator.validate(UserConstants.IAM_A2.toString(),userRequest.getClassLevel2())) {
					errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.CLASSLEVEL2);
					return true;
				}
			}
			
			/**
			 * marketSegment Length Validation check
			 */

			if ((checkMandatoryFields)&& (null == userRequest.getMarketSegment() || userRequest.getMarketSegment().isEmpty())) {
				errorResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.MARKETSEGMENT);
				return true;
			} else if ((null != userRequest.getMarketSegment() && !userRequest.getMarketSegment().isEmpty())) {
				if (!pickListValidator.validate(UserConstants.MY_INDUSTRY_SEGMENT, userRequest.getMarketSegment())) {
					errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.MARKETSEGMENT);
					return true;
				}
			}
		}
		
		
		/**
		 * marketSubSegment Length & PickList Validation check
		 */

		if ((null != userRequest.getMarketSubSegment() && !userRequest.getMarketSubSegment().isEmpty())) {

			 if (!pickListValidator.validate(UserConstants.MY_INDUSTRY_SUB_SEGMENT,userRequest.getMarketSubSegment())) {
				errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.MARKETSUBSEGMENT);
				return true;
			}
		}
		
		/**
		 * marketServed Length Validation check PickList
		 */

		if ((null != userRequest.getMarketServed()&& !userRequest.getMarketServed().isEmpty())
				&& (!multiPickListValidator.validate(UserConstants.IDMS_COMPANY_MARKET_SERVED_C,
						userRequest.getMarketServed()))) {
			errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.MARKETSERVED);
			return true;
		}
		
		
		/**
		 * employeeSize Length Validation check PickList
		 */
		if ((null != userRequest.getEmployeeSize())&& !userRequest.getEmployeeSize().isEmpty())
			 if (!pickListValidator.validate(UserConstants.IDMS_COMPANY_NBR_EMPLOYEES_C,userRequest.getEmployeeSize())) {
			errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.EMPLOYEESIZE);
			return true;
		}
		
		/**
		 * department Length Validation check
		 */
		if ((null != userRequest.getDepartment() && !userRequest.getDepartment().isEmpty())
				&& (!legthValidator.validate(UserConstants.DEPARTMENT, userRequest.getDepartment()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.DEPARTMENT);
			return true;
		}
		
		
		/**
		 * headquarter Length Validation check
		 */

		if ((null != userRequest.getHeadquarter()	&& !userRequest.getHeadquarter().isEmpty())
				&& !(UserConstants.TRUE.equalsIgnoreCase(userRequest.getHeadquarter())
						|| UserConstants.FALSE.equalsIgnoreCase(userRequest.getHeadquarter()))) {
			errorResponse
					.setMessage(UserConstants.INVALID_VALUE_HEADQUARTER + DirectApiConstants.HEADQUARTER);
			return true;
		}
		
		/**
		 * annualRevenue Length Validation check
		 */

		if ((null != userRequest.getAnnualRevenue()) && !userRequest.getAnnualRevenue().isEmpty()) {

			try {
				new BigDecimal(userRequest.getAnnualRevenue()).toPlainString();
			} catch (Exception e) {
				if (null != userRequest.getRegistrationSource()
						&& UserConstants.UIMS.equalsIgnoreCase(userRequest.getRegistrationSource())) {
					userRequest.setAnnualRevenue(null);
				} else {
					errorResponse
							.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.ANNUALREVENUE);
					return true;
				}
			}
		}
		
		
		/**
		 * taxIdentificationNumber Length Validation check
		 */

		if ((null != userRequest.getTaxIdentificationNumber()&& !userRequest.getTaxIdentificationNumber().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_TAX_IDENTIFICATION_NUMBER_C,userRequest.getTaxIdentificationNumber()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.TAXIDENTIFICATIONNUMBER);
			return true;
		}
		
		/**
		 * jobTitle Length Validation check
		 */
		if ((null != userRequest.getJobTitle() && !userRequest.getJobTitle().isEmpty())) {

			if (!pickListValidator.validate(UserConstants.JOB_TITLE.toString(), userRequest.getJobTitle())) {
				errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.JOBTITLE);
				return true;
			}
		}
		
		/**
		 * jobFunction Length Validation check
		 */
		if ((null != userRequest.getJobFunction() && !userRequest.getJobFunction().isEmpty())) {

			 if (!pickListValidator.validate(UserConstants.JOB_FUNCTION, userRequest.getJobFunction())) {
				errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.JOBFUNCTION);
				return true;
			}
		}
		
		/**
		 * jobDescription Length Validation check
		 */

		if ((null != userRequest.getJobDescription() && !userRequest.getJobDescription().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_JOB_DESCRIPTION_C,
						userRequest.getJobDescription()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.JOBDESCRIPTION);
			return true;
		}
		
		/**
		 * workPhone Length Validation check
		 */
		if ((null != userRequest.getWorkPhone() && !userRequest.getWorkPhone().isEmpty())
				&& (!legthValidator.validate(UserConstants.MOBILE_PHONE, userRequest.getWorkPhone()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.WORKPHONE);
			return true;
		}
		
		/**
		 * companyFederatedId Length Validation check
		 */

		if ((null != userRequest.getCompanyFederatedId()&& !userRequest.getCompanyFederatedId().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_COMAPNY_FED_IDENTIFIER_C,userRequest.getCompanyFederatedId()))) {
			errorResponse
					.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.COMPANYFEDERATEDID);
			return true;
		}
		
		
		/**
		 * profileLastUpdateSource validation and length check
		 */
		if ((!checkMandatoryFields) && (null == userRequest.getProfileLastUpdateSource() || userRequest.getProfileLastUpdateSource().isEmpty())) {
			errorResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.PROFILELASTUPDATESOURCE);
			return true;
		} else if ((null != userRequest.getProfileLastUpdateSource()&& !userRequest.getProfileLastUpdateSource().isEmpty())
				&& (!pickListValidator.validate(UserConstants.UPDATE_SOURCE,userRequest.getProfileLastUpdateSource()))) {
			errorResponse.setMessage(UserConstants.INVALID_VALUE_IDMS + DirectApiConstants.PROFILELASTUPDATESOURCE);
			return true;
		}
		
		/**
		 *  channel picklist validation check
		 */
		if ((null != userRequest.getChannel()&& !userRequest.getChannel().isEmpty())) {
			if (!pickListValidator.validate(UserConstants.IAM_A1, userRequest.getChannel())) {
				errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.CHANNEL);
				return true;
			}
		}
		

		/**
		 *  subChannel picklist validation check
		 */
		if ((null != userRequest.getSubChannel()&& !userRequest.getSubChannel().isEmpty())) {
			if (!pickListValidator.validate(UserConstants.IAM_A2, userRequest.getSubChannel())) {
				errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.CHANNEL);
				return true;
			}
		}
		
		return false;
	}

	/**
	 * This method will verify the user password against the regex provided by
	 * the user.
	 * 
	 */
	protected boolean checkPasswordPolicy(String userPassword, String firstName, String lastName) {
		//LOGGER.info("Entered checkPasswordPolicy() -> Start");
		//LOGGER.info("Parameter userPassword -> " + userPassword);
		//LOGGER.info("Parameter firstName -> " + firstName + " ,lastName" + lastName);

		if (userPassword.contains(firstName) | userPassword.contains(lastName)
				| !userPassword.matches(UserConstants.PASSWORD_REGEX))
			return false;
		else
			return true;
	}

	/**
	 * Authorization token from OPENAM
	 * @return
	 */
	public String getSSOToken() {
		LOGGER.info("Entered getSSOToken() -> Start");
		LOGGER.info(AUDIT_REQUESTING_USER.concat(AUDIT_TECHNICAL_USER).concat(AUDIT_IMPERSONATING_USER)
				.concat(AUDIT_API_ADMIN).concat(AUDIT_OPENAM_API).concat(AUDIT_OPENAM_AUTHENTICATE_CALL)
				.concat(AUDIT_LOG_CLOSURE));

		/*cache = (EhCacheCache) cacheManager.getCache("iPlanetToken");

		if (null != cache) {
			LOGGER.info("cacahe NotNull");
		}*/

		String tokenResponse = productService.authenticateUser(adminUserName, adminPassword, UserConstants.REALM);
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		DocumentContext productDocCtx = JsonPath.using(conf).parse(tokenResponse);
		LOGGER.info("getSSOToken() -> End");
		return productDocCtx.read(JsonConstants.TOKEN_ID);

	}

	/**
	 * This method will generate the random password based on langUtils with
	 * string characters
	 * 
	 */
	protected String generateRamdomPassWord() {
		//LOGGER.info("Entered generateRamdomPassWord() -> Start");
		String tmpPr = RandomStringUtils.random(10, UserConstants.RANDOM_PR_CHARS);
		return tmpPr;
	}

	public static String getValue(String key) {
		//LOGGER.info("Entered getValue() -> Start");
		//LOGGER.info("Parameter key -> " + key);
		if (null != key) {
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

	protected String getSaleforceToken() {

		//DocumentContext productDocCtx = null;
		//Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();

		/*LOGGER.info("getSalesForceToken : => " + "PASSWORD_GRANT_TYPE : " + UserConstants.PR_GRANT_TYPE
				+ " salesForceClientId: " + salesForceClientId + " salesForceClientSecret :" + salesForceClientSecret
				+ " salesForceUserName: " + salesForceUserName + " salesForcePassword :" + salesForcePassword);*/
		String bfoAuthorizationToken = sfSyncServiceImpl.getSFToken();
		/*conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		productDocCtx = JsonPath.using(conf).parse(bfoAuthorization);
		String bfoAuthorizationToken = productDocCtx.read("$.access_token");*/

		return "Bearer " + bfoAuthorizationToken;
	}

	public static String getValues(String key) {
		//LOGGER.info("Entered getValues() -> Start");
		//LOGGER.info("Parameter key -> " + key);
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

	protected String getDelimeter() {
		return UserConstants.USER_DELIMETER;
	}

	public StringBuilder getContentFromTemplate(String scenarioName, String prefferedLanguage) throws IOException {
		//LOGGER.info("Entered getContentFromTemplate() -> Start");
		//LOGGER.info("Parameter scenarioName -> " + scenarioName);
		//LOGGER.info("Parameter prefferedLanguage -> " + prefferedLanguage);
		StringBuilder contentBuilder = new StringBuilder();
		BufferedReader in = null;
		FileReader file = null;
		String filePath = null;

		// Need to check the scenario //UPDATE EMAIL NOTIFICATION
		if (UserConstants.UPDATE_EMAIL_NOTIFICATION.equalsIgnoreCase(scenarioName)) {
			if (UserConstants.LANGUAGE_CHINA.equalsIgnoreCase(prefferedLanguage)) {
				filePath = EMAIL_TEMPLATE_DIR + "Schneider_Electric-Email_Change_Notification_CHINA.html";
			} else {
				filePath = EMAIL_TEMPLATE_DIR + "Schneider_Electric-Email_Change_Notification_ENGLISH.html";
			}
		} else if (UserConstants.UPDATE_USER_RECORD.equalsIgnoreCase(scenarioName)) {

		} else if (UserConstants.SET_USER_PR.equalsIgnoreCase(scenarioName)) {

		}
		try {

			file = new FileReader(filePath);

			in = new BufferedReader(file);
			String str;
			while ((str = in.readLine()) != null) {
				contentBuilder.append(str);
			}
			in.close();
			file.close();
		} catch (IOException e) {
			throw new FileNotFoundException("Email template not found in the location");
		}

		return contentBuilder;
	}

	protected boolean validateMobile(String mobileNumber) {
		//LOGGER.info("Entered validateMobile() -> Start");
		//LOGGER.info("Parameter mobileNumber -> " + mobileNumber);

		if (mobileNumber.matches("\\d{11}")) {
			return true;
		}
		return false;
	}

	public String getUserType(String registrationSource) throws IOException {

		DocumentContext productDocCtx = null;

		Response applicationDetails = openDJService.getUser(openDJUserName, openDJUserPassword, registrationSource);
		productDocCtx = JsonPath.using(conf).parse(IOUtils.toString((InputStream) applicationDetails.getEntity()));
		String userLevel = productDocCtx.read(JsonConstants.USER_LEVEL);

		return userLevel;
	}

	public boolean getTechnicalUserDetails(String authorizationToken) {
		try {
			String userInfo = openDJService.getUserDetails(authorizationToken);
			Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
			DocumentContext productDocCtx = JsonPath.using(conf).parse(userInfo);

			String userSubject = getValue(productDocCtx.read("$.sub").toString());

			if (userSubject.contains(DirectApiConstants.TECHNICAL_USER)) {
				return true;
			}

		} catch (Exception e) {
			e.printStackTrace();
			return false;
		}
		return false;
	}
}

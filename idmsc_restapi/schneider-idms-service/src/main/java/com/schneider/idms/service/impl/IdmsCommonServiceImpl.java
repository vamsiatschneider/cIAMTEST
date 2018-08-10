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
import com.schneider.idms.common.ErrorResponseCode;
import com.schneider.idms.mapper.DirectApiIdmsMapper;
import com.schneider.idms.model.IdmsUserRequest;
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
	protected DirectApiIdmsMapper mapper;

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
			boolean checkMandatoryFields, String applicationType) throws IOException {

		LOGGER.info("Entered checkMandatoryFieldsFromRequest() -> Start");
		LOGGER.info("Parameter userRequest -> " + userRequest);
		LOGGER.info("Parameter userResponse -> " + errorResponse);
		LOGGER.info("Parameter checkMandatoryFields -> " + checkMandatoryFields);

		

		String userType = null;

		/**
		 * Validating Application configured or not
		 */

		userType = getUserType(userRequest.getRegistrationSource());

		/***
		 * HomeContext Mandatory checks start
		 */

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
		}

		/**
		 * FirstName Mandatory validation and length check
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
		 * Country validation and length check Mandatory
		 */

		if ((checkMandatoryFields) && (null == userRequest.getCounty() || userRequest.getCounty().isEmpty())) {
			errorResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.COUNTY);
			return true;
		} else if ((null != userRequest.getCounty() && !userRequest.getCounty().isEmpty())) {

			if (!legthValidator.validate(UserConstants.COUNTRY, userRequest.getCounty())) {
				errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.COUNTY);
				return true;

			} else if (!pickListValidator.validate(UserConstants.COUNTRY, userRequest.getCounty())) {
				errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.COUNTY);
				return true;
			}

		}

		/**
		 * IDMS_PreferredLanguage__c validation and length check Mandatory
		 */

		if ((checkMandatoryFields)
				&& (!UserConstants.UIMS.equalsIgnoreCase(userRequest.getRegistrationSource()))
				&& (null == userRequest.getLanguageCode()
						|| userRequest.getLanguageCode().isEmpty())) {
			errorResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.LANGUAGECODE);
			return true;
		} else if ((null != userRequest.getLanguageCode()
				&& !userRequest.getLanguageCode().isEmpty())
				&& !pickListValidator.validate(UserConstants.PREFERRED_LANGUAGE,
						userRequest.getLanguageCode().toLowerCase())) {
			errorResponse.setMessage(UserConstants.INVALID_VALUE_IDMS + DirectApiConstants.LANGUAGECODE);
			return true;
		}

		if ((UserConstants.UIMS.equalsIgnoreCase(userRequest.getLanguageCode()))
				&& (null != userRequest.getLanguageCode()
						&& !userRequest.getLanguageCode().isEmpty())) {
			if (!legthValidator.validate(UserConstants.PREFERRED_LANGUAGE,
					userRequest.getLanguageCode())) {
				errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.LANGUAGECODE);
				return true;

			}
			if (!pickListValidator.validate(UserConstants.PREFERRED_LANGUAGE,
					userRequest.getLanguageCode())) {
				errorResponse.setMessage(UserConstants.INVALID_VALUE_IDMS + DirectApiConstants.LANGUAGECODE);
				return true;
			}
		}

		/**
		 * IDMS_Registration_Source__c validation and length check Mandatory
		 */

		if ((checkMandatoryFields) && (null == userRequest.getRegistrationSource()
				|| userRequest.getRegistrationSource().isEmpty())) {
			errorResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.REGISTRATIONSOURCE);
			return true;
		} else if ((checkMandatoryFields)
				&& (null != userRequest.getRegistrationSource()
						&& !userRequest.getRegistrationSource().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_REGISTRATION_SOURCE_C,
						userRequest.getRegistrationSource()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.REGISTRATIONSOURCE);
			return true;
		}

		if (null != userRequest.getRegistrationSource() && ((pickListValidator
				.validate(UserConstants.APPLICATIONS, userRequest.getRegistrationSource().toUpperCase()))
				|| UserConstants.UIMS.equalsIgnoreCase(userRequest.getRegistrationSource()))) {

			if ((checkMandatoryFields) && (null == userRequest.getFederationId()
					|| userRequest.getFederationId().isEmpty())) {
				errorResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.REGISTRATIONSOURCE);
				return true;
			}
		}

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
		 * IDMS_User_Context__c validation and length check Mandatory
		 */
		if ((checkMandatoryFields)
				&& (!UserConstants.UIMS.equalsIgnoreCase(userRequest.getRegistrationSource()))
				&& (null == userRequest.getUserContext() || userRequest.getUserContext().isEmpty())) {
			errorResponse
					.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.USERCONTEXT);
			return true;
		} else if (null != userRequest.getUserContext() && !userRequest.getUserContext().isEmpty()) {

			if (!legthValidator.validate(UserConstants.IDMS_USER_CONTEXT_C, userRequest.getUserContext())) {
				errorResponse.setMessage(
						UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.USERCONTEXT);
				return true;

			} else if (!pickListValidator.validate(UserConstants.IDMS_USER_CONTEXT_C,
					userRequest.getUserContext())) {
				errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.USERCONTEXT);
				return true;
			}
		}

		/***
		 * HomeContext Mandatory checks end
		 */

		/***
		 * WorkContext Mandatory checks started
		 */

		if ((UserConstants.USER_TYPE_L2.equalsIgnoreCase(userType)
				|| UserConstants.USER_TYPE_L3.equalsIgnoreCase(userType))) {

			/*
			 * Need to add the below condition if there any failures
			 * 
			 * || (UserConstants.USER_CONTEXT_WORK.equalsIgnoreCase(userRequest.getIDMS_User_Context__c())
					|| UserConstants.USER_CONTEXT_WORK_1.equalsIgnoreCase(userRequest.getIDMS_User_Context__c()))*/
			
			/**
			 * CompanyName Length Validation check
			 */
			if ((checkMandatoryFields)
					&& (!UserConstants.UIMS.equalsIgnoreCase(userRequest.getRegistrationSource()))
					&& (null == userRequest.getCompanyName() || userRequest.getCompanyName().isEmpty())) {
				errorResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.COMPANYNAME);
				return true;
			} else if ((null != userRequest.getCompanyName() && !userRequest.getCompanyName().isEmpty())
					&& (!legthValidator.validate(UserConstants.COMPANY_NAME, userRequest.getCompanyName()))) {
				errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.COMPANYNAME);
				return true;
			}

			/**
			 * IDMSClassLevel1__c validation and length check
			 */

			if ((checkMandatoryFields)
					&& (!UserConstants.UIMS.equalsIgnoreCase(userRequest.getRegistrationSource()))
					&& (null == userRequest.getClassLevel1() || userRequest.getClassLevel1().isEmpty())) {
				errorResponse.setMessage(
						UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.CLASSLEVEL1);
				return true;
			} else if ((null != userRequest.getClassLevel1()
					&& !userRequest.getClassLevel1().isEmpty())) {

				/*
				 * if (!legthValidator.validate(UserConstants.IAM_A1,
				 * userRequest.getIDMSClassLevel1__c())) {
				 * userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH
				 * + UserConstants.IDMS_CLASS_LEVEL_C);
				 * 
				 * } else
				 */ if (!pickListValidator.validate(UserConstants.IAM_A1, userRequest.getClassLevel1())) {
					errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.CLASSLEVEL1);
					return true;
				}
			}

		}

		if (UserConstants.USER_TYPE_L3.equalsIgnoreCase(userType)) {

			/**
			 * Company_Address1__c Length Validation check
			 */

			if ((checkMandatoryFields)
					&& (!UserConstants.UIMS.equalsIgnoreCase(userRequest.getRegistrationSource()))
					&& (null == userRequest.getCompanyStreet()
					|| userRequest.getCompanyStreet().isEmpty())) {
				errorResponse.setMessage(
						UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.COMPANYSTREET);
				return true;
			} else if ((null != userRequest.getCompanyStreet() && !userRequest.getCompanyStreet().isEmpty())
					&& (!legthValidator.validate(UserConstants.COMPANY_ADDRESS1_C,
							userRequest.getCompanyStreet()))) {
				errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.COMPANYSTREET);
				return true;
			}

			/**
			 * Company_City__c Length Validation check
			 */

			if ((checkMandatoryFields)
					&& (!UserConstants.UIMS.equalsIgnoreCase(userRequest.getRegistrationSource()))
					&& (null == userRequest.getCompanyCity() || userRequest.getCompanyCity().isEmpty())) {
				errorResponse
				.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.COMPANYCITY);
				return true;
			} else if ((null != userRequest.getCompanyCity() && !userRequest.getCompanyCity().isEmpty())
					&& (!legthValidator.validate(UserConstants.COMPANY_CITY_C, userRequest.getCompanyCity()))) {
				errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.COMPANYCITY);
				return true;
			}

			/**
			 * Company_Postal_Code__c Length Validation check
			 */

			if ((checkMandatoryFields)
					&& (!UserConstants.UIMS.equalsIgnoreCase(userRequest.getRegistrationSource()))
					&& (null == userRequest.getCompanyZipCode() || userRequest.getCompanyZipCode().isEmpty())) {
				errorResponse
				.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.COMPANYZIPCODE);
				return true;
			} else if ((null != userRequest.getCompanyZipCode() && !userRequest.getCompanyZipCode().isEmpty())
					&& (!legthValidator.validate(UserConstants.COMPANY_POSTAL_CODE_C,
							userRequest.getCompanyZipCode()))) {
				errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.COMPANYZIPCODE);
				return true;
			}

			/**
			 * IDMSCompanyCounty__c Length Validation check
			 */

			if ((checkMandatoryFields)
					&& (!UserConstants.UIMS.equalsIgnoreCase(userRequest.getRegistrationSource()))
					&& (null == userRequest.getCompanyCounty()) || userRequest.getCompanyCounty().isEmpty()) {
				errorResponse
				.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.COMPANYCOUNTY);
				return true;
			} else if ((null != userRequest.getCompanyCounty() && !userRequest.getCompanyCounty().isEmpty())
					&& (!pickListValidator.validate(UserConstants.COUNTRY, userRequest.getCompanyCounty()))) {
				errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.COMPANYCOUNTY);
				return true;
			}

			/**
			 * IDMSClassLevel2__c Length Validation check
			 */

			if ((checkMandatoryFields)
					&& (!UserConstants.UIMS.equalsIgnoreCase(userRequest.getClassLevel2()))
					&& (null == userRequest.getClassLevel2() || userRequest.getClassLevel2().isEmpty())) {
				errorResponse
				.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.CLASSLEVEL2);
				return true;
			} else if ((null != userRequest.getClassLevel2() && !userRequest.getClassLevel2().isEmpty())) {

				/*
				 * if (!legthValidator.validate(UserConstants.IAM_A2.toString(),
				 * userRequest.getIDMSClassLevel2__c())) {
				 * userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH
				 * + UserConstants.IDMS_CLASS_LEVEL2_C);
				 * 
				 * } else
				 */ if (!pickListValidator.validate(UserConstants.IAM_A2.toString(),
						 userRequest.getClassLevel2())) {
					 errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.CLASSLEVEL2);
					 return true;
				 }
			}

			/**
			 * IDMSMarketSegment__c Length Validation check
			 */

			if ((checkMandatoryFields)
					&& (!UserConstants.UIMS.equalsIgnoreCase(userRequest.getRegistrationSource()))
					&& (null == userRequest.getMarketSegment() || userRequest.getMarketSegment().isEmpty())) {
				errorResponse
				.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.MARKETSEGMENT);
				return true;
			} else if ((null != userRequest.getMarketSegment() && !userRequest.getMarketSegment().isEmpty())) {

				/*
				 * if
				 * (!legthValidator.validate(UserConstants.MY_INDUSTRY_SEGMENT,
				 * userRequest.getIDMSMarketSegment__c())) {
				 * userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH
				 * + UserConstants.IDMS_MARKET_SEGMENT_C);
				 * 
				 * } else
				 */ if (!pickListValidator.validate(UserConstants.MY_INDUSTRY_SEGMENT,
						 userRequest.getMarketSegment())) {
					 errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.MARKETSEGMENT);
					 return true;
				 }
			}
		}
		/***
		 * WorkContext Mandatory checks end
		 */

		/**
		 * IDMS_Email_opt_in__c length check
		 */

		if (null != userRequest.getEmailOptIn() && !userRequest.getEmailOptIn().isEmpty()) {

			if (!legthValidator.validate(UserConstants.IDMS_Email_opt_in__c, userRequest.getEmailOptIn())) {
				errorResponse
						.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.EMAILOPTIN);
				return true;

			} else if (!pickListValidator.validate(UserConstants.EMLAIL_OPT_IN,
					userRequest.getEmailOptIn())) {
				errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.EMAILOPTIN);
				return true;
			}
		}

		/**
		 * DefaultCurrencyIsoCode validation and length check Mandatory
		 */
		if ((null != userRequest.getCurrencyCode() && !userRequest.getCurrencyCode().isEmpty())) {

			if (!legthValidator.validate(UserConstants.CURRENCY, userRequest.getCurrencyCode())) {
				errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.CURRENCYCODE);
				return true;

			} else if (!pickListValidator.validate(UserConstants.CURRENCY, userRequest.getCurrencyCode())) {
				errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.CURRENCYCODE);
				return true;
			}
		}

		/**
		 * Length Validation check :: Street
		 */

		if ((null != userRequest.getStreet() && !userRequest.getStreet().isEmpty())
				&& (!legthValidator.validate(UserConstants.STREET, userRequest.getStreet()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.STREET);
			return true;
		}

		/**
		 * Length Validation check :: City
		 */
		if ((null != userRequest.getCity() && !userRequest.getCity().isEmpty())
				&& (!legthValidator.validate(UserConstants.CITY, userRequest.getCity()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.CITY);
			return true;
		}

		/**
		 * Length Validation check :: PostalCode
		 */

		if ((null != userRequest.getZipCode() && !userRequest.getZipCode().isEmpty())
				&& (!legthValidator.validate(UserConstants.POSTAL_CODE, userRequest.getZipCode()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.ZIPCODE);
			return true;
		}

		/**
		 * Length Validation check :: State
		 */

		if ((null != userRequest.getRegistrationSource()
				&& !UserConstants.UIMS.equalsIgnoreCase(userRequest.getRegistrationSource()))
				|| (null != userRequest.getProfileLastUpdateSource()
						&& !UserConstants.UIMS.equalsIgnoreCase(userRequest.getProfileLastUpdateSource()))) {
			if ((null != userRequest.getStateOrProvinceCode() && !userRequest.getStateOrProvinceCode().isEmpty())) {

				if (!legthValidator.validate(UserConstants.STATE, userRequest.getStateOrProvinceCode())) {
					errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.STATEORPROVINCECODE);
					return true;

				} else if (!pickListValidator.validate(UserConstants.STATE, userRequest.getStateOrProvinceCode())) {
					errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.STATEORPROVINCECODE);
					return true;
				}
			}
		}

		/**
		 * IDMS_County__c Length Validation check
		 */
		if ((null != userRequest.getCounty() && !userRequest.getCounty().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_COUNTY_C, userRequest.getCounty()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.COUNTY);
			return true;
		}

		/**
		 * IDMS_POBox__c Length Validation check
		 */
		if ((null != userRequest.getpOBox() && !userRequest.getpOBox().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_PO_BOX_C, userRequest.getpOBox()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.POBOX);
			return true;
		}

		/**
		 * IDMS_Federated_ID__c Length Validation check
		 */

		if ((checkMandatoryFields)
				&& (UserConstants.UIMS.equalsIgnoreCase(userRequest.getRegistrationSource()))
				&& (null == userRequest.getFederationId() || userRequest.getFederationId().isEmpty())) {
			errorResponse
					.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.FEDERATIONID);
			return true;
		} else if ((null != userRequest.getFederationId() && !userRequest.getFederationId().isEmpty())
				&& (!legthValidator.validate(UserConstants.FEDERATION_IDENTIFIER,
						userRequest.getFederationId()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.FEDERATIONID);
			return true;
		}

		/**
		 * IDMS_Profile_update_source__c validation and length check
		 */
		if ((!checkMandatoryFields) && (null == userRequest.getProfileLastUpdateSource()
				|| userRequest.getProfileLastUpdateSource().isEmpty())) {
			errorResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.PROFILELASTUPDATESOURCE);
			return true;
		} else if ((null != userRequest.getProfileLastUpdateSource()
				&& !userRequest.getProfileLastUpdateSource().isEmpty())
				&& (!pickListValidator.validate(UserConstants.UPDATE_SOURCE,
						userRequest.getProfileLastUpdateSource()))) {
			errorResponse.setMessage(UserConstants.INVALID_VALUE_IDMS + DirectApiConstants.PROFILELASTUPDATESOURCE);
			return true;
		}

		/**
		 * IDMS_Profile_update_source__c validation and length check
		 */
		if ((!checkMandatoryFields) && (null != userRequest.getProfileLastUpdateSource()
				&& UserConstants.UIMS.equalsIgnoreCase(userRequest.getProfileLastUpdateSource())
				&& null == userRequest.getFederationId())) {
			errorResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.FEDERATIONID);
			return true;
		}

		/**
		 * IDMS_AdditionalAddress__c Length Validation check
		 */

		if ((null != userRequest.getCompanyAdditionalAddress()
				&& !userRequest.getCompanyAdditionalAddress().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_ADDITIONAL_ADDRESS_C,
						userRequest.getCompanyAdditionalAddress()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.COMPANYADDITIONALADDRESS);
			return true;
		}

		/**
		 * Company_State__c Pick List Validation check
		 */

		if ((null != userRequest.getRegistrationSource()
				&& !UserConstants.UIMS.equalsIgnoreCase(userRequest.getRegistrationSource()))
				|| (null != userRequest.getProfileLastUpdateSource()
						&& !UserConstants.UIMS.equalsIgnoreCase(userRequest.getProfileLastUpdateSource()))) {

			if ((null != userRequest.getCompanyStateOrProvinceCode() && !userRequest.getCompanyStateOrProvinceCode().isEmpty())) {

				if (!pickListValidator.validate(UserConstants.COMPANY_STATE_C, userRequest.getCompanyStateOrProvinceCode())) {
					errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.COMPANYSTATEORPROVINCECODE);
					return true;
				}
			}
		}
		/**
		 * IDMSCompanyPoBox__c Length Validation check
		 */

		if ((null != userRequest.getCompanyPOBox() && !userRequest.getCompanyPOBox().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_COMPANY_PO_BOX_C,
						userRequest.getCompanyPOBox()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.COMPANYPOBOX);
			return true;
		}

		/**
		 * Company_Country__c validation and length check
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
		 * Company_Address2__c Length Validation check
		 */

		if ((null != userRequest.getCompanyAdditionalAddress() && !userRequest.getCompanyAdditionalAddress().isEmpty())
				&& (!legthValidator.validate(UserConstants.COMPANY_ADDRESS2_C, userRequest.getCompanyAdditionalAddress()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.COMPANYADDITIONALADDRESS);
			return true;
		}

		/**
		 * IDMSClassLevel2__c Length Validation check
		 */

		if ((null != userRequest.getClassLevel2() && !userRequest.getClassLevel2().isEmpty())) {

			/*
			 * if (!legthValidator.validate(UserConstants.IAM_A2.toString(),
			 * userRequest.getIDMSClassLevel2__c())) {
			 * userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH +
			 * UserConstants.IDMS_CLASS_LEVEL2_C);
			 * 
			 * } else
			 */ if (!pickListValidator.validate(UserConstants.IAM_A2.toString(), userRequest.getClassLevel2())) {
				errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.CLASSLEVEL2);
				return true;
			}
		}

		/**
		 * IDMSMarketSubSegment__c Length Validation checkJob_Title__c
		 */

		if ((null != userRequest.getMarketSubSegment() && !userRequest.getMarketSubSegment().isEmpty())) {

			/*
			 * if
			 * (!legthValidator.validate(UserConstants.MY_INDUSTRY_SUB_SEGMENT,
			 * userRequest.getIDMSMarketSubSegment__c())) {
			 * userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH +
			 * UserConstants.IDMS_MARKET_SUB_SEGMENT_C);
			 * 
			 * } else
			 */ if (!pickListValidator.validate(UserConstants.MY_INDUSTRY_SUB_SEGMENT,
					userRequest.getMarketSubSegment())) {
				errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.MARKETSUBSEGMENT);
				return true;
			}
		}

		/**
		 * Phone Length Validation check
		 */
		if ((null != userRequest.getWorkPhone() && !userRequest.getWorkPhone().isEmpty())
				&& (!legthValidator.validate(UserConstants.MOBILE_PHONE, userRequest.getWorkPhone()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.WORKPHONE);
			return true;
		}

		/**
		 * Phone Length Validation check
		 */
		if (null != userRequest.getRegistrationSource()
				&& !UserConstants.UIMS.equalsIgnoreCase(userRequest.getRegistrationSource())) {

			if ((null != userRequest.getWorkPhone() && !userRequest.getWorkPhone().isEmpty())
					&& (!legthValidator.validate(UserConstants.MOBILE_PHONE, userRequest.getWorkPhone()))) {
				errorResponse.setMessage(UserConstants.COUNTRY_FIELDS_MISSING + DirectApiConstants.WORKPHONE);
				return true;
			}
		}
		/**
		 * Job_Title__c Length Validation check
		 */
		if ((null != userRequest.getJobTitle() && !userRequest.getJobTitle().isEmpty())) {

			/*
			 * if (!legthValidator.validate(UserConstants.JOB_TITLE.toString(),
			 * userRequest.getJob_Title__c())) {
			 * userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH +
			 * UserConstants.JOB_TITLE_C);
			 * 
			 * } else
			 */ if (!pickListValidator.validate(UserConstants.JOB_TITLE.toString(), userRequest.getJobTitle())) {
				errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.JOBTITLE);
				return true;
			}
		}

		/**
		 * Job_Function__c Length Validation check
		 */
		if ((null != userRequest.getJobFunction() && !userRequest.getJobFunction().isEmpty())) {

			/*
			 * if (!legthValidator.validate(UserConstants.JOB_FUNCTION,
			 * userRequest.getJob_Function__c())) {
			 * userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH +
			 * UserConstants.JOB_FUNCTION_C);
			 * 
			 * } else
			 */ if (!pickListValidator.validate(UserConstants.JOB_FUNCTION, userRequest.getJobFunction())) {
				errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.JOBFUNCTION);
				return true;
			}
		}

		/**
		 * IDMSJobDescription__c Length Validation check
		 */

		if ((null != userRequest.getJobDescription() && !userRequest.getJobDescription().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_JOB_DESCRIPTION_C,
						userRequest.getJobDescription()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.JOBDESCRIPTION);
			return true;
		}

		/**
		 * IDMSCompanyMarketServed__c Length Validation check PickList
		 */

		if ((null != userRequest.getMarketServed()
				&& !userRequest.getMarketServed().isEmpty())
				&& (!multiPickListValidator.validate(UserConstants.IDMS_COMPANY_MARKET_SERVED_C,
						userRequest.getMarketServed()))) {
			errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.MARKETSERVED);
			return true;
		}


		/**
		 * IDMSCompanyHeadquarters__c Length Validation check
		 */

		if ((null != userRequest.getHeadquarter()
				&& !userRequest.getHeadquarter().isEmpty())
				&& !(UserConstants.TRUE.equalsIgnoreCase(userRequest.getHeadquarter())
						|| UserConstants.FALSE.equalsIgnoreCase(userRequest.getHeadquarter()))) {
			errorResponse
					.setMessage(UserConstants.INVALID_VALUE_HEADQUARTER + DirectApiConstants.HEADQUARTER);
			return true;
		}

		/**
		 * IDMSAnnualRevenue__c Length Validation check
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
		 * IDMSTaxIdentificationNumber__c Length Validation check
		 */

		if ((null != userRequest.getTaxIdentificationNumber()
				&& !userRequest.getTaxIdentificationNumber().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_TAX_IDENTIFICATION_NUMBER_C,
						userRequest.getTaxIdentificationNumber()))) {
			errorResponse
					.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.TAXIDENTIFICATIONNUMBER);
			return true;
		}

		/**
		 * IDMSMiddleName__c Length Validation check
		 */
		if ((null != userRequest.getMiddleName() && !userRequest.getMiddleName().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_MIDDLE_NAME_C, userRequest.getMiddleName()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.MIDDLENAME);
			return true;
		}

		/**
		 * Company_Website__c Length Validation check
		 */

		if ((null != userRequest.getCompanyWebsite() && !userRequest.getCompanyWebsite().isEmpty())
				&& (!legthValidator.validate(UserConstants.COMPANY_WEBSITE_C, userRequest.getCompanyWebsite()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.COMPANYWEBSITE);
			return true;
		}

		/**
		 * IDMSSalutation__c Length Validation check
		 */
		if ((null != userRequest.getSalutation() && !userRequest.getSalutation().isEmpty())
				&& (!pickListValidator.validate(UserConstants.SALUTATION.toString(),
						userRequest.getSalutation()))) {
			errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.SALUTATION);
			return true;
		}

		/**
		 * Department Length Validation check
		 */
		if ((null != userRequest.getDepartment() && !userRequest.getDepartment().isEmpty())
				&& (!legthValidator.validate(UserConstants.DEPARTMENT, userRequest.getDepartment()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.DEPARTMENT);
			return true;
		}
		/**
		 * IDMSSuffix__c Length Validation check
		 */
		if ((null != userRequest.getSuffix() && !userRequest.getSuffix().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_SUFFIX_C, userRequest.getSuffix()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.SUFFIX);
			return true;
		}

		/**
		 * Fax Length Validation check
		 */
		if ((null != userRequest.getFax() && !userRequest.getFax().isEmpty())
				&& (!legthValidator.validate(UserConstants.FAX, userRequest.getFax()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.FAX);
			return true;
		}

		/**
		 * IDMSCompanyFederationIdentifier__c Length Validation check
		 */

		if ((null != userRequest.getCompanyFederatedId()
				&& !userRequest.getCompanyFederatedId().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_COMAPNY_FED_IDENTIFIER_C,
						userRequest.getCompanyFederatedId()))) {
			errorResponse
					.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.COMPANYFEDERATEDID);
			return true;
		}

		/**
		 * IDMSDelegatedIdp__c Length Validation check
		 */

		if ((null != userRequest.getDelegatedIdp() && !userRequest.getDelegatedIdp().isEmpty())
				&& (!pickListValidator.validate(UserConstants.DELEGATED_IDP.toString(),
						userRequest.getDelegatedIdp()))) {
			errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.DELEGATEDIDP);
			return true;
		}

		/**
		 * IDMSIdentityType__c Length Validation check
		 */

		if ((null != userRequest.getIdentityType() && !userRequest.getIdentityType().isEmpty())
				&& (!pickListValidator.validate(UserConstants.IDENTITY_TYPE.toString(),
						userRequest.getIdentityType()))) {
			errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.IDENTITYTYPE);
			return true;
		}

		/**
		 * IDMSCompanyCounty__c Length Validation check
		 */

		if ((null != userRequest.getCompanyCounty() && !userRequest.getCompanyCounty().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMSCompanyCounty__c, userRequest.getCompanyCounty()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.COMPANYCOUNTY);
			return true;
		}

		/**
		 * IDMSPrimaryContact__c Length Validation check
		 */

		if ((null != userRequest.getPrimaryContact() && !userRequest.getPrimaryContact().isEmpty())
				&& (!(UserConstants.TRUE.equalsIgnoreCase(userRequest.getPrimaryContact())
						|| (UserConstants.FALSE.equalsIgnoreCase(userRequest.getPrimaryContact()))))) {
			errorResponse.setMessage(UserConstants.INVALID_VALUE + DirectApiConstants.PRIMARYCONTACT);
			return true;
		}

		// Need to check mandatory field for GoDigiatal

		if (null != goDigitalValue && goDigitalValue.equalsIgnoreCase(userRequest.getRegistrationSource())) {

			/**
			 * FirstName Mandatory validation and length check
			 */
			if ((checkMandatoryFields)
					&& (null == userRequest.getFirstName() || userRequest.getFirstName().isEmpty())) {
				errorResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.FIRSTNAME);
				return true;
			}

			/**
			 * LastName validation and length check
			 */
			if ((checkMandatoryFields) && (null == userRequest.getLastName() || userRequest.getLastName().isEmpty())) {
				errorResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.LASTNAME);
				return true;
			}

			/**
			 * validate e-mail or mobile attribute values should be present
			 */
			if ((checkMandatoryFields) && (null == userRequest.getEmail() || userRequest.getEmail().isEmpty())
					&& (null == userRequest.getMobilePhone() || userRequest.getMobilePhone().isEmpty())) {
				errorResponse.setMessage(
						UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.EMAIL + " OR " + DirectApiConstants.MOBILEPHONE);
				return true;
			}

			/**
			 * validate preferred Language attribute values should be present
			 */
			if ((checkMandatoryFields) && (null == userRequest.getLanguageCode()
					|| userRequest.getLanguageCode().isEmpty())) {
				errorResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.LANGUAGECODE);
				return true;
			}

			/**
			 * validate Country Code attribute values should be present
			 */
			if ((checkMandatoryFields) && (null == userRequest.getCountryCode() || userRequest.getCountryCode().isEmpty())) {
				errorResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.COUNTRYCODE);
				return true;
			}

			/**
			 * validate COMPANY_NAME attribute values should be present
			 */
			if ((checkMandatoryFields)
					&& (null == userRequest.getCompanyName() || userRequest.getCompanyName().isEmpty())) {
				errorResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.COMPANYNAME);
				return true;
			}

			/**
			 * validate COMPANY_ADDRESS1_C attribute values should be present
			 */
			if ((checkMandatoryFields) && (null == userRequest.getCompanyStreet()
					|| userRequest.getCompanyStreet().isEmpty())) {
				errorResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.COMPANYSTREET);
				return true;
			}

			/**
			 * validate COMPANY_CITY_C attribute values should be present
			 */
			if ((checkMandatoryFields)
					&& (null == userRequest.getCompanyCity() || userRequest.getCompanyCity().isEmpty())) {
				errorResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.COMPANYCITY);
				return true;
			}

			/**
			 * validate COMPANY_POSTAL_CODE_C attribute values should be present
			 */
			if ((checkMandatoryFields) && (null == userRequest.getCompanyZipCode()
					|| userRequest.getCompanyZipCode().isEmpty())) {
				errorResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.COMPANYZIPCODE);
				return true;
			}

			/**
			 * validate COMPANY_COUNTRY_C attribute values should be present
			 */
			if ((checkMandatoryFields)
					&& (null == userRequest.getCompanyCountryCode() || userRequest.getCompanyCountryCode().isEmpty())) {
				errorResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.COMPANYCOUNTRYCODE);
				return true;
			}

		}

		if ((null != userRequest.getAboutMe() && !userRequest.getAboutMe().isEmpty())
				&& (!legthValidator.validate(UserConstants.ABOUT_ME, userRequest.getFirstName()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + DirectApiConstants.ABOUTME);
			return true;
		}

		if ((null != userRequest.getAccountId() && !userRequest.getAccountId().isEmpty())
				&& (!legthValidator.validate(UserConstants.BFO_ACCOUNT_ID, userRequest.getAccountId()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.BFO_ACCOUNT_ID);
			return true;
		}

		if ((null != userRequest.getAccountId() && !userRequest.getAccountId().isEmpty())
				&& (!legthValidator.validate(UserConstants.ACCOUNT_ID, userRequest.getAccountId()))) {
			errorResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.ACCOUNT_ID);
			return true;
		}

		if ((null != userRequest.getTrustedAdmin() && !userRequest.getTrustedAdmin().isEmpty())) {

			if (UserConstants.TRUE.equalsIgnoreCase(userRequest.getTrustedAdmin())) {
				userRequest.setTrustedAdmin(UserConstants.SE_TRUSTED_ADMIN);
			}
		}

		if ((checkMandatoryFields) && (null == userRequest.getIsActivated() || null == userRequest.getIsActivated())) {
			userRequest.setIsActivated(UserConstants.FALSE);
		}

		/**
		 * IDMS_Profile_update_source__c validation and length check for PRM
		 * Update users
		 */
		if ((!checkMandatoryFields)
				&& (null != userRequest.getProfileLastUpdateSource()
						&& !userRequest.getProfileLastUpdateSource().isEmpty())
				&& (pickListValidator.validate(UserConstants.IDMS_BFO_profile,
						userRequest.getProfileLastUpdateSource()))
				&& (null == userRequest.getEmail() || userRequest.getEmail().isEmpty())) {
			errorResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.EMAIL);
			return true;
		}

		if (null != userRequest.getRegistrationSource() && ((pickListValidator.validate(UserConstants.APPLICATIONS,userRequest.getRegistrationSource()))
				|| UserConstants.UIMS.equalsIgnoreCase(userRequest.getRegistrationSource()))) {

			if ((checkMandatoryFields)
					&& (null == userRequest.getFederationId() || userRequest.getFederationId().isEmpty())) {
				userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + DirectApiConstants.FEDERATIONID);
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
		LOGGER.info("Entered checkPasswordPolicy() -> Start");
		LOGGER.info("Parameter userPassword -> " + userPassword);
		LOGGER.info("Parameter firstName -> " + firstName + " ,lastName" + lastName);

		if (userPassword.contains(firstName) | userPassword.contains(lastName)
				| !userPassword.matches(UserConstants.PR_REGEX))
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
		LOGGER.info("Entered generateRamdomPassWord() -> Start");
		String tmpPr = RandomStringUtils.random(10, UserConstants.RANDOM_PR_CHARS);
		return tmpPr;
	}

	public static String getValue(String key) {
		LOGGER.info("Entered getValue() -> Start");
		LOGGER.info("Parameter key -> " + key);
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

		DocumentContext productDocCtx = null;
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();

		LOGGER.info("getSalesForceToken : => " + "PASSWORD_GRANT_TYPE : " + UserConstants.PR_GRANT_TYPE
				+ " salesForceClientId: " + salesForceClientId + " salesForceClientSecret :" + salesForceClientSecret
				+ " salesForceUserName: " + salesForceUserName + " salesForcePassword :" + salesForcePassword);
		String bfoAuthorization = salesForceService.getSalesForceToken(UserConstants.CONTENT_TYPE_URL_FROM,
				UserConstants.PR_GRANT_TYPE, salesForceClientId, salesForceClientSecret, salesForceUserName,
				salesForcePassword);
		conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		productDocCtx = JsonPath.using(conf).parse(bfoAuthorization);
		String bfoAuthorizationToken = productDocCtx.read("$.access_token");

		return "Bearer " + bfoAuthorizationToken;
	}

	public static String getValues(String key) {
		LOGGER.info("Entered getValues() -> Start");
		LOGGER.info("Parameter key -> " + key);
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
		LOGGER.info("Entered getContentFromTemplate() -> Start");
		LOGGER.info("Parameter scenarioName -> " + scenarioName);
		LOGGER.info("Parameter prefferedLanguage -> " + prefferedLanguage);
		StringBuilder contentBuilder = new StringBuilder();
		BufferedReader in = null;
		FileReader file = null;
		String filePath = null;

		// Need to check the scenario //UPDATE EMAIL NOTIFICATION
		if (UserConstants.UPDATE_EMAIL_NOTIFICATION.equalsIgnoreCase(scenarioName)) {
			if (UserConstants.LANGUAGE_CHINA.equalsIgnoreCase(prefferedLanguage)) {
				filePath = CacheTypes.EMAIL_TEMPLATE_DIR + "Schneider_Electric-Email_Change_Notification_CHINA.html";
			} else {
				filePath = CacheTypes.EMAIL_TEMPLATE_DIR + "Schneider_Electric-Email_Change_Notification_ENGLISH.html";
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
		LOGGER.info("Entered validateMobile() -> Start");
		LOGGER.info("Parameter mobileNumber -> " + mobileNumber);

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

	public boolean getTechnicalUserDetails(String authorizationToken){

		String userInfo = openDJService.getUserDetails(authorizationToken);
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		DocumentContext productDocCtx = JsonPath.using(conf).parse(userInfo);

		String userSubject =  getValue(productDocCtx.read("$.sub").toString());

		if(userSubject.contains(DirectApiConstants.TECHNICAL_USER))
		{
			return true;
		}
		return false;
	}
}

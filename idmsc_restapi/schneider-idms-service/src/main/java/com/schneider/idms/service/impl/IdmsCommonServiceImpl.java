package com.schneider.idms.service.impl;

import static com.se.idms.util.UserConstants.AUDIT_API_ADMIN;
import static com.se.idms.util.UserConstants.AUDIT_IMPERSONATING_USER;
import static com.se.idms.util.UserConstants.AUDIT_LOG_CLOSURE;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_API;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_AUTHENTICATE_CALL;
import static com.se.idms.util.UserConstants.AUDIT_REQUESTING_USER;
import static com.se.idms.util.UserConstants.AUDIT_TECHNICAL_USER;

import java.math.BigDecimal;
import java.text.SimpleDateFormat;

import javax.annotation.Resource;
import javax.inject.Inject;

import org.apache.commons.lang3.RandomStringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.ehcache.EhCacheCache;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import com.idms.mapper.IdmsMapper;
import com.idms.model.IFWUser;
import com.idms.product.client.IFWService;
import com.idms.product.client.OpenAMService;
import com.idms.product.client.OpenAMTokenService;
import com.idms.product.client.SalesForceService;
import com.idms.service.SendEmail;
import com.idms.service.UIMSAccessManagerSoapService;
import com.idms.service.UIMSUserManagerSoapService;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Option;
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

	private static final Logger EMAIL_CHANGE_LOGGER = LoggerFactory.getLogger("emailChangeLogger");

	// private static final Logger LOGGER =
	// LoggerFactory.getLogger("errorLogger");

	/**
	 * Service to fetch information about {@link Product}s.
	 */

	@Inject
	protected OpenAMService productService;

	/*
	 * @Inject private OpenAMProvisionalService provisionalService;
	 */

	@Inject
	protected OpenAMTokenService openAMTokenService;

	@Inject
	protected IFWService ifwService;

	@Inject
	protected SalesForceService salesForceService;

	@Inject
	protected IdmsMapper mapper;

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

	@Autowired
	protected static UserServiceResponse userResponse;

	@Inject
	@Qualifier("phoneValidator")
	protected PhoneValidator phoneValidator;

	@Inject
	@Qualifier("emailService")
	@Lazy
	protected SendEmail sendEmail;

	@Inject
	protected UIMSUserManagerSoapService uimsUserManagerSoapService;

	@Inject
	protected UIMSAccessManagerSoapService uimsAccessManagerSoapService;

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

	protected static String userAction = "submitRequirements";

	protected static String errorStatus = "Error";

	protected static String successStatus = "Success";

	protected static EmailValidator emailValidator = null;

	protected static SimpleDateFormat formatter;

	protected static EhCacheCache cache = null;

	@Resource(name = "cacheManager")
	protected org.springframework.cache.ehcache.EhCacheCacheManager cacheManager;

	static {
		emailValidator = EmailValidator.getInstance();
		formatter = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");
		userResponse = new UserServiceResponse();
	}

	protected boolean checkMandatoryFieldsFromRequest(IFWUser userRequest, UserServiceResponse userResponse,
			boolean checkMandatoryFields) {
		LOGGER.info("Entered checkMandatoryFieldsFromRequest() -> Start");
		LOGGER.info("Parameter userRequest -> " + userRequest);
		LOGGER.info("Parameter userResponse -> " + userResponse);
		LOGGER.info("Parameter checkMandatoryFields -> " + checkMandatoryFields);

		userResponse.setStatus(errorStatus);

		if (null != userRequest.getIDMS_Registration_Source__c() && ((pickListValidator
				.validate(UserConstants.APPLICATIONS, userRequest.getIDMS_Registration_Source__c().toUpperCase()))
				|| UserConstants.UIMS.equalsIgnoreCase(userRequest.getIDMS_Registration_Source__c()))) {

			if ((checkMandatoryFields) && (null == userRequest.getIDMS_Federated_ID__c()
					|| userRequest.getIDMS_Federated_ID__c().isEmpty())) {
				userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.FEDERATION_IDENTIFIER);
				return true;
			}
		}

		if ((null != userRequest.getEmail() && !userRequest.getEmail().isEmpty())
				&& (userRequest.getEmail().length() > 65)) {
			userResponse.setStatus(errorStatus);
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.EMAIL);
			return true;
		}

		if ((null != userRequest.getEmail()) && (!userRequest.getEmail().isEmpty())) {
			if (!emailValidator.validate(userRequest.getEmail())) {
				userResponse.setStatus(errorStatus);
				userResponse.setMessage(UserConstants.EMAIL_VALIDATION + userRequest.getEmail());
				return true;
			}
		}
		if (null != userRequest.getIDMS_Registration_Source__c()
				&& !UserConstants.UIMS.equalsIgnoreCase(userRequest.getIDMS_Registration_Source__c())) {

			if ((null != userRequest.getMobilePhone()) && (!userRequest.getMobilePhone().isEmpty())) {
				if (!legthValidator.validate(UserConstants.MOBILE_PHONE, userRequest.getMobilePhone())) {

					userResponse.setStatus(errorStatus);
					userResponse.setMessage("Field(s) not in correct format -" + UserConstants.MOBILE_PHONE);
					return true;
				}
			}
		}
		/**
		 * validate e-mail or mobile attribute values should be present
		 */
		if ((checkMandatoryFields) && (null == userRequest.getEmail() || userRequest.getEmail().isEmpty())
				&& (null == userRequest.getMobilePhone() || userRequest.getMobilePhone().isEmpty())) {
			userResponse.setStatus(errorStatus);
			userResponse.setMessage(
					UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.EMAIL + " OR " + UserConstants.MOBILE);
			return true;
		}

		/**
		 * FirstName Mandatory validation and length check
		 */
		if ((checkMandatoryFields) && (null == userRequest.getFirstName() || userRequest.getFirstName().isEmpty())) {
			userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.FIRST_NAME);
			return true;
		} else if ((null != userRequest.getFirstName() && !userRequest.getFirstName().isEmpty())
				&& (!legthValidator.validate(UserConstants.FIRST_NAME, userRequest.getFirstName()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.FIRST_NAME);
			return true;
		}

		/**
		 * LastName validation and length check
		 */
		if ((checkMandatoryFields) && (null == userRequest.getLastName() || userRequest.getLastName().isEmpty())) {
			userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.LAST_NAME);
			return true;
		} else if ((null != userRequest.getLastName() && !userRequest.getLastName().isEmpty())
				&& (!legthValidator.validate(UserConstants.LAST_NAME, userRequest.getLastName()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.LAST_NAME);
			return true;
		}

		/**
		 * IDMS_Email_opt_in__c length check
		 */

		if (null != userRequest.getIDMS_Email_opt_in__c() && !userRequest.getIDMS_Email_opt_in__c().isEmpty()) {

			if (!legthValidator.validate(UserConstants.IDMS_Email_opt_in__c, userRequest.getIDMS_Email_opt_in__c())) {
				userResponse
						.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.EMLAIL_OPT_IN_DOC.toString());
				return true;

			} else if (!pickListValidator.validate(UserConstants.EMLAIL_OPT_IN,
					userRequest.getIDMS_Email_opt_in__c())) {
				userResponse.setMessage(UserConstants.INVALID_VALUE + UserConstants.EMLAIL_OPT_IN_DOC.toString());
				return true;
			}
		}

		/**
		 * IDMS_User_Context__c validation and length check Mandatory
		 */
		if ((checkMandatoryFields)
				&& (!UserConstants.UIMS.equalsIgnoreCase(userRequest.getIDMS_Registration_Source__c()))
				&& (null == userRequest.getIDMS_User_Context__c() || userRequest.getIDMS_User_Context__c().isEmpty())) {
			userResponse
					.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.IDMS_USER_CONTEXT_C.toString());
			return true;
		} else if (null != userRequest.getIDMS_User_Context__c() && !userRequest.getIDMS_User_Context__c().isEmpty()) {

			if (!legthValidator.validate(UserConstants.IDMS_USER_CONTEXT_C, userRequest.getIDMS_User_Context__c())) {
				userResponse.setMessage(
						UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.IDMS_USER_CONTEXT_C.toString());
				return true;

			} else if (!pickListValidator.validate(UserConstants.IDMS_USER_CONTEXT_C,
					userRequest.getIDMS_User_Context__c())) {
				userResponse.setMessage(UserConstants.INVALID_VALUE + UserConstants.IDMS_USER_CONTEXT_C.toString());
				return true;
			}
		}

		/**
		 * Country validation and length check Mandatory
		 */

		if ((checkMandatoryFields) && (null == userRequest.getCountry() || userRequest.getCountry().isEmpty())) {
			userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.COUNTRY);
			return true;
		} else if ((null != userRequest.getCountry() && !userRequest.getCountry().isEmpty())) {

			if (!legthValidator.validate(UserConstants.COUNTRY, userRequest.getCountry())) {
				userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.COUNTRY);
				return true;

			} else if (!pickListValidator.validate(UserConstants.COUNTRY, userRequest.getCountry())) {
				userResponse.setMessage(UserConstants.INVALID_VALUE + UserConstants.COUNTRY);
				return true;
			}

		}

		/**
		 * IDMS_Registration_Source__c validation and length check Mandatory
		 */

		if ((checkMandatoryFields) && (null == userRequest.getIDMS_Registration_Source__c()
				|| userRequest.getIDMS_Registration_Source__c().isEmpty())) {
			userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.IDMS_REGISTRATION_SOURCE_C);
			return true;
		} else if ((checkMandatoryFields)
				&& (null != userRequest.getIDMS_Registration_Source__c()
						&& !userRequest.getIDMS_Registration_Source__c().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_REGISTRATION_SOURCE_C,
						userRequest.getIDMS_Registration_Source__c()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.IDMS_REGISTRATION_SOURCE_C);
			return true;
		}

		/**
		 * IDMS_PreferredLanguage__c validation and length check Mandatory
		 */

		if ((checkMandatoryFields)
				&& (!UserConstants.UIMS.equalsIgnoreCase(userRequest.getIDMS_Registration_Source__c()))
				&& (null == userRequest.getIDMS_PreferredLanguage__c()
						|| userRequest.getIDMS_PreferredLanguage__c().isEmpty())) {
			userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.PREFERRED_LANGUAGE);
			return true;
		} else if ((null != userRequest.getIDMS_PreferredLanguage__c()
				&& !userRequest.getIDMS_PreferredLanguage__c().isEmpty())
				&& !pickListValidator.validate(UserConstants.PREFERRED_LANGUAGE,
						userRequest.getIDMS_PreferredLanguage__c().toLowerCase())) {
			userResponse.setMessage(UserConstants.INVALID_VALUE_IDMS + UserConstants.PREFERRED_LANGUAGE);
			return true;
		}

		if ((UserConstants.UIMS.equalsIgnoreCase(userRequest.getIDMS_PreferredLanguage__c()))
				&& (null != userRequest.getIDMS_PreferredLanguage__c()
						&& !userRequest.getIDMS_PreferredLanguage__c().isEmpty())) {
			if (!legthValidator.validate(UserConstants.PREFERRED_LANGUAGE,
					userRequest.getIDMS_PreferredLanguage__c())) {
				userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.PREFERRED_LANGUAGE);
				return true;

			}
			if (!pickListValidator.validate(UserConstants.PREFERRED_LANGUAGE,
					userRequest.getIDMS_PreferredLanguage__c())) {
				userResponse.setMessage(UserConstants.INVALID_VALUE_IDMS + UserConstants.PREFERRED_LANGUAGE);
				return true;
			}
		}

		/**
		 * DefaultCurrencyIsoCode validation and length check Mandatory
		 */
		if ((null != userRequest.getDefaultCurrencyIsoCode() && !userRequest.getDefaultCurrencyIsoCode().isEmpty())) {

			if (!legthValidator.validate(UserConstants.CURRENCY, userRequest.getDefaultCurrencyIsoCode())) {
				userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.CURRENCY);
				return true;

			} else if (!pickListValidator.validate(UserConstants.CURRENCY, userRequest.getDefaultCurrencyIsoCode())) {
				userResponse.setMessage(UserConstants.INVALID_VALUE + UserConstants.CURRENCY);
				return true;
			}
		}

		/**
		 * Length Validation check :: Street
		 */

		if ((null != userRequest.getStreet() && !userRequest.getStreet().isEmpty())
				&& (!legthValidator.validate(UserConstants.STREET, userRequest.getStreet()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.STREET);
			return true;
		}

		/**
		 * Length Validation check :: City
		 */
		if ((null != userRequest.getCity() && !userRequest.getCity().isEmpty())
				&& (!legthValidator.validate(UserConstants.CITY, userRequest.getCity()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.CITY);
			return true;
		}

		/**
		 * Length Validation check :: PostalCode
		 */

		if ((null != userRequest.getPostalCode() && !userRequest.getPostalCode().isEmpty())
				&& (!legthValidator.validate(UserConstants.POSTAL_CODE, userRequest.getPostalCode()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.POSTAL_CODE);
			return true;
		}

		/**
		 * Length Validation check :: State
		 */

		if ((null != userRequest.getIDMS_Registration_Source__c()
				&& !UserConstants.UIMS.equalsIgnoreCase(userRequest.getIDMS_Registration_Source__c()))
				|| (null != userRequest.getIDMS_Profile_update_source__c()
						&& !UserConstants.UIMS.equalsIgnoreCase(userRequest.getIDMS_Profile_update_source__c()))) {
			if ((null != userRequest.getState() && !userRequest.getState().isEmpty())) {

				if (!legthValidator.validate(UserConstants.STATE, userRequest.getState())) {
					userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.STATE);
					return true;

				} else if (!pickListValidator.validate(UserConstants.STATE, userRequest.getState())) {
					userResponse.setMessage(UserConstants.INVALID_VALUE + UserConstants.STATE);
					return true;
				}
			}
		}

		/**
		 * IDMS_County__c Length Validation check
		 */
		if ((null != userRequest.getIDMS_County__c() && !userRequest.getIDMS_County__c().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_COUNTY_C, userRequest.getIDMS_County__c()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.IDMS_COUNTY_C);
			return true;
		}

		/**
		 * IDMS_POBox__c Length Validation check
		 */
		if ((null != userRequest.getIDMS_POBox__c() && !userRequest.getIDMS_POBox__c().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_PO_BOX_C, userRequest.getIDMS_POBox__c()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.IDMS_PO_BOX_C);
			return true;
		}

		/**
		 * IDMS_Federated_ID__c Length Validation check
		 */

		if ((checkMandatoryFields)
				&& (UserConstants.UIMS.equalsIgnoreCase(userRequest.getIDMS_Registration_Source__c()))
				&& (null == userRequest.getIDMS_Federated_ID__c() || userRequest.getIDMS_Federated_ID__c().isEmpty())) {
			userResponse
					.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.FEDERATION_IDENTIFIER.toString());
			return true;
		} else if ((null != userRequest.getIDMS_Federated_ID__c() && !userRequest.getIDMS_Federated_ID__c().isEmpty())
				&& (!legthValidator.validate(UserConstants.FEDERATION_IDENTIFIER,
						userRequest.getIDMS_Federated_ID__c()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.FEDERATION_IDENTIFIER);
			return true;
		}

		/**
		 * IDMS_Profile_update_source__c validation and length check
		 */
		if ((!checkMandatoryFields) && (null == userRequest.getIDMS_Profile_update_source__c()
				|| userRequest.getIDMS_Profile_update_source__c().isEmpty())) {
			userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.UPDATE_SOURCE);
			return true;
		} else if ((null != userRequest.getIDMS_Profile_update_source__c()
				&& !userRequest.getIDMS_Profile_update_source__c().isEmpty())
				&& (!pickListValidator.validate(UserConstants.UPDATE_SOURCE,
						userRequest.getIDMS_Profile_update_source__c()))) {
			userResponse.setMessage(UserConstants.INVALID_VALUE_IDMS + UserConstants.UPDATE_SOURCE);
			return true;
		}

		/**
		 * IDMS_Profile_update_source__c validation and length check
		 */
		if ((!checkMandatoryFields) && (null != userRequest.getIDMS_Profile_update_source__c()
				&& UserConstants.UIMS.equalsIgnoreCase(userRequest.getIDMS_Profile_update_source__c())
				&& null == userRequest.getIDMS_Federated_ID__c())) {
			userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.FEDERATION_IDENTIFIER);
			return true;
		}

		/**
		 * IDMS_AdditionalAddress__c Length Validation check
		 */

		if ((null != userRequest.getIDMS_AdditionalAddress__c()
				&& !userRequest.getIDMS_AdditionalAddress__c().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_ADDITIONAL_ADDRESS_C,
						userRequest.getIDMS_AdditionalAddress__c()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.IDMS_ADDITIONAL_ADDRESS_C);
			return true;
		}

		/**
		 * CompanyName Length Validation check
		 */

		if ((null != userRequest.getCompanyName() && !userRequest.getCompanyName().isEmpty())
				&& (!legthValidator.validate(UserConstants.COMPANY_NAME, userRequest.getCompanyName()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.COMPANY_NAME);
			return true;
		}

		/**
		 * Company_Address1__c Length Validation check
		 */

		if ((null != userRequest.getCompany_Address1__c() && !userRequest.getCompany_Address1__c().isEmpty())
				&& (!legthValidator.validate(UserConstants.COMPANY_ADDRESS1_C, userRequest.getCompany_Address1__c()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.COMPANY_ADDRESS1_C);
			return true;
		}

		/**
		 * Company_City__c Length Validation check
		 */

		if ((null != userRequest.getCompany_City__c() && !userRequest.getCompany_City__c().isEmpty())
				&& (!legthValidator.validate(UserConstants.COMPANY_CITY_C, userRequest.getCompany_City__c()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.COMPANY_CITY_C);
			return true;
		}

		/**
		 * Company_Postal_Code__c Length Validation check
		 */

		if ((null != userRequest.getCompany_Postal_Code__c() && !userRequest.getCompany_Postal_Code__c().isEmpty())
				&& (!legthValidator.validate(UserConstants.COMPANY_POSTAL_CODE_C,
						userRequest.getCompany_Postal_Code__c()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.COMPANY_POSTAL_CODE_C);
			return true;
		}

		/**
		 * Company_State__c Pick List Validation check
		 */

		if ((null != userRequest.getIDMS_Registration_Source__c()
				&& !UserConstants.UIMS.equalsIgnoreCase(userRequest.getIDMS_Registration_Source__c()))
				|| (null != userRequest.getIDMS_Profile_update_source__c()
						&& !UserConstants.UIMS.equalsIgnoreCase(userRequest.getIDMS_Profile_update_source__c()))) {

			if ((null != userRequest.getCompany_State__c() && !userRequest.getCompany_State__c().isEmpty())) {

				if (!pickListValidator.validate(UserConstants.COMPANY_STATE_C, userRequest.getCompany_State__c())) {
					userResponse.setMessage(UserConstants.INVALID_VALUE + UserConstants.COMPANY_STATE_C);
					return true;
				}
			}
		}
		/**
		 * IDMSCompanyPoBox__c Length Validation check
		 */

		if ((null != userRequest.getIDMSCompanyPoBox__c() && !userRequest.getIDMSCompanyPoBox__c().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_COMPANY_PO_BOX_C,
						userRequest.getIDMSCompanyPoBox__c()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.IDMS_COMPANY_PO_BOX_C);
			return true;
		}

		/**
		 * Company_Country__c validation and length check
		 */

		if ((null != userRequest.getCompany_Country__c() && !userRequest.getCompany_Country__c().isEmpty())) {
			if (!legthValidator.validate(UserConstants.COUNTRY, userRequest.getCompany_Country__c())) {
				userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.COMPANY_COUNTRY_C);
				return true;

			} else if (!pickListValidator.validate(UserConstants.COUNTRY, userRequest.getCompany_Country__c())) {
				userResponse.setMessage(UserConstants.INVALID_VALUE + UserConstants.COMPANY_COUNTRY_C);
				return true;
			}
		}

		/**
		 * Company_Address2__c Length Validation check
		 */

		if ((null != userRequest.getCompany_Address2__c() && !userRequest.getCompany_Address2__c().isEmpty())
				&& (!legthValidator.validate(UserConstants.COMPANY_ADDRESS2_C, userRequest.getCompany_Address2__c()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.COMPANY_ADDRESS2_C);
			return true;
		}

		/**
		 * IDMSClassLevel1__c validation and length check
		 */
		if ((null != userRequest.getIDMSClassLevel1__c() && !userRequest.getIDMSClassLevel1__c().isEmpty())) {

			/*
			 * if (!legthValidator.validate(UserConstants.IAM_A1,
			 * userRequest.getIDMSClassLevel1__c())) {
			 * userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH +
			 * UserConstants.IDMS_CLASS_LEVEL_C);
			 * 
			 * } else
			 */ if (!pickListValidator.validate(UserConstants.IAM_A1, userRequest.getIDMSClassLevel1__c())) {
				userResponse.setMessage(UserConstants.INVALID_VALUE + UserConstants.IDMS_CLASS_LEVEL_C);
				return true;
			}
		}

		/**
		 * IDMSClassLevel2__c Length Validation check
		 */

		if ((null != userRequest.getIDMSClassLevel2__c() && !userRequest.getIDMSClassLevel2__c().isEmpty())) {

			/*
			 * if (!legthValidator.validate(UserConstants.IAM_A2.toString(),
			 * userRequest.getIDMSClassLevel2__c())) {
			 * userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH +
			 * UserConstants.IDMS_CLASS_LEVEL2_C);
			 * 
			 * } else
			 */ if (!pickListValidator.validate(UserConstants.IAM_A2.toString(), userRequest.getIDMSClassLevel2__c())) {
				userResponse.setMessage(UserConstants.INVALID_VALUE + UserConstants.IDMS_CLASS_LEVEL2_C);
				return true;
			}
		}

		/**
		 * IDMSMarketSegment__c Length Validation check
		 */

		if ((null != userRequest.getIDMSMarketSegment__c() && !userRequest.getIDMSMarketSegment__c().isEmpty())) {

			/*
			 * if (!legthValidator.validate(UserConstants.MY_INDUSTRY_SEGMENT,
			 * userRequest.getIDMSMarketSegment__c())) {
			 * userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH +
			 * UserConstants.IDMS_MARKET_SEGMENT_C);
			 * 
			 * } else
			 */ if (!pickListValidator.validate(UserConstants.MY_INDUSTRY_SEGMENT,
					userRequest.getIDMSMarketSegment__c())) {
				userResponse.setMessage(UserConstants.INVALID_VALUE + UserConstants.IDMS_MARKET_SEGMENT_C);
				return true;
			}
		}

		/**
		 * IDMSMarketSubSegment__c Length Validation checkJob_Title__c
		 */

		if ((null != userRequest.getIDMSMarketSubSegment__c() && !userRequest.getIDMSMarketSubSegment__c().isEmpty())) {

			/*
			 * if
			 * (!legthValidator.validate(UserConstants.MY_INDUSTRY_SUB_SEGMENT,
			 * userRequest.getIDMSMarketSubSegment__c())) {
			 * userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH +
			 * UserConstants.IDMS_MARKET_SUB_SEGMENT_C);
			 * 
			 * } else
			 */ if (!pickListValidator.validate(UserConstants.MY_INDUSTRY_SUB_SEGMENT,
					userRequest.getIDMSMarketSubSegment__c())) {
				userResponse.setMessage(UserConstants.INVALID_VALUE + UserConstants.IDMS_MARKET_SUB_SEGMENT_C);
				return true;
			}
		}

		/**
		 * Phone Length Validation check
		 */
		if ((null != userRequest.getPhone() && !userRequest.getPhone().isEmpty())
				&& (!legthValidator.validate(UserConstants.MOBILE_PHONE, userRequest.getPhone()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.PHONE);
			return true;
		}

		/**
		 * Phone Length Validation check
		 */
		if (null != userRequest.getIDMS_Registration_Source__c()
				&& !UserConstants.UIMS.equalsIgnoreCase(userRequest.getIDMS_Registration_Source__c())) {

			if ((null != userRequest.getPhone() && !userRequest.getPhone().isEmpty())
					&& (!legthValidator.validate(UserConstants.MOBILE_PHONE, userRequest.getPhone()))) {
				userResponse.setMessage(UserConstants.COUNTRY_FIELDS_MISSING + UserConstants.PHONE);
				return true;
			}
		}
		/**
		 * Job_Title__c Length Validation check
		 */
		if ((null != userRequest.getJob_Title__c() && !userRequest.getJob_Title__c().isEmpty())) {

			/*
			 * if (!legthValidator.validate(UserConstants.JOB_TITLE.toString(),
			 * userRequest.getJob_Title__c())) {
			 * userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH +
			 * UserConstants.JOB_TITLE_C);
			 * 
			 * } else
			 */ if (!pickListValidator.validate(UserConstants.JOB_TITLE.toString(), userRequest.getJob_Title__c())) {
				userResponse.setMessage(UserConstants.INVALID_VALUE + UserConstants.JOB_TITLE_C);
				return true;
			}
		}

		/**
		 * Job_Function__c Length Validation check
		 */
		if ((null != userRequest.getJob_Function__c() && !userRequest.getJob_Function__c().isEmpty())) {

			/*
			 * if (!legthValidator.validate(UserConstants.JOB_FUNCTION,
			 * userRequest.getJob_Function__c())) {
			 * userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH +
			 * UserConstants.JOB_FUNCTION_C);
			 * 
			 * } else
			 */ if (!pickListValidator.validate(UserConstants.JOB_FUNCTION, userRequest.getJob_Function__c())) {
				userResponse.setMessage(UserConstants.INVALID_VALUE + UserConstants.JOB_FUNCTION_C);
				return true;
			}
		}

		/**
		 * IDMSJobDescription__c Length Validation check
		 */

		if ((null != userRequest.getIDMSJobDescription__c() && !userRequest.getIDMSJobDescription__c().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_JOB_DESCRIPTION_C,
						userRequest.getIDMSJobDescription__c()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.IDMS_JOB_DESCRIPTION_C);
			return true;
		}

		/**
		 * IDMSCompanyMarketServed__c Length Validation check PickList
		 */

		if ((null != userRequest.getIDMSCompanyMarketServed__c()
				&& !userRequest.getIDMSCompanyMarketServed__c().isEmpty())
				&& (!multiPickListValidator.validate(UserConstants.IDMS_COMPANY_MARKET_SERVED_C,
						userRequest.getIDMSCompanyMarketServed__c()))) {
			userResponse.setMessage(UserConstants.INVALID_VALUE + UserConstants.IDMS_COMPANY_MARKET_SERVED_C);
			return true;
		}

		/**
		 * IDMSCompanyNbrEmployees__c Length Validation check
		 */
		if ((null != userRequest.getIDMSCompanyNbrEmployees__c()
				&& !userRequest.getIDMSCompanyNbrEmployees__c().isEmpty())) {

			/*
			 * if (!legthValidator.validate(UserConstants.
			 * IDMS_COMPANY_NBR_EMPLOYEES_C,
			 * userRequest.getIDMSCompanyNbrEmployees__c())) {
			 * userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH +
			 * UserConstants.IDMS_COMPANY_NBR_EMPLOYEES_C);
			 * 
			 * } else
			 */if (!pickListValidator.validate(UserConstants.IDMS_COMPANY_NBR_EMPLOYEES_C,
					userRequest.getIDMSCompanyNbrEmployees__c())) {
				userResponse.setMessage(UserConstants.INVALID_VALUE + UserConstants.IDMS_COMPANY_NBR_EMPLOYEES_C);
				return true;
			}
		}

		/**
		 * IDMSCompanyHeadquarters__c Length Validation check
		 */

		if ((null != userRequest.getIDMSCompanyHeadquarters__c()
				&& !userRequest.getIDMSCompanyHeadquarters__c().isEmpty())
				&& !(UserConstants.TRUE.equalsIgnoreCase(userRequest.getIDMSCompanyHeadquarters__c())
						|| UserConstants.FALSE.equalsIgnoreCase(userRequest.getIDMSCompanyHeadquarters__c()))) {
			userResponse
					.setMessage(UserConstants.INVALID_VALUE_HEADQUARTER + UserConstants.IDMS_COMPANY_HEAD_QUARTERS_C);
			return true;
		}

		/**
		 * IDMSAnnualRevenue__c Length Validation check
		 */

		if ((null != userRequest.getIDMSAnnualRevenue__c()) && !userRequest.getIDMSAnnualRevenue__c().isEmpty()) {

			try {
				new BigDecimal(userRequest.getIDMSAnnualRevenue__c()).toPlainString();
			} catch (Exception e) {
				if (null != userRequest.getIDMS_Registration_Source__c()
						&& UserConstants.UIMS.equalsIgnoreCase(userRequest.getIDMS_Registration_Source__c())) {
					userRequest.setIDMSAnnualRevenue__c(null);
				} else {
					userResponse
							.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.IDMS_ANNUAL_REVENUE_C);
					return true;
				}
			}
		}

		/**
		 * IDMSTaxIdentificationNumber__c Length Validation check
		 */

		if ((null != userRequest.getIDMSTaxIdentificationNumber__c()
				&& !userRequest.getIDMSTaxIdentificationNumber__c().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_TAX_IDENTIFICATION_NUMBER_C,
						userRequest.getIDMSTaxIdentificationNumber__c()))) {
			userResponse
					.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.IDMS_TAX_IDENTIFICATION_NUMBER_C);
			return true;
		}

		/**
		 * IDMSMiddleName__c Length Validation check
		 */
		if ((null != userRequest.getIDMSMiddleName__c() && !userRequest.getIDMSMiddleName__c().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_MIDDLE_NAME_C, userRequest.getIDMSMiddleName__c()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.IDMS_MIDDLE_NAME_C);
			return true;
		}

		/**
		 * Company_Website__c Length Validation check
		 */

		if ((null != userRequest.getCompany_Website__c() && !userRequest.getCompany_Website__c().isEmpty())
				&& (!legthValidator.validate(UserConstants.COMPANY_WEBSITE_C, userRequest.getCompany_Website__c()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.COMPANY_WEBSITE_C);
			return true;
		}

		/**
		 * IDMSSalutation__c Length Validation check
		 */
		if ((null != userRequest.getIDMSSalutation__c() && !userRequest.getIDMSSalutation__c().isEmpty())
				&& (!pickListValidator.validate(UserConstants.SALUTATION.toString(),
						userRequest.getIDMSSalutation__c()))) {
			userResponse.setMessage(UserConstants.INVALID_VALUE + UserConstants.SALUTATION);
			return true;
		}

		/**
		 * Department Length Validation check
		 */
		if ((null != userRequest.getDepartment() && !userRequest.getDepartment().isEmpty())
				&& (!legthValidator.validate(UserConstants.DEPARTMENT, userRequest.getDepartment()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.DEPARTMENT);
			return true;
		}
		/**
		 * IDMSSuffix__c Length Validation check
		 */
		if ((null != userRequest.getIDMSSuffix__c() && !userRequest.getIDMSSuffix__c().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_SUFFIX_C, userRequest.getIDMSSuffix__c()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.IDMS_SUFFIX_C);
			return true;
		}

		/**
		 * Fax Length Validation check
		 */
		if ((null != userRequest.getFax() && !userRequest.getFax().isEmpty())
				&& (!legthValidator.validate(UserConstants.FAX, userRequest.getFax()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.FAX);
			return true;
		}

		/**
		 * IDMSCompanyFederationIdentifier__c Length Validation check
		 */

		if ((null != userRequest.getIDMSCompanyFederationIdentifier__c()
				&& !userRequest.getIDMSCompanyFederationIdentifier__c().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_COMAPNY_FED_IDENTIFIER_C,
						userRequest.getIDMSCompanyFederationIdentifier__c()))) {
			userResponse
					.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.IDMS_COMAPNY_FED_IDENTIFIER_C);
			return true;
		}

		/**
		 * IDMSDelegatedIdp__c Length Validation check
		 */

		if ((null != userRequest.getIDMSDelegatedIdp__c() && !userRequest.getIDMSDelegatedIdp__c().isEmpty())
				&& (!pickListValidator.validate(UserConstants.DELEGATED_IDP.toString(),
						userRequest.getIDMSDelegatedIdp__c()))) {
			userResponse.setMessage(UserConstants.INVALID_VALUE + UserConstants.IDMS_DELEGATED_IDP_C);
			return true;
		}

		/**
		 * IDMSIdentityType__c Length Validation check
		 */

		if ((null != userRequest.getIDMSIdentityType__c() && !userRequest.getIDMSIdentityType__c().isEmpty())
				&& (!pickListValidator.validate(UserConstants.IDENTITY_TYPE.toString(),
						userRequest.getIDMSIdentityType__c()))) {
			userResponse.setMessage(UserConstants.INVALID_VALUE + UserConstants.IDMS_IDENTITY_TYPE_C);
			return true;
		}

		/**
		 * IDMSCompanyCounty__c Length Validation check
		 */

		if ((null != userRequest.getMobilePhone() && !userRequest.getMobilePhone().isEmpty())
				&& (!legthValidator.validate(UserConstants.MOBILE_PHONE, userRequest.getMobilePhone()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.MOBILE_PHONE);
			return true;
		}

		/**
		 * IDMSPrimaryContact__c Length Validation check
		 */

		if ((null != userRequest.getIDMSPrimaryContact__c() && !userRequest.getIDMSPrimaryContact__c().isEmpty())
				&& (!(UserConstants.TRUE.equalsIgnoreCase(userRequest.getIDMSPrimaryContact__c())
						|| (UserConstants.FALSE.equalsIgnoreCase(userRequest.getIDMSPrimaryContact__c()))))) {
			userResponse.setMessage(UserConstants.INVALID_VALUE + UserConstants.IDMS_PRIMARY_CONTACT_C);
			return true;
		}

		// Need to check mandatory field for GoDigiatal

		if (null != goDigitalValue && goDigitalValue.equalsIgnoreCase(userRequest.getIDMS_Registration_Source__c())) {

			/**
			 * FirstName Mandatory validation and length check
			 */
			if ((checkMandatoryFields)
					&& (null == userRequest.getFirstName() || userRequest.getFirstName().isEmpty())) {
				userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.FIRST_NAME);
				return true;
			}

			/**
			 * LastName validation and length check
			 */
			if ((checkMandatoryFields) && (null == userRequest.getLastName() || userRequest.getLastName().isEmpty())) {
				userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.LAST_NAME);
				return true;
			}

			/**
			 * validate e-mail or mobile attribute values should be present
			 */
			if ((checkMandatoryFields) && (null == userRequest.getEmail() || userRequest.getEmail().isEmpty())
					&& (null == userRequest.getMobilePhone() || userRequest.getMobilePhone().isEmpty())) {
				userResponse.setMessage(
						UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.EMAIL + " OR " + UserConstants.MOBILE);
				return true;
			}

			/**
			 * validate preferred Language attribute values should be present
			 */
			if ((checkMandatoryFields) && (null == userRequest.getIDMS_PreferredLanguage__c()
					|| userRequest.getIDMS_PreferredLanguage__c().isEmpty())) {
				userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.PREFERRED_LANGUAGE);
				return true;
			}

			/**
			 * validate Country Code attribute values should be present
			 */
			if ((checkMandatoryFields) && (null == userRequest.getCountry() || userRequest.getCountry().isEmpty())) {
				userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.COUNTRY);
				return true;
			}

			/**
			 * validate COMPANY_NAME attribute values should be present
			 */
			if ((checkMandatoryFields)
					&& (null == userRequest.getCompanyName() || userRequest.getCompanyName().isEmpty())) {
				userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.COMPANY_NAME);
				return true;
			}

			/**
			 * validate COMPANY_ADDRESS1_C attribute values should be present
			 */
			if ((checkMandatoryFields) && (null == userRequest.getCompany_Address1__c()
					|| userRequest.getCompany_Address1__c().isEmpty())) {
				userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.COMPANY_ADDRESS1_C);
				return true;
			}

			/**
			 * validate COMPANY_CITY_C attribute values should be present
			 */
			if ((checkMandatoryFields)
					&& (null == userRequest.getCompany_City__c() || userRequest.getCompany_City__c().isEmpty())) {
				userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.COMPANY_CITY_C);
				return true;
			}

			/**
			 * validate COMPANY_POSTAL_CODE_C attribute values should be present
			 */
			if ((checkMandatoryFields) && (null == userRequest.getCompany_Postal_Code__c()
					|| userRequest.getCompany_Postal_Code__c().isEmpty())) {
				userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.COMPANY_POSTAL_CODE_C);
				return true;
			}

			/**
			 * validate COMPANY_COUNTRY_C attribute values should be present
			 */
			if ((checkMandatoryFields)
					&& (null == userRequest.getCompany_Country__c() || userRequest.getCompany_Country__c().isEmpty())) {
				userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.COMPANY_COUNTRY_C);
				return true;
			}

		}

		if ((null != userRequest.getAboutMe() && !userRequest.getAboutMe().isEmpty())
				&& (!legthValidator.validate(UserConstants.ABOUT_ME, userRequest.getFirstName()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.ABOUT_ME);
			return true;
		}

		if ((null != userRequest.getBFO_ACCOUNT_ID__c() && !userRequest.getBFO_ACCOUNT_ID__c().isEmpty())
				&& (!legthValidator.validate(UserConstants.BFO_ACCOUNT_ID, userRequest.getBFO_ACCOUNT_ID__c()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.BFO_ACCOUNT_ID);
			return true;
		}

		if ((null != userRequest.getAccountId() && !userRequest.getAccountId().isEmpty())
				&& (!legthValidator.validate(UserConstants.ACCOUNT_ID, userRequest.getAccountId()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.ACCOUNT_ID);
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
				&& (null != userRequest.getIDMS_Profile_update_source__c()
						&& !userRequest.getIDMS_Profile_update_source__c().isEmpty())
				&& (pickListValidator.validate(UserConstants.IDMS_BFO_profile,
						userRequest.getIDMS_Profile_update_source__c()))
				&& (null == userRequest.getEmail() || userRequest.getEmail().isEmpty())) {
			userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.EMAIL);
			return true;
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

	public String getSSOToken() {

		LOGGER.info("Entered getSSOToken() -> Start");
		LOGGER.info(AUDIT_REQUESTING_USER.concat(AUDIT_TECHNICAL_USER).concat(AUDIT_IMPERSONATING_USER)
				.concat(AUDIT_API_ADMIN).concat(AUDIT_OPENAM_API).concat(AUDIT_OPENAM_AUTHENTICATE_CALL)
				.concat(AUDIT_LOG_CLOSURE));

		cache = (EhCacheCache) cacheManager.getCache("iPlanetToken");

		if (null != cache) {
			LOGGER.info("cacahe NotNull");
		}

		String tokenResponse = productService.authenticateUser(adminUserName, adminPassword, UserConstants.REALM);
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		DocumentContext productDocCtx = JsonPath.using(conf).parse(tokenResponse);
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

		return  "Bearer " + bfoAuthorizationToken;
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
}

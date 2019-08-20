package com.idms.service;

import static com.se.idms.util.UserConstants.AUDIT_API_ADMIN;
import static com.se.idms.util.UserConstants.AUDIT_IMPERSONATING_USER;
import static com.se.idms.util.UserConstants.AUDIT_LOG_CLOSURE;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_API;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_AUTHENTICATE_CALL;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_AUTHORIZE_CALL;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_AUTHORIZE_POST_CALL;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_GET_CALL;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_UPDATE_CALL;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_USER_EXISTS_CALL;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_USER_INFO_CALL;
import static com.se.idms.util.UserConstants.AUDIT_OPENAM_USER_REGISTRATION_CALL;
import static com.se.idms.util.UserConstants.AUDIT_REQUESTING_USER;
import static com.se.idms.util.UserConstants.AUDIT_TECHNICAL_USER;
import static com.se.idms.util.UserConstants.GET_USER_BY_TOKEN_TIME_LOG;
import static com.se.idms.util.UserConstants.GET_USER_TIME_LOG;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.math.BigDecimal;
import java.net.MalformedURLException;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.security.NoSuchAlgorithmException;
import java.text.SimpleDateFormat;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Calendar;
import java.util.Date;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.annotation.Resource;
import javax.inject.Inject;
import javax.ws.rs.BadRequestException;
import javax.ws.rs.ClientErrorException;
import javax.ws.rs.NotAuthorizedException;
import javax.ws.rs.NotFoundException;
import javax.ws.rs.WebApplicationException;
import javax.ws.rs.core.Cookie;
import javax.ws.rs.core.NewCookie;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import org.apache.commons.codec.binary.Base64;
import org.apache.commons.lang3.RandomStringUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.cxf.helpers.IOUtils;
import org.json.simple.JSONArray;
import org.json.simple.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.cache.ehcache.EhCacheCache;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.github.rholder.retry.Retryer;
import com.github.rholder.retry.RetryerBuilder;
import com.github.rholder.retry.StopStrategies;
import com.github.rholder.retry.WaitStrategies;
import com.google.common.base.Predicates;
import com.ibm.icu.text.Transliterator;
import com.idms.mapper.IdmsMapper;
import com.idms.model.AILRequest;
import com.idms.model.ActivateUser;
import com.idms.model.ActivateUserRequest;
import com.idms.model.AddEmailRequest;
import com.idms.model.AddMobileRequest;
import com.idms.model.CheckUserExistsRequest;
import com.idms.model.CheckUserIdentityRequest;
import com.idms.model.ConfirmPinErrorResponse;
import com.idms.model.ConfirmPinRequest;
import com.idms.model.ConfirmPinResponse;
import com.idms.model.CreateUserRequest;
import com.idms.model.CreateUserResponse;
import com.idms.model.GetUserByApplicationResponse;
import com.idms.model.GetUserRecordResponse;
import com.idms.model.IDMSUserResponse;
import com.idms.model.IFWUser;
import com.idms.model.PasswordRecoveryRequest;
import com.idms.model.RegistrationAttributes;
import com.idms.model.ResendEmailChangeRequest;
import com.idms.model.ResendPinRequest;
import com.idms.model.ResendRegEmailRequest;
import com.idms.model.SendInvitationRequest;
import com.idms.model.SendOTPRequest;
import com.idms.model.TransliteratorAttributes;
import com.idms.model.TransliteratorConversionRequest;
import com.idms.model.TransliteratorConversionResponse;
import com.idms.model.TransliteratorErrorResponse;
import com.idms.model.TransliteratorRequest;
import com.idms.model.TransliteratorResponse;
import com.idms.model.UpdatePasswordRequest;
import com.idms.model.UpdateUserRequest;
import com.idms.model.UpdateUserResponse;
import com.idms.model.UserDetailByApplicationRequest;
import com.idms.model.UserMFADataRequest;
import com.idms.model.VerifyPinRequest;
import com.idms.product.client.IFWService;
import com.idms.product.client.IdentityService;
import com.idms.product.client.OpenAMService;
import com.idms.product.client.OpenAMTokenService;
import com.idms.product.client.OpenDjService;
import com.idms.product.client.SalesForceService;
import com.idms.product.model.Attributes;
import com.idms.product.model.OpenAMGetUserHomeResponse;
import com.idms.product.model.OpenAMGetUserWorkResponse;
import com.idms.product.model.OpenAmUser;
import com.idms.product.model.OpenAmUserRequest;
import com.idms.product.model.PasswordRecoveryUser;
import com.idms.product.model.PostMobileRecord;
import com.idms.service.uims.sync.UIMSUserManagerSoapServiceSync;
import com.idms.service.util.AsyncUtil;
import com.idms.service.util.ChinaIdmsUtil;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Option;
import com.schneider.idms.common.DirectApiConstants;
import com.schneider.idms.common.ErrorCodeConstants;
import com.schneider.idms.salesforce.service.SaleforceServiceImpl;
import com.schneider.idms.salesforce.service.SalesforceSyncServiceImpl;
import com.schneider.ims.service.uimsv2.CompanyV3;
import com.se.idms.cache.utils.EmailConstants;
import com.se.idms.cache.validate.IValidator;
import com.se.idms.dto.AILResponse;
import com.se.idms.dto.ErrorResponse;
import com.se.idms.dto.GetUserHomeByOauthResponse;
import com.se.idms.dto.GetUserWorkByOauthResponse;
import com.se.idms.dto.IDMSUserAIL;
import com.se.idms.dto.IDMSUserRecord;
import com.se.idms.dto.IDMSUser__r;
import com.se.idms.dto.IFWCustomAttributesForWork;
import com.se.idms.dto.ParseValuesByOauthHomeWorkContextDto;
import com.se.idms.dto.PasswordRecoveryResponse;
import com.se.idms.dto.SetPasswordErrorResponse;
import com.se.idms.dto.SetPasswordRequest;
import com.se.idms.dto.SetPasswordResponse;
import com.se.idms.dto.SocialLoginResponse;
import com.se.idms.dto.UIMSResponse;
import com.se.idms.dto.UIMSStatusInfo;
import com.se.idms.dto.UserExistsResponse;
import com.se.idms.dto.UserServiceResponse;
import com.se.idms.util.EmailValidator;
import com.se.idms.util.JsonConstants;
import com.se.idms.util.LangSupportUtil;
import com.se.idms.util.PhoneValidator;
import com.se.idms.util.UserConstants;
import com.uims.authenticatedUsermanager.UserV6;

@Service("userService")
public class UserServiceImpl implements UserService {

	/**
	 * Logger instance.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(UserServiceImpl.class);

	private static final Logger EMAIL_CHANGE_LOGGER = LoggerFactory.getLogger("emailChangeLogger");

	// private static final Logger LOGGER =
	// LoggerFactory.getLogger("errorLogger");

	// CODE-RE-STRUCTURING
	@Value("${email.template.dir}")
	private String EMAIL_TEMPLATE_DIR;

	// CODE-RE-STRUCTURING
	@Value("${caller.fid}")
	private String CALLER_FID;

	@Value("${caller.fid}")
	private String LOGIN_ERROR;

	/**
	 * Service to fetch information about {@link Product}s.
	 */

	@Inject
	private OpenAMService productService;

	/*
	 * @Inject private OpenAMProvisionalService provisionalService;
	 */

	@Inject
	private OpenAMTokenService openAMTokenService;

	@Inject
	private IdentityService identityService;

	@Inject
	private IFWService ifwService;

	@Inject
	private SalesForceService salesForceService;

	@Inject
	private SaleforceServiceImpl datePopulationSerivce;

	@Inject
	protected OpenDjService openDJService;

	@Inject
	private IdmsMapper mapper;

	@Inject
	@Qualifier("pickListValidator")
	private IValidator pickListValidator;

	@Inject
	@Qualifier("multiPickListValidator")
	private IValidator multiPickListValidator;

	@Inject
	@Qualifier("legthValidator")
	private IValidator legthValidator;

	@Autowired
	private ParseValuesByOauthHomeWorkContextDto valuesByOauthHomeWorkContext;

	@Autowired
	private static UserServiceResponse userResponse;

	/*
	 * @Inject
	 * 
	 * @Qualifier("emailValidator") private EmailValidator emailValidator;
	 */

	@Inject
	@Qualifier("phoneValidator")
	private PhoneValidator phoneValidator;

	@Inject
	@Qualifier("emailService")
	@Lazy
	private SendEmail sendEmail;

	@Inject
	private UIMSUserManagerSoapService uimsUserManagerSoapService;

	@Inject
	private UIMSAccessManagerSoapService uimsAccessManagerSoapService;

	@Inject
	private UimsSetPasswordSoapService uimsSetPasswordSoapService;

	@Inject
	private UIMSUserManagerSoapServiceSync uimsUserManagerSync;

	@Value("${authCsvPath}")
	private String authCsvPath;

	@Value("${registrationCsvPath}")
	private String registrationCsvPath;

	@Value("${adminUserName}")
	private String adminUserName;

	@Value("${adminPassword}")
	private String adminPassword;

	@Value("${ifwClientId}")
	private String ifwClientId;

	@Value("${ifwClientSecret}")
	private String ifwClientSecret;

	@Value("${salesForceClientId}")
	private String salesForceClientId;

	@Value("${salesForceClientSecret}")
	private String salesForceClientSecret;

	@Value("${salesForceUserName}")
	private String salesForceUserName;

	@Value("${salesForcePassword}")
	private String salesForcePassword;

	@Value("${ha_mode}")
	private String ha_mode;

	@Value("${fromUserName}")
	private String fromUserName;

	@Value("${goDitalToken}")
	private String goDitalToken;

	@Value("${goDigitalValue}")
	private String goDigitalValue;

	@Value("${uimsClientId}")
	private String uimsClientId;

	@Value("${uimsClientSecret}")
	private String uimsClientSecret;

	@Value("${redirect.uri}")
	private String redirectUri;

	@Value("${openAMService.url}")
	private String prefixStartUrl;

	@Value("${identityService.url}")
	private String prefixIdentityUrl;

	@Value("${register.prmUser.idp}")
	private String registerPRMUserIdp;

	@Value("${otpvalidationtimeinminute}")
	private String otpvalidationtimeinminute;

	@Value("${openDJUserName}")
	private String djUserName;

	@Value("${openDJUserPassword}")
	private String djUserPwd;

	@Value("${domain.name}")
	private String domainName;
	
	@Value("${enable.sendOtpOverEmail}")
	private String sendOTPOverEmail;
	
	@Value("${enableTestMailDomain}")
	private String enableTestMailDomain;
	
	@Value("${idmsc.maintenance_mode_global}")
	private String maintenanceModeGlobal;
	
	private static String userAction = "submitRequirements";

	private static String errorStatus = "Error";

	private static String successStatus = "Success";

	private static EmailValidator emailValidator = null;

	// private static Map<String,String> userPinMap = null;
	private static SimpleDateFormat formatter;
	@Inject
	private SalesforceSyncServiceImpl sfSyncServiceImpl;

	// private static Ehcache cache = null;

	private static EhCacheCache cache = null;

	String userIdExistInUIMS = null;
	
	@Value("${idmsc.emailUserNameFormat}")
	private String defaultUserNameFormat;
	
	/*
	 * @Resource(name="cacheManager") private CacheManager cacheManager;
	 */

	/*
	 * @Resource(name="cacheManager") private
	 * org.springframework.cache.support.SimpleCacheManager cacheManager;
	 */

	@Resource(name = "cacheManager")
	private org.springframework.cache.ehcache.EhCacheCacheManager cacheManager;

	// protected static List<String> appList = null;

	static {
		emailValidator = EmailValidator.getInstance();
		formatter = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");
		userResponse = new UserServiceResponse();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.idms.service.UserServiceImpl#authenticateUser(java.lang.String,
	 * java.lang.String, java.lang.String)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public Response authenticateUser(String userName, String password, String realm) {
		LOGGER.info("Entered authenticateUser() -> Start");
		LOGGER.info("Testing****");
		LOGGER.info("Parameter userName -> " + userName + " ,realm -> " + realm);

		String successResponse = null;
		String regSource = "";
		Response checkUserExistsResponse = null;
		UserExistsResponse checkUserExistsFlag = null;
		JSONObject jsonObject = new JSONObject();
		// LOGGER.info(JsonConstants.JSON_STRING + userName);
		LOGGER.info(AUDIT_REQUESTING_USER.concat(userName).concat(AUDIT_IMPERSONATING_USER).concat(AUDIT_API_ADMIN)
				.concat(AUDIT_OPENAM_API).concat(AUDIT_OPENAM_AUTHENTICATE_CALL).concat(AUDIT_LOG_CLOSURE));

		cache = (EhCacheCache) cacheManager.getCache("iPlanetToken");

		if (null != cache) {
			LOGGER.info("cacahe NotNull");
			// cache.evictExpiredElements();
		}
		// Response authenticateResponse =
		// productService.authenticateIdmsChinaUser(userName, password, realm);

		try {

			// The below snippet for authentication logs.
			String PlanetDirectoryKey = null;
			try {
				PlanetDirectoryKey = getSSOToken();
			} catch (IOException ioExcep) {
				LOGGER.error("Unable to get SSO Token" + ioExcep.getMessage(),ioExcep);
			}

			LOGGER.info("Start: checkUserExistsWithEmailMobile() of OpenAMService for userName=" + userName);
			String userData = productService.checkUserExistsWithEmailMobile(
					UserConstants.CHINA_IDMS_TOKEN + PlanetDirectoryKey,
					"loginid eq " + "\"" + URLEncoder.encode(URLDecoder.decode(userName, "UTF-8"), "UTF-8")
							+ "\" or login_mobile eq " + "\""
							+ URLEncoder.encode(URLDecoder.decode(userName, "UTF-8"), "UTF-8") + "\"");

			LOGGER.info("End: checkUserExistsWithEmailMobile() of OpenAMService finished for userName=" + userName);

			Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
			// getting the context
			DocumentContext productDocCtx = JsonPath.using(conf).parse(userData);
			productDocCtx = JsonPath.using(conf).parse(userData);
			Integer resultCount = productDocCtx.read("$.resultCount");
			LOGGER.info("resultCount = "+resultCount);
			if (resultCount.intValue() > 0) {
				regSource = null != productDocCtx.read("$.result[0].registerationSource[0]")
						? getValue(productDocCtx.read("$.result[0].registerationSource[0]").toString()) : null;
				LOGGER.info("regSource: " + regSource);
			}
			LOGGER.info("Start: aunthenticate User of OPENAMService for username=" + userName);
			Response authenticateResponse = ChinaIdmsUtil.executeHttpClient(prefixStartUrl, realm, userName, password);
			LOGGER.info("End: aunthenticate User of OPENAMService finished for username=" + userName);
			successResponse = (String) authenticateResponse.getEntity();
			LOGGER.info("Authentication status code from OPENAMService:" + authenticateResponse.getStatus());
			if (401 == authenticateResponse.getStatus() && successResponse.contains(UserConstants.ACCOUNT_BLOCKED)) {
				jsonObject.put("message", UserConstants.ACCOUNT_BLOCKED);
				AsyncUtil.generateCSV(authCsvPath, new Date() + "," + userName + "," + errorStatus + "," + regSource);
				return Response.status(Response.Status.UNAUTHORIZED.getStatusCode()).entity(jsonObject).build();

			} else if (401 == authenticateResponse.getStatus()) {
				checkUserExistsResponse = checkUserExists(userName, UserConstants.FALSE);
				checkUserExistsFlag = (UserExistsResponse) checkUserExistsResponse.getEntity();

				if (UserConstants.TRUE.equalsIgnoreCase(checkUserExistsFlag.getMessage())) {
					jsonObject.put("user_store", "CN");
					AsyncUtil.generateCSV(authCsvPath,
							new Date() + "," + userName + "," + errorStatus + "," + regSource);
					return Response.status(Response.Status.UNAUTHORIZED.getStatusCode()).entity(jsonObject).build();
				} else {
					checkUserExistsResponse = checkUserExists(userName, UserConstants.TRUE);
					checkUserExistsFlag = (UserExistsResponse) checkUserExistsResponse.getEntity();

					if (UserConstants.TRUE.equalsIgnoreCase(checkUserExistsFlag.getMessage())) {
						jsonObject.put("user_store", "GLOBAL");
						AsyncUtil.generateCSV(authCsvPath,
								new Date() + "," + userName + "," + errorStatus + "," + regSource);
						return Response.status(Response.Status.UNAUTHORIZED.getStatusCode()).entity(jsonObject).build();
					} else {
						jsonObject.put("user_store", "None");
						AsyncUtil.generateCSV(authCsvPath,
								new Date() + "," + userName + "," + errorStatus + "," + regSource);
						return Response.status(Response.Status.UNAUTHORIZED.getStatusCode()).entity(jsonObject).build();
					}
				}
			}

			// successResponse = IOUtils.toString((InputStream)
			// authenticateResponse.getEntity());
		} catch (Exception e) {
			LOGGER.error("Exception in authenticateUser():" + e.getMessage(),e);
			jsonObject.put("user_store", "None");
			AsyncUtil.generateCSV(authCsvPath, new Date() + "," + userName + "," + successStatus + "," + regSource);
			return Response.status(Response.Status.UNAUTHORIZED.getStatusCode()).entity(jsonObject).build();
		}

		// LOGGER.debug(JsonConstants.JSON_STRING, successResponse);
		AsyncUtil.generateCSV(authCsvPath, new Date() + "," + userName + "," + successStatus + "," + regSource);
		LOGGER.info("authenticateUser() -> Ending");
		return Response.status(Response.Status.OK.getStatusCode()).entity(successResponse).build();
	}

	/**
	 * To get the amadmin token
	 * 
	 * @return token
	 */
	public String getSSOToken() throws IOException {
		LOGGER.info("Entered getSSOToken() -> Start");
		LOGGER.info(AUDIT_REQUESTING_USER.concat(AUDIT_TECHNICAL_USER).concat(AUDIT_IMPERSONATING_USER)
				.concat(AUDIT_API_ADMIN).concat(AUDIT_OPENAM_API).concat(AUDIT_OPENAM_AUTHENTICATE_CALL)
				.concat(AUDIT_LOG_CLOSURE));

		// cache = cacheManger.getCache("iPlanetToken");
		cache = (EhCacheCache) cacheManager.getCache("iPlanetToken");

		// final Ehcache cacahe = cacheManger.getCache("iPlanetToken");
		if (null != cache) {
			// LOGGER.info("cacahe NotNull");

			// cache.evictExpiredElements();
			// cache.getQuiet(adminUserName);
			// LOGGER.info("
			// getKeysWithExpiryCheck"+cache.getKeysWithExpiryCheck());
			// LOGGER.info(" expired : "+expired("iPlanetToken"));
			/*
			 * List<Object> keys = cache.getKeys();
			 * 
			 * if (keys.size() > 0) { for (Object key : keys) { Element element
			 * = cache.get(key); if (element != null) {
			 * 
			 * Object cachedObject = element.getObjectValue(); LOGGER.info(
			 * "cachedObject : "+cachedObject);
			 * 
			 * } } }
			 */
			// cacahe.flush();
		}

		String tokenResponse = productService.authenticateUser(adminUserName, adminPassword, UserConstants.REALM);
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		DocumentContext productDocCtx = JsonPath.using(conf).parse(tokenResponse);
		return productDocCtx.read(JsonConstants.TOKEN_ID);

	}
	/*
	 * public boolean expired(final String key) { boolean expired = false; final
	 * Element element = cache.get(key); if (element != null) { expired =
	 * cache.isExpired(element); } return expired; }
	 */

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.idms.service.UserServiceImpl#getUser(java.lang.String)
	 */
	@SuppressWarnings("unchecked")
	public Response getUser(String userId) {
		LOGGER.info("Entered getUser() -> Start");
		LOGGER.info("Parameter userId -> " + userId);
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		String userData = null;
		// getting iPlanetDirectoryPro token from OpenAM
		String token = null;

		try {
			token = getSSOToken();
		} catch (IOException ioExp) {
			LOGGER.error("Unable to get SSO Token" + ioExp.getMessage(),ioExp);
		}

		try {
			if (null == userId || userId.isEmpty()) {
				JSONObject jsonObject = new JSONObject();
				jsonObject.put("errorCode", "NOT_FOUND");
				jsonObject.put("message", "Provided external ID field does not exist or is  not accessible: " + userId);

				JSONArray jsonArray = new JSONArray();
				jsonArray.add(jsonObject);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info(GET_USER_TIME_LOG + elapsedTime);
				LOGGER.error("userId is null or empty");
				return Response.status(Response.Status.NOT_FOUND.getStatusCode()).entity(jsonArray).build();

			} else if (null != userId) {
				LOGGER.info(AUDIT_REQUESTING_USER.concat(AUDIT_TECHNICAL_USER).concat(AUDIT_IMPERSONATING_USER)
						.concat(AUDIT_API_ADMIN).concat(AUDIT_OPENAM_API).concat(AUDIT_OPENAM_GET_CALL).concat(userId)
						.concat(AUDIT_LOG_CLOSURE));
				LOGGER.info("Start: getUser() of OpenAMService for userId=" + userId);
				userData = productService.getUser(token, userId);
				LOGGER.info("End: getUser() of OpenAMService finished with userdata: " + ChinaIdmsUtil.printOpenAMInfo(userData));
			}
		} catch (Exception e) {
			LOGGER.error("Error in getUser() openam service->" + e.getMessage(),e);
			JSONObject jsonObject = new JSONObject();
			jsonObject.put("errorCode", "Unauthorized");
			jsonObject.put("message", "OpenAM issue of Authorization for " + userId);

			JSONArray jsonArray = new JSONArray();
			jsonArray.add(jsonObject);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info(GET_USER_TIME_LOG + elapsedTime);
			return Response.status(Response.Status.UNAUTHORIZED.getStatusCode()).entity(jsonArray).build();
		}
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		// getting the context
		DocumentContext productDocCtx = JsonPath.using(conf).parse(userData);
		String context = null != productDocCtx.read(JsonConstants.EMPLOYEE_TYPE)
				? getValue(productDocCtx.read(JsonConstants.EMPLOYEE_TYPE).toString()) : null;
		LOGGER.info("context=" + context);

		OpenAMGetUserHomeResponse userHomeResponse = new OpenAMGetUserHomeResponse();
		OpenAMGetUserWorkResponse userWorkResponse = new OpenAMGetUserWorkResponse();
		DocumentContext userProductDocCtx = JsonPath.using(conf).parse(userData);
		Attributes attributes = new Attributes();
		userHomeResponse.setAttributes(attributes);
		userWorkResponse.setAttributes(attributes);
		if ("@home".equalsIgnoreCase(context) || "home".equalsIgnoreCase(context)) {
			return returnGetUserHomeContext(startTime, userHomeResponse, userProductDocCtx);
		} else if ("@work".equalsIgnoreCase(context) || "work".equalsIgnoreCase(context)
				|| "Both".equalsIgnoreCase(context) || "@Both".equalsIgnoreCase(context)) {
			valuesByOauthHomeWorkContext.parseValuesWorkContext(userWorkResponse, userProductDocCtx);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info(GET_USER_TIME_LOG + elapsedTime);
			return Response.status(Response.Status.OK.getStatusCode()).entity(userWorkResponse).build();
		} else if (null == context || "".equals(context)) {
			return returnGetUserHomeContext(startTime, userHomeResponse, userProductDocCtx);
		}
		elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		LOGGER.info(GET_USER_TIME_LOG + elapsedTime);
		//productService.sessionLogout(UserConstants.IPLANET_DIRECTORY_PRO+token, "logout");
		return Response.status(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode()).build();
	}
	
	/**
	 * 
	 * @param applicationContext
	 * @param userId
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public Response getUserWhenContextIsNull(String applicationContext, String userId) {
		LOGGER.info("Entered getUserWhenContextIsNull() -> Start");
		LOGGER.info("Parameter applicationContext -> " + applicationContext);
		LOGGER.info("userId = "+userId);

		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		String userData = null;
		String token = null;

		try {
			token = getSSOToken();
		} catch (IOException ioExp) {
			LOGGER.error("Unable to get SSO Token" + ioExp.getMessage(),ioExp);
		}

		try {
			if (null == userId || userId.isEmpty()) {
				JSONObject jsonObject = new JSONObject();
				jsonObject.put("errorCode", "NOT_FOUND");
				jsonObject.put("message", "Provided external ID field does not exist or is  not accessible: " + userId);

				JSONArray jsonArray = new JSONArray();
				jsonArray.add(jsonObject);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info(GET_USER_TIME_LOG + elapsedTime);
				LOGGER.error("userId is null or empty");
				return Response.status(Response.Status.NOT_FOUND.getStatusCode()).entity(jsonArray).build();

			} else if (null != userId) {
				LOGGER.info(AUDIT_REQUESTING_USER.concat(AUDIT_TECHNICAL_USER).concat(AUDIT_IMPERSONATING_USER)
						.concat(AUDIT_API_ADMIN).concat(AUDIT_OPENAM_API).concat(AUDIT_OPENAM_GET_CALL).concat(userId)
						.concat(AUDIT_LOG_CLOSURE));
				LOGGER.info("Start: getUser() of OpenAMService for userId=" + userId);
				userData = productService.getUser(token, userId);
				LOGGER.info("End: getUser() of OpenAMService finished with userdata: " + ChinaIdmsUtil.printOpenAMInfo(userData));
			}
		} catch (Exception e) {
			LOGGER.error("Error in getUser() openam service->" + e.getMessage(),e);
			LOGGER.error(e.toString());
			if (userData == null) {
				JSONObject jsonObject = new JSONObject();
				jsonObject.put("errorCode", "NOT_FOUND");
				jsonObject.put("message", "Provided external ID field does not exist or is  not accessible: " + userId);

				JSONArray jsonArray = new JSONArray();
				jsonArray.add(jsonObject);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info(GET_USER_TIME_LOG + elapsedTime);
				return Response.status(Response.Status.NOT_FOUND.getStatusCode()).entity(jsonArray).build();
			}
		}
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		// getting the context
		DocumentContext productDocCtx = JsonPath.using(conf).parse(userData);
		String context = null != productDocCtx.read(JsonConstants.EMPLOYEE_TYPE)
				? getValue(productDocCtx.read(JsonConstants.EMPLOYEE_TYPE).toString()) : null;
		LOGGER.info("user context=" + context);

		OpenAMGetUserHomeResponse userHomeResponse = new OpenAMGetUserHomeResponse();
		OpenAMGetUserWorkResponse userWorkResponse = new OpenAMGetUserWorkResponse();
		DocumentContext userProductDocCtx = JsonPath.using(conf).parse(userData);
		Attributes attributes = new Attributes();
		userHomeResponse.setAttributes(attributes);
		userWorkResponse.setAttributes(attributes);
		
		if ("@home".equalsIgnoreCase(applicationContext) || "home".equalsIgnoreCase(applicationContext)) {
			return returnGetUserHomeContext(startTime, userHomeResponse, userProductDocCtx);
		} else if ("@work".equalsIgnoreCase(applicationContext) || "work".equalsIgnoreCase(applicationContext) 
				|| "Both".equalsIgnoreCase(applicationContext) || "@Both".equalsIgnoreCase(applicationContext)) {
			valuesByOauthHomeWorkContext.parseValuesWorkContext(userWorkResponse, userProductDocCtx);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info(GET_USER_TIME_LOG + elapsedTime);
			return Response.status(Response.Status.OK.getStatusCode()).entity(userWorkResponse).build();
		} else if (null == applicationContext || "".equals(applicationContext)) {
			return returnGetUserHomeContext(startTime, userHomeResponse, userProductDocCtx);
		}
		elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		LOGGER.info(GET_USER_TIME_LOG + elapsedTime);
		return Response.status(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode()).build();
	}

	/**
	 * get user by oauth token method for IFW
	 * 
	 * @param userId
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public Response getUserByOauthToken(String userId) {
		LOGGER.info("Entered getUserByOauthToken() -> Start");
		LOGGER.info("Parameter userId -> " + userId);
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		String userData = null;
		// getting iPlanetDirectoryPro token from OpenAM
		String token;
		try {
			token = getSSOToken();
		} catch (IOException ioExp) {
			LOGGER.error("Unable to get SSO Token " + ioExp.getMessage(),ioExp);
			token = "";
		}

		try {
			if (null != userId) {
				LOGGER.info(AUDIT_REQUESTING_USER.concat(AUDIT_TECHNICAL_USER).concat(AUDIT_IMPERSONATING_USER)
						.concat(AUDIT_API_ADMIN).concat(AUDIT_OPENAM_API).concat(AUDIT_OPENAM_GET_CALL).concat(userId)
						.concat(AUDIT_LOG_CLOSURE));
				LOGGER.info("Start: getUser() of OpenAMService with userId:" + userId);
				userData = productService.getUser(token, userId);
				LOGGER.info("End: getUser() of OpenAMService finished with userdata: " + ChinaIdmsUtil.printOpenAMInfo(userData));
			}
		} catch (Exception e) {
			//LOGGER.error(e.toString());
			LOGGER.error("UserServiceImpl:getUserByOauthToken() ->" + e.getMessage(),e);
			if (userData == null) {
				JSONObject jsonObject = new JSONObject();
				jsonObject.put("errorCode", "NOT_FOUND");
				jsonObject.put("message", "Provided external ID field does not exist or is  not accessible: " + userId);

				JSONArray jsonArray = new JSONArray();
				jsonArray.add(jsonObject);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info(GET_USER_TIME_LOG + elapsedTime);
				// productService.sessionLogout(UserConstants.IPLANET_DIRECTORY_PRO+token,
				// "logout");
				return Response.status(Response.Status.NOT_FOUND.getStatusCode()).entity(jsonArray).build();
			}
		}
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		// getting the context
		DocumentContext productDocCtx = JsonPath.using(conf).parse(userData);
		String context = null != productDocCtx.read(JsonConstants.EMPLOYEE_TYPE)
				? getValue(productDocCtx.read(JsonConstants.EMPLOYEE_TYPE).toString()) : null;
		LOGGER.info("context=" + context);

		GetUserHomeByOauthResponse userHomeResponse = new GetUserHomeByOauthResponse();
		GetUserWorkByOauthResponse userWorkResponse = new GetUserWorkByOauthResponse();
		DocumentContext userProductDocCtx = JsonPath.using(conf).parse(userData);
		if ("@home".equalsIgnoreCase(context) || "home".equalsIgnoreCase(context)) {
			return returnGetUserByOauthHomeContext(startTime, userHomeResponse, userProductDocCtx);
		} else if ("@work".equalsIgnoreCase(context) || "work".equalsIgnoreCase(context)
				|| "Both".equalsIgnoreCase(context) || "@Both".equalsIgnoreCase(context)) {
			valuesByOauthHomeWorkContext.parseValuesByOauthWorkContext(userWorkResponse, userProductDocCtx);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info(GET_USER_TIME_LOG + elapsedTime);
			return Response.status(Response.Status.OK.getStatusCode()).entity(userWorkResponse).build();
		} else if (null == context || "".equals(context)) {
			return returnGetUserByOauthHomeContext(startTime, userHomeResponse, userProductDocCtx);
		}
		elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		LOGGER.info(GET_USER_TIME_LOG + elapsedTime);
		//productService.sessionLogout(UserConstants.IPLANET_DIRECTORY_PRO+token, "logout");
		return Response.status(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode()).build();
	}

	private Response returnGetUserByOauthHomeContext(long startTime, GetUserHomeByOauthResponse userResponse,
			DocumentContext userProductDocCtx) {
		long elapsedTime;
		valuesByOauthHomeWorkContext.parseValuesByOauthHomeContext(userResponse, userProductDocCtx);
		elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		LOGGER.info(GET_USER_TIME_LOG + elapsedTime);
		return Response.status(Response.Status.OK.getStatusCode()).entity(userResponse).build();
	}

	private Response returnGetUserHomeContext(long startTime, OpenAMGetUserHomeResponse userHomeResponse,
			DocumentContext userProductDocCtx) {
		long elapsedTime;
		valuesByOauthHomeWorkContext.parseValuesHomeContext(userHomeResponse, userProductDocCtx);
		elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		LOGGER.info(GET_USER_TIME_LOG + elapsedTime);
		return Response.status(Response.Status.OK.getStatusCode()).entity(userHomeResponse).build();
	}

	private String getDelimeter() {
		return UserConstants.USER_DELIMETER;
	}

	public static String getValue(String key) {
		// LOGGER.info("Entered getValue() -> Start");
		// LOGGER.info("Parameter key -> " + key);
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

	public static String getValues(String key) {
		// LOGGER.info("Entered getValues() -> Start");
		// LOGGER.info("Parameter key -> " + key);
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

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.idms.service.UserServiceImpl#userRegistration(java.lang.String,
	 * java.lang.String, com.idms.model.CreateUserRequest)
	 */
	@Override
	public Response userRegistration(String clientId, String clientSecret, CreateUserRequest userRequest) {
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		DocumentContext productDocCtx = null;
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		CreateUserResponse sucessRespone;
		ErrorResponse errorResponse = new ErrorResponse();
		String loginIdentifier = null;
		String identifierType = null;
		ObjectMapper objMapper = null;
		String userName = null, userExists = null;
		String iPlanetDirectoryKey = null;
		boolean uimsAlreadyCreatedFlag = false, mobileRegFlag = false;
		Response userCreation = null, checkUserExist = null;
		String otpinOpendj = null, hexPinMobile = null, otpStatus = null;
		List<String> accssControlList=null;
		boolean maintenanceMode=false;
		try {
			objMapper = new ObjectMapper();

			LOGGER.info("Entered userRegistration() -> Start");
			LOGGER.info("Access Control List:"+maintenanceModeGlobal);
			LOGGER.info(
					"Parameter userRequest -> " + ChinaIdmsUtil.printData(objMapper.writeValueAsString(userRequest)));

			// Step 1:
			/**
			 * Check mandatory values and user type (home/work)
			 */

			try {
				if(null == userRequest){
					errorResponse.setStatus(errorStatus);
					errorResponse.setMessage("Request body is null or empty");
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
					LOGGER.error("Error is :: Request body is null or empty");
					return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
				}
				if(null == userRequest.getUserRecord().getIDMS_Registration_Source__c() || 
						userRequest.getUserRecord().getIDMS_Registration_Source__c().isEmpty()){
					errorResponse.setStatus(errorStatus);
					errorResponse.setMessage("Registration source is null or empty");
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
					LOGGER.error("Error is :: Registration source is null or empty");
					return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
				}
				if(maintenanceModeGlobal!=null)
					accssControlList = Arrays.asList(maintenanceModeGlobal.split(","));
				if(accssControlList!=null && accssControlList.size()>0 && !(accssControlList.contains("False"))){
					if(accssControlList.contains(UserConstants.MAINTENANCE_MODE_COMPLETE) || accssControlList.contains(UserConstants.MAINTENANCE_MODE_REGISTRATION) ){
						errorResponse.setStatus(errorStatus);
						errorResponse.setMessage(UserConstants.MAINTENANCE_MODE_MESSAGE);
						LOGGER.error("Error :: Maintenance mode in progress");
						maintenanceMode=true;
					}
					//Consider  exclusions for maintenance mode as below
					if(maintenanceMode){
						maintenanceMode = excludeMaintenanceMode(userRequest.getUserRecord().getIDMS_Registration_Source__c(), UserConstants.MAINTENANCE_MODE_REGISTRATION);
					}
					if(maintenanceMode){
						return Response.status(Response.Status.SERVICE_UNAVAILABLE).entity(errorResponse).build();
					}
				}
				//Here if maintenanceMode==true then return 503
				if (null != userRequest.getUserRecord().getMobilePhone()
						&& !userRequest.getUserRecord().getMobilePhone().isEmpty()) {
					userRequest.getUserRecord().setMobilePhone(
							ChinaIdmsUtil.mobileTransformation(userRequest.getUserRecord().getMobilePhone()));
				}
				
				mobileRegFlag = Boolean.parseBoolean(userRequest.getMobileRegFlag());

				if (mobileRegFlag) {
					String mobileStr = userRequest.getUserRecord().getMobilePhone();
					LOGGER.info("Start: getMobileOTPDetails() of OpenDjService for mobile=" + mobileStr);
					Response otpDetails = openDJService.getMobileOTPDetails(djUserName, djUserPwd, mobileStr);
					LOGGER.info("End: getMobileOTPDetails() of OpenDjService finished for mobile=" + mobileStr);
					LOGGER.info("Response code from OpenDJ for get call: " + otpDetails.getStatus());

					if (null != otpDetails && 200 == otpDetails.getStatus()) {
						Configuration confg = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
						DocumentContext productDocCtxt = JsonPath.using(confg)
								.parse(IOUtils.toString((InputStream) otpDetails.getEntity()));
						otpStatus = productDocCtxt.read("tokenStatus");
						otpinOpendj = productDocCtxt.read("otpToken");
					}

					if (null != otpStatus && otpStatus.equalsIgnoreCase(UserConstants.PIN_VERIFIED)) {
						LOGGER.info("Mobile verified. Registration process continue.");
					} else {
						errorResponse.setStatus(errorStatus);
						errorResponse.setMessage("Mobile not verified. Registration terminated.");
						elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
						LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
						LOGGER.error("Mobile not verified. Registration terminated.");
						return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
					}
				}

				if (null != userRequest.getUserRecord().getIDMS_Federated_ID__c()
						&& !userRequest.getUserRecord().getIDMS_Federated_ID__c().isEmpty()
						&& userRequest.getUserRecord().getIDMS_Federated_ID__c().startsWith("cn00")
						&& userRequest.getUserRecord().getIDMS_Registration_Source__c().equalsIgnoreCase("UIMS")) {
					errorResponse.setStatus(errorStatus);
					errorResponse.setMessage(
							"Registration from UIMS, federationID should not contain cn00. May be duplicate entry.");
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
					LOGGER.error(
							"Registration from UIMS, federationID should not contain cn00. May be duplicate entry.");
					//return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
					return handleUIMSError(Response.Status.BAD_REQUEST, "Registration from UIMS, federationID should not contain cn00. May be duplicate entry.");
				}
				if (null != userRequest.getUserRecord().getIDMS_Federated_ID__c()
						&& !userRequest.getUserRecord().getIDMS_Federated_ID__c().isEmpty()
						&& !userRequest.getUserRecord().getIDMS_Federated_ID__c().startsWith("cn00")
						&& !userRequest.getUserRecord().getIDMS_Registration_Source__c().equalsIgnoreCase("UIMS")) {
					errorResponse.setStatus(errorStatus);
					errorResponse.setMessage("Registration from non-UIMS, federationID must contain cn00.");
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
					LOGGER.error("Registration from non-UIMS, federationID must contain cn00.");
					return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
					
				}

				if (!mobileRegFlag) {
					// checkUserExist
					CheckUserExistsRequest checkRequest = new CheckUserExistsRequest();
					if(null != userRequest.getUserRecord().getEmail() && !userRequest.getUserRecord().getEmail().isEmpty()){
						checkRequest.setEmail(userRequest.getUserRecord().getEmail().trim());
					}
					if((null == userRequest.getUserRecord().getEmail() || userRequest.getUserRecord().getEmail().isEmpty())
							&& (null != userRequest.getUserRecord().getMobilePhone() && !userRequest.getUserRecord().getMobilePhone().isEmpty())){
						checkRequest.setMobile(userRequest.getUserRecord().getMobilePhone().trim());
					}

					LOGGER.info("checking bfo reg source");
					if (pickListValidator.validate(UserConstants.IDMS_BFO_profile,
							userRequest.getUserRecord().getIDMS_Registration_Source__c())) {
						LOGGER.info("Reg source belongs to BFO profile, setting WithGlobalUsers to false");
						checkRequest.setWithGlobalUsers("false");
					} else {
						LOGGER.info("Reg source belongs to non-BFO profile, setting WithGlobalUsers to true");
						checkRequest.setWithGlobalUsers("true");
					}
					if(null != userRequest.getUserRecord().getIDMS_Registration_Source__c() && !userRequest.getUserRecord().getIDMS_Registration_Source__c().isEmpty()){
						checkRequest.setApplicationName(userRequest.getUserRecord().getIDMS_Registration_Source__c().trim());
					}

					checkUserExist = idmsCheckUserExists(checkRequest);
					LOGGER.info("idmsCheckUserExists reponse ::" + objMapper.writeValueAsString(checkUserExist));
					org.json.simple.JSONObject checkUserJson = (org.json.simple.JSONObject) checkUserExist.getEntity();
					String messageUser = checkUserJson.get(UserConstants.MESSAGE_L).toString();
					if (!messageUser.equalsIgnoreCase(UserConstants.FALSE)) {
						if (200 != checkUserExist.getStatus()) {
							errorResponse.setMessage(messageUser);
							errorResponse.setStatus("Error");
							elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
							LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
							LOGGER.error("Error while idmsCheckUserExists is " + errorResponse.getMessage());
							if(UserConstants.UIMS
									.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Registration_Source__c())){
								return handleUIMSError(Response.Status.fromStatusCode(checkUserExist.getStatus()),messageUser);
							}
							return Response.status(checkUserExist.getStatus()).entity(errorResponse).build();
						}
						if (200 == checkUserExist.getStatus()) {
							errorResponse.setStatus("Error");
							errorResponse.setMessage(UserConstants.USER_EXISTS);
							elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
							LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
							LOGGER.error("User exists/registered in OpenAM");
							if(UserConstants.UIMS
									.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Registration_Source__c())){
								return handleUIMSError(Response.Status.CONFLICT,UserConstants.USER_EXISTS);
							}
							return Response.status(Response.Status.CONFLICT).entity(errorResponse).build();
						}
					}
				}
				// MandatoryCheck for fields
				if (checkMandatoryFieldsFromRequest(userRequest.getUserRecord(), userResponse, true)) {
					errorResponse.setMessage(userResponse.getMessage());
					errorResponse.setStatus(userResponse.getStatus());
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
					LOGGER.error("Error while processing checkMandatoryFields is " + errorResponse.getMessage());
					if(UserConstants.UIMS
							.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Registration_Source__c())){
						return handleUIMSError(Response.Status.BAD_REQUEST,userResponse.getMessage());
					}
					return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
				}

				/**
				 * R4 Release changes
				 */

				if (null != userRequest.getUIFlag() && UserConstants.TRUE.equalsIgnoreCase(userRequest.getUIFlag())) {
					if (((null != userRequest.getPassword() && !userRequest.getPassword().isEmpty()))
							&& !checkPasswordPolicy(userRequest.getPassword(),
									userRequest.getUserRecord().getFirstName(),
									userRequest.getUserRecord().getLastName(),
									userRequest.getUserRecord().getEmail(),
									userRequest.getUserRecord().getMobilePhone())) {
						errorResponse.setStatus(errorStatus);
						errorResponse.setMessage(UserConstants.PR_POLICY);
						elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
						LOGGER.info("Time taken by UserServiceImpl.userRegistration() : " + elapsedTime);
						LOGGER.error("Error while processing is " + errorResponse.getMessage());
						if(UserConstants.UIMS
								.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Registration_Source__c())){
							return handleUIMSError(Response.Status.BAD_REQUEST,UserConstants.PR_POLICY);
						}
						return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
					}
				} else if (((null == userRequest.getUIFlag()
						|| !UserConstants.TRUE.equalsIgnoreCase(userRequest.getUIFlag()))
						&& (!UserConstants.UIMS
								.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Registration_Source__c())))
						&& (null != userRequest.getPassword() && !userRequest.getPassword().isEmpty())) {
					errorResponse.setStatus(errorStatus);
					errorResponse.setMessage(UserConstants.PASSWORD_WITH_USER_REG_BLCOKED);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by UserServiceImpl.userRegistration() : " + elapsedTime);
					LOGGER.error("Error while processing is " + errorResponse.getMessage());
					return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
				}
			} catch (Exception e) {
				LOGGER.error("UserServiceImpl:userRegistration ->" + e.getMessage() , e);
				errorResponse.setMessage(UserConstants.ATTRIBUTE_NOT_AVAILABELE);
				errorResponse.setStatus(userResponse.getStatus());
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
				LOGGER.error("Error while processing is "+errorResponse.getMessage(),e);
				if(UserConstants.UIMS
						.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Registration_Source__c())){
					return handleUIMSError(Response.Status.BAD_REQUEST,UserConstants.ATTRIBUTE_NOT_AVAILABELE);
				}
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
			}

			LOGGER.info("CheckMandatoryFieldsFromRequest Completed ");

			if (null != userRequest.getUserRecord().getIDMS_Registration_Source__c() && UserConstants.UIMS
					.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Registration_Source__c())) {

				if (null == clientId || null == clientSecret) {
					errorResponse.setStatus(errorStatus);
					errorResponse.setMessage(UserConstants.UIMS_CLIENTID_SECRET);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by UserServiceImpl.updateUser() : " + elapsedTime);
					LOGGER.error("Error while processing is " + userResponse.getMessage());
					//return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
					return handleUIMSError(Response.Status.BAD_REQUEST,UserConstants.UIMS_CLIENTID_SECRET);
				}

				if ((null != clientId && !clientId.equalsIgnoreCase(uimsClientId))
						|| (null != clientSecret && !clientSecret.equalsIgnoreCase(uimsClientSecret))) {
					errorResponse.setStatus(errorStatus);
					errorResponse.setMessage(UserConstants.INVALID_UIMS_CREDENTIALS);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by UserServiceImpl.updateUser() : " + elapsedTime);
					LOGGER.error("Error while processing is " + userResponse.getMessage());
					//return Response.status(Response.Status.UNAUTHORIZED).entity(errorResponse).build();
					return handleUIMSError(Response.Status.UNAUTHORIZED,UserConstants.INVALID_UIMS_CREDENTIALS);
				}
			}
			/**
			 * Checking primary contact is coming from source map the property
			 * or if it is not coming from UIMS set it false otherwise true
			 * 
			 */

			if (null == userRequest.getUserRecord().getIDMSPrimaryContact__c()
					|| userRequest.getUserRecord().getIDMSPrimaryContact__c().isEmpty()) {

				if (UserConstants.UIMS.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Registration_Source__c())) {
					userRequest.getUserRecord().setIDMSPrimaryContact__c(UserConstants.FALSE);
				} else {
					userRequest.getUserRecord().setIDMSPrimaryContact__c(UserConstants.TRUE);
				}

			}

			if (null != userRequest.getUserRecord().getAdminCompanyFederatedId()
					&& !userRequest.getUserRecord().getAdminCompanyFederatedId().isEmpty()) {
				userRequest.getUserRecord().setIDMSPrimaryContact__c(UserConstants.FALSE);
			}

			/**
			 * Checking companyFederation is not passing generating new one and
			 * adding
			 */

			if ((UserConstants.USER_CONTEXT_WORK.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_User_Context__c())
					|| UserConstants.USER_CONTEXT_WORK_1
							.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_User_Context__c()))
					&& (null == userRequest.getUserRecord().getIDMSCompanyFederationIdentifier__c()
							|| userRequest.getUserRecord().getIDMSCompanyFederationIdentifier__c().isEmpty())
					&& !(null == userRequest.getUserRecord().getCompanyName()
							|| userRequest.getUserRecord().getCompanyName().isEmpty())
					&& (!UserConstants.UIMS
							.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Registration_Source__c()))) {

				userRequest.getUserRecord().setIDMSCompanyFederationIdentifier__c(ChinaIdmsUtil.generateFedId());

			}

			// Step 2:

			OpenAmUserRequest openAmReq = mapper.map(userRequest, OpenAmUserRequest.class);

			/**
			 * Setting registration
			 */

			if (null != userRequest.getAttributes() && userRequest.getAttributes().size() > 0) {
				openAmReq.getInput().getUser()
						.setRegistrationAttributes__c(objMapper.writeValueAsString(userRequest.getAttributes()));

				List<RegistrationAttributes> attributeList = userRequest.getAttributes();
				for (int i = 0; i < attributeList.size(); i++) {
					String KeyName = attributeList.get(i).getKeyName();
					String KeyValue = attributeList.get(i).getKeyValue();
					LOGGER.info("KeyName = " + KeyName + " and KeyValue =" + KeyValue);
					if (KeyName.equalsIgnoreCase("alink") && null != KeyValue && !KeyValue.isEmpty()) {
						LOGGER.info("inside alink block");
						openAmReq.getInput().getUser().setAlink(KeyValue);
					}
					if (KeyName.equalsIgnoreCase("publicVisibility") && null != KeyValue && !KeyValue.isEmpty()) {
						LOGGER.info("inside publicVisibility block");
						openAmReq.getInput().getUser().setPublicVisibility(KeyValue);
					}
				}
			}

			/**
			 * call /json/authenticate to iplanetDirectoryPro token for admin
			 */
			/*
			 * LOGGER.info(AUDIT_REQUESTING_USER + AUDIT_TECHNICAL_USER +
			 * AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN + AUDIT_OPENAM_API +
			 * AUDIT_OPENAM_AUTHENTICATE_CALL + AUDIT_LOG_CLOSURE);
			 */
			// String response =
			// getSSOToken();//productService.authenticateUser(adminUserName,
			// adminPassword, UserConstants.REALM);

			// LOGGER.info("Admin Token Generated SuccessFully {} ");

			/**
			 * check email and mobile phone for login identifier
			 */
			if ((null != userRequest.getUserRecord().getEmail() && !userRequest.getUserRecord().getEmail().isEmpty())
					&& (null != userRequest.getUserRecord().getMobilePhone()
							&& !userRequest.getUserRecord().getMobilePhone().isEmpty())) {

				openAmReq.getInput().getUser().setIdmsuid(userRequest.getUserRecord().getEmail());
				loginIdentifier = userRequest.getUserRecord().getEmail();
				identifierType = UserConstants.EMAIL;
			} else if ((null != userRequest.getUserRecord().getEmail())
					&& (!userRequest.getUserRecord().getEmail().isEmpty())) {

				openAmReq.getInput().getUser().setIdmsuid(userRequest.getUserRecord().getEmail());
				loginIdentifier = userRequest.getUserRecord().getEmail();
				identifierType = UserConstants.EMAIL;
			} else if ((null != userRequest.getUserRecord().getMobilePhone())
					&& (!userRequest.getUserRecord().getMobilePhone().isEmpty())) {
				openAmReq.getInput().getUser()
						.setIdmsuid(userRequest.getUserRecord().getMobilePhone() + "bridge-fo.com");
				openAmReq.getInput().getUser().setMobile_reg(userRequest.getUserRecord().getMobilePhone());
				loginIdentifier = userRequest.getUserRecord().getMobilePhone();
				identifierType = UserConstants.MOBILE;
				if (null != userRequest.getUserRecord().getEmail()
						&& userRequest.getUserRecord().getEmail().isEmpty()) {
					userRequest.getUserRecord().setEmail(null);
				}
			}

			LOGGER.info(
					"LoginIdentifier Assigned,  identifierType -> " + identifierType + " ,value -> " + loginIdentifier);
			/**
			 * login identifier eq email or mobile call
			 * /se/users?_queryFilter=mail eq 'email value'
			 */
			// productDocCtx = JsonPath.using(conf).parse(response);
			// iPlanetDirectoryKey =
			// response;//productDocCtx.read(JsonConstants.TOKEN_ID);

			try {
				iPlanetDirectoryKey = getSSOToken();
			} catch (IOException ioExp) {
				LOGGER.error("Unable to get SSO Token" + ioExp.getMessage(),ioExp);
				iPlanetDirectoryKey = "";
			}
			LOGGER.info(AUDIT_REQUESTING_USER + AUDIT_TECHNICAL_USER + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN
					+ AUDIT_OPENAM_API + AUDIT_OPENAM_USER_EXISTS_CALL + loginIdentifier + AUDIT_LOG_CLOSURE);

			LOGGER.info(
					"Start: checkUserExistsWithEmailMobile() of OpenAMService for loginIdentifier=" + loginIdentifier);
			if (null != openAmReq.getInput().getUser().getRegisterationSource()
					&& UserConstants.UIMS.equalsIgnoreCase(openAmReq.getInput().getUser().getRegisterationSource())) {

				userExists = productService.checkUserExistsWithEmailMobile(
						UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
						"federationID eq " + "\"" + openAmReq.getInput().getUser().getFederationID()
								+ "\" or loginid eq " + "\""
								+ URLEncoder.encode(URLDecoder.decode(loginIdentifier, "UTF-8"), "UTF-8")
								+ "\" or login_mobile eq " + "\""
								+ URLEncoder.encode(URLDecoder.decode(loginIdentifier, "UTF-8"), "UTF-8") + "\"");
			} else {
				userExists = productService.checkUserExistsWithEmailMobile(
						UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
						"loginid eq " + "\"" + URLEncoder.encode(URLDecoder.decode(loginIdentifier, "UTF-8"), "UTF-8")
								+ "\" or login_mobile eq " + "\""
								+ URLEncoder.encode(URLDecoder.decode(loginIdentifier, "UTF-8"), "UTF-8") + "\"");
			}
			LOGGER.info("End: checkUserExistsWithEmailMobile() of OpenAMService finished for loginIdentifier="
					+ loginIdentifier);
			productDocCtx = JsonPath.using(conf).parse(userExists);
			Integer resultCount = productDocCtx.read(JsonConstants.RESULT_COUNT);
			LOGGER.info("resultCount = " + resultCount);
			if (resultCount.intValue() > 0) {
				errorResponse.setStatus(errorStatus);
				errorResponse.setMessage(UserConstants.USER_EXISTS);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
				LOGGER.error("User exists/registered in OpenAM");
				if(UserConstants.UIMS
						.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Registration_Source__c())){
					handleUIMSError(Response.Status.CONFLICT, UserConstants.USER_EXISTS);
				}
				return Response.status(Response.Status.CONFLICT).entity(errorResponse).build();
			}

			// LOGGER.info("CheckUserExistsWithEmailMobile Success");
			/**
			 * check login identifier e-email or mobile already handled in step2
			 */

			// Step 3:
			/**
			 * Check password exits and assign to openAM
			 */
			if (null != userRequest.getPassword() && !userRequest.getPassword().isEmpty()) {
				openAmReq.getInput().getUser().setUserPassword(userRequest.getPassword());
				openAmReq.getInput().getUser()
						.setTmp_password(new String(Base64.encodeBase64(userRequest.getPassword().getBytes())));
			} else {
				if (null != userRequest.getUserRecord().getIDMS_Federated_ID__c() && !userRequest.getUserRecord()
						.getIDMS_Federated_ID__c().startsWith(UserConstants.SOCIAL_LOGIN_PREFIX)) {
					openAmReq.getInput().getUser().setUserPassword(generateRamdomPassWord());
				} else if (null == userRequest.getUserRecord().getIDMS_Federated_ID__c()) {
					openAmReq.getInput().getUser().setUserPassword(generateRamdomPassWord());
				}
			}

			// new logic
			/*
			 * LOGGER.info(
			 * "Start: checkUserExistsWithEmailMobile() for unverified user to delete and create new reg with same email/mobile"
			 * ); String userExistsInOpenam =
			 * productService.checkUserExistsWithEmailMobile(
			 * UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, "mail eq "
			 * + "\"" +
			 * URLEncoder.encode(URLDecoder.decode(loginIdentifier,"UTF-8"),
			 * "UTF-8") + "\" or mobile eq " + "\"" +
			 * URLEncoder.encode(URLDecoder.decode(loginIdentifier,"UTF-8"),
			 * "UTF-8") + "\""); LOGGER.info(
			 * "End: checkUserExistsWithEmailMobile() for unverified user to delete and create new reg with same email/mobile"
			 * ); productDocCtxCheck =
			 * JsonPath.using(conf).parse(userExistsInOpenam); Integer
			 * resultCountCheck =
			 * productDocCtxCheck.read(JsonConstants.RESULT_COUNT); LOGGER.info(
			 * "resultCountCheck for loginIdentifier="+loginIdentifier+" is:"
			 * +resultCountCheck);
			 */

			// delete records from OPENAM if application is PRM
			/*
			 * if ((resultCountCheck.intValue() > 0) && (null !=
			 * userRequest.getUserRecord().getIDMS_Registration_Source__c() &&
			 * pickListValidator.validate(UserConstants.APPLICATIONS,
			 * userRequest.getUserRecord().getIDMS_Registration_Source__c().
			 * toUpperCase()))) { for (int i = 0; i <
			 * resultCountCheck.intValue(); i++) { String
			 * isActivated=productDocCtxCheck.read("$.result["+i+
			 * "].isActivated[0]"); if (!Boolean.valueOf(isActivated)) {
			 * Response deleteResponse = productService.deleteUser(
			 * UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
			 * productDocCtxCheck.read("$.result[" +i+ "].username")); if
			 * (deleteResponse.getStatus() == 200) { LOGGER.info(
			 * "Deleted the old PRM entry from openam" +
			 * IOUtils.toString((InputStream) deleteResponse.getEntity())); }
			 * else { LOGGER.error(
			 * "Failed to delete the old PRM entry from openam" +
			 * IOUtils.toString((InputStream) deleteResponse.getEntity()) +
			 * " Status=" + deleteResponse.getStatus()); } } } }
			 */

			/*
			 * if ((resultCountCheck.intValue() > 0) && ((null ==
			 * userRequest.getUserRecord().getIDMS_Federated_ID__c() ||
			 * userRequest.getUserRecord().getIDMS_Federated_ID__c().isEmpty()))
			 * ) { //deleting already existing id in openam
			 * 
			 * String userIdFromOpenam =
			 * productDocCtxCheck.read("$.result[0].username"); Response
			 * deleteResponse =
			 * productService.deleteUser(UserConstants.CHINA_IDMS_TOKEN +
			 * iPlanetDirectoryKey, userIdFromOpenam);
			 * 
			 * if (deleteResponse.getStatus() == 200) { LOGGER.info(
			 * "Deleted the old entry from openam" +
			 * IOUtils.toString((InputStream) deleteResponse.getEntity())); }
			 * else { LOGGER.error("Failed to delete the old entry from openam"
			 * + IOUtils.toString((InputStream) deleteResponse.getEntity()) +
			 * " Status=" + deleteResponse.getStatus()); }
			 * 
			 * userName = userIdFromOpenam; uimsAlreadyCreatedFlag = true;
			 * 
			 * 
			 * } else {
			 */
			// Step 4:
			/**
			 * Generate Random login ID and map it to Open AM Username attribute
			 * Condition added for social login issue // (null ==
			 * userRequest.getUserRecord().getIDMS_Federated_ID__c()||
			 * userRequest.getUserRecord().getIDMS_Federated_ID__c().isEmpty()))
			 * {
			 * 
			 */

			if ((!UserConstants.UIMS.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Registration_Source__c()))
					&& (!pickListValidator.validate(UserConstants.APPLICATIONS,
							userRequest.getUserRecord().getIDMS_Registration_Source__c().toUpperCase()))
					&& (null == userRequest.getUserRecord().getIDMS_Federated_ID__c()
							|| userRequest.getUserRecord().getIDMS_Federated_ID__c().isEmpty())) {

				// new logic to generate fedId/userId
				// userName = UserConstants.UID_PREFIX +
				// UUID.randomUUID().toString();
				userName = ChinaIdmsUtil.generateFedId();
			} else {
				userName = userRequest.getUserRecord().getIDMS_Federated_ID__c();
			}

			// }
			LOGGER.info("loginId mail/mobile =" + loginIdentifier + " and userName =" + userName);
			openAmReq.getInput().getUser().setUsername(userName);
			/**
			 * Adding below line for R4 Release
			 */
			openAmReq.getInput().getUser().setFederationID(userName);

			/*
			 * openAmReq.getInput().getUser().setIdmsail_c("[]");
			 * openAmReq.getInput().getUser().setIdmsail_Applications_c("[]");
			 * openAmReq.getInput().getUser().setIdmsail_Features_c("[]");
			 * openAmReq.getInput().getUser().setIdmsail_Programs_c("[]");
			 */
			openAmReq.getInput().getUser().setCn(
					userRequest.getUserRecord().getFirstName() + " " + userRequest.getUserRecord().getLastName());

			openAmReq.getInput().getUser()
					.setHotpEmailVerification(userRequest.getUserRecord().getEmail() + ":" + userName + ":"
							+ userRequest.getUserRecord().getIDMS_PreferredLanguage__c() + ":"
							+ userRequest.getUserRecord().getIDMS_Registration_Source__c() + ":"
							+ openAmReq.getInput().getUser().getCn());

			openAmReq.getInput().getUser()
					.setHotpMobileVerification(userRequest.getUserRecord().getMobilePhone() + ":" + userName + ":"
							+ userRequest.getUserRecord().getIDMS_PreferredLanguage__c() + ":"
							+ userRequest.getUserRecord().getIDMS_Registration_Source__c() + ":"
							+ openAmReq.getInput().getUser().getCn());

			// Step 5:
			/**
			 * call /json/se/selfservice/userRegistration
			 */

			// setting isInternal value to false
			openAmReq.getInput().getUser().setIDMSisInternal__c("FALSE");
			openAmReq.getInput().getUser().setEmailcount("0");

			if (null != openAmReq.getInput().getUser().getMail()
					&& openAmReq.getInput().getUser().getMail().isEmpty()) {
				openAmReq.getInput().getUser().setMail(null);
			}

			String json = objMapper.writeValueAsString(openAmReq);
			json = json.replace("\"\"", "[]");
			LOGGER.info("Open AM  user  Request ------------->" + ChinaIdmsUtil.printOpenAMInfo(json));
			LOGGER.info(AUDIT_REQUESTING_USER + AUDIT_TECHNICAL_USER + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN
					+ AUDIT_OPENAM_API + AUDIT_OPENAM_USER_REGISTRATION_CALL + userAction + AUDIT_LOG_CLOSURE);

			/**
			 * The below or condition added for social login scenario for update
			 * user
			 * 
			 */

			if ((!pickListValidator.validate(UserConstants.IDMS_BFO_profile,
					userRequest.getUserRecord().getIDMS_Registration_Source__c()))
					&& ((pickListValidator.validate(UserConstants.APPLICATIONS,
							userRequest.getUserRecord().getIDMS_Registration_Source__c().toUpperCase()))
							|| ((null != userRequest.getUserRecord().getIDMS_Federated_ID__c()
									&& !userRequest.getUserRecord().getIDMS_Federated_ID__c().isEmpty())
									&& !UserConstants.UIMS.equalsIgnoreCase(
											userRequest.getUserRecord().getIDMS_Registration_Source__c())))) {
				// openAmReq.getInput().getUser().setUsername(null);
				json = objMapper.writeValueAsString(openAmReq.getInput().getUser());
				json = json.replace("\"\"", "[]");
				LOGGER.info("productService.userRegistration :  Request -> " + ChinaIdmsUtil.printOpenAMInfo(json));
				LOGGER.info("Start: calling updateUser() of OpenAMService...userName=" + userName);
				productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userName, json);
				LOGGER.info("End: updateUser() of OpenAMService finished for userName: " + userName);

			} else {
				LOGGER.info("productService.userRegistration :  Request -> " + ChinaIdmsUtil.printOpenAMInfo(json));
				LOGGER.info("Start: calling userRegistration() of OpenAMService...userAction=" + userAction);
				userCreation = productService.userRegistration(iPlanetDirectoryKey, userAction, json);
				LOGGER.info("End: userRegistration() of OpenAMService finished with status code: "
						+ userCreation.getStatus());
				// return productDocCtx;
				if (userCreation.getStatus() != 200) {
					// productService.sessionLogout(UserConstants.IPLANET_DIRECTORY_PRO+iPlanetDirectoryKey,
					// "logout");
					LOGGER.error("Exception while Registering User in OpenAM "
							+ IOUtils.toString((InputStream) userCreation.getEntity()));
					throw new Exception("Exception while Registering User in Open "
							+ IOUtils.toString((InputStream) userCreation.getEntity()));
				}
				LOGGER.info("User Registered Succssfully in openAM:: -> "
						+ IOUtils.toString((InputStream) userCreation.getEntity()));
			}
			// log for user stats
			AsyncUtil.generateCSV(registrationCsvPath,
					new Date() + "," + userName + "," + userRequest.getUserRecord().getIDMS_User_Context__c() + ","
							+ userRequest.getUserRecord().getIDMS_Registration_Source__c());

			String version = "{" + "\"V_Old\": \"" + UserConstants.V_OLD + "\",\"V_New\": \"" + UserConstants.V_NEW
					+ "\"" + "}";
			// Adding v_old and v_new
			LOGGER.info("version -> " + version);
			LOGGER.info(
					"Start: calling updateUser() of openamservice with username=" + userName + " ,version =" + version);
			productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userName, version);
			LOGGER.info("End: updateUser() call of openamservice finished for username=" + userName + " ,version ="
					+ version);
			// Checking profile update and update login id

			if (null == openAmReq.getInput().getUser().getRegisterationSource()
					|| !UserConstants.UIMS.equalsIgnoreCase(openAmReq.getInput().getUser().getRegisterationSource())) {

				if (UserConstants.EMAIL.equalsIgnoreCase(identifierType)) {
					LOGGER.info("For Email users--");

					/*
					 * PRODUCT_JSON_STRING = sendOtp(UserConstants.HOTP_EMAIL,
					 * userName,
					 * openAmReq.getInput().getUser().getUserPassword(),
					 * UserConstants.CREATE_USER_SERVICE);
					 */

					// if Registration source is not PRM then send mail
					if (null != userRequest.getUserRecord().getIDMS_Registration_Source__c()
							&& (!pickListValidator.validate(UserConstants.IDMS_BFO_profile,
									userRequest.getUserRecord().getIDMS_Registration_Source__c()))) {

						LOGGER.info("Start: generateOtp() of SendEmail for non-PRM, userName:" + userName);
						String otp = sendEmail.generateOtp(userName);
						LOGGER.info("Start: sendOpenAmEmail() of SendEmail for non-PRM, userName:" + userName);
						sendEmail.sendOpenAmEmail(otp, EmailConstants.USERREGISTRATION_OPT_TYPE, userName,
								userRequest.getUserRecord().getIDMS_Registration_Source__c());
						LOGGER.info("End: sendOpenAmEmail() of SendEmail finished for non-PRM, userName:" + userName);
					} else if (null != userRequest.getUserRecord().getIDMS_Registration_Source__c()
							&& (pickListValidator.validate(UserConstants.IDMS_BFO_profile,
									userRequest.getUserRecord().getIDMS_Registration_Source__c()))) {

						// HashedToken field is to store the hashed pin which
						// comes from global IDMS
						LOGGER.info(
								"Start: storePRMOtp() of SendEmail for PRM to store the hashed pin which comes from global IDMS, userName:"
										+ userName);
						sendEmail.storePRMOtp(userName, userRequest.getUserRecord().getIdmsHashedToken());
						LOGGER.info("End: storePRMOtp() of SendEmail finsihed for PRM, userName:" + userName);
					}
					/**
					 * To update authId in openAM extended attribute
					 */
					/*
					 * if (null != PRODUCT_JSON_STRING &&
					 * !PRODUCT_JSON_STRING.isEmpty()) { LOGGER.info(
					 * "To update authId in openAM extended attribute :: updateUser -> "
					 * + PRODUCT_JSON_STRING); LOGGER.info(AUDIT_REQUESTING_USER
					 * + AUDIT_TECHNICAL_USER + AUDIT_IMPERSONATING_USER +
					 * AUDIT_API_ADMIN + AUDIT_OPENAM_API +
					 * AUDIT_OPENAM_UPDATE_CALL + userName + AUDIT_LOG_CLOSURE);
					 * LOGGER.info(
					 * "UserServiceImpl:userRegistration -> productService.updateUser :  Request -> "
					 * + PRODUCT_JSON_STRING);
					 * productService.updateUser(UserConstants.
					 * IPLANET_DIRECTORY_PRO + iPlanetDirectoryKey, userName,
					 * PRODUCT_JSON_STRING);
					 * 
					 * }
					 */

				} else if (identifierType.equalsIgnoreCase(UserConstants.MOBILE)) {
					LOGGER.info("For Mobile users--");

					/**
					 * we need check when we are working for mobile scenario
					 */

					if (!mobileRegFlag) {
						LOGGER.info("Start: generateOtp() for mobile, userName:" + userName);
						String otp = sendEmail.generateOtp(userName);
						LOGGER.info("Start: sendSMSMessage() for mobile userName:" + userName);
						sendEmail.sendSMSNewGateway(otp, EmailConstants.USERREGISTRATION_OPT_TYPE, userName,
								userRequest.getUserRecord().getIDMS_Registration_Source__c());
						LOGGER.info("End: sendSMSMessage() finished for  mobile userName:" + userName);
						if(Boolean.valueOf(sendOTPOverEmail)){
							LOGGER.info("Start: sendOpenAmMobileEmail() for mobile userName:" + userName);
							sendEmail.sendOpenAmMobileEmail(otp, EmailConstants.USERREGISTRATION_OPT_TYPE, userName,
									userRequest.getUserRecord().getIDMS_Profile_update_source__c());
							LOGGER.info("End: sendOpenAmMobileEmail() finsihed for  mobile userName:" + userName);
							}
						else{
							LOGGER.info("Send Mobile OTP over Email() for mobile userName:" + userName +" disallowed");
							/**LOGGER.info("Start: sendOpenAmMobileEmail() for mobile userName:" + userName);
							sendEmail.sendOpenAmMobileEmail(otp, EmailConstants.USERREGISTRATION_OPT_TYPE, userName,
									userRequest.getUserRecord().getIDMS_Profile_update_source__c());
							LOGGER.info("End: sendOpenAmMobileEmail() finsihed for  mobile userName:" + userName);*/
							
						}
					}
					if (mobileRegFlag) {
						if (otpStatus.equalsIgnoreCase(UserConstants.PIN_VERIFIED)) {
							hexPinMobile = ChinaIdmsUtil.generateHashValue(otpinOpendj);
							LocalDateTime currentDatenTime = LocalDateTime.now();
							long currentDatenTimeInMillisecs = currentDatenTime.atZone(ZoneId.systemDefault())
									.toInstant().toEpochMilli();

							hexPinMobile = hexPinMobile + ":" + currentDatenTimeInMillisecs;
							String product_pin_string = "{" + "\"authId\": \"" + hexPinMobile + "\"}";
							// update hashkey in openAM.
							LOGGER.info(
									"Start: updateUser() of openamservice to update hashkey for userId:" + userName);
							productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userName,
									product_pin_string);
							LOGGER.info("End: updateUser() of openamservice to update hashkey finished for userId:"
									+ userName);
						}
						SendOTPRequest sendOTPRequest = new SendOTPRequest();
						sendOTPRequest.setMobile(userRequest.getUserRecord().getMobilePhone());
						deleteMobile(sendOTPRequest);
					}
				}
			}
		} catch (BadRequestException e) {
			errorResponse.setStatus(errorStatus);
			errorResponse.setMessage(UserConstants.ERROR_CREATE_USER);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
			LOGGER.error("BadRequestException while user Registration :: -> " + e.getMessage(),e);
			//productService.sessionLogout(UserConstants.IPLANET_DIRECTORY_PRO+iPlanetDirectoryKey, "logout");
			if(UserConstants.UIMS
					.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Registration_Source__c())){
				handleUIMSError(Response.Status.BAD_REQUEST,UserConstants.ERROR_CREATE_USER);
			}
			return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
		} catch (NotFoundException e) {
			errorResponse.setStatus(errorStatus);
			errorResponse.setMessage(UserConstants.ERROR_CREATE_USER);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
			LOGGER.error("NotFoundException while user Registration :: -> " + e.getMessage(),e);
			//productService.sessionLogout(UserConstants.IPLANET_DIRECTORY_PRO+iPlanetDirectoryKey, "logout");
			if(UserConstants.UIMS
					.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Registration_Source__c())){
				handleUIMSError(Response.Status.NOT_FOUND,UserConstants.ERROR_CREATE_USER);
			}
			return Response.status(Response.Status.NOT_FOUND).entity(errorResponse).build();
		} catch (Exception e) {
			errorResponse.setStatus(errorStatus);
			errorResponse.setMessage(UserConstants.ERROR_CREATE_USER);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
			LOGGER.error("Exception while user Registration :: -> " + e.getMessage(),e);
			//productService.sessionLogout(UserConstants.IPLANET_DIRECTORY_PRO+iPlanetDirectoryKey, "logout");
			if(UserConstants.UIMS
					.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Registration_Source__c())){
				handleUIMSError(Response.Status.INTERNAL_SERVER_ERROR,UserConstants.ERROR_CREATE_USER);
			}
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(errorResponse).build();
		}
		userRequest.getUserRecord().setIDMS_Federated_ID__c(userName);
		LOGGER.info("!uimsAlreadyCreatedFlag Value is -> " + !uimsAlreadyCreatedFlag);
		if (!uimsAlreadyCreatedFlag && null != userRequest.getUserRecord().getIDMS_Registration_Source__c()
				&& !UserConstants.UIMS.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Registration_Source__c())) {
			try {
				LOGGER.info("Now ready to create UIMS users, userRequest=" + ChinaIdmsUtil.printData(objMapper.writeValueAsString(userRequest)));
			} catch (JsonProcessingException e) {
				LOGGER.error("JsonProcessingException = "+e.getMessage(),e);
			}

			Thread thread = new Thread(new Runnable() {
				public void run() {
					LOGGER.info("Start: In thread, UIMS executeCreateUserAndCompany()");
					executeCreateUserAndCompany(userRequest);
					LOGGER.info("End: In thread, UIMS executeCreateUserAndCompany()");
				}
			});

			thread.start();

		}
		sucessRespone = new CreateUserResponse();
		sucessRespone.setStatus(successStatus);
		sucessRespone.setMessage(UserConstants.CREATE_USER_SUCCESS_MESSAGE);
		Attributes attributes = new Attributes();
		attributes.setType("User");
		userRequest.getUserRecord().setAttributes(attributes);
		userRequest.getUserRecord().setId(userName);
		userRequest.getUserRecord().setDefaultCurrencyIsoCode("CNY");
		IDMSUserResponse idmsResponse = mapper.map(userRequest, IDMSUserResponse.class);
		idmsResponse.setActive(userRequest.getUserRecord().isActive());
		sucessRespone.setIDMSUserRecord(idmsResponse);
		elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		LOGGER.info(UserConstants.USER_REGISTRATION_TIME_LOG + elapsedTime);
		if(UserConstants.UIMS
				.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Registration_Source__c())){
			UIMSResponse response= new UIMSResponse();
			response.setHasErrors("false");
			response.setStatus("Success");
			UIMSStatusInfo userResponse=new UIMSStatusInfo();
			userResponse.setStatusCode(String.valueOf(Response.Status.OK.getStatusCode()));
			userResponse.setMessage(UserConstants.CREATE_USER_SUCCESS_MESSAGE);
			response.setResults(userResponse);
			LOGGER.info("User Registration -> : UIMS Response -> " + response);
			return Response.status(Response.Status.OK).entity(response).build();
		}
		return Response.status(Response.Status.OK).entity(sucessRespone).build();
	}

	private boolean excludeMaintenanceMode(String sourceApp, String action)
			throws Exception {
		DocumentContext productDJData;
		String exclusionList;
		boolean maintenanceMode=false;
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		try{
		Response applicationDetails = openDJService.getUser(djUserName, djUserPwd, sourceApp);
		productDJData = JsonPath.using(conf).parse(IOUtils.toString((InputStream) applicationDetails.getEntity()));
		if (null != applicationDetails && 200 == applicationDetails.getStatus()) {
			String maintenanceModeOpenDJ = productDJData.read("_maintenance_mode_exclusion");
			LOGGER.info("userRegistration maintenance_mode:"+maintenanceModeOpenDJ);
			if(null != maintenanceModeOpenDJ && !maintenanceModeOpenDJ.isEmpty()){
				exclusionList = maintenanceModeOpenDJ;
				List<String> exclusionAppList = Arrays.asList(exclusionList.split(","));
				if(action.equalsIgnoreCase(UserConstants.MAINTENANCE_MODE_REGISTRATION)){
					if(exclusionAppList.contains(UserConstants.MAINTENANCE_MODE_REGISTRATION)){
						maintenanceMode=false;
					}
					else{
						maintenanceMode=true;	
					}
				}
				else if(action.equalsIgnoreCase(UserConstants.MAINTENANCE_MODE_PROFILE_UPDATE)){
					if(exclusionAppList.contains(UserConstants.MAINTENANCE_MODE_PROFILE_UPDATE)){
						maintenanceMode=false;
					}
					else{
						maintenanceMode=true;	
					}
				}
				else if(action.equalsIgnoreCase(UserConstants.MAINTENANCE_MODE_AIL_UPDATE)){
					if(exclusionAppList.contains(UserConstants.MAINTENANCE_MODE_AIL_UPDATE)){
						maintenanceMode=false;
					}
					else{
						maintenanceMode=true;	
					}
				}
				else if(action.equalsIgnoreCase(UserConstants.MAINTENANCE_MODE_LOGIN)){
					if(exclusionAppList.contains(UserConstants.MAINTENANCE_MODE_LOGIN)){
						maintenanceMode=false;
					}
					else{
						maintenanceMode=true;	
					}
				}
			}
			else{
				maintenanceMode=true;
			}
			//maintenanceMode=false;
		}
		if (null != applicationDetails && 200 != applicationDetails.getStatus()) {//revisit here
			maintenanceMode=true;
		}
	}
	catch(Exception exception){
		LOGGER.error("Error in maintenance mode exclusion"+exception);
	}
		return maintenanceMode;
	}

	/**
	 * This get user method is required for UI response
	 * 
	 * @param token
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public Response getUserbyTokenUI(String token, String appName) {
		LOGGER.info("Entered getUserbyTokenUI() -> Start");
		LOGGER.info("Parameter token -> " + token);
		LOGGER.info("Parameter appName -> " + appName);

		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		Response userResponse = null, applicationDetails = null;
		String applicationContext = null;
		try {
			if (null != token) {

				LOGGER.info(AUDIT_REQUESTING_USER + AUDIT_TECHNICAL_USER + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN
						+ AUDIT_OPENAM_API + AUDIT_OPENAM_USER_INFO_CALL + "/se" + AUDIT_LOG_CLOSURE);
				LOGGER.info("Start: getUserInfoByAccessToken() of OpenAMTokenService");
				String userInfoByAccessToken = openAMTokenService.getUserInfoByAccessToken(token, "/se");
				LOGGER.info("End: getUserInfoByAccessToken() of OpenAMTokenService finished");
				LOGGER.info("Accesstoken from the API call: " + userInfoByAccessToken);

				Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
				DocumentContext productDocCtx = JsonPath.using(conf).parse(userInfoByAccessToken);
				String userId = productDocCtx.read("$.sub");
				String employeeType = productDocCtx.read("$.employeeType");
				LOGGER.info("User employeeType = "+employeeType);
				
				if(null == employeeType || employeeType.isEmpty() 
						|| !(employeeType.equalsIgnoreCase(UserConstants.USER_CONTEXT_HOME)
						|| employeeType.equalsIgnoreCase(UserConstants.USER_CONTEXT_HOME_1) 
						|| employeeType.equalsIgnoreCase(UserConstants.USER_CONTEXT_WORK)
						|| employeeType.equalsIgnoreCase(UserConstants.USER_CONTEXT_WORK_1))){
					LOGGER.info("Start: getAppInfo() of OpenDjService for appName="+appName);
					applicationDetails = openDJService.getAppInfo(djUserName, djUserPwd,"_id eq " + "\"" + appName + "\"");
					LOGGER.info("End: getAppInfo() of OpenDjService for appName="+appName);
					LOGGER.info("Response code from OpenDJ for getAppInfo() call: " + applicationDetails.getStatus());
					
					if (null != applicationDetails && 200 == applicationDetails.getStatus()) {
						productDocCtx = JsonPath.using(conf).parse(IOUtils.toString((InputStream) applicationDetails.getEntity()));
						applicationContext = productDocCtx.read(JsonConstants.APP_CONTEXT);
						LOGGER.info("applicationContext=" + applicationContext);
					}
					userResponse = getUserWhenContextIsNull(applicationContext,userId);					
				} else {
					userResponse = getUser(userId);
				}
				
				LOGGER.info("User details derived from access token: " + userId);
			}
		} catch (NotAuthorizedException e) {
			// LOGGER.debug("Unauthorized!");
			JSONObject jsonObject = new JSONObject();
			jsonObject.put("errorCode", "Unauthorized");
			jsonObject.put("message", "Provided external ID field does not exist or is  not accessible ");

			JSONArray jsonArray = new JSONArray();
			jsonArray.add(jsonObject);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info(GET_USER_BY_TOKEN_TIME_LOG + elapsedTime);
			LOGGER.error("NotAuthorizedException in getUserbyTokenUI() -> "+e.getMessage(),e);
			return Response.status(Response.Status.NOT_FOUND.getStatusCode()).entity(jsonArray).build();
		} catch (IOException e) {
			JSONObject jsonObject = new JSONObject();
			jsonObject.put("errorCode", "Parsing Error");
			jsonObject.put("message", "System is unable to parse user details.");

			JSONArray jsonArray = new JSONArray();
			jsonArray.add(jsonObject);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info(GET_USER_BY_TOKEN_TIME_LOG + elapsedTime);
			LOGGER.error("IOException in getUserbyTokenUI() -> "+e.getMessage(),e);
			return Response.status(Response.Status.SERVICE_UNAVAILABLE.getStatusCode()).entity(jsonArray).build();
		}
		elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		LOGGER.info(GET_USER_BY_TOKEN_TIME_LOG + elapsedTime);
		return userResponse;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.idms.service.UserServiceImpl#getUserbyToken(java.lang.String)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public Response getUserbyToken(String token) {
		LOGGER.info("Entered getUserbyToken() -> Start");
		LOGGER.info("Parameter token -> "+token);

		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		Response userResponse = null;
		try {
			if (null != token) {

				/*
				 * LOGGER.info(AUDIT_REQUESTING_USER + AUDIT_TECHNICAL_USER +
				 * AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN + AUDIT_OPENAM_API
				 * + AUDIT_OPENAM_USER_INFO_CALL + "/se" + AUDIT_LOG_CLOSURE);
				 */
				// LOGGER.info("Start: getUserInfoByAccessToken() of
				// OpenAMTokenService");
				String userInfoByAccessToken = openAMTokenService.getUserInfoByAccessToken(token, "/se");
				// LOGGER.info("End: getUserInfoByAccessToken() of
				// OpenAMTokenService finished");
				// LOGGER.info("Accesstoken from the API call: " +
				// userInfoByAccessToken);

				Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
				DocumentContext productDocCtx = JsonPath.using(conf).parse(userInfoByAccessToken);
				String userId = productDocCtx.read("$.sub");
				userResponse = getUserByOauthToken(userId);
				// LOGGER.info("User details derived from access token: " +
				// userId);
			}
		} catch (NotAuthorizedException e) {
			// LOGGER.debug("InvalidSessionId!");
			JSONObject jsonObject = new JSONObject();
			jsonObject.put("errorCode", "INVALID_SESSION_ID");
			jsonObject.put("message", "Session expired or invalid");

			JSONArray jsonArray = new JSONArray();
			jsonArray.add(jsonObject);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			// LOGGER.info(GET_USER_BY_TOKEN_TIME_LOG + elapsedTime);
			// LOGGER.error("Error in getUserInfoByAccessToken() of
			// OpenAMTokenService:"+e.getMessage());
			return Response.status(Response.Status.UNAUTHORIZED.getStatusCode()).entity(jsonArray).build();
		} catch (Exception e) {
			// LOGGER.debug("Unauthorized!");
			JSONObject jsonObject = new JSONObject();
			jsonObject.put("errorCode", "Unauthorized");
			jsonObject.put("message", "Provided external ID field does not exist or is  not accessible ");

			JSONArray jsonArray = new JSONArray();
			jsonArray.add(jsonObject);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			// LOGGER.info(GET_USER_BY_TOKEN_TIME_LOG + elapsedTime);
			// LOGGER.error("Exception in getUserInfoByAccessToken() of
			// OpenAMTokenService->"+e.getMessage());
			return Response.status(Response.Status.NOT_FOUND.getStatusCode()).entity(jsonArray).build();

		}
		elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		// LOGGER.info(GET_USER_BY_TOKEN_TIME_LOG + elapsedTime);
		return userResponse;
	}

	private boolean checkMandatoryFieldsFromRequest(IFWUser userRequest, UserServiceResponse userResponse,
			boolean checkMandatoryFields) {
		LOGGER.info("Entered checkMandatoryFieldsFromRequest() -> Start");
		LOGGER.info("Parameter checkMandatoryFields -> " + checkMandatoryFields);

		userResponse.setStatus(errorStatus);

		String regOrUpdateSource = null;
		if (null != userRequest.getIDMS_Registration_Source__c()
				&& !userRequest.getIDMS_Registration_Source__c().isEmpty()) {
			regOrUpdateSource = userRequest.getIDMS_Registration_Source__c();
		} else if (null != userRequest.getIDMS_Profile_update_source__c()
				&& !userRequest.getIDMS_Profile_update_source__c().isEmpty()) {
			regOrUpdateSource = userRequest.getIDMS_Profile_update_source__c();
		}

		if (null != regOrUpdateSource && !regOrUpdateSource.isEmpty()
				&& ((pickListValidator.validate(UserConstants.APPLICATIONS, regOrUpdateSource))
						|| (pickListValidator.validate(UserConstants.IDMS_BFO_profile, regOrUpdateSource))
						|| (pickListValidator.validate(UserConstants.UPDATE_SOURCE, regOrUpdateSource)))) {
			LOGGER.info("Registration/update source is OK and continues..");
		} else {
			userResponse.setMessage(UserConstants.INVALID_REG_SOURCE);
			LOGGER.error(UserConstants.INVALID_REG_SOURCE);
			return true;
		}

		if (null != userRequest.getIDMS_Registration_Source__c() && ((pickListValidator
				.validate(UserConstants.APPLICATIONS, userRequest.getIDMS_Registration_Source__c().toUpperCase()))
				|| UserConstants.UIMS.equalsIgnoreCase(userRequest.getIDMS_Registration_Source__c()))) {
			if ((checkMandatoryFields) && (null == userRequest.getIDMS_Federated_ID__c()
					|| userRequest.getIDMS_Federated_ID__c().isEmpty())) {
				userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.FEDERATION_IDENTIFIER);
				LOGGER.error(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.FEDERATION_IDENTIFIER);
				return true;
			}
		}

		if ((null != userRequest.getEmail() && !userRequest.getEmail().isEmpty())
				&& (userRequest.getEmail().length() > 65)) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.EMAIL);
			LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.EMAIL);
			return true;
		}

		if ((null != userRequest.getEmail()) && (!userRequest.getEmail().isEmpty())) {
			if (!emailValidator.validate(userRequest.getEmail())) {
				userResponse.setMessage(UserConstants.EMAIL_VALIDATION + userRequest.getEmail());
				LOGGER.error(UserConstants.EMAIL_VALIDATION + userRequest.getEmail());
				return true;
			}

			if (userRequest.getEmail().contains(UserConstants.SE_MAIL)
					|| userRequest.getEmail().contains(UserConstants.NON_SE_MAIL)
					|| userRequest.getEmail().contains(UserConstants.SCHNEIDER_MAIL)
					|| userRequest.getEmail().contains(UserConstants.NON_SCHNEIDER_MAIL)) {
				userResponse.setMessage(UserConstants.EMAIL_VALIDATION_INTERNALUSER + userRequest.getEmail());
				LOGGER.error(UserConstants.EMAIL_VALIDATION_INTERNALUSER + userRequest.getEmail());
				return true;
			}
		}
		if (null != userRequest.getIDMS_Registration_Source__c()
				&& !UserConstants.UIMS.equalsIgnoreCase(userRequest.getIDMS_Registration_Source__c())) {
			if ((null != userRequest.getMobilePhone()) && (!userRequest.getMobilePhone().isEmpty())) {
				if (!legthValidator.validate(UserConstants.MOBILE_PHONE, userRequest.getMobilePhone())) {
					userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.MOBILE_PHONE);
					LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.MOBILE_PHONE);
					return true;
				}
			}
		}
		/**
		 * validate e-mail or mobile attribute values should be present
		 */
		if ((checkMandatoryFields) && (null == userRequest.getEmail() || userRequest.getEmail().isEmpty())
				&& (null == userRequest.getMobilePhone() || userRequest.getMobilePhone().isEmpty())) {
			userResponse.setMessage(
					UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.EMAIL + " OR " + UserConstants.MOBILE);
			LOGGER.error(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.EMAIL + " OR " + UserConstants.MOBILE);
			return true;
		}

		/**
		 * FirstName Mandatory validation and length check
		 */
		if ((checkMandatoryFields) && (null == userRequest.getFirstName() || userRequest.getFirstName().isEmpty())) {
			userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.FIRST_NAME);
			LOGGER.error(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.FIRST_NAME);
			return true;
		} else if ((null != userRequest.getFirstName() && !userRequest.getFirstName().isEmpty())
				&& (!legthValidator.validate(UserConstants.FIRST_NAME, userRequest.getFirstName()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.FIRST_NAME);
			LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.FIRST_NAME);
			return true;
		}

		/**
		 * LastName validation and length check
		 */
		if ((checkMandatoryFields) && (null == userRequest.getLastName() || userRequest.getLastName().isEmpty())) {
			userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.LAST_NAME);
			LOGGER.error(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.LAST_NAME);
			return true;
		} else if ((null != userRequest.getLastName() && !userRequest.getLastName().isEmpty())
				&& (!legthValidator.validate(UserConstants.LAST_NAME, userRequest.getLastName()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.LAST_NAME);
			LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.LAST_NAME);
			return true;
		}

		/**
		 * IDMS_Email_opt_in__c length check
		 */

		if (null != userRequest.getIDMS_Email_opt_in__c() && !userRequest.getIDMS_Email_opt_in__c().isEmpty()) {
			if (!legthValidator.validate(UserConstants.IDMS_Email_opt_in__c, userRequest.getIDMS_Email_opt_in__c())) {
				userResponse
						.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.EMLAIL_OPT_IN_DOC.toString());
				LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.EMLAIL_OPT_IN_DOC.toString());
				return true;

			} else if (!pickListValidator.validate(UserConstants.EMLAIL_OPT_IN,
					userRequest.getIDMS_Email_opt_in__c())) {
				userResponse.setMessage(UserConstants.INVALID_VALUE + UserConstants.EMLAIL_OPT_IN_DOC.toString());
				LOGGER.error(UserConstants.INVALID_VALUE + UserConstants.EMLAIL_OPT_IN_DOC.toString());
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
			LOGGER.error(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.IDMS_USER_CONTEXT_C.toString());
			return true;
		} else if (null != userRequest.getIDMS_User_Context__c() && !userRequest.getIDMS_User_Context__c().isEmpty()) {
			if (!legthValidator.validate(UserConstants.IDMS_USER_CONTEXT_C, userRequest.getIDMS_User_Context__c())) {
				userResponse.setMessage(
						UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.IDMS_USER_CONTEXT_C.toString());
				LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.IDMS_USER_CONTEXT_C.toString());
				return true;

			} else if (!pickListValidator.validate(UserConstants.IDMS_USER_CONTEXT_C,
					userRequest.getIDMS_User_Context__c())) {
				userResponse.setMessage(UserConstants.INVALID_VALUE + UserConstants.IDMS_USER_CONTEXT_C.toString());
				LOGGER.error(UserConstants.INVALID_VALUE + UserConstants.IDMS_USER_CONTEXT_C.toString());
				return true;
			}
		}

		/**
		 * Country validation and length check Mandatory
		 */

		if ((checkMandatoryFields) && (null == userRequest.getCountry() || userRequest.getCountry().isEmpty())) {
			userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.COUNTRY);
			LOGGER.error(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.COUNTRY);
			return true;
		} else if ((null != userRequest.getCountry() && !userRequest.getCountry().isEmpty())) {
			if (!legthValidator.validate(UserConstants.COUNTRY, userRequest.getCountry())) {
				userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.COUNTRY);
				LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.COUNTRY);
				return true;

			} else if (!pickListValidator.validate(UserConstants.COUNTRY, userRequest.getCountry())) {
				userResponse.setMessage(UserConstants.INVALID_VALUE + UserConstants.COUNTRY);
				LOGGER.error(UserConstants.INVALID_VALUE + UserConstants.COUNTRY);
				return true;
			}

		}

		/**
		 * IDMS_Registration_Source__c validation and length check Mandatory
		 */

		if ((checkMandatoryFields) && (null == userRequest.getIDMS_Registration_Source__c()
				|| userRequest.getIDMS_Registration_Source__c().isEmpty())) {
			userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.IDMS_REGISTRATION_SOURCE_C);
			LOGGER.error(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.IDMS_REGISTRATION_SOURCE_C);
			return true;
		} else if ((checkMandatoryFields)
				&& (null != userRequest.getIDMS_Registration_Source__c()
						&& !userRequest.getIDMS_Registration_Source__c().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_REGISTRATION_SOURCE_C,
						userRequest.getIDMS_Registration_Source__c()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.IDMS_REGISTRATION_SOURCE_C);
			LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.IDMS_REGISTRATION_SOURCE_C);
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
			LOGGER.error(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.PREFERRED_LANGUAGE);
			return true;
		} else if ((null != userRequest.getIDMS_PreferredLanguage__c()
				&& !userRequest.getIDMS_PreferredLanguage__c().isEmpty())
				&& !pickListValidator.validate(UserConstants.PREFERRED_LANGUAGE,
						userRequest.getIDMS_PreferredLanguage__c().toLowerCase())) {
			userResponse.setMessage(UserConstants.INVALID_VALUE_IDMS + UserConstants.PREFERRED_LANGUAGE);
			LOGGER.error(UserConstants.INVALID_VALUE_IDMS + UserConstants.PREFERRED_LANGUAGE);
			return true;
		}

		if ((UserConstants.UIMS.equalsIgnoreCase(userRequest.getIDMS_PreferredLanguage__c()))
				&& (null != userRequest.getIDMS_PreferredLanguage__c()
						&& !userRequest.getIDMS_PreferredLanguage__c().isEmpty())) {
			if (!legthValidator.validate(UserConstants.PREFERRED_LANGUAGE,
					userRequest.getIDMS_PreferredLanguage__c())) {
				userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.PREFERRED_LANGUAGE);
				LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.PREFERRED_LANGUAGE);
				return true;

			}
			if (!pickListValidator.validate(UserConstants.PREFERRED_LANGUAGE,
					userRequest.getIDMS_PreferredLanguage__c())) {
				userResponse.setMessage(UserConstants.INVALID_VALUE_IDMS + UserConstants.PREFERRED_LANGUAGE);
				LOGGER.error(UserConstants.INVALID_VALUE_IDMS + UserConstants.PREFERRED_LANGUAGE);
				return true;
			}
		}

		/**
		 * DefaultCurrencyIsoCode validation and length check Mandatory
		 */
		if ((null != userRequest.getDefaultCurrencyIsoCode() && !userRequest.getDefaultCurrencyIsoCode().isEmpty())) {
			if (!legthValidator.validate(UserConstants.CURRENCY, userRequest.getDefaultCurrencyIsoCode())) {
				userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.CURRENCY);
				LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.CURRENCY);
				return true;

			} else if (!pickListValidator.validate(UserConstants.CURRENCY, userRequest.getDefaultCurrencyIsoCode())) {
				userResponse.setMessage(UserConstants.INVALID_VALUE + UserConstants.CURRENCY);
				LOGGER.error(UserConstants.INVALID_VALUE + UserConstants.CURRENCY);
				return true;
			}
		}

		/**
		 * Length Validation check :: Street
		 */

		if ((null != userRequest.getStreet() && !userRequest.getStreet().isEmpty())
				&& (!legthValidator.validate(UserConstants.STREET, userRequest.getStreet()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.STREET);
			LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.STREET);
			return true;
		}

		/**
		 * Length Validation check :: City
		 */
		if ((null != userRequest.getCity() && !userRequest.getCity().isEmpty())
				&& (!legthValidator.validate(UserConstants.CITY, userRequest.getCity()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.CITY);
			LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.CITY);
			return true;
		}

		/**
		 * Length Validation check :: PostalCode
		 */

		if ((null != userRequest.getPostalCode() && !userRequest.getPostalCode().isEmpty())
				&& (!legthValidator.validate(UserConstants.POSTAL_CODE, userRequest.getPostalCode()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.POSTAL_CODE);
			LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.POSTAL_CODE);
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
					LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.STATE);
					return true;
				} else if (!pickListValidator.validate(UserConstants.STATE, userRequest.getState())) {
					userResponse.setMessage(UserConstants.INVALID_VALUE + UserConstants.STATE);
					LOGGER.error(UserConstants.INVALID_VALUE + UserConstants.STATE);
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
			LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.IDMS_COUNTY_C);
			return true;
		}

		/**
		 * IDMS_POBox__c Length Validation check
		 */
		if ((null != userRequest.getIDMS_POBox__c() && !userRequest.getIDMS_POBox__c().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_PO_BOX_C, userRequest.getIDMS_POBox__c()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.IDMS_PO_BOX_C);
			LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.IDMS_PO_BOX_C);
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
			LOGGER.error(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.FEDERATION_IDENTIFIER.toString());
			return true;
		} else if ((null != userRequest.getIDMS_Federated_ID__c() && !userRequest.getIDMS_Federated_ID__c().isEmpty())
				&& (!legthValidator.validate(UserConstants.FEDERATION_IDENTIFIER,
						userRequest.getIDMS_Federated_ID__c()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.FEDERATION_IDENTIFIER);
			LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.FEDERATION_IDENTIFIER);
			return true;
		}

		/**
		 * IDMS_Profile_update_source__c validation and length check
		 */
		if ((!checkMandatoryFields) && (null == userRequest.getIDMS_Profile_update_source__c()
				|| userRequest.getIDMS_Profile_update_source__c().isEmpty())) {
			userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.UPDATE_SOURCE);
			LOGGER.error(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.UPDATE_SOURCE);
			return true;
		} else if ((null != userRequest.getIDMS_Profile_update_source__c()
				&& !userRequest.getIDMS_Profile_update_source__c().isEmpty())
				&& (!pickListValidator.validate(UserConstants.UPDATE_SOURCE,
						userRequest.getIDMS_Profile_update_source__c()))) {
			userResponse.setMessage(UserConstants.INVALID_VALUE_IDMS + UserConstants.UPDATE_SOURCE);
			LOGGER.error(UserConstants.INVALID_VALUE_IDMS + UserConstants.UPDATE_SOURCE);
			return true;
		}

		/**
		 * IDMS_Profile_update_source__c validation and length check
		 */
		if ((!checkMandatoryFields) && (null != userRequest.getIDMS_Profile_update_source__c()
				&& UserConstants.UIMS.equalsIgnoreCase(userRequest.getIDMS_Profile_update_source__c())
				&& null == userRequest.getIDMS_Federated_ID__c())) {
			userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.FEDERATION_IDENTIFIER);
			LOGGER.error(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.FEDERATION_IDENTIFIER);
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
			LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.IDMS_ADDITIONAL_ADDRESS_C);
			return true;
		}

		/**
		 * CompanyName Length Validation check
		 */

		if ((null != userRequest.getCompanyName() && !userRequest.getCompanyName().isEmpty())
				&& (!legthValidator.validate(UserConstants.COMPANY_NAME, userRequest.getCompanyName()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.COMPANY_NAME);
			LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.COMPANY_NAME);
			return true;
		}

		/**
		 * Company_Address1__c Length Validation check
		 */

		if ((null != userRequest.getCompany_Address1__c() && !userRequest.getCompany_Address1__c().isEmpty())
				&& (!legthValidator.validate(UserConstants.COMPANY_ADDRESS1_C, userRequest.getCompany_Address1__c()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.COMPANY_ADDRESS1_C);
			LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.COMPANY_ADDRESS1_C);
			return true;
		}

		/**
		 * Company_City__c Length Validation check
		 */

		if ((null != userRequest.getCompany_City__c() && !userRequest.getCompany_City__c().isEmpty())
				&& (!legthValidator.validate(UserConstants.COMPANY_CITY_C, userRequest.getCompany_City__c()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.COMPANY_CITY_C);
			LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.COMPANY_CITY_C);
			return true;
		}

		/**
		 * Company_Postal_Code__c Length Validation check
		 */

		if ((null != userRequest.getCompany_Postal_Code__c() && !userRequest.getCompany_Postal_Code__c().isEmpty())
				&& (!legthValidator.validate(UserConstants.COMPANY_POSTAL_CODE_C,
						userRequest.getCompany_Postal_Code__c()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.COMPANY_POSTAL_CODE_C);
			LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.COMPANY_POSTAL_CODE_C);
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
					LOGGER.error(UserConstants.INVALID_VALUE + UserConstants.COMPANY_STATE_C);
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
			LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.IDMS_COMPANY_PO_BOX_C);
			return true;
		}

		/**
		 * Company_Country__c validation and length check
		 */

		if ((null != userRequest.getCompany_Country__c() && !userRequest.getCompany_Country__c().isEmpty())) {
			if (!legthValidator.validate(UserConstants.COUNTRY, userRequest.getCompany_Country__c())) {
				userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.COMPANY_COUNTRY_C);
				LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.COMPANY_COUNTRY_C);
				return true;
			} else if (!pickListValidator.validate(UserConstants.COUNTRY, userRequest.getCompany_Country__c())) {
				userResponse.setMessage(UserConstants.INVALID_VALUE + UserConstants.COMPANY_COUNTRY_C);
				LOGGER.error(UserConstants.INVALID_VALUE + UserConstants.COMPANY_COUNTRY_C);
				return true;
			}
		}

		/**
		 * Company_Address2__c Length Validation check
		 */

		if ((null != userRequest.getCompany_Address2__c() && !userRequest.getCompany_Address2__c().isEmpty())
				&& (!legthValidator.validate(UserConstants.COMPANY_ADDRESS2_C, userRequest.getCompany_Address2__c()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.COMPANY_ADDRESS2_C);
			LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.COMPANY_ADDRESS2_C);
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
				LOGGER.error(UserConstants.INVALID_VALUE + UserConstants.IDMS_CLASS_LEVEL_C);
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
				LOGGER.error(UserConstants.INVALID_VALUE + UserConstants.IDMS_CLASS_LEVEL2_C);
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
				LOGGER.error(UserConstants.INVALID_VALUE + UserConstants.IDMS_MARKET_SEGMENT_C);
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
				LOGGER.error(UserConstants.INVALID_VALUE + UserConstants.IDMS_MARKET_SUB_SEGMENT_C);
				return true;
			}
		}

		/**
		 * Phone Length Validation check
		 */
		if ((null != userRequest.getPhone() && !userRequest.getPhone().isEmpty())
				&& (!legthValidator.validate(UserConstants.MOBILE_PHONE, userRequest.getPhone()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.PHONE);
			LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.PHONE);
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
				LOGGER.error(UserConstants.COUNTRY_FIELDS_MISSING + UserConstants.PHONE);
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
				LOGGER.error(UserConstants.INVALID_VALUE + UserConstants.JOB_TITLE_C);
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
				LOGGER.error(UserConstants.INVALID_VALUE + UserConstants.JOB_FUNCTION_C);
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
			LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.IDMS_JOB_DESCRIPTION_C);
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
			LOGGER.error(UserConstants.INVALID_VALUE + UserConstants.IDMS_COMPANY_MARKET_SERVED_C);
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
				LOGGER.error(UserConstants.INVALID_VALUE + UserConstants.IDMS_COMPANY_NBR_EMPLOYEES_C);
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
			LOGGER.error(UserConstants.INVALID_VALUE_HEADQUARTER + UserConstants.IDMS_COMPANY_HEAD_QUARTERS_C);
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
					LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.IDMS_ANNUAL_REVENUE_C);
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
			LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.IDMS_TAX_IDENTIFICATION_NUMBER_C);
			return true;
		}

		/**
		 * IDMSMiddleName__c Length Validation check
		 */
		if ((null != userRequest.getIDMSMiddleName__c() && !userRequest.getIDMSMiddleName__c().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_MIDDLE_NAME_C, userRequest.getIDMSMiddleName__c()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.IDMS_MIDDLE_NAME_C);
			LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.IDMS_MIDDLE_NAME_C);
			return true;
		}

		/**
		 * Company_Website__c Length Validation check
		 */

		if ((null != userRequest.getCompany_Website__c() && !userRequest.getCompany_Website__c().isEmpty())
				&& (!legthValidator.validate(UserConstants.COMPANY_WEBSITE_C, userRequest.getCompany_Website__c()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.COMPANY_WEBSITE_C);
			LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.COMPANY_WEBSITE_C);
			return true;
		}

		/**
		 * IDMSSalutation__c Length Validation check
		 */
		if ((null != userRequest.getIDMSSalutation__c() && !userRequest.getIDMSSalutation__c().isEmpty())
				&& (!pickListValidator.validate(UserConstants.SALUTATION.toString(),
						userRequest.getIDMSSalutation__c()))) {
			userResponse.setMessage(UserConstants.INVALID_VALUE + UserConstants.SALUTATION);
			LOGGER.error(UserConstants.INVALID_VALUE + UserConstants.SALUTATION);
			return true;
		}

		/**
		 * Department Length Validation check
		 */
		if ((null != userRequest.getDepartment() && !userRequest.getDepartment().isEmpty())
				&& (!legthValidator.validate(UserConstants.DEPARTMENT, userRequest.getDepartment()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.DEPARTMENT);
			LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.DEPARTMENT);
			return true;
		}
		/**
		 * IDMSSuffix__c Length Validation check
		 */
		if ((null != userRequest.getIDMSSuffix__c() && !userRequest.getIDMSSuffix__c().isEmpty())
				&& (!legthValidator.validate(UserConstants.IDMS_SUFFIX_C, userRequest.getIDMSSuffix__c()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.IDMS_SUFFIX_C);
			LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.IDMS_SUFFIX_C);
			return true;
		}

		/**
		 * Fax Length Validation check
		 */
		if ((null != userRequest.getFax() && !userRequest.getFax().isEmpty())
				&& (!legthValidator.validate(UserConstants.FAX, userRequest.getFax()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.FAX);
			LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.FAX);
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
			LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.IDMS_COMAPNY_FED_IDENTIFIER_C);
			return true;
		}

		/**
		 * IDMSDelegatedIdp__c Length Validation check
		 */

		if ((null != userRequest.getIDMSDelegatedIdp__c() && !userRequest.getIDMSDelegatedIdp__c().isEmpty())
				&& (!pickListValidator.validate(UserConstants.DELEGATED_IDP.toString(),
						userRequest.getIDMSDelegatedIdp__c()))) {
			userResponse.setMessage(UserConstants.INVALID_VALUE + UserConstants.IDMS_DELEGATED_IDP_C);
			LOGGER.error(UserConstants.INVALID_VALUE + UserConstants.IDMS_DELEGATED_IDP_C);
			return true;
		}

		/**
		 * IDMSIdentityType__c Length Validation check
		 */

		if ((null != userRequest.getIDMSIdentityType__c() && !userRequest.getIDMSIdentityType__c().isEmpty())
				&& (!pickListValidator.validate(UserConstants.IDENTITY_TYPE.toString(),
						userRequest.getIDMSIdentityType__c()))) {
			userResponse.setMessage(UserConstants.INVALID_VALUE + UserConstants.IDMS_IDENTITY_TYPE_C);
			LOGGER.error(UserConstants.INVALID_VALUE + UserConstants.IDMS_IDENTITY_TYPE_C);
			return true;
		}

		/**
		 * IDMSCompanyCounty__c Length Validation check
		 */

		if ((null != userRequest.getMobilePhone() && !userRequest.getMobilePhone().isEmpty())
				&& (!legthValidator.validate(UserConstants.MOBILE_PHONE, userRequest.getMobilePhone()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.MOBILE_PHONE);
			LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.MOBILE_PHONE);
			return true;
		}

		/**
		 * IDMSPrimaryContact__c Length Validation check
		 */

		if ((null != userRequest.getIDMSPrimaryContact__c() && !userRequest.getIDMSPrimaryContact__c().isEmpty())
				&& (!(UserConstants.TRUE.equalsIgnoreCase(userRequest.getIDMSPrimaryContact__c())
						|| (UserConstants.FALSE.equalsIgnoreCase(userRequest.getIDMSPrimaryContact__c()))))) {
			userResponse.setMessage(UserConstants.INVALID_VALUE + UserConstants.IDMS_PRIMARY_CONTACT_C);
			LOGGER.error(UserConstants.INVALID_VALUE + UserConstants.IDMS_PRIMARY_CONTACT_C);
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
				LOGGER.error(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.FIRST_NAME);
				return true;
			}

			/**
			 * LastName validation and length check
			 */
			if ((checkMandatoryFields) && (null == userRequest.getLastName() || userRequest.getLastName().isEmpty())) {
				userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.LAST_NAME);
				LOGGER.error(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.LAST_NAME);
				return true;
			}

			/**
			 * validate e-mail or mobile attribute values should be present
			 */
		 /*	if ((checkMandatoryFields) && (null == userRequest.getEmail() || userRequest.getEmail().isEmpty())
					&& (null == userRequest.getMobilePhone() || userRequest.getMobilePhone().isEmpty())) {
				userResponse.setMessage(
						UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.EMAIL + " OR " + UserConstants.MOBILE);
				return true;
			}
		 */
			/**
			 * validate preferred Language attribute values should be present
			 */
			if ((checkMandatoryFields) && (null == userRequest.getIDMS_PreferredLanguage__c()
					|| userRequest.getIDMS_PreferredLanguage__c().isEmpty())) {
				userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.PREFERRED_LANGUAGE);
				LOGGER.error(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.PREFERRED_LANGUAGE);
				return true;
			}

			/**
			 * validate Country Code attribute values should be present
			 */
			if ((checkMandatoryFields) && (null == userRequest.getCountry() || userRequest.getCountry().isEmpty())) {
				userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.COUNTRY);
				LOGGER.error(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.COUNTRY);
				return true;
			}

			/**
			 * validate COMPANY_NAME attribute values should be present
			 */
			if ((checkMandatoryFields)
					&& (null == userRequest.getCompanyName() || userRequest.getCompanyName().isEmpty())) {
				userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.COMPANY_NAME);
				LOGGER.error(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.COMPANY_NAME);
				return true;
			}

			/**
			 * validate COMPANY_ADDRESS1_C attribute values should be present
			 */
			if ((checkMandatoryFields) && (null == userRequest.getCompany_Address1__c()
					|| userRequest.getCompany_Address1__c().isEmpty())) {
				userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.COMPANY_ADDRESS1_C);
				LOGGER.error(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.COMPANY_ADDRESS1_C);
				return true;
			}

			/**
			 * validate COMPANY_CITY_C attribute values should be present
			 */
			if ((checkMandatoryFields)
					&& (null == userRequest.getCompany_City__c() || userRequest.getCompany_City__c().isEmpty())) {
				userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.COMPANY_CITY_C);
				LOGGER.error(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.COMPANY_CITY_C);
				return true;
			}

			/**
			 * validate COMPANY_POSTAL_CODE_C attribute values should be present
			 */
			if ((checkMandatoryFields) && (null == userRequest.getCompany_Postal_Code__c()
					|| userRequest.getCompany_Postal_Code__c().isEmpty())) {
				userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.COMPANY_POSTAL_CODE_C);
				LOGGER.error(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.COMPANY_POSTAL_CODE_C);
				return true;
			}

			/**
			 * validate COMPANY_COUNTRY_C attribute values should be present
			 */
			if ((checkMandatoryFields)
					&& (null == userRequest.getCompany_Country__c() || userRequest.getCompany_Country__c().isEmpty())) {
				userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.COMPANY_COUNTRY_C);
				LOGGER.error(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.COMPANY_COUNTRY_C);
				return true;
			}

		}

		if ((null != userRequest.getAboutMe() && !userRequest.getAboutMe().isEmpty())
				&& (!legthValidator.validate(UserConstants.ABOUT_ME, userRequest.getFirstName()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.ABOUT_ME);
			LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.ABOUT_ME);
			return true;
		}

		if ((null != userRequest.getBFO_ACCOUNT_ID__c() && !userRequest.getBFO_ACCOUNT_ID__c().isEmpty())
				&& (!legthValidator.validate(UserConstants.BFO_ACCOUNT_ID, userRequest.getBFO_ACCOUNT_ID__c()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.BFO_ACCOUNT_ID);
			LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.BFO_ACCOUNT_ID);
			return true;
		}

		if ((null != userRequest.getAccountId() && !userRequest.getAccountId().isEmpty())
				&& (!legthValidator.validate(UserConstants.ACCOUNT_ID, userRequest.getAccountId()))) {
			userResponse.setMessage(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.ACCOUNT_ID);
			LOGGER.error(UserConstants.INCORRECT_FIELDS_LENGTH + UserConstants.ACCOUNT_ID);
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
				&& (null == userRequest.getEmail() || userRequest.getEmail().isEmpty()) && 
				(null == userRequest.getMobilePhone() || userRequest.getMobilePhone().isEmpty())) {
			userResponse.setMessage(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.EMAIL+" OR " + UserConstants.MOBILE);
			LOGGER.error(UserConstants.REQUIRED_FIELDS_MISSING + UserConstants.EMAIL+" OR " + UserConstants.MOBILE);
			return true;
		}

		/**
		 * Channel__c PickList validation
		 */

		if ((null != userRequest.getChannel__c() && !userRequest.getChannel__c().isEmpty())
				&& (!pickListValidator.validate(UserConstants.IAM_A1, userRequest.getChannel__c()))) {
			userResponse.setMessage(UserConstants.INVALID_VALUE + UserConstants.CHANNEL);
			LOGGER.error(UserConstants.INVALID_VALUE + UserConstants.CHANNEL);
			return true;
		}

		/**
		 * SubChannel__c PickList validation
		 */
		if ((null != userRequest.getSubChannel__c() && !userRequest.getSubChannel__c().isEmpty())
				&& (!pickListValidator.validate(UserConstants.IAM_A2, userRequest.getSubChannel__c()))) {
			userResponse.setMessage(UserConstants.INVALID_VALUE + UserConstants.SUBCHANNEL);
			LOGGER.error(UserConstants.INVALID_VALUE + UserConstants.SUBCHANNEL);
			return true;
		}

		return false;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.idms.service.UserServiceImpl#checkUserExists(java.lang.String,
	 * java.lang.String)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public Response checkUserExists(String loginIdentifier, String withGlobalUsers) {
		LOGGER.info("Entered checkUserExists() -> Start");
		LOGGER.info("Parameter loginIdentifier -> " + loginIdentifier + " ,withGlobalUsers -> " + withGlobalUsers);

		DocumentContext productDocCtx = null;
		String iPlanetDirectoryKey = null;
		String ifwAccessToken = null, userExists = null;
		JSONObject response = new JSONObject();
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		Response ifwResponse = null;
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		Integer resultCount = 0;

		try {
			if ((null != withGlobalUsers) && (!UserConstants.TRUE.equalsIgnoreCase(withGlobalUsers)
					&& !UserConstants.FALSE.equalsIgnoreCase(withGlobalUsers))) {
				response.put(UserConstants.STATUS_L, errorStatus);
				response.put(UserConstants.MESSAGE_L, UserConstants.GLOBAL_USER_BOOLEAN);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.checkUserExists() : " + elapsedTime);
				LOGGER.error("Error with GlobalUSerField:" + UserConstants.GLOBAL_USER_BOOLEAN);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}

			if (loginIdentifier.contains("@")) {
				if (!emailValidator.validate(loginIdentifier.trim())) {
					response.put(UserConstants.STATUS_L, errorStatus);
					response.put(UserConstants.MESSAGE_L, "Email validation failed.");
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by checkUserExists() : " + elapsedTime);
					LOGGER.error("Error in checkUserExists is :: Email validation failed.");
					return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
				}
			} else {
				String id = loginIdentifier.trim();
				id = ChinaIdmsUtil.mobileTransformation(id);
				if (StringUtils.isNumeric(id)) {
					if (id.length() < 11) {
						response.put(UserConstants.STATUS_L, errorStatus);
						response.put(UserConstants.MESSAGE_L, "Mobile validation failed.");
						elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
						LOGGER.info("Time taken by checkUserExists() : " + elapsedTime);
						LOGGER.error("Error in checkUserExists is :: Mobile validation failed.");
						return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
					}
				} else {
					response.put(UserConstants.STATUS_L, errorStatus);
					response.put(UserConstants.MESSAGE_L, "Not valid email or mobile");
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by checkUserExists() : " + elapsedTime);
					LOGGER.error("Error in checkUserExists is :: Not valid email or mobile.");
					return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
				}
			}

			try {
				iPlanetDirectoryKey = getSSOToken();
			} catch (IOException ioExp) {
				LOGGER.error("Unable to get SSO Token" + ioExp.getMessage());
				iPlanetDirectoryKey = "";
			}

			if (null != loginIdentifier && !loginIdentifier.isEmpty()) {
				if(!UserConstants.TRUE.equalsIgnoreCase(withGlobalUsers)){
					LOGGER.info(AUDIT_REQUESTING_USER + AUDIT_TECHNICAL_USER + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN
							+ AUDIT_OPENAM_API + AUDIT_OPENAM_USER_EXISTS_CALL + loginIdentifier + AUDIT_LOG_CLOSURE);
					LOGGER.info("Start: checkUserExistsWithEmailMobile() of openam for login/login_mobile="
							+ loginIdentifier);
					userExists = productService.checkUserExistsWithEmailMobile(
							UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
							"loginid eq " + "\""
									+ URLEncoder.encode(URLDecoder.decode(loginIdentifier.trim(), "UTF-8"), "UTF-8")
									+ "\" or login_mobile eq " + "\""
									+ URLEncoder.encode(URLDecoder.decode(loginIdentifier.trim(), "UTF-8"), "UTF-8")
									+ "\"");
					LOGGER.info("End: checkUserExistsWithEmailMobile() of openam finished for login/login_mobile="
							+ loginIdentifier);
					productDocCtx = JsonPath.using(conf).parse(userExists);
					resultCount = productDocCtx.read("$.resultCount");
					LOGGER.info("resultCount of loginIdentifier = " + resultCount);
					if(resultCount == 1){
						//loginIdentifier found but password is incorrect						
						response.put(UserConstants.USER_INFO, UserConstants.CN_USER_ACTIVE);
						response.put(UserConstants.MESSAGE_L, UserConstants.TRUE);						
						return Response.status(Response.Status.OK).entity(response).build();
					}
					//loginIdentifier not found, checking if user is registered but not activated
					if(resultCount == 0){
						LOGGER.info("Start: checkUserExistsWithEmailMobile() of openam for mail/mobile="
								+ loginIdentifier);
						userExists = productService.checkUserExistsWithEmailMobile(
								UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
								"mail eq " + "\""
										+ URLEncoder.encode(URLDecoder.decode(loginIdentifier.trim(), "UTF-8"), "UTF-8")
										+ "\" or mobile_reg eq " + "\""
										+ URLEncoder.encode(URLDecoder.decode(loginIdentifier.trim(), "UTF-8"), "UTF-8")
										+ "\"");
						LOGGER.info("End: checkUserExistsWithEmailMobile() of openam finished for mail/mobile="
								+ loginIdentifier);
						productDocCtx = JsonPath.using(conf).parse(userExists);
						resultCount = productDocCtx.read("$.resultCount");
						LOGGER.info("resultCount of mail or mobile = " + resultCount);
						if(resultCount == 1){							
							response.put(UserConstants.USER_INFO, UserConstants.CN_USER_INACTIVE);
							response.put(UserConstants.MESSAGE_L, UserConstants.TRUE);
							return Response.status(Response.Status.OK).entity(response).build();
						}
						if(resultCount > 1){							
							response.put(UserConstants.USER_INFO, UserConstants.CN_USER_MULTIPLE_EXIST);
							response.put(UserConstants.MESSAGE_L, UserConstants.TRUE);
							return Response.status(Response.Status.OK).entity(response).build();
						}
					}
				} else {
					if (UserConstants.TRUE.equalsIgnoreCase(withGlobalUsers)) {
						LOGGER.info("Start: getIFWToken() of IFWService");
						ifwAccessToken = ifwService.getIFWToken(UserConstants.CONTENT_TYPE_URL_FROM,
								UserConstants.IFW_GRANT_TYPE, ifwClientId, ifwClientSecret);
						LOGGER.info("End: getIFWToken() of IFWService finished");
						
						productDocCtx = JsonPath.using(conf).parse(ifwAccessToken);
						String accessToken = productDocCtx.read("$.access_token");
						String bfoAuthorizationToken = sfSyncServiceImpl.getSFToken();
						String authorization = "Bearer " + accessToken;

						if (loginIdentifier.contains("@")) {
							LOGGER.info("Start: checkUserExistsWithEmail() of IFWService for loginIdentifier="
									+ loginIdentifier);
							ifwResponse = ifwService.checkUserExistsWithEmail(bfoAuthorizationToken,
									UserConstants.APPLICATION_NAME, UserConstants.COUNTRY_CODE,
									UserConstants.LANGUAGE_CODE, UserConstants.REQUEST_ID, authorization,
									loginIdentifier.trim(), false);
							LOGGER.info("End: checkUserExistsWithEmail() of IFWService finished for loginIdentifier="
									+ loginIdentifier);

						} else {
							LOGGER.info("Start: checkUserExistsWithEmail() of IFWService for loginIdentifier="
									+ loginIdentifier);
							ifwResponse = ifwService.checkUserExistsWithMobile(bfoAuthorizationToken,
									UserConstants.APPLICATION_NAME, UserConstants.COUNTRY_CODE,
									UserConstants.LANGUAGE_CODE, UserConstants.REQUEST_ID, authorization,
									loginIdentifier.trim(), false);
							LOGGER.info("End: checkUserExistsWithEmail() of IFWService finished for loginIdentifier="
									+ loginIdentifier);
						}

						if (null != ifwResponse && 200 == ifwResponse.getStatus()) {
							
							
							response.put(UserConstants.MESSAGE_L, UserConstants.TRUE);
							return Response.status(ifwResponse.getStatus()).entity(response).build();
						}
					}
				}
				
				response.put(UserConstants.MESSAGE_L, UserConstants.FALSE);
			}
		} catch (BadRequestException e) {
			response.put(UserConstants.STATUS_L, errorStatus);
			response.put(UserConstants.MESSAGE_L, UserConstants.BAD_REQUEST);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.checkUserExists() : " + elapsedTime);
			LOGGER.error("BadRequestException while checkUserExists :: -> " + e.getMessage(),e);
			return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
		} catch (NotAuthorizedException e) {
			response.put(UserConstants.STATUS_L, errorStatus);
			response.put(UserConstants.MESSAGE_L, "Authorization Failed");
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.checkUserExists() : " + elapsedTime);
			LOGGER.error("NotAuthorizedException while checkUserExists :: -> " + e.getMessage(),e);
			return Response.status(Response.Status.UNAUTHORIZED).entity(response).build();
		} catch (NotFoundException e) {
			response.put(UserConstants.STATUS_L, errorStatus);
			response.put(UserConstants.MESSAGE_L, UserConstants.USER_NOT_FOUND);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.ActivateUser() : " + elapsedTime);
			LOGGER.error("NotFoundException while checkUserExists :: -> " + e.getMessage(),e);
			return Response.status(Response.Status.NOT_FOUND).entity(response).build();
		} catch (Exception e) {
			response.put(UserConstants.STATUS_L, errorStatus);
			response.put(UserConstants.MESSAGE_L, e.getMessage());
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.checkUserExists() : " + elapsedTime);
			LOGGER.error("Exception while checkUserExists :: -> " + e.getMessage(),e);
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(response).build();
		}
		LOGGER.error("USER not found while executing checkUserExists()");
		return Response.status(Response.Status.NOT_FOUND).entity(response).build();
	}

	/**
	 * Checks if User exists for email passed as a query parameter.
	 * 
	 */
	@Override
	public Response userExists(String email) {
		/**
		 * login identifier eq email or mobile call /se/users?_queryFilter=mail
		 * eq 'email value'
		 */
		LOGGER.info("Entered userExists() -> Start");
		LOGGER.info("Parameter email -> " + email);

		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		DocumentContext productDocCtx = null;
		String iPlanetDirectoryKey = null;
		String userExists = null;
		UserExistsResponse userResponse = new UserExistsResponse();
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		try {
			iPlanetDirectoryKey = getSSOToken();
		} catch (IOException ioExp) {
			LOGGER.error("Unable to get SSO Token " + ioExp.getMessage(),ioExp);
			iPlanetDirectoryKey = "";
		}

		if (null != email && !email.isEmpty()) {
			LOGGER.info(AUDIT_REQUESTING_USER + AUDIT_TECHNICAL_USER + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN
					+ AUDIT_OPENAM_API + AUDIT_OPENAM_USER_EXISTS_CALL + email + AUDIT_LOG_CLOSURE);

			try {
				LOGGER.info("Start: checkUserExistsWithEmailMobile() of OpenAMService for email=" + email);
				userExists = productService.checkUserExistsWithEmailMobile(
						UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
						"loginid eq " + "\"" + URLEncoder.encode(URLDecoder.decode(email, "UTF-8"), "UTF-8")
								+ "\" or login_mobile eq " + "\""
								+ URLEncoder.encode(URLDecoder.decode(email, "UTF-8"), "UTF-8") + "\"");

				LOGGER.info("End: checkUserExistsWithEmailMobile() of OpenAMService fisnihed for email=" + email);
			} catch (UnsupportedEncodingException e) {
				LOGGER.error("Error in userExists() is-> " + e.getMessage(),e);
			}

			// user exists and resultcount > 0
			productDocCtx = JsonPath.using(conf).parse(userExists);
			Integer resultCount = productDocCtx.read(JsonConstants.RESULT_COUNT);
			LOGGER.info("resultCount=" + resultCount);
			if (resultCount.intValue() > 0) {
				userResponse.setMessage("true");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.userExists() : " + elapsedTime);
				return Response.status(Response.Status.OK).entity(userResponse).build();

			}
			userResponse.setMessage("false");
		}
		elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		LOGGER.info("Time taken by UserServiceImpl.userExists() : " + elapsedTime);
		// productService.sessionLogout(UserConstants.IPLANET_DIRECTORY_PRO+iPlanetDirectoryKey,
		// "logout");
		LOGGER.error("User not found in userExists() with email=" + email);
		return Response.status(Response.Status.NOT_FOUND).entity(userResponse).build();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.idms.service.UserServiceImpl#getOauthFromIPlanet(java.lang.String)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public Response getOauthFromIPlanet(String token) {
		LOGGER.info("Entered getOauthFromIPlanet() -> Start");
		LOGGER.info("Parameter token -> " + token);

		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		Response oauthFromIPlanet = null;
		String accessTokenresponse = null;
		String csrf = token;
		String cookie = UserConstants.CHINA_IDMS_TOKEN + token;
		String authorizationCode = "";
		DocumentContext productDocCtx = null;
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		String accessToken = "";
		try {

			// To fetch the authorization code from the redirect URI
			LOGGER.info(AUDIT_REQUESTING_USER + AUDIT_TECHNICAL_USER + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN
					+ AUDIT_OPENAM_API + AUDIT_OPENAM_AUTHORIZE_CALL + AUDIT_LOG_CLOSURE);
			LOGGER.info("Start: getOauthFromIPlanet() of OpenAMTokenService");
			oauthFromIPlanet = openAMTokenService.getOauthFromIPlanet(cookie, UserConstants.CACHE,
					UserConstants.CONTENT_TYPE, UserConstants.RESPONSE_TYPE, redirectUri, UserConstants.SCOPE, "/se",
					UserConstants.CLIENT_ID, csrf, "allow", "1");
			LOGGER.info("End: getOauthFromIPlanet() of OpenAMTokenService finished");
			/*
			 * String[] parts =
			 * oauthFromIPlanet.getHeaderString("Location").split("="); parts =
			 * parts[1].trim().split("&"); authorizationCode = parts[0].trim();
			 */

			String locationValue = oauthFromIPlanet.getHeaderString("Location");
			String codeString[] = locationValue
					.substring(locationValue.indexOf(UserConstants.CODE_FIELD) + UserConstants.CODE_FIELD.length(),
							locationValue.length())
					.split("&");

			if (null != codeString && codeString.length > 0) {
				authorizationCode = codeString[0];

				LOGGER.info("Authorisation Code ------------------>" + authorizationCode);

				// Fetch the accessToken from the autorization code
				LOGGER.info(AUDIT_REQUESTING_USER + AUDIT_TECHNICAL_USER + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN
						+ AUDIT_OPENAM_API + AUDIT_OPENAM_AUTHORIZE_POST_CALL + AUDIT_LOG_CLOSURE);
				LOGGER.info("Start: getOauthTokenFromCode() of OpenAMTokenService");
				accessTokenresponse = openAMTokenService.getOauthTokenFromCode(UserConstants.CACHE,
						UserConstants.CONTENT_TYPE, UserConstants.BEARER_TOKEN, UserConstants.GRANT_TYPE, "/se",
						redirectUri, authorizationCode);
				LOGGER.info("End: getOauthTokenFromCode() of OpenAMTokenService finished");
				productDocCtx = JsonPath.using(conf).parse(accessTokenresponse);
				accessToken = productDocCtx.read("$.access_token");

				LOGGER.info("Access Token -------------->" + accessToken);

				JSONObject jsonObject = new JSONObject();
				jsonObject.put("AccessToken", accessToken);
				JSONArray jsonArray = new JSONArray();
				jsonArray.add(jsonObject);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.getOauthFromIPlanet() : " + elapsedTime);
				return Response.status(Response.Status.OK.getStatusCode()).entity(jsonArray).build();
			} else {
				JSONObject jsonObject = new JSONObject();
				jsonObject.put("Error Message", UserConstants.CODE_FIELD + " Not Found");
				JSONArray jsonArray = new JSONArray();
				jsonArray.add(jsonObject);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.getOauthFromIPlanet() : " + elapsedTime);
				LOGGER.error("Error in getting authorizationCode / AccessToken");
				return Response.status(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode()).entity(jsonArray).build();
			}

		} catch (Exception e) {
			JSONObject jsonObject = new JSONObject();
			jsonObject.put("Error Message", "Internal Service Error");
			JSONArray jsonArray = new JSONArray();
			jsonArray.add(jsonObject);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.getOauthFromIPlanet() : " + elapsedTime);
			LOGGER.error("Error in getting authorizationCode / AccessToken =" + e.getMessage());
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode()).entity(jsonArray).build();

		}

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.idms.service.UserServiceImpl#userPinConfirmation(com.idms.model.
	 * ConfirmPinRequest)
	 */
	public Response userPinConfirmation(ConfirmPinRequest confirmRequest) {
		LOGGER.info("Entered userPinConfirmation() -> Start");

		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		ConfirmPinResponse response = new ConfirmPinResponse();
		ConfirmPinErrorResponse errorResponse = new ConfirmPinErrorResponse();
		DocumentContext productDocCtx = null;
		DocumentContext provProductDocCtx = null;
		String iPlanetDirectoryKey = null;
		String getUserResponse = null;
		String authId = null;
		String emailOrMobile = null;
		String loginIdentifierType = null;
		String firstName = null;
		String lastName = null;
		String amlbcookieValue = null;
		String PRODUCT_JSON_STRING = null;
		String hotpEmailVerification = null;
		String hotpMobileVerification = null;
		String openamVnew = null;
		Integer vNewCntValue = 0;
		String ifwAccessToken = null;
		boolean validPinStatus = false;
		ObjectMapper objMapper = new ObjectMapper();
		String uniqueIdentifier = null;
		String federationID = null, loginIdCheck = null;
		Response passwordOpenAMResponse = null;
		boolean isPasswordUpdatedInUIMS = false;
		try {

			LOGGER.info("Parameter confirmRequest -> "
					+ ChinaIdmsUtil.printInfo(ChinaIdmsUtil.printData(objMapper.writeValueAsString(confirmRequest))));

			if (null == confirmRequest.getPinCode() || confirmRequest.getPinCode().isEmpty()) {
				response.setStatus(errorStatus);
				response.setMessage(UserConstants.MANDATORY_PINCODE);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
				LOGGER.error("User pincode is null or empty");
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}

			if ((null == confirmRequest.getId() || confirmRequest.getId().isEmpty())
					&& (null == confirmRequest.getIDMS_Federated_ID__c()
							|| confirmRequest.getIDMS_Federated_ID__c().isEmpty())
					&& (null == confirmRequest.getFederationIdentifier()
							|| confirmRequest.getFederationIdentifier().isEmpty())) {
				response.setStatus(errorStatus);
				response.setMessage(UserConstants.MANDATORY_FEDERATION_ID);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
				LOGGER.error(response.getMessage());
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}

			if (null == confirmRequest.getIDMS_Profile_update_source()
					|| confirmRequest.getIDMS_Profile_update_source().isEmpty()) {
				response.setStatus(errorStatus);
				response.setMessage(UserConstants.PROFILE_UPDATE_SOURCE);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
				LOGGER.error(response.getMessage());
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}
			if ((null != confirmRequest.getIDMS_Profile_update_source()
					&& !confirmRequest.getIDMS_Profile_update_source().isEmpty())
					&& (!pickListValidator.validate(UserConstants.UPDATE_SOURCE,
							confirmRequest.getIDMS_Profile_update_source()))) {
				response.setMessage(UserConstants.INVALID_VALUE_IDMS + UserConstants.UPDATE_SOURCE);
				LOGGER.error(response.getMessage());
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}

			if ((null != confirmRequest.getOperation())
					&& !(UserConstants.USER_REGISTRATION.equalsIgnoreCase(confirmRequest.getOperation())
							|| UserConstants.SET_USER_PR.equalsIgnoreCase(confirmRequest.getOperation())
							|| UserConstants.UPDATE_USER_RECORD.equalsIgnoreCase(confirmRequest.getOperation()))) {

				response.setStatus(errorStatus);
				response.setMessage(UserConstants.OPERATION_MISMATCH);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
				LOGGER.error("Current operation: " + confirmRequest.getOperation() + " is not allowed");
				LOGGER.error(response.getMessage());
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			} else if (null != confirmRequest.getOperation()
					&& UserConstants.SET_USER_PR.equalsIgnoreCase(confirmRequest.getOperation())
					&& (null == confirmRequest.getPassword() || confirmRequest.getPassword().isEmpty())) {

				if (null != confirmRequest.getUIFlag() && !confirmRequest.getUIFlag().isEmpty()
						&& UserConstants.TRUE.equalsIgnoreCase(confirmRequest.getUIFlag())) {
					response.setStatus(errorStatus);
					response.setMessage(UserConstants.MANDATORY_PR);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
					LOGGER.error(response.getMessage());
					return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
				} else {
					response.setStatus(errorStatus);
					response.setMessage(UserConstants.OPERATION_BLCOKED);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
					LOGGER.error(response.getMessage());
					return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
				}
			} else if (null == confirmRequest.getOperation() || confirmRequest.getOperation().isEmpty()) {

				response.setStatus(errorStatus);
				response.setMessage(UserConstants.OPERATION_MISMATCH);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
				LOGGER.error(response.getMessage());
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}

			/**
			 * The below change applying for R5 Release start
			 * 
			 */

			if (((null == confirmRequest.getUIFlag()
					|| !UserConstants.TRUE.equalsIgnoreCase(confirmRequest.getUIFlag()))
					|| UserConstants.UPDATE_USER_RECORD.equalsIgnoreCase(confirmRequest.getOperation()))
					&& (null != confirmRequest.getPassword() && !confirmRequest.getPassword().isEmpty())
					&& (!UserConstants.UIMS.equalsIgnoreCase(confirmRequest.getIDMS_Profile_update_source()))) {

				response.setStatus(errorStatus);
				response.setMessage(UserConstants.OPERATION_BLCOKED);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
				LOGGER.error(response.getMessage());
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}

			if (null != confirmRequest.getIDMS_Federated_ID__c()
					&& !confirmRequest.getIDMS_Federated_ID__c().isEmpty()) {

				uniqueIdentifier = confirmRequest.getIDMS_Federated_ID__c();
			} else if (null != confirmRequest.getFederationIdentifier()
					&& !confirmRequest.getFederationIdentifier().isEmpty()) {

				uniqueIdentifier = confirmRequest.getFederationIdentifier();
			} else if (null != confirmRequest.getId() && !confirmRequest.getId().isEmpty()) {

				uniqueIdentifier = confirmRequest.getId();
			}

			/**
			 * call /json/authenticate to iplanetDirectoryPro token for admins
			 */
			try {
				iPlanetDirectoryKey = getSSOToken();
			} catch (IOException ioExp) {
				LOGGER.error("Unable to get SSO Token" + ioExp.getMessage(),ioExp);
				iPlanetDirectoryKey = "";
			}

			/**
			 * Call GET : /se/users/{userId}
			 */

			String getUserReponseProv = null;
			if (null != iPlanetDirectoryKey) {
				LOGGER.info(AUDIT_REQUESTING_USER + AUDIT_TECHNICAL_USER + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN
						+ AUDIT_OPENAM_API + AUDIT_OPENAM_GET_CALL + uniqueIdentifier + AUDIT_LOG_CLOSURE);
				LOGGER.info("Start: getUser() of OpenAMService for uniqueIdentifier=" + uniqueIdentifier);
				getUserResponse = productService.getUser(iPlanetDirectoryKey, uniqueIdentifier);
				LOGGER.info("End: getUser() of OpenAMService finished for uniqueIdentifier=" + uniqueIdentifier);
				LOGGER.info("getUser(): Response :  -> " + ChinaIdmsUtil.printOpenAMInfo(getUserResponse));
				productDocCtx = JsonPath.using(conf).parse(getUserResponse);

				/*
				 * String loginIdCheck = null !=
				 * productDocCtx.read(JsonConstants.LOGIN_ID_UPPER_0) ?
				 * getValue(productDocCtx.read(JsonConstants.LOGIN_ID_UPPER_0))
				 * : getDelimeter();
				 */

				if (null != productDocCtx.read(JsonConstants.LOGIN_ID_UPPER_0))
					loginIdCheck = getValue(productDocCtx.read(JsonConstants.LOGIN_ID_UPPER_0));

				if (null == loginIdCheck || loginIdCheck.isEmpty())
					loginIdCheck = getValue(productDocCtx.read(JsonConstants.LOGIN_MOBILE_0));
				LOGGER.info("loginIdCheck =" + loginIdCheck);

				// Start: New Requirement to check passed email/mobile with
				// openam email
				String userPassedEmailorMobile = confirmRequest.getEmail();
				if (null == userPassedEmailorMobile || userPassedEmailorMobile.isEmpty()) {
					userPassedEmailorMobile = confirmRequest.getMobilePhone();
				}

				if ((null != userPassedEmailorMobile && !userPassedEmailorMobile.isEmpty())
						&& UserConstants.USER_REGISTRATION.equalsIgnoreCase(confirmRequest.getOperation())) {
					String userEmailorMobileFromOpenAm = productDocCtx.read(JsonConstants.MAIL_0);
					if (null == userEmailorMobileFromOpenAm || userEmailorMobileFromOpenAm.isEmpty()) {
						userEmailorMobileFromOpenAm = productDocCtx.read(JsonConstants.MOBILEREG_0);
					}

					if (!userEmailorMobileFromOpenAm.equalsIgnoreCase(userPassedEmailorMobile)) {
						response.setStatus(errorStatus);
						response.setMessage(UserConstants.EMAIL_OR_MOBILE_NOT_MATCHING);
						elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
						LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
						LOGGER.error(response.getMessage());
						return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
					}
				}
				// End: New Requirement to check passed email/mobile with openam
				// email

				if (null != loginIdCheck && !loginIdCheck.isEmpty()
						&& "userRegistration".equals(confirmRequest.getOperation())) {
					response.setMessage("The user is already activated");
					response.setId(uniqueIdentifier);
					response.setFederation_Id(uniqueIdentifier);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
					LOGGER.error(response.getMessage());
					return Response.status(Response.Status.CONFLICT).entity(response).build();
				}

				openamVnew = null != productDocCtx.read("$.V_New[0]") ? getValue(productDocCtx.read("$.V_New[0]"))
						: getDelimeter();
				if (null != vNewCntValue && null != openamVnew) {
					vNewCntValue = Integer.parseInt(openamVnew) + 1;
				}
				String version = "{\"V_New\": \"" + vNewCntValue + "\"" + "}";
				// Adding V_New
				LOGGER.info("Start: updateUser() of OpenAMService for uniqueIdentifier=" + uniqueIdentifier);
				productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, uniqueIdentifier,
						version);
				LOGGER.info("End: updateUser() of OpenAMService finished for uniqueIdentifier=" + uniqueIdentifier);
				amlbcookieValue = null != productDocCtx.read("$.amlbcookie")
						? getValue(productDocCtx.read("$.amlbcookie").toString()) : getDelimeter();

				if ("[]".equalsIgnoreCase(productDocCtx.read("$.AuthID[0]"))
						|| "[]".equalsIgnoreCase(productDocCtx.read("$.authId[0]"))) {
					throw new Exception("Pin got expired or invalid!!");
				} else {
					authId = productDocCtx.read("$.authId[0]");
					if (null == authId) {
						authId = productDocCtx.read("$.AuthID[0]");
					}
				}
				emailOrMobile = productDocCtx.read("$.mail[0]");
				loginIdentifierType = UserConstants.EMAIL;
				if (null == emailOrMobile) {
					emailOrMobile = productDocCtx.read("$.mobile_reg[0]");
					loginIdentifierType = UserConstants.MOBILE;

				}

				federationID = productDocCtx.read("$.federationID[0]");

				if (UserConstants.UPDATE_USER_RECORD.equalsIgnoreCase(confirmRequest.getOperation())) {
					LOGGER.info("Start: getUser() of OpenAMService for uniqueIdentifier=" + uniqueIdentifier);
					getUserReponseProv = productService.getUser(iPlanetDirectoryKey, uniqueIdentifier);
					LOGGER.info("End: getUser() of OpenAMService finished for uniqueIdentifier=" + uniqueIdentifier);
					provProductDocCtx = JsonPath.using(conf).parse(getUserReponseProv);
					amlbcookieValue = null != provProductDocCtx.read("$.amlbcookie")
							? getValue(provProductDocCtx.read("$.amlbcookie").toString()) : getDelimeter();
					emailOrMobile = provProductDocCtx.read("$.newmail[0]");
					loginIdentifierType = UserConstants.EMAIL;
					if (null == emailOrMobile) {
						emailOrMobile = provProductDocCtx.read("$.newmobile[0]");
						loginIdentifierType = UserConstants.MOBILE;
					}

					// newmail is assigning to hotpEmailVerification since we
					// are getting user from se realm

					hotpEmailVerification = emailOrMobile;
				}

			} else {
				response.setStatus(errorStatus);
				response.setMessage(UserConstants.TOKEN_INVALID);
				response.setId(uniqueIdentifier);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
				LOGGER.error(response.getMessage());
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}

			firstName = null != productDocCtx.read("$.givenName")
					? getValue(productDocCtx.read("$.givenName").toString()) : getDelimeter();
			lastName = null != productDocCtx.read("$.sn") ? getValue(productDocCtx.read("$.sn").toString())
					: getDelimeter();

			if (((null != confirmRequest.getPassword() && !confirmRequest.getPassword().isEmpty()))
					&& !checkPasswordPolicy(confirmRequest.getPassword(), firstName, lastName, productDocCtx.read("$.mail[0]"),
							productDocCtx.read("$.mobile_reg[0]"))) {
				response.setStatus(errorStatus);
				response.setMessage(UserConstants.PR_POLICY);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
				LOGGER.error(response.getMessage());
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}

			// LOGGER.info("User Reponse Document " +
			// productDocCtx.jsonString());

			/*
			 * productDocCtx =
			 * JsonPath.using(conf).parse(UserConstants.OPT_SUBMIT_REQUEST);
			 * productDocCtx.set(JsonConstants.AUTH_ID, authId);
			 * productDocCtx.set("$.callbacks[0].input[0].value",
			 * confirmRequest.getPinCode());
			 * productDocCtx.set("$.callbacks[1].input[0].value", 0);
			 */

			/**
			 * HOTP Call 4 to Submit HOTP
			 */
			String hotpService = null;
			String userService = null;
			if (UserConstants.USER_REGISTRATION.equalsIgnoreCase(confirmRequest.getOperation())
					&& UserConstants.MOBILE.equals(loginIdentifierType)) {
				hotpService = UserConstants.HOTP_MOBILE_USER_REGISTRATION;
				userService = UserConstants.CREATE_USER_SERVICE;
			} else if (UserConstants.USER_REGISTRATION.equalsIgnoreCase(confirmRequest.getOperation())
					&& UserConstants.EMAIL.equals(loginIdentifierType)) {
				hotpService = UserConstants.HOTP_EMAIL;
				userService = UserConstants.CREATE_USER_SERVICE;
			} else if (UserConstants.SET_USER_PR.equalsIgnoreCase(confirmRequest.getOperation())
					&& UserConstants.EMAIL.equals(loginIdentifierType)) {
				hotpService = UserConstants.HOTP_EMAIL_RESET_PR;
				userService = UserConstants.CREATE_USER_SERVICE;
			} else if (UserConstants.SET_USER_PR.equalsIgnoreCase(confirmRequest.getOperation())
					&& UserConstants.MOBILE.equals(loginIdentifierType)) {
				hotpService = UserConstants.HOTP_MOBILE_RESET_PR;
				userService = UserConstants.CREATE_USER_SERVICE;
			}
			if (UserConstants.UPDATE_USER_RECORD.equalsIgnoreCase(confirmRequest.getOperation())
					&& UserConstants.MOBILE.equals(loginIdentifierType)) {
				hotpService = UserConstants.HOTP_MOBILE_UPDATE;
				userService = UserConstants.UPDATE_USER_SERVICE;
			} else if (UserConstants.UPDATE_USER_RECORD.equalsIgnoreCase(confirmRequest.getOperation())
					&& UserConstants.EMAIL.equals(loginIdentifierType)) {
				hotpService = UserConstants.HOTP_EMAIL_UPDATE;
				userService = UserConstants.UPDATE_USER_SERVICE;
			}
			try {

				// LOGGER.info("UserServiceImpl:userPinConfirmation -> :
				// executeHotpCall: Requset : -> ");
				LOGGER.info("hotpService ->" + hotpService);
				LOGGER.info("productDocCtx.jsonString() - >" + ChinaIdmsUtil.printOpenAMInfo(productDocCtx.jsonString()));
				LOGGER.info("userService ->" + userService);
				if (UserConstants.USER_REGISTRATION.equalsIgnoreCase(confirmRequest.getOperation())) {
					LOGGER.info("Start: validatePin() for User-Registration for uniqueIdentifier=" + uniqueIdentifier);
					validPinStatus = sendEmail.validatePin(confirmRequest.getPinCode(), uniqueIdentifier);
					LOGGER.info("End: validatePin() for User-Registration finished for uniqueIdentifier="
							+ uniqueIdentifier);
				} else {
					LOGGER.info("Start: validatePin() for other-than-User-Registration for uniqueIdentifier="
							+ uniqueIdentifier);
					validPinStatus = sendEmail.validatePin(confirmRequest.getPinCode(), uniqueIdentifier);
					LOGGER.info("End: validatePin() for other-than-User-Registration finished for uniqueIdentifier="
							+ uniqueIdentifier);
				}
				if (!validPinStatus) {
					throw new Exception("Pin got expired or invalid!!");
				}
			} catch (NotAuthorizedException e) {
				response.setStatus(errorStatus);
				response.setMessage(e.getMessage());
				response.setId(uniqueIdentifier);
				response.setFederation_Id(uniqueIdentifier);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
				LOGGER.error("Exception while userPinConfirmation :: -> " + e.getMessage(),e);
				return Response.status(Response.Status.UNAUTHORIZED).entity(response).build();
			} catch (Exception e) {
				errorResponse.setStatus(errorStatus);
				//errorResponse.setMessage(UserConstants.INVALID_PINCODE);
				if(e.getMessage().contains(UserConstants.PIN_CONFIRMATION_ERROR_CODE))
					errorResponse.setMessage(UserConstants.PIN_CONFIRMATION_ERROR);
				else
					errorResponse.setMessage(e.getMessage());
				errorResponse.setId(uniqueIdentifier);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
				LOGGER.error("Exception while confirming the User pin:: -> " + e.getMessage(),e);
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
			}

			LOGGER.info("UserServiceImpl:userPinConfirmation -> : Operation: Requset :  -> "
					+ confirmRequest.getOperation());
			if (UserConstants.USER_REGISTRATION.equalsIgnoreCase(confirmRequest.getOperation())) {

				if (UserConstants.MOBILE.equalsIgnoreCase(loginIdentifierType)) {

					PRODUCT_JSON_STRING = "{" + "\"login_mobile\": \"" + emailOrMobile + "\",\"mobile_reg\": \""
							+ emailOrMobile + "\"" + "}";

					if ((null != confirmRequest.getUIFlag() && !confirmRequest.getUIFlag().isEmpty())
							&& (null != confirmRequest.getPassword() && !confirmRequest.getPassword().isEmpty())) {
						PRODUCT_JSON_STRING = "{" + "\"login_mobile\": \"" + emailOrMobile + "\",\"mobile_reg\": \""
								+ emailOrMobile + "\",\"userPassword\": \"" + confirmRequest.getPassword().trim() + "\""
								+ "}";
					}
				} else if (UserConstants.EMAIL.equalsIgnoreCase(loginIdentifierType)) {
					PRODUCT_JSON_STRING = "{" + "\"loginid\": \"" + emailOrMobile + "\",\"mail\": \"" + emailOrMobile
							+ "\"" + "}";
					if ((null != confirmRequest.getUIFlag() && !confirmRequest.getUIFlag().isEmpty())
							&& (null != confirmRequest.getPassword() && !confirmRequest.getPassword().isEmpty())) {
						PRODUCT_JSON_STRING = "{" + "\"loginid\": \"" + emailOrMobile + "\",\"mail\": \""
								+ emailOrMobile + "\",\"userPassword\": \"" + confirmRequest.getPassword().trim() + "\""
								+ "}";
					}
				}

				if (null != confirmRequest.getIDMS_Email_opt_in__c()
						&& !confirmRequest.getIDMS_Email_opt_in__c().isEmpty()) {
					PRODUCT_JSON_STRING = PRODUCT_JSON_STRING.substring(0, PRODUCT_JSON_STRING.length() - 1)
							.concat(",\"emailOptIn\":\"" + confirmRequest.getIDMS_Email_opt_in__c() + "\"}");
				}
				if (null != confirmRequest.getTncFlag() && !confirmRequest.getTncFlag().isEmpty()) {
					PRODUCT_JSON_STRING = PRODUCT_JSON_STRING.substring(0, PRODUCT_JSON_STRING.length() - 1)
							.concat(",\"tncFlag\":\"" + confirmRequest.getTncFlag() + "\"}");
				}

				/**
				 * For User Activation
				 * 
				 */

				PRODUCT_JSON_STRING = PRODUCT_JSON_STRING.substring(0, PRODUCT_JSON_STRING.length() - 1)
						.concat(",\"isActivated\":\"true\"}");

				/*
				 * if (null != emailOrMobile && !emailOrMobile.isEmpty()) {
				 * LOGGER.info(AUDIT_REQUESTING_USER + uniqueIdentifier +
				 * AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN + AUDIT_OPENAM_API
				 * + AUDIT_OPENAM_UPDATE_CALL + uniqueIdentifier +
				 * AUDIT_LOG_CLOSURE); //LOGGER.info(
				 * "Email/Mobile userPinConfirmation(): Request :  -> ");
				 *//**
					 * Commenting below line updateuser since we are updating
					 * after uims sync
					 *//*
					 * LOGGER.info(
					 * "going to call updateUser() of OpenAMService to update email/mobile for uniqueIdentifier="
					 * +uniqueIdentifier);
					 * productService.updateUser(UserConstants.CHINA_IDMS_TOKEN
					 * + iPlanetDirectoryKey, uniqueIdentifier,
					 * PRODUCT_JSON_STRING); LOGGER.info(
					 * "updateUser() of OpenAMService finished to update email/mobile for uniqueIdentifier="
					 * +uniqueIdentifier); }
					 */

				/**
				 * The below code to activate the IDMSSetActivationDate
				 * 
				 */

				if (pickListValidator.validate(UserConstants.IDMS_BFO_profile,
						confirmRequest.getIDMS_Profile_update_source())) {
					datePopulationSerivce.populatePrmActivationDate(uniqueIdentifier,
							confirmRequest.getIDMS_Profile_update_source());
				}

			}
			if (UserConstants.UPDATE_USER_RECORD.equalsIgnoreCase(confirmRequest.getOperation())) {

				if (UserConstants.MOBILE.equalsIgnoreCase(loginIdentifierType)) {

					PRODUCT_JSON_STRING = "{" + "\"login_mobile\": \"" + emailOrMobile + "\",\"mobile_reg\": \""
							+ emailOrMobile + "\",\"hotpMobileVerification\": \"" + hotpMobileVerification + "\"" + "}";

				} else if (UserConstants.EMAIL.equalsIgnoreCase(loginIdentifierType)) {
					PRODUCT_JSON_STRING = "{" + "\"loginid\": \"" + emailOrMobile + "\",\"mail\": \"" + emailOrMobile
							+ "\",\"idmsuid\": \"" + emailOrMobile + "\",\"hotpEmailVerification\": \""
							+ hotpEmailVerification + "\"" + "}";
				}

				/**
				 * Commenting below line updateuser since we are updating after
				 * uims sync
				 */

				/*
				 * if (null != emailOrMobile && !emailOrMobile.isEmpty()) {
				 * LOGGER.info(AUDIT_REQUESTING_USER + uniqueIdentifier +
				 * AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN + AUDIT_OPENAM_API
				 * + AUDIT_OPENAM_UPDATE_CALL + uniqueIdentifier +
				 * AUDIT_LOG_CLOSURE); LOGGER.info(
				 * "userPinConfirmation -> : productService.updateUser: Requset :  -> "
				 * +PRODUCT_JSON_STRING); LOGGER.info(
				 * "going to call updateUser() of OpenAMService for uniqueIdentifier ="
				 * +uniqueIdentifier);
				 * productService.updateUser(UserConstants.CHINA_IDMS_TOKEN +
				 * iPlanetDirectoryKey, uniqueIdentifier, PRODUCT_JSON_STRING);
				 * LOGGER.info(
				 * "updateUser() of OpenAMService finished for uniqueIdentifier ="
				 * +uniqueIdentifier); }
				 */

				EMAIL_CHANGE_LOGGER.info("{},{},{}", formatter.format(new Date()), uniqueIdentifier, emailOrMobile);

			}
			if ((null != confirmRequest.getUIFlag() && !confirmRequest.getUIFlag().isEmpty())
					&& (UserConstants.SET_USER_PR.equalsIgnoreCase(confirmRequest.getOperation()))) {

				String isUserAcitvated = null != productDocCtx.read("$.isActivated")
						? getValue(productDocCtx.read("$.isActivated").toString()) : getDelimeter();

				if (UserConstants.FALSE.equalsIgnoreCase(isUserAcitvated)) {

					if (UserConstants.MOBILE.equalsIgnoreCase(loginIdentifierType)) {

						PRODUCT_JSON_STRING = "{" + "\"login_mobile\": \"" + emailOrMobile + "\",\"mobile_reg\": \""
								+ emailOrMobile + "\"" + "}";

						if ((null != confirmRequest.getUIFlag() && !confirmRequest.getUIFlag().isEmpty())
								&& (null != confirmRequest.getPassword() && !confirmRequest.getPassword().isEmpty())) {
							PRODUCT_JSON_STRING = "{" + "\"login_mobile\": \"" + emailOrMobile + "\",\"mobile_reg\": \""
									+ emailOrMobile + "\",\"userPassword\": \"" + confirmRequest.getPassword().trim()
									+ "\"" + "}";
						}
					} else if (UserConstants.EMAIL.equalsIgnoreCase(loginIdentifierType)) {
						PRODUCT_JSON_STRING = "{" + "\"loginid\": \"" + emailOrMobile + "\",\"mail\": \""
								+ emailOrMobile + "\"" + "}";
						if ((null != confirmRequest.getUIFlag() && !confirmRequest.getUIFlag().isEmpty())
								&& (null != confirmRequest.getPassword() && !confirmRequest.getPassword().isEmpty())) {
							PRODUCT_JSON_STRING = "{" + "\"loginid\": \"" + emailOrMobile + "\",\"mail\": \""
									+ emailOrMobile + "\",\"userPassword\": \"" + confirmRequest.getPassword().trim()
									+ "\"" + "}";
						}
					}
					PRODUCT_JSON_STRING = PRODUCT_JSON_STRING.substring(0, PRODUCT_JSON_STRING.length() - 1)
							.concat(",\"isActivated\":\"true\"}");
				} else {

					/**
					 * Checking if password want to update
					 */
					if (null != confirmRequest.getPassword() && !confirmRequest.getPassword().isEmpty()) {

						PRODUCT_JSON_STRING = "{" + "\"userPassword\": \"" + confirmRequest.getPassword().trim() + "\""
								+ "}";
						/*
						 * LOGGER.info(AUDIT_REQUESTING_USER + uniqueIdentifier
						 * + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN +
						 * AUDIT_OPENAM_API + AUDIT_OPENAM_GET_CALL +
						 * uniqueIdentifier + AUDIT_LOG_CLOSURE);
						 */
						// LOGGER.info("productService.updateUser: Requset : ->
						// ");

						/**
						 * Commenting below line updateuser since we are
						 * updating after uims sync
						 */
						/*
						 * LOGGER.info(
						 * "Going to call updateUser() of OpenAMService for uniqueIdentifier="
						 * +uniqueIdentifier);
						 * productService.updateUser(UserConstants.
						 * CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
						 * uniqueIdentifier, PRODUCT_JSON_STRING); LOGGER.info(
						 * "updateUser() of OpenAMService finished for uniqueIdentifier="
						 * +uniqueIdentifier);
						 */
					}

				}

			}

			LOGGER.info("authToken  " + authId);

			PRODUCT_JSON_STRING = PRODUCT_JSON_STRING.substring(0, PRODUCT_JSON_STRING.length() - 1)
					.concat(",\"authId\":\"" + "[]" + "\"}");

			// After creating an user and while calling confirm pin api, if
			// �password� comes in the request then call setPassword UIMS
			// api
			// Otherwise if there is no password then call Activate User UIMS
			// api.
			if (null != confirmRequest.getIDMS_Profile_update_source()
					&& !UserConstants.UIMS.equalsIgnoreCase(confirmRequest.getIDMS_Profile_update_source())
					&& (null != confirmRequest.getOperation()
							&& UserConstants.USER_REGISTRATION.equalsIgnoreCase(confirmRequest.getOperation()))) {
				confirmRequest.setId(uniqueIdentifier);
				confirmRequest.setIDMS_Federated_ID__c(federationID);

				// Updating records in OPENAM
				updateOpenamDetails(iPlanetDirectoryKey, uniqueIdentifier, PRODUCT_JSON_STRING);

				if (pickListValidator.validate(UserConstants.UIMSPasswordSync, UserConstants.TRUE)) {
					LOGGER.info("Start: SYNC activateUIMSUserConfirmPIN() of UimsSetPasswordSoapService");
					uimsSetPasswordSoapService.activateUIMSUserConfirmPIN(confirmRequest, vNewCntValue.toString(),
							UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, loginIdentifierType, emailOrMobile);
					// updateOpenamDetails(iPlanetDirectoryKey,
					// uniqueIdentifier, PRODUCT_JSON_STRING);
					LOGGER.info("End: SYNC activateUIMSUserConfirmPIN() of UimsSetPasswordSoapService finished");
				} else {
					LOGGER.info("Start: ASYNC activateUIMSUserConfirmPIN() of UimsSetPasswordSoapService");
					uimsUserManagerSoapService.activateUIMSUserConfirmPIN(confirmRequest, vNewCntValue.toString(),
							UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, loginIdentifierType, emailOrMobile);
					// updateOpenamDetails(iPlanetDirectoryKey,
					// uniqueIdentifier, PRODUCT_JSON_STRING);
					LOGGER.info("End: ASYNC activateUIMSUserConfirmPIN() of UimsSetPasswordSoapService finished");
				}
			} else if (null != confirmRequest.getIDMS_Profile_update_source()
					&& !UserConstants.UIMS.equalsIgnoreCase(confirmRequest.getIDMS_Profile_update_source())
					&& (null != confirmRequest.getOperation()
							&& UserConstants.SET_USER_PR.equalsIgnoreCase(confirmRequest.getOperation()))
					&& (null != confirmRequest.getUIFlag() && !confirmRequest.getUIFlag().isEmpty())) {
				// Set password and validating against password history
				passwordOpenAMResponse = updatePasswordHistory(iPlanetDirectoryKey, uniqueIdentifier,
						PRODUCT_JSON_STRING);
				if (200 != passwordOpenAMResponse.getStatus()) {
					return passwordOpenAMResponse;
				}
				// check UIMSPasswordSync to call sync or Async method
				if (pickListValidator.validate(UserConstants.UIMSPasswordSync, UserConstants.TRUE)) {
					// Calling Sync method of setUIMSPassword
					LOGGER.info("Start: SYNC setUIMSPassword() of UimsSetPasswordSoapService for federationID="
							+ federationID);
					isPasswordUpdatedInUIMS = uimsSetPasswordSoapService.setUIMSPassword(
							UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, uniqueIdentifier, federationID,
							confirmRequest.getPassword(), vNewCntValue.toString(), loginIdentifierType, emailOrMobile);
					// updateOpenamDetails(iPlanetDirectoryKey,
					// uniqueIdentifier, PRODUCT_JSON_STRING);
					LOGGER.info("End: SYNC setUIMSPassword() of UimsSetPasswordSoapService finished for federationID="
							+ federationID);
				} else {
					// Calling Async method of setUIMSPassword
					LOGGER.info("Start: ASYNC setUIMSPassword() of uimsUserManagerSoapService for federationID="
							+ federationID);
					uimsUserManagerSoapService.setUIMSPassword(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
							uniqueIdentifier, federationID, confirmRequest.getPassword(), vNewCntValue.toString(),
							loginIdentifierType, emailOrMobile);
					// updateOpenamDetails(iPlanetDirectoryKey,
					// uniqueIdentifier, PRODUCT_JSON_STRING);
					LOGGER.info("End: ASYNC setUIMSPassword() of uimsUserManagerSoapService finished for federationID="
							+ federationID);
				}
			} else if (null != confirmRequest.getIDMS_Profile_update_source()
					&& !UserConstants.UIMS.equalsIgnoreCase(confirmRequest.getIDMS_Profile_update_source())
					&& (null != confirmRequest.getOperation()
							&& UserConstants.UPDATE_USER_RECORD.equalsIgnoreCase(confirmRequest.getOperation()))) {
				LOGGER.info("Start: ASYNC updateChangeEmailOrMobile() of uimsUserManagerSoapService for federationID="
						+ federationID);
				uimsUserManagerSoapService.updateChangeEmailOrMobile(iPlanetDirectoryKey, uniqueIdentifier,
						federationID, openamVnew, loginIdentifierType, emailOrMobile);
				updateOpenamDetails(iPlanetDirectoryKey, uniqueIdentifier, PRODUCT_JSON_STRING);
				LOGGER.info("End: ASYNC updateChangeEmailOrMobile() of uimsUserManagerSoapService finished for federationID="
						+ federationID);
			}
			LOGGER.info("activateUIMSUserConfirmPIN is completed successfully");
		} catch (BadRequestException e) {
			response.setStatus(errorStatus);
			response.setMessage(e.getMessage());
			response.setId(uniqueIdentifier);
			response.setFederation_Id(uniqueIdentifier);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.userPinConfirmation() : " + elapsedTime);
			LOGGER.error("Error while userPinConfirmation :: -> " + e.getMessage(),e);
			return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
		} catch (NotFoundException e) {
			LOGGER.error("Error is " + e.getMessage(),e);
			// logic for PRM set password, if the user not found, call the
			// Global get user api
			// and retrieve the user details and pass it to create user
			if (null != confirmRequest.getIDMS_Profile_update_source() && (pickListValidator
					.validate(UserConstants.IDMS_BFO_profile, confirmRequest.getIDMS_Profile_update_source()))) {
				LOGGER.info("Going to call getIFWToken() of IFWService");
				ifwAccessToken = ifwService.getIFWToken(UserConstants.CONTENT_TYPE_URL_FROM,
						UserConstants.IFW_GRANT_TYPE, ifwClientId, ifwClientSecret);
				LOGGER.info("getIFWToken() of IFWService finished");
				productDocCtx = JsonPath.using(conf).parse(ifwAccessToken);
				String accessToken = productDocCtx.read("$.access_token");

				/*
				 * LOGGER.info("getSalesForceToken : => " +
				 * "PASSWORD_GRANT_TYPE : " + UserConstants.PR_GRANT_TYPE +
				 * " salesForceClientId: " + salesForceClientId +
				 * " salesForceClientSecret :" + salesForceClientSecret +
				 * " salesForceUserName: " + salesForceUserName +
				 * " salesForcePassword :" + salesForcePassword);
				 */
				// LOGGER.info("Start: getSalesForceToken() of
				// SalesForceService");
				String bfoAuthorizationToken = sfSyncServiceImpl.getSFToken();
				// LOGGER.info("End: getSalesForceToken() of SalesForceService
				// finished");
				/*
				 * conf =
				 * Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).
				 * build(); productDocCtx =
				 * JsonPath.using(conf).parse(bfoAuthorization); String
				 * bfoAuthorizationToken = productDocCtx.read("$.access_token");
				 */

				String authorization = "Bearer " + accessToken;

				LOGGER.info("Start: getUser() of IFWService");
				Response globalGetUserResponse = ifwService.getUser(authorization, bfoAuthorizationToken,
						UserConstants.ACCEPT_TYPE_APP_JSON, "", "", "", "", confirmRequest.getIDMS_Federated_ID__c());
				LOGGER.info("End: getUser() of IFWService finished");
				productDocCtx = JsonPath.using(conf).parse(globalGetUserResponse);
				String responseAsString = globalGetUserResponse.readEntity(String.class);
				LOGGER.info("globalGetUserResponse : " + responseAsString);
				try {
					IFWCustomAttributesForWork idmsUser = objMapper.readValue(responseAsString,
							IFWCustomAttributesForWork.class);
					// CreateUserRequest iDMSUser = mapper.map(idmsUser,
					// CreateUserRequest.class);

					/**
					 * Added the below condition if user doesn't send the hashed
					 * token form api
					 */
					if (null == idmsUser.getIdmsHashedToken() || idmsUser.getIdmsHashedToken().isEmpty()) {
						idmsUser.setIdmsHashedToken(ChinaIdmsUtil.generateHashValue(confirmRequest.getPinCode()));
					}

					IFWUser ifwUser = mapper.map(idmsUser, IFWUser.class);
					// ifwUser.setIdmsHashedToken(ChinaIdmsUtil.generateHashValue(confirmRequest.getPinCode()));
					LOGGER.info("IDMSUser : " + objMapper.writeValueAsString(ifwUser));
					// creating the user
					CreateUserRequest createUserRequest = new CreateUserRequest();
					createUserRequest.setUserRecord(ifwUser);
					Response userRegistrationResponse = userRegistration("", "", createUserRequest);
					if (200 == userRegistrationResponse.getStatus()) {
						// confirm the user
						ConfirmPinRequest confirmPinRequest = new ConfirmPinRequest();
						confirmPinRequest.setId(uniqueIdentifier);
						confirmPinRequest.setIDMS_Email_opt_in__c(confirmRequest.getIDMS_Email_opt_in__c());
						confirmPinRequest.setIDMS_Federated_ID__c(uniqueIdentifier);
						confirmPinRequest.setIDMS_Profile_update_source(confirmRequest.getIDMS_Profile_update_source());
						confirmPinRequest.setOperation(confirmRequest.getOperation());
						confirmPinRequest.setPassword(confirmRequest.getPassword());
						confirmPinRequest.setPinCode(confirmRequest.getPinCode());
						confirmPinRequest.setTncFlag(confirmRequest.getTncFlag());
						confirmPinRequest.setUIFlag("true");
						userPinConfirmation(confirmPinRequest);
					} else {
						return userRegistrationResponse;
					}

				} catch (IOException e1) {
					LOGGER.error("IOException in userPinConfirmation()-> " + e1.getMessage(),e1);
				}

				// PRM success response
				Attributes attributes = new Attributes();
				IDMSUserRecord idmsUserRecord = new IDMSUserRecord();
				idmsUserRecord.setAttributes(attributes);
				idmsUserRecord.setId(uniqueIdentifier);
				idmsUserRecord.setIDMS_Federated_ID__c(uniqueIdentifier);

				PasswordRecoveryResponse passwordRecoveryResponse = new PasswordRecoveryResponse(idmsUserRecord);
				passwordRecoveryResponse.setStatus(successStatus);
				passwordRecoveryResponse.setMessage("PIN validated Successfully");

				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by userPinConfirmation() : " + elapsedTime);
				return Response.status(Response.Status.OK).entity(passwordRecoveryResponse).build();
			}
			response.setStatus(errorStatus);
			response.setMessage("404 Not Found");
			response.setId(uniqueIdentifier);
			response.setFederation_Id(uniqueIdentifier);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by userPinConfirmation() : " + elapsedTime);
			LOGGER.error("Error in userPinConfirmation -> " + e.getMessage(),e);
			//productService.sessionLogout(UserConstants.IPLANET_DIRECTORY_PRO+iPlanetDirectoryKey, "logout");
			return Response.status(Response.Status.NOT_FOUND).entity(response).build();
		}

		catch (Exception e) {
			LOGGER.error("Error is " + e.getMessage(),e);
			LOGGER.info("userPinConfirmation() User ID:"+uniqueIdentifier+"Federation_Id:"+uniqueIdentifier);
			boolean connectionError=false;
			if(e.getMessage().contains(UserConstants.PIN_CONFIRMATION_ERROR_CODE)){
				errorResponse.setId(uniqueIdentifier);
				errorResponse.setStatus(errorStatus);
				errorResponse.setMessage(UserConstants.PIN_CONFIRMATION_ERROR);
				connectionError=true;
			}
			else{
				response.setStatus(errorStatus);
				response.setMessage(UserConstants.SERVER_ERROR);
				response.setId(uniqueIdentifier);
				response.setFederation_Id(uniqueIdentifier);
			}
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by userPinConfirmation() : " + elapsedTime);
			//return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(response).build();
			if(connectionError)
				return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(errorResponse).build();
			else
				return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(response).build();
		}

		Attributes attributes = new Attributes();
		IDMSUserRecord idmsUserRecord = new IDMSUserRecord();
		idmsUserRecord.setAttributes(attributes);
		idmsUserRecord.setId(uniqueIdentifier);
		idmsUserRecord.setIDMS_Federated_ID__c(uniqueIdentifier);

		PasswordRecoveryResponse passwordRecoveryResponse = new PasswordRecoveryResponse(idmsUserRecord);
		passwordRecoveryResponse.setStatus(successStatus);
		passwordRecoveryResponse.setMessage("PIN validated Successfully");

		elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		LOGGER.info("Time taken by userPinConfirmation() : " + elapsedTime);
		return Response.status(Response.Status.OK).entity(passwordRecoveryResponse).build();
	}

	/**
	 * Update the AIL Record
	 * 
	 *
	 */

	@Override
	public Response updateAIL(String authorizedToken, String clientId, String clientSecret, AILRequest ailRequest) {
		LOGGER.info("Entered updateAIL() -> Start");
		// LOGGER.info("Parameter clientId -> "+clientId+" ,clientSecret ->
		// "+clientSecret);
		String IDMSAil__c = "";
		String userData = "";
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		String idmsAclType_c = null;
		String userId = null;
		String openamVnew = null;
		String iPlanetDirectoryKey = null;
		String userName = "";
		Integer vNewCntValue = 0;
		List<String> listOfAil_c = null;
		String PRODUCT_JSON_STRING = "";
		String usermail = "";
		// Validate Input Paramenters
		ErrorResponse errorResponse = new ErrorResponse();
		ObjectMapper objMapper = new ObjectMapper();
		//List<String> accssControlList = Arrays.asList(maintenanceModeGlobal.split(","));
		List<String> accssControlList =null;
		boolean maintenanceMode=false;
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		try {
			LOGGER.info("UserServiceImpl:updateAIL -> : Requset :  -> " + objMapper.writeValueAsString(ailRequest));
			LOGGER.info("Access Control List:"+maintenanceModeGlobal);
			if(maintenanceModeGlobal!=null)
				accssControlList = Arrays.asList(maintenanceModeGlobal.split(","));
			if(accssControlList!=null && accssControlList.size()>0 && !(accssControlList.contains("False"))){
			//if(accssControlList!=null && accssControlList.size()>0){//Through error if maintenance mode is enabled
				if(accssControlList.contains(UserConstants.MAINTENANCE_MODE_COMPLETE) || accssControlList.contains(UserConstants.MAINTENANCE_MODE_AIL_UPDATE) ){
					errorResponse.setStatus(errorStatus);
					errorResponse.setMessage(UserConstants.MAINTENANCE_MODE_MESSAGE);
					LOGGER.error("Error :: Maintenance mode in progress");
					maintenanceMode=true;
					//return Response.status(Response.Status.SERVICE_UNAVAILABLE).entity(errorResponse).build();
				 }
			//}
			//Consider  exclusions for maintenance mode as below
			if(maintenanceMode){
				maintenanceMode = excludeMaintenanceMode(ailRequest.getUserAILRecord().getIDMS_Profile_update_source__c(),  UserConstants.MAINTENANCE_MODE_AIL_UPDATE);
			}
			if(maintenanceMode){
				return Response.status(Response.Status.SERVICE_UNAVAILABLE).entity(errorResponse).build();
			}
		  }
			// Profile Update Source
			if (null == ailRequest.getUserAILRecord().getIDMS_Profile_update_source__c()
					|| ailRequest.getUserAILRecord().getIDMS_Profile_update_source__c().isEmpty()) {
				userResponse.setStatus(errorStatus);
				userResponse.setMessage(UserConstants.MANDATORY_PROFILE_UPDATE_SOURCE);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error is " + userResponse.getMessage());
				LOGGER.info("Time taken by updateAIL() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
			}

			// Admin token check
			if (null == authorizedToken || authorizedToken.isEmpty()) {
				userResponse.setStatus(errorStatus);
				userResponse.setMessage(UserConstants.ADMIN_TOKEN_MANDATORY);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error is " + userResponse.getMessage());
				LOGGER.info("Time taken by updateAIL() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
			}

			// UID
			if (null != ailRequest.getUserAILRecord().getIDMS_Profile_update_source__c() && !UserConstants.UIMS
					.equalsIgnoreCase(ailRequest.getUserAILRecord().getIDMS_Profile_update_source__c())) {
				if ((null == ailRequest.getUserAILRecord().getIDMS_Federated_ID__c()
						|| ailRequest.getUserAILRecord().getIDMS_Federated_ID__c().isEmpty())
						&& (null == ailRequest.getUserAILRecord().getIDMSUser__c()
								|| ailRequest.getUserAILRecord().getIDMSUser__c().isEmpty())) {
					userResponse.setStatus(errorStatus);
					userResponse.setMessage(UserConstants.MANDATORY_ID);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.error("Error is " + userResponse.getMessage());
					LOGGER.info("Time taken by updateAIL() : " + elapsedTime);
					return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
				}
			}

			// FedrationID
			if (null != ailRequest.getUserAILRecord().getIDMS_Profile_update_source__c() && UserConstants.UIMS
					.equalsIgnoreCase(ailRequest.getUserAILRecord().getIDMS_Profile_update_source__c())) {
				if (null == ailRequest.getUserAILRecord().getIDMS_Federated_ID__c()
						|| ailRequest.getUserAILRecord().getIDMS_Federated_ID__c().isEmpty()) {
					userResponse.setStatus(errorStatus);
					userResponse.setMessage(UserConstants.MANDATORY_FEDERATION_ID);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.error("Error is " + userResponse.getMessage());
					LOGGER.info("Time taken by updateAIL() : " + elapsedTime);
					return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
				}

				if (null == clientId || null == clientSecret) {
					userResponse.setStatus(errorStatus);
					userResponse.setMessage(UserConstants.UIMS_CLIENTID_SECRET);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.error("Error is " + userResponse.getMessage());
					LOGGER.info("Time taken by updateAIL() : " + elapsedTime);
					return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
				}

				if ((null != clientId && !clientId.equalsIgnoreCase(uimsClientId))
						|| (null != clientSecret && !clientSecret.equalsIgnoreCase(uimsClientSecret))) {
					userResponse.setStatus(errorStatus);
					userResponse.setMessage(UserConstants.INVALID_UIMS_CREDENTIALS);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.error("Error is " + userResponse.getMessage());
					LOGGER.info("Time taken by updateAIL() : " + elapsedTime);
					return Response.status(Response.Status.UNAUTHORIZED).entity(userResponse).build();
				}
			}

			// IDMSAclType__c
			if (null == ailRequest.getUserAILRecord().getIDMSAclType__c()
					|| ailRequest.getUserAILRecord().getIDMSAclType__c().isEmpty()
					|| (!pickListValidator.validate(UserConstants.IDMS_ACL_TYPE_C,
							ailRequest.getUserAILRecord().getIDMSAclType__c()))) {
				userResponse.setStatus(errorStatus);
				userResponse.setMessage(UserConstants.INVALID_ACL_TYPE);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error is " + userResponse.getMessage());
				LOGGER.info("Time taken by updateAIL() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
			}

			// IDMSAcl__c
			if (null == ailRequest.getUserAILRecord().getIDMSAcl__c()
					|| ailRequest.getUserAILRecord().getIDMSAcl__c().isEmpty()) {
				userResponse.setStatus(errorStatus);
				userResponse.setMessage(UserConstants.MANDATORY_ACL);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error is " + userResponse.getMessage());
				LOGGER.info("Time taken by updateAIL() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
			}

			// Operation
			if (null == ailRequest.getUserAILRecord().getIDMSOperation__c()
					|| ailRequest.getUserAILRecord().getIDMSOperation__c().isEmpty()
					|| (!pickListValidator.validate(UserConstants.IDMS_OPERATION_C,
							ailRequest.getUserAILRecord().getIDMSOperation__c()))) {
				userResponse.setStatus(errorStatus);
				userResponse.setMessage(UserConstants.INVALID_OPERATION);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error is " + userResponse.getMessage());
				LOGGER.info("Time taken by updateAIL() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
			}

			/**
			 * Disabling the below condition as part of R5 Release changes
			 * 
			 */
			// Update source and ACL value have to be equal
			/*
			 * if (null !=
			 * ailRequest.getUserAILRecord().getIDMS_Profile_update_source__c()
			 * && !UserConstants.UIMS
			 * .equalsIgnoreCase(ailRequest.getUserAILRecord().
			 * getIDMS_Profile_update_source__c())) { if
			 * (!ailRequest.getUserAILRecord().getIDMSAcl__c()
			 * .equals(ailRequest.getUserAILRecord().
			 * getIDMS_Profile_update_source__c())) {
			 * 
			 * ErrorResponse errorResponse = new ErrorResponse();
			 * errorResponse.setStatus(errorStatus); errorResponse.setMessage(
			 * " Update source and ACL value are different "); elapsedTime =
			 * UserConstants.TIME_IN_MILLI_SECONDS - startTime; LOGGER.info(
			 * "Time taken by UserServiceImpl.updateAIL() : " + elapsedTime);
			 * return
			 * Response.status(Response.Status.BAD_REQUEST.getStatusCode()).
			 * entity(errorResponse).build();
			 * 
			 * } }
			 */

			if (!getTechnicalUserDetails(authorizedToken)) {
				userResponse.setStatus(errorStatus);
				userResponse.setMessage("Unauthorized or session expired");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error is " + userResponse.getMessage());
				LOGGER.info("Time taken by updateAIL() : " + elapsedTime);
				return Response.status(Response.Status.UNAUTHORIZED).entity(userResponse).build();
			}

			idmsAclType_c = getIDMSAclType(ailRequest.getUserAILRecord().getIDMSAclType__c());
			LOGGER.info("AIL type = " + idmsAclType_c);
			// Getting the user data
			// LOGGER.info("Going to call getSSOToken()");
			try {
				iPlanetDirectoryKey = getSSOToken();
			} catch (IOException ioExp) {
				LOGGER.error("Unable to get SSO Token " + ioExp.getMessage(),ioExp);
				iPlanetDirectoryKey = "";
			}
			// LOGGER.info("call getSSOToken() finished");

			if (null != ailRequest.getUserAILRecord().getIDMS_Profile_update_source__c() && UserConstants.UIMS
					.equalsIgnoreCase(ailRequest.getUserAILRecord().getIDMS_Profile_update_source__c())) {

				// LOGGER.info("Going to call
				// checkUserExistsWithFederationID()");
				Response fedResponse = checkUserExistsWithFederationID(iPlanetDirectoryKey,
						ailRequest.getUserAILRecord().getIDMS_Federated_ID__c(), startTime);
				// LOGGER.info("checkUserExistsWithFederationID() finished");
				if (fedResponse.getStatus() == 200) {
					JSONObject uimsResponse = (JSONObject) fedResponse.getEntity();
					userId = (String) uimsResponse.get("userId");
				} else {
					return fedResponse;
				}
			} else {
				userId = ailRequest.getUserAILRecord().getIDMSUser__c();
				if (null == userId) {
					userId = ailRequest.getUserAILRecord().getIDMS_Federated_ID__c();
				}
			}

			if (null != userId) {
				LOGGER.info("AUDIT:requestingUser->" + userId + "," + "impersonatingUser : amadmin,"
						+ "openAMApi:GET/getUser/{userId}");

				LOGGER.info("Start: getUser() of OpenAMService for userId=" + userId);
				userData = productService.getUser(iPlanetDirectoryKey, userId);
				LOGGER.info("End: getUser() of OpenAMService finished for userId=" + userId);
				// LOGGER.info("productService.getUser : Response ->
				// "+userData);
			}
			//Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();//Senthil
			DocumentContext productDocCtx = JsonPath.using(conf).parse(userData);
			// LOGGER.info("SSOTOKEN--------------------------->" +
			// iPlanetDirectoryKey);
			LOGGER.info("productDocCtx in updateAil=" + ChinaIdmsUtil.printOpenAMInfo(productDocCtx.jsonString()));
			IDMSAil__c = productDocCtx.read("$.IDMSAil_c[0]");

			// IDMSAil__c = IDMSAil__c.replace("\"", "");
			// LOGGER.info("1st var IDMSAil_c" + IDMSAil__c);

			if (null != IDMSAil__c) {
				listOfAil_c = Arrays.asList(IDMSAil__c.replaceAll("[\\(\\)\\[\\]\\{\\}]", "").split(","));
			} else {
				listOfAil_c = new ArrayList<String>();
			}

			usermail = productDocCtx.read("$.mail[0]");

			// Updating the IDMSAil__c attribute based on the provided operation
			if ((!listOfAil_c.contains(ailRequest.getUserAILRecord().getIDMSAclType__c() + ";"
					+ ailRequest.getUserAILRecord().getIDMSAcl__c()))
					&& ("GRANT".equalsIgnoreCase(ailRequest.getUserAILRecord().getIDMSOperation__c()))) {
				String aclType_c = productDocCtx.read("$.IDMSAIL_" + idmsAclType_c + "_c[0]");
				/*
				 * LOGGER.info("2nd Var ------->" + idmsAclType_c + "--------->"
				 * + aclType_c); aclType_c = aclType_c.substring(0,
				 * aclType_c.length() - 1); aclType_c = aclType_c.substring(1);
				 * aclType_c = aclType_c.replace("\"", ""); aclType_c =
				 * aclType_c.replaceAll("\\[", ""); aclType_c =
				 * aclType_c.replaceAll("\\]", "");
				 */
				// Checking the value does not contain null value
				if (!(aclType_c == null || aclType_c.length() == 0))
					aclType_c = aclType_c + "," + ailRequest.getUserAILRecord().getIDMSAcl__c();
				else
					aclType_c = ailRequest.getUserAILRecord().getIDMSAcl__c();
				// aclType_c = "[" + aclType_c + "]";
				PRODUCT_JSON_STRING = "{" + "\"IDMSAIL_" + idmsAclType_c + "_c\": \"" + aclType_c.trim() + "\"" + "}";
				LOGGER.info("AUDIT:requestingUser->" + userId + "," + "impersonatingUser : amadmin,"
						+ "openAMApi:GET/updateUser/{userId}");
				LOGGER.info("Grant Operation: updateAIL : Request -> " + PRODUCT_JSON_STRING);
				LOGGER.info("Start: updateUser() of OpenAMService for userId=" + userId);
				productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userId,
						PRODUCT_JSON_STRING);
				LOGGER.info("End: updateUser() of OpenAMService finished for userId=" + userId);

				/*
				 * String tempString = IDMSAil__c.substring(0,
				 * IDMSAil__c.length() - 2); tempString =
				 * tempString.replaceAll("\\[", "");
				 */// LOGGER.info("tempString---------->" + tempString);
					// tempString=tempString.substring(1);
				if (null != IDMSAil__c && !IDMSAil__c.isEmpty())
					IDMSAil__c = "" + IDMSAil__c + ",(" + ailRequest.getUserAILRecord().getIDMSAclType__c() + ";"
							+ ailRequest.getUserAILRecord().getIDMSAcl__c() + ")";
				else
					IDMSAil__c = "(" + ailRequest.getUserAILRecord().getIDMSAclType__c() + ";"
							+ ailRequest.getUserAILRecord().getIDMSAcl__c() + ")";

				// Update the IDMSAil__c in OpenAm
				PRODUCT_JSON_STRING = "{" + "\"IDMSAil_c\": \"" + IDMSAil__c.trim() + "\"" + "}";
				LOGGER.info("AUDIT:requestingUser->" + userId + "," + "impersonatingUser : amadmin,"
						+ "openAMApi:GET/getUser/{userId}");
				LOGGER.info("updateAIL : Request -> " + PRODUCT_JSON_STRING);
				LOGGER.info("Start: updateUser() of OpenAMService for userId=" + userId);
				productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userId,
						PRODUCT_JSON_STRING);
				LOGGER.info("End: updateUser() of OpenAMService finished for userId=" + userId);
				LOGGER.info("IDMSAil__c Modified After Grant Operation -------------->" + IDMSAil__c);
			} else if ((listOfAil_c.contains(ailRequest.getUserAILRecord().getIDMSAclType__c() + ";"
					+ ailRequest.getUserAILRecord().getIDMSAcl__c()))
					&& ("REVOKE".equalsIgnoreCase(ailRequest.getUserAILRecord().getIDMSOperation__c()))) {
				IDMSAil__c = productDocCtx.read("$.IDMSAil_c[0]");
				IDMSAil__c = IDMSAil__c.replaceAll("\\[", "");
				IDMSAil__c = IDMSAil__c.replaceAll("\\]", "");
				String[] ailParts = IDMSAil__c.split(",");
				IDMSAil__c = "";
				for (String pair : ailParts) {
					pair = pair.replace("\"", "");
					String revokepair = "(" + ailRequest.getUserAILRecord().getIDMSAclType__c() + ";"
							+ ailRequest.getUserAILRecord().getIDMSAcl__c() + ")";
					if (!pair.equalsIgnoreCase(revokepair)) {
						IDMSAil__c = IDMSAil__c + pair + ",";
					}
				}
				IDMSAil__c = IDMSAil__c.replace("\"", "");
				if (!(IDMSAil__c == null || IDMSAil__c.length() == 0))
					IDMSAil__c = IDMSAil__c.substring(0, IDMSAil__c.length() - 1);
				// IDMSAil__c = "[" + IDMSAil__c + "]";
				String aclType = productDocCtx.read("$.IDMSAIL_" + idmsAclType_c + "_c[0]");
				/*
				 * aclType = aclType.substring(0, aclType.length() - 1); aclType
				 * = aclType.substring(1); aclType = aclType.replace("\"", "");
				 * aclType = aclType.replaceAll("\\[", ""); aclType =
				 * aclType.replaceAll("\\]", "");
				 */
				ailParts = aclType.split(",");
				aclType = "";
				for (String pair : ailParts) {
					String revokepair = ailRequest.getUserAILRecord().getIDMSAcl__c();
					if (!pair.equalsIgnoreCase(revokepair)) {
						aclType = aclType + pair + ",";
					}
				}

				if (!(aclType == null || aclType.length() == 0)) {
					aclType = aclType.substring(0, aclType.length() - 1);
					aclType = aclType.replaceAll("\\[", "");
					aclType = aclType.replaceAll("\\]", "");
				}
				// aclType = "[" + aclType + "]";

				/*
				 * if(null == aclType || aclType.isEmpty()) { aclType = "[]";
				 * }else{ aclType = aclType.trim(); }
				 */
				/*
				 * PRODUCT_JSON_STRING = "{" + "\"IDMSAIL_" + idmsAclType_c +
				 * "_c\": \"" + aclType + "\"" + "}";
				 */

				if (null == aclType || aclType.isEmpty()) {
					PRODUCT_JSON_STRING = "{" + "\"IDMSAIL_" + idmsAclType_c + "_c\":".concat("[]}");
				} else {
					PRODUCT_JSON_STRING = "{" + "\"IDMSAIL_" + idmsAclType_c + "_c\": \"" + aclType + "\"" + "}";
				}
				LOGGER.info("AUDIT:requestingUser->" + userId + "," + "impersonatingUser : amadmin,"
						+ "openAMApi:GET/getUser/{userId}");
				LOGGER.info("Revoke Operation: productService.updateAIL : Request -> " + PRODUCT_JSON_STRING);
				LOGGER.info("Start: updateUser() of OpenAMService for userId=" + userId);
				productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userId,
						PRODUCT_JSON_STRING);
				LOGGER.info("End: updateUser() of OpenAMService finished for userId=" + userId);

				// Update the IDMSAil__c in OpenAm
				if (null == IDMSAil__c || IDMSAil__c.isEmpty()) {
					PRODUCT_JSON_STRING = "{" + "\"IDMSAil_c\":".concat("[]}");
				} else {
					PRODUCT_JSON_STRING = "{" + "\"IDMSAil_c\": \"" + IDMSAil__c.trim() + "\"" + "}";
				}
				LOGGER.info("AUDIT:requestingUser->" + userId + "," + "impersonatingUser : amadmin,"
						+ "openAMApi:GET/getUser/{userId}");
				LOGGER.info("Revoke Operation -> : productService.updateAIL : Request -> " + PRODUCT_JSON_STRING);
				LOGGER.info("Start: updateUser() of OpenAMService for userId=" + userId);
				productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userId,
						PRODUCT_JSON_STRING);
				LOGGER.info("End: updateUser() of OpenAMService finished for userId=" + userId);
			}

			// Building the Response
			Attributes idmsUser_rAttributes = new Attributes();
			IDMSUser__r idmsUser__r = new IDMSUser__r();
			idmsUser__r.setAttributes(idmsUser_rAttributes);
			idmsUser__r.setId(userId);
			idmsUser__r.setIDMS_Federated_ID__c(userId);

			IDMSUserAIL idmsUserAIL = new IDMSUserAIL(idmsUser__r);
			Attributes attributes = new Attributes();
			attributes.setType("IDMSUserAIL__c");
			idmsUserAIL.setAttributes(attributes);
			idmsUserAIL.setId(userId);
			idmsUserAIL.setIDMS_Federated_Id__c(userId);
			idmsUserAIL
					.setIdms_Profile_update_source__c(ailRequest.getUserAILRecord().getIDMS_Profile_update_source__c());
			idmsUserAIL.setIdmsaclType__c(ailRequest.getUserAILRecord().getIDMSAclType__c());
			if ("REVOKE".equalsIgnoreCase(ailRequest.getUserAILRecord().getIDMSOperation__c()))
				idmsUserAIL.setIdmsisRevokedOperation__c(true);
			else
				idmsUserAIL.setIdmsisRevokedOperation__c(false);
			idmsUserAIL.setIdmsoperation__c(ailRequest.getUserAILRecord().getIDMSOperation__c());
			idmsUserAIL.setIdmsacl__c(ailRequest.getUserAILRecord().getIDMSAcl__c());
			idmsUserAIL.setIdmsuser__c(userId);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by updateAIL() : " + elapsedTime);
			userName = productDocCtx.read("$.result[0].username");
			openamVnew = null != productDocCtx.read("$.V_New[0]") ? getValue(productDocCtx.read("$.V_New[0]"))
					: getDelimeter();

			if (null != vNewCntValue && null != openamVnew) {
				vNewCntValue = Integer.parseInt(openamVnew) + 1;
			}
			String version = "{\"V_New\": \"" + vNewCntValue + "\"" + "}";

			// calling Async methods of UIMS api in updateUserAil IDMS api
			if (null != ailRequest.getUserAILRecord().getIDMS_Profile_update_source__c() && !UserConstants.UIMS
					.equalsIgnoreCase(ailRequest.getUserAILRecord().getIDMS_Profile_update_source__c())) {
				// Adding V_New
				LOGGER.info("UserServiceImpl:updateAIL -> Request -> " + version);
				LOGGER.info("Start: updateUser() of OpenAMService for userId=" + userId + " ,version=" + version);
				productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userId, version);
				LOGGER.info(
						"End: updateUser() of OpenAMService finished for userId=" + userId + " ,version=" + version);
				LOGGER.info("Start: updateUIMSUserAIL() of UIMSAccessManagerSoapService for usermail=" + usermail);
				uimsAccessManagerSoapService.updateUIMSUserAIL(ailRequest, idmsUserAIL, vNewCntValue.toString(),
						productService, UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, usermail);
				LOGGER.info(
						"End: updateUIMSUserAIL() of UIMSAccessManagerSoapService finished for usermail=" + usermail);
			} else {
				// productService.sessionLogout(UserConstants.IPLANET_DIRECTORY_PRO+iPlanetDirectoryKey,
				// "logout");
			}
			return updateAILSuccessResponse(idmsUserAIL);
		} catch (NotFoundException e) {
			userResponse.setStatus(errorStatus);
			userResponse.setMessage("User not found based on user Id");
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.updateAIL() : " + elapsedTime);
			LOGGER.error("Executing while updateAIL() :: -> " + userResponse.getMessage());
			LOGGER.error("Executing while updateAIL() :: -> " + e.getMessage(),e);
			return Response.status(Response.Status.NOT_FOUND).entity(userResponse).build();
		} catch (Exception e) {
			userResponse.setStatus(errorStatus);
			userResponse.setMessage("Error in Updating the AIL Object");
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.updateAIL() : " + elapsedTime);
			LOGGER.error("Executing while updateAIL() :: -> " + userResponse.getMessage());
			LOGGER.error("Executing while updateAIL() :: -> " + e.getMessage(),e);
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(userResponse).build();
		}
	}

	private Response updateAILSuccessResponse(IDMSUserAIL idmsUserAIL) {
		LOGGER.info("Entered updateAILSuccessResponse() -> Start");
		LOGGER.info("Parameter idmsUserAIL -> " + idmsUserAIL);

		AILResponse ailResponse;
		ailResponse = new AILResponse(idmsUserAIL);
		ailResponse.setStatus(successStatus);
		ailResponse.setMessage("User AIL updated successfully");
		LOGGER.info("updateAILSuccessResponse() -> End");
		return Response.status(Response.Status.OK).entity(ailResponse).build();

	}

	/**
	 * Resending email with PIN in case of forget password
	 */
	@Override
	public Response passwordRecovery(PasswordRecoveryRequest passwordRecoveryRequest) {
		LOGGER.info("Entered passwordRecovery() -> Start");

		// check email validation
		String loginIdentifier = null;
		String identifierType = null;
		boolean validMobile = false;
		boolean validEmail = false;
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		ErrorResponse errorResponse = null;
		ObjectMapper objMapper = new ObjectMapper();
		try {
			LOGGER.info("Parameter  passwordRecoveryRequest=" + objMapper.writeValueAsString(passwordRecoveryRequest));

			// validations
			if (null == passwordRecoveryRequest.getUserRecord().getIDMS_Profile_update_source__c()
					|| passwordRecoveryRequest.getUserRecord().getIDMS_Profile_update_source__c().isEmpty()
					|| (!pickListValidator.validate(UserConstants.IDMS_PROFILE_UPDATE_SOURCE,
							passwordRecoveryRequest.getUserRecord().getIDMS_Profile_update_source__c()))) {
				errorResponse = new ErrorResponse();
				errorResponse.setStatus(errorStatus);
				errorResponse.setMessage(UserConstants.INVALID_UPDATE_SOURCE);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error is " + errorResponse.getMessage());
				LOGGER.info("Time taken by UserServiceImpl.passwordRecovery() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
			}

			if ((null != passwordRecoveryRequest.getWithGlobalUsers())
					&& (!UserConstants.TRUE.equalsIgnoreCase(passwordRecoveryRequest.getWithGlobalUsers())
							&& !UserConstants.FALSE.equalsIgnoreCase(passwordRecoveryRequest.getWithGlobalUsers()))) {
				errorResponse = new ErrorResponse();
				errorResponse.setStatus(errorStatus);
				errorResponse.setMessage(UserConstants.GLOBAL_USER_BOOLEAN);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error is " + errorResponse.getMessage());
				LOGGER.info("Time taken by UserServiceImpl.checkUserExists() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
			}

			Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
			if ((null != passwordRecoveryRequest.getUserRecord().getEmail()
					&& !passwordRecoveryRequest.getUserRecord().getEmail().isEmpty())
					&& (null != passwordRecoveryRequest.getUserRecord().getMobilePhone()
							&& !passwordRecoveryRequest.getUserRecord().getMobilePhone().isEmpty())) {
				loginIdentifier = passwordRecoveryRequest.getUserRecord().getEmail();
				identifierType = UserConstants.EMAIL;
				validEmail = emailValidator.validate(passwordRecoveryRequest.getUserRecord().getEmail());

			} else if ((null != passwordRecoveryRequest.getUserRecord().getEmail())
					&& (!passwordRecoveryRequest.getUserRecord().getEmail().isEmpty())) {
				loginIdentifier = passwordRecoveryRequest.getUserRecord().getEmail();
				identifierType = UserConstants.EMAIL;
				validEmail = emailValidator.validate(passwordRecoveryRequest.getUserRecord().getEmail());

			} else if ((null != passwordRecoveryRequest.getUserRecord().getMobilePhone())
					&& (!passwordRecoveryRequest.getUserRecord().getMobilePhone().isEmpty())) {
				validMobile = legthValidator.validate(UserConstants.MOBILE_PHONE,
						passwordRecoveryRequest.getUserRecord().getMobilePhone());
				loginIdentifier = passwordRecoveryRequest.getUserRecord().getMobilePhone();
				identifierType = UserConstants.MOBILE;
			}

			// getting the source from the request
			if (validEmail && UserConstants.EMAIL.equals(identifierType)) {
				return getPasswordRecoveryResponse(UserConstants.HOTP_EMAIL_RESET_PR, loginIdentifier, conf,
						passwordRecoveryRequest, startTime, passwordRecoveryRequest.getWithGlobalUsers());
			} else if (validMobile && UserConstants.MOBILE.equals(identifierType)) {
				return getPasswordRecoveryResponse(UserConstants.HOTP_MOBILE_RESET_PR, loginIdentifier, conf,
						passwordRecoveryRequest, startTime, passwordRecoveryRequest.getWithGlobalUsers());
			} else if ((null != passwordRecoveryRequest.getUserRecord().getEmail())
					&& !("@".contains(passwordRecoveryRequest.getUserRecord().getEmail()))) {
				PasswordRecoveryResponse serviceResponse = new PasswordRecoveryResponse();
				serviceResponse.setStatus(errorStatus);
				serviceResponse.setMessage("Email should contain @");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error is" + serviceResponse.getMessage());
				LOGGER.info("Time taken by UserServiceImpl.passwordRecovery() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(serviceResponse).build();
			} else {
				// email/phone is not a valid one, returning error response
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by UserServiceImpl.passwordRecovery() : " + elapsedTime);
				return passwordRecoveryErrorResponse();
			}
		} catch (Exception e) {
			LOGGER.error("UserServiceImpl.passwordRecovery() : " + e.getMessage(),e);
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	private Response getPasswordRecoveryResponse(String hotpService, String loginIdentifier, Configuration conf,
			PasswordRecoveryRequest passwordRecoveryRequest, long startTime, String withGlobalUsers) {
		LOGGER.info("Entered getPasswordRecoveryResponse() -> Start");
		LOGGER.info("Parameter hotpService -> " + hotpService+" ,loginIdentifier -> "+loginIdentifier);
		LOGGER.info("Parameter withGlobalUsers -> "+withGlobalUsers);

		String userData = null;
		DocumentContext productDocCtx;
		String userName;
		String iPlanetDirectoryKey = null;
		long elapsedTime;
		String ifwAccessToken = null;
		JSONObject response = new JSONObject();
		ObjectMapper objMapper = new ObjectMapper();
		try {
			LOGGER.info(
					"Parameter  passwordRecoveryRequest -> " + objMapper.writeValueAsString(passwordRecoveryRequest));

			// LOGGER.info("calling getSSOToken()");
			try {
				iPlanetDirectoryKey = getSSOToken();
			} catch (IOException ioExp) {
				LOGGER.error("Unable to get SSO Token" + ioExp.getMessage(),ioExp);
				iPlanetDirectoryKey = "";
			}
			// LOGGER.info("getSSOToken() finished");
			LOGGER.info(AUDIT_REQUESTING_USER + AUDIT_TECHNICAL_USER + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN
					+ AUDIT_OPENAM_API + AUDIT_OPENAM_USER_EXISTS_CALL + loginIdentifier + AUDIT_LOG_CLOSURE);
			LOGGER.info(
					"Start: checkUserExistsWithEmailMobile() of OpenAMService for loginIdentifier=" + loginIdentifier);
			String userExists = productService.checkUserExistsWithEmailMobile(
					UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
					"mail eq " + "\"" + URLEncoder.encode(URLDecoder.decode(loginIdentifier, "UTF-8"), "UTF-8")
							+ "\" or mobile_reg eq " + "\""
							+ URLEncoder.encode(URLDecoder.decode(loginIdentifier, "UTF-8"), "UTF-8") + "\"");
			LOGGER.info("End: checkUserExistsWithEmailMobile() of OpenAMService finished for loginIdentifier="
					+ loginIdentifier);
			productDocCtx = JsonPath.using(conf).parse(userExists);
			Integer resultCount = productDocCtx.read(JsonConstants.RESULT_COUNT);
			userName = productDocCtx.read("$.result[0].username");

			// user exists and resultcount > 0
			LOGGER.info("resultCount=" + resultCount);
			LOGGER.info("userName=" + userName);
			if (resultCount.intValue() > 0) {
				String otp = sendEmail.generateOtp(userName);
				LOGGER.info("Successfully OTP generated for " + userName);
				if (UserConstants.HOTP_EMAIL_RESET_PR.equalsIgnoreCase(hotpService)) {
					sendEmail.sendOpenAmEmail(otp, EmailConstants.SETUSERPWD_OPT_TYPE, userName,
							passwordRecoveryRequest.getUserRecord().getIDMS_Profile_update_source__c());
				} else if (UserConstants.HOTP_MOBILE_RESET_PR.equalsIgnoreCase(hotpService)) {
					sendEmail.sendOpenAmMobileEmail(otp, EmailConstants.SETUSERPWD_OPT_TYPE, userName,
							passwordRecoveryRequest.getUserRecord().getIDMS_Profile_update_source__c());
					sendEmail.sendSMSNewGateway(otp, EmailConstants.SETUSERPWD_OPT_TYPE, userName,
							passwordRecoveryRequest.getUserRecord().getIDMS_Profile_update_source__c());
				}
			} else if (resultCount.intValue() < 1) {
				if (UserConstants.TRUE.equalsIgnoreCase(withGlobalUsers)) {

					LOGGER.info("Start: getIFWToken() of IFWService");
					ifwAccessToken = ifwService.getIFWToken(UserConstants.CONTENT_TYPE_URL_FROM,
							UserConstants.IFW_GRANT_TYPE, ifwClientId, ifwClientSecret);
					LOGGER.info("End: getIFWToken() of IFWService finished");

					productDocCtx = JsonPath.using(conf).parse(ifwAccessToken);
					String accessToken = productDocCtx.read("$.access_token");
					String bfoAuthorizationToken = sfSyncServiceImpl.getSFToken();
					String authorization = "Bearer " + accessToken;
					
					PasswordRecoveryUser input = mapper.map(passwordRecoveryRequest,
							PasswordRecoveryUser.class);
					objMapper = new ObjectMapper();
					String json = objMapper.writeValueAsString(input);

					LOGGER.info("Start: initiatePasswordRecovery() of IFWService");
					String ifwResponse = ifwService.initiatePasswordRecovery(UserConstants.ACCEPT_TYPE_APP_JSON, bfoAuthorizationToken,
							UserConstants.APPLICATION_NAME, UserConstants.COUNTRY_CODE, UserConstants.LANGUAGE_CODE,
							UserConstants.REQUEST_ID, authorization, UserConstants.ACCEPT_TYPE_APP_JSON,
							UserConstants.FALSE, json);
					LOGGER.info("End: initiatePasswordRecovery() of IFWService finished");

					response.put(UserConstants.STATUS, successStatus);
					response.put(UserConstants.MESSAGE, UserConstants.RESET_PR_SUCCESS);
					response.put("IDMSUserRecord", null);
					return Response.status(Response.Status.OK).entity(response).build();

				} else {
					response.put(UserConstants.STATUS, errorStatus);
					response.put(UserConstants.MESSAGE, UserConstants.USER_NOT_FOUND_EMAIL_MOBILE);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.error("Error is -> " + UserConstants.USER_NOT_FOUND_EMAIL_MOBILE);
					LOGGER.info("Time taken by UserServiceImpl.passwordRecovery() : " + elapsedTime);
					return Response.status(Response.Status.NOT_FOUND).entity(response).build();
				}
			}
			userData = productService.getUser(iPlanetDirectoryKey, userName);
			LOGGER.info("getPasswordRecoveryResponse -> " + ChinaIdmsUtil.printOpenAMInfo(userData));
		} catch (BadRequestException e) {
			response.put(UserConstants.STATUS, errorStatus);
			response.put(UserConstants.MESSAGE, UserConstants.USER_NOT_FOUND);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.passwordRecovery() : " + elapsedTime);
			LOGGER.error("BadRequestException in getPasswordRecoveryResponse() :: -> " + e.getMessage(),e);
			return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
		} catch (NotAuthorizedException e) {
			response.put(UserConstants.STATUS, errorStatus);
			response.put(UserConstants.MESSAGE, UserConstants.USER_NOT_FOUND);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.passwordRecovery() : " + elapsedTime);
			LOGGER.error("NotAuthorizedException in getPasswordRecoveryResponse() :: -> " + e.getMessage(),e);
			return Response.status(Response.Status.UNAUTHORIZED).entity(response).build();
		} catch (NotFoundException e) {
			response.put(UserConstants.STATUS, errorStatus);
			response.put(UserConstants.MESSAGE, UserConstants.USER_NOT_FOUND);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.passwordRecovery() : " + elapsedTime);
			LOGGER.error("NotFoundException in getPasswordRecoveryResponse() :: -> " + e.getMessage(),e);
			return Response.status(Response.Status.NOT_FOUND).entity(response).build();
		} catch (Exception e) {
			response.put(UserConstants.STATUS, errorStatus);
			response.put(UserConstants.MESSAGE, UserConstants.ERROR_RESET_PR);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.passwordRecovery() : " + elapsedTime);
			LOGGER.error("Exception in getPasswordRecoveryResponse() :: -> " + e.getMessage(),e);
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(response).build();
		}
		return passwordRecoverySuccessResponse(userName, startTime, userData);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.idms.service.UserServiceImpl#updateUser(java.lang.String,
	 * java.lang.String, java.lang.String, com.idms.model.UpdateUserRequest)
	 */
	@SuppressWarnings("unchecked")
	public Response updateUser(String authorizedToken, String clientId, String clientSecret,
			UpdateUserRequest userRequest) {
		LOGGER.info("Entered updateUser() -> Start");
		LOGGER.info("Parameter authorizedToken -> " + authorizedToken);

		UpdateUserResponse sucessRespone = null;
		String userName = null;
		String iPlanetDirectoryKey = null;
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		ObjectMapper objMapper = new ObjectMapper();
		userResponse.setStatus(errorStatus);
		String companyFedIdInRequest = null;
		boolean updateMobileIdentifierCheck = false;

		try {
			LOGGER.info("updateUser -> : Request -> " + objMapper.writeValueAsString(userRequest));

			Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
			DocumentContext productDocCtx = null;
			DocumentContext productDocCtxUser = null;
			String userId = null;
			String hotpService = null;
			String identifierType = null;
			String jsonRequset = null;
			String jsonResponse = null;
			OpenAmUserRequest openAmReq = null;
			String userData = null;
			String updatingUser = null;
			boolean userUpdateforSameUser = false;
			IFWUser userRecord = null;
			String loginIdentifier = null;
			OpenAmUser user = new OpenAmUser();
			StringBuilder contentBuilder = null;
			String firstName = null;
			boolean booleanTrue = true;
			String fedId = null;
			Integer vNewCntValue = 0;
			String companyFedIdInOpenAM = "";
			String usermail = "";
			boolean isUserFromSocialLogin = false;
			String attributeText = null;
			JSONObject responseCheck = new JSONObject();
			DocumentContext  productDJData = null;
			String emailUserNameFormat = null;
			boolean maintenanceMode=false;
			ErrorResponse errorResponse = new ErrorResponse();
			//List<String> accssControlList = Arrays.asList(maintenanceModeGlobal.split(","));
			List<String> accssControlList =null;
			// Step 1:

			// LOGGER.info(" UserServiceImpl :: updateUser
			// getUserInfoByAccessToken ");

			/**
			 * Check mandatory values and user type (home/work)
			 */

			try {
				LOGGER.info("Access Control List:"+maintenanceModeGlobal);
				if(maintenanceModeGlobal!=null)
					accssControlList = Arrays.asList(maintenanceModeGlobal.split(","));
				if(accssControlList!=null && accssControlList.size()>0 && !(accssControlList.contains("False"))){
				//if(accssControlList!=null &&accssControlList.size()>0){//Through error if maintenance mode is enabled
					if(accssControlList.contains(UserConstants.MAINTENANCE_MODE_COMPLETE) || accssControlList.contains(UserConstants.MAINTENANCE_MODE_PROFILE_UPDATE) ){
						errorResponse.setStatus(errorStatus);
						errorResponse.setMessage(UserConstants.MAINTENANCE_MODE_MESSAGE);
						LOGGER.error("Error :: Maintenance mode in progress");
						maintenanceMode=true;
						//return Response.status(Response.Status.SERVICE_UNAVAILABLE).entity(errorResponse).build();
					 }
				//}
				//Consider  exclusions for maintenance mode as below
				if(maintenanceMode){
					maintenanceMode = excludeMaintenanceMode(userRequest.getUserRecord().getIDMS_Profile_update_source__c(),  UserConstants.MAINTENANCE_MODE_PROFILE_UPDATE);
				}
				if(maintenanceMode){
					return Response.status(Response.Status.SERVICE_UNAVAILABLE).entity(errorResponse).build();
				}
			}
				/**
				 * Get iPlanetDirectory Pro Admin token for admin
				 */
				// LOGGER.info(" UserServiceImpl :: updateUser getSSOToken ");
				try {
					iPlanetDirectoryKey = getSSOToken();
				} catch (IOException ioExp) {
					LOGGER.error("Unable to get SSO Token" + ioExp.getMessage(),ioExp);
					iPlanetDirectoryKey = "";
				}

				if ((null != userRequest.getUserRecord().getIDMSAnnualRevenue__c())
						&& (userRequest.getUserRecord().getIDMSAnnualRevenue__c().matches("^\\D+$") == true)) {
					userResponse.setStatus(errorStatus);
					userResponse.setMessage(UserConstants.INCORRECT_REVENUE);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.error("Error is " + userResponse.getMessage());
					LOGGER.info("Time taken by updateUser() : " + elapsedTime);
					if(UserConstants.UIMS
							.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Profile_update_source__c())){
						return handleUIMSError(Response.Status.BAD_REQUEST, UserConstants.INCORRECT_REVENUE);
					}
					return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
				}

				if (checkMandatoryFieldsFromRequest(userRequest.getUserRecord(), userResponse, false)) {
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by updateUser() : " + elapsedTime);
					if(UserConstants.UIMS
							.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Profile_update_source__c())){
						return handleUIMSError(Response.Status.BAD_REQUEST, userResponse.getMessage());
					}
					return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
				}
			} catch (Exception e) {
				userResponse.setMessage(UserConstants.ATTRIBUTE_NOT_AVAILABELE);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by updateUser() : " + elapsedTime);
				LOGGER.error("Exception in updateUser()->" + userResponse.getMessage());
				// productService.sessionLogout(UserConstants.IPLANET_DIRECTORY_PRO+iPlanetDirectoryKey,
				// "logout");
				if(UserConstants.UIMS
						.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Profile_update_source__c())){
					return handleUIMSError(Response.Status.BAD_REQUEST, UserConstants.ATTRIBUTE_NOT_AVAILABELE);
				}
				return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
			}

			// Need to check do we need pass the user FirstName
			// sendEmail.emailReadyToSendEmail("Suresh.Bachu@non.schneider-electric.com",
			// "bsuresh.infi@gmail.com", "EMAIL CHANGE NOTIFICATION",
			// UserConstants.EMAIL_BODY);

			if (null != userRequest.getUserRecord().getIDMS_Profile_update_source__c() && UserConstants.UIMS
					.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Profile_update_source__c())) {

				if (null == clientId || null == clientSecret) {
					userResponse.setStatus(errorStatus);
					userResponse.setMessage(UserConstants.UIMS_CLIENTID_SECRET);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.error("Error in updateUser()-> " + userResponse.getMessage());
					LOGGER.info("Time taken by updateUser() : " + elapsedTime);
					//return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
					return handleUIMSError(Response.Status.BAD_REQUEST,UserConstants.UIMS_CLIENTID_SECRET);
				}

				if ((null != clientId && !clientId.equalsIgnoreCase(uimsClientId))
						|| (null != clientSecret && !clientSecret.equalsIgnoreCase(uimsClientSecret))) {
					userResponse.setStatus(errorStatus);
					userResponse.setMessage(UserConstants.INVALID_UIMS_CREDENTIALS);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.error("Error in updateUser()-> " + userResponse.getMessage());
					LOGGER.info("Time taken by updateUser() : " + elapsedTime);
					//return Response.status(Response.Status.UNAUTHORIZED).entity(userResponse).build();
					return handleUIMSError(Response.Status.UNAUTHORIZED,UserConstants.INVALID_UIMS_CREDENTIALS);
				}

				Response fedResponse = checkUserExistsWithFederationID(iPlanetDirectoryKey,
						userRequest.getUserRecord().getIDMS_Federated_ID__c(), startTime);
				if (fedResponse.getStatus() == 200) {
					openAmReq = mapper.map(userRequest, OpenAmUserRequest.class);
					JSONObject uimsResponse = (JSONObject) fedResponse.getEntity();
					if (("Email".equalsIgnoreCase((String) uimsResponse.get("loginIdentity")))
							&& null != userRequest.getUserRecord().getEmail()
							&& !userRequest.getUserRecord().getEmail().isEmpty()) {
						openAmReq.getInput().getUser().setLoginid(userRequest.getUserRecord().getEmail());
					} else if (("Mobile".equalsIgnoreCase((String) uimsResponse.get("loginIdentity")))
							&& null != userRequest.getUserRecord().getMobilePhone()
							&& !userRequest.getUserRecord().getMobilePhone().isEmpty()) {
						// openAmReq.getInput().getUser().setLoginid(userRequest.getUserRecord().getMobilePhone());
						openAmReq.getInput().getUser().setLogin_mobile(userRequest.getUserRecord().getMobilePhone());
					}
					userId = (String) uimsResponse.get("userId");
				} else {
					return fedResponse;
				}

			} else {

				/**
				 * Calling UserInfoByAccessToken to validate user accessToken
				 * and to get the user information
				 */
				LOGGER.info(AUDIT_REQUESTING_USER + userRequest.getUserRecord().getId() + AUDIT_IMPERSONATING_USER
						+ AUDIT_API_ADMIN + AUDIT_OPENAM_API + AUDIT_OPENAM_USER_INFO_CALL + "/se" + userId
						+ AUDIT_LOG_CLOSURE);

				LOGGER.info("IDMS_Profile_update_source__c="
						+ userRequest.getUserRecord().getIDMS_Profile_update_source__c());
				if (pickListValidator.validate(UserConstants.IDMS_BFO_profile,
						userRequest.getUserRecord().getIDMS_Profile_update_source__c())) {
					// userId =
					// userRequest.getUserRecord().getIDMS_Federated_ID__c();
					// fedId =
					// userRequest.getUserRecord().getIDMS_Federated_ID__c();

					LOGGER.info("In BFO Profile block");
					LOGGER.info("Start: getUserInfoByAccessToken() of openamtokenservice");
					String userInfoByAccessToken = openAMTokenService.getUserInfoByAccessToken(authorizedToken, "/se");
					LOGGER.info("End: getUserInfoByAccessToken() of openamtokenservice, userInfoByAccessToken="
							+ userInfoByAccessToken);

					productDocCtx = JsonPath.using(conf).parse(userInfoByAccessToken);
					LOGGER.info("productDocCtx = " + ChinaIdmsUtil.printOpenAMInfo(productDocCtx.jsonString()));

					if (null != productDocCtx.read("$.email")
							&& productDocCtx.read("$.email").toString().contains(UserConstants.TECHNICAL_USER)) {
						LOGGER.info("Start: checkUserExistsWithEmailMobile() of openam for email="
								+ userRequest.getUserRecord().getEmail());
						String userExistsInOpenam = productService
								.checkUserExistsWithEmailMobile(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
										"loginid eq " + "\""
												+ URLEncoder.encode(URLDecoder.decode(
														userRequest.getUserRecord().getEmail(), "UTF-8"), "UTF-8")
												+ "\"");

						LOGGER.info("End: checkUserExistsWithEmailMobile() of openam for email="
								+ userRequest.getUserRecord().getEmail());
						productDocCtx = JsonPath.using(conf).parse(userExistsInOpenam);
						userId = productDocCtx.read("$.result[0].username");
						fedId = productDocCtx.read("$.result[0].federationID[0]");
						LOGGER.info("userId=" + userId + " ,fedId=" + fedId);
					} else {
						userId = productDocCtx.read("$.sub");
						fedId = productDocCtx.read("$.federationID");
						LOGGER.info("in else block: userId=" + userId + " ,fedId=" + fedId);
					}
					usermail = productDocCtx.read("$.email");
					if (usermail == null) {
						usermail = productDocCtx.read("$.Email");
					}
					LOGGER.info("usermail=" + usermail);
				} else {
					LOGGER.info("In non-BFO Profile block");
					String userInfoByAccessToken = openAMTokenService.getUserInfoByAccessToken(authorizedToken, "/se");
					productDocCtx = JsonPath.using(conf).parse(userInfoByAccessToken);
					LOGGER.info("productDocCtx = " + ChinaIdmsUtil.printOpenAMInfo(productDocCtx.jsonString()));
					userId = productDocCtx.read("$.sub");
					fedId = productDocCtx.read("$.federationID");

					usermail = productDocCtx.read("$.email");
					if (usermail == null) {
						usermail = productDocCtx.read("$.Email");
					}
					LOGGER.info("In non-bfo block: userId=" + userId + " ,fedId=" + fedId + " ,usermail=" + usermail);
				}
				userResponse.setId(userId);

				/**
				 * Getting the user to check user is updating for same user or
				 * other if updating for same user dont need to call provisional
				 * realm , directly user can update on se realm only
				 */

				/*LOGGER.info(AUDIT_REQUESTING_USER + userId + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN
						+ AUDIT_OPENAM_API + AUDIT_OPENAM_GET_CALL + userId + AUDIT_LOG_CLOSURE);*/
				LOGGER.info("Start: getUser() of OpenAMService for userId:" + userId);
				userData = productService.getUser(iPlanetDirectoryKey, userId);

				LOGGER.info("userData -> " + ChinaIdmsUtil.printOpenAMInfo(userData));

				productDocCtxUser = JsonPath.using(conf).parse(userData);
				updatingUser = productDocCtxUser.read(JsonConstants.LOGIN_ID_LOWER_0);
				if (null == updatingUser) {
					updatingUser = productDocCtxUser.read(JsonConstants.LOGIN_ID_UPPER_0);
				}
				if (null == updatingUser) {
					updatingUser = productDocCtxUser.read(JsonConstants.LOGIN_MOBILE_0);
				}

				/**
				 * dual identifier changes checking mobile modification against
				 * mobile_reg or login_mobile
				 */

				String mobileIdentityInOpenam = productDocCtxUser.read(JsonConstants.LOGIN_MOBILE_0);
				if (null == mobileIdentityInOpenam || mobileIdentityInOpenam.isEmpty()) {
					mobileIdentityInOpenam = productDocCtxUser.read(JsonConstants.MOBILEREG_0);
				}
				LOGGER.info("mobileIdentityInOpenam as identifier= " + mobileIdentityInOpenam);
				String modifiedMobileInRequest = userRequest.getUserRecord().getMobilePhone();

				if (null != modifiedMobileInRequest && !modifiedMobileInRequest.isEmpty()
						&& !modifiedMobileInRequest.equalsIgnoreCase(mobileIdentityInOpenam)) {
					CheckUserExistsRequest checkRequest = new CheckUserExistsRequest();
					checkRequest.setMobile(modifiedMobileInRequest);
					checkRequest.setWithGlobalUsers("false");
									
					Response checkUserExist = idmsCheckUserExists(checkRequest);
					LOGGER.info("idmsCheckUserExists reponse in updateUser() for mobile check::"
							+ objMapper.writeValueAsString(checkUserExist));

					org.json.simple.JSONObject checkUserJson = (org.json.simple.JSONObject) checkUserExist.getEntity();
					String messageUser = checkUserJson.get(UserConstants.MESSAGE_L).toString();
					if (!messageUser.equalsIgnoreCase(UserConstants.FALSE)) {
						if (200 != checkUserExist.getStatus()) {
							responseCheck.put(UserConstants.STATUS, errorStatus);
							responseCheck.put(UserConstants.MESSAGE,
									"Mobile identifier cannot be modified :: " + messageUser);
							LOGGER.error("Error while mobile updation, idmsCheckUserExists in updateUser() ->  "
									+ messageUser);
							return Response.status(checkUserExist.getStatus()).entity(responseCheck).build();
						}
						if (200 == checkUserExist.getStatus()) {
							responseCheck.put(UserConstants.STATUS, errorStatus);
							responseCheck.put(UserConstants.MESSAGE,
									"Mobile identifier cannot be modified :: " + UserConstants.USER_EXISTS);
							LOGGER.error("Error while mobile updation, idmsCheckUserExists in updateUser() -> "
									+ UserConstants.USER_EXISTS);
							return Response.status(Response.Status.CONFLICT).entity(responseCheck).build();
						}
					}
				}

				/**
				 * Email changes
				 */

				String mailIdentityInOpenam = productDocCtxUser.read(JsonConstants.LOGIN_ID_UPPER_0);
				if (null == mailIdentityInOpenam || mailIdentityInOpenam.isEmpty()) {
					mailIdentityInOpenam = productDocCtxUser.read(JsonConstants.LOGIN_ID_LOWER_0);
				} else if (null == mailIdentityInOpenam || mailIdentityInOpenam.isEmpty()) {
					mailIdentityInOpenam = productDocCtxUser.read(JsonConstants.MAIL);
				}
				LOGGER.info("mailIdentityInOpenam = " + mailIdentityInOpenam);
				String modifiedMailInRequest = userRequest.getUserRecord().getEmail();

				if (null != modifiedMailInRequest && !modifiedMailInRequest.isEmpty()
						&& !modifiedMailInRequest.trim().equalsIgnoreCase(mailIdentityInOpenam)) {
					CheckUserExistsRequest checkRequest = new CheckUserExistsRequest();
					checkRequest.setEmail(modifiedMailInRequest);
					checkRequest.setWithGlobalUsers("true");
					if (null != userRequest.getUserRecord().getIDMS_Profile_update_source__c()
							&& !userRequest.getUserRecord().getIDMS_Profile_update_source__c().isEmpty()) {
						checkRequest.setApplicationName(userRequest.getUserRecord().getIDMS_Profile_update_source__c().trim());
					}
					Response checkUserExist = idmsCheckUserExists(checkRequest);
					LOGGER.info("idmsCheckUserExists reponse ::" + objMapper.writeValueAsString(checkUserExist));

					org.json.simple.JSONObject checkUserJson = (org.json.simple.JSONObject) checkUserExist.getEntity();
					String messageUser = checkUserJson.get(UserConstants.MESSAGE_L).toString();
					if (!messageUser.equalsIgnoreCase(UserConstants.FALSE)) {
						if (200 != checkUserExist.getStatus()) {
							responseCheck.put(UserConstants.STATUS,errorStatus);
							responseCheck.put(UserConstants.MESSAGE,"Mail identifier cannot be modified :: "+messageUser);
							LOGGER.error("Error while email updation, idmsCheckUserExists in updateUser() ->  " + messageUser);
							return Response.status(checkUserExist.getStatus()).entity(responseCheck).build();
						}
						if (200 == checkUserExist.getStatus()) {
							responseCheck.put(UserConstants.STATUS,errorStatus);
							responseCheck.put(UserConstants.MESSAGE,"Mail identifier cannot be modified :: "+UserConstants.USER_EXISTS);
							LOGGER.error("Error while email updation, idmsCheckUserExists in updateUser() -> "+UserConstants.USER_EXISTS);
							return Response.status(Response.Status.CONFLICT).entity(responseCheck).build();
						}
					}
				}

				if (null != modifiedMobileInRequest && !modifiedMobileInRequest.isEmpty()
						&& null != mobileIdentityInOpenam && !mobileIdentityInOpenam.isEmpty()) {
					if (!modifiedMobileInRequest.equalsIgnoreCase(mobileIdentityInOpenam)) {
						LOGGER.info("modifiedMobileInRequest = " + modifiedMobileInRequest);
						updateMobileIdentifierCheck = true;
						AddMobileRequest addMobileRequest = new AddMobileRequest();
						addMobileRequest.setAccesstoken(authorizedToken);
						addMobileRequest.setMobile(modifiedMobileInRequest);
						addMobileRequest.setFedId(userId);
						addMobileRequest
								.setProfileUpdateSource(userRequest.getUserRecord().getIDMS_Profile_update_source__c());
						Response res = addMobile(addMobileRequest);
						LOGGER.info("mobile verification as identifier, status=" + res.getStatus());
						if (200 != res.getStatus()) {
							org.json.simple.JSONObject checkMobileResponse = (org.json.simple.JSONObject) res
									.getEntity();
							String messageUser = checkMobileResponse.get("Message").toString();

							responseCheck.put(UserConstants.STATUS, errorStatus);
							responseCheck.put(UserConstants.MESSAGE,
									"Mobile identifier cannot be modified :: " + messageUser);
							elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
							LOGGER.error("Mobile identifier cannot be modified :: Error is " + messageUser);
							LOGGER.info("Time taken by updateUser() : " + elapsedTime);
							return Response.status(Response.Status.BAD_REQUEST).entity(responseCheck).build();
						}
					}
				}

				userName = productDocCtxUser.read(JsonConstants.USER_NAME);
				openAmReq = mapper.map(userRequest, OpenAmUserRequest.class);
				openAmReq.getInput().setUser(user);
				companyFedIdInOpenAM = productDocCtxUser.read("$.companyFederatedID[0]");
				LOGGER.info("companyFedIdInOpenAM = " + companyFedIdInOpenAM);

				/**
				 * Setting attributes in openam registration
				 */

				if (null != userRequest.getAttributes() && userRequest.getAttributes().size() > 0) {
					openAmReq.getInput().getUser()
							.setRegistrationAttributes__c(objMapper.writeValueAsString(userRequest.getAttributes()));
				}

				/**
				 * Adding for social login
				 */

				if (updatingUser == null && (!userName.startsWith(UserConstants.UID_PREFIX))) {
					updatingUser = usermail;
					isUserFromSocialLogin = true;
				}

				//
				/**
				 * check email and mobile phone for login identifier
				 */

				if (((null != updatingUser) ? emailValidator.validate(updatingUser) : true)
						&& (null != userRequest.getUserRecord().getEmail()
								&& !userRequest.getUserRecord().getEmail().isEmpty())
						&& (null != userRequest.getUserRecord().getMobilePhone()
								&& !userRequest.getUserRecord().getMobilePhone().isEmpty())) {

					openAmReq.getInput().getUser().setIdmsuid(userRequest.getUserRecord().getEmail());
					hotpService = UserConstants.HOTP_EMAIL_UPDATE;
					identifierType = UserConstants.EMAIL;
					user.setMail(userRequest.getUserRecord().getEmail());
					user.setMobile(userRequest.getUserRecord().getMobilePhone());
					loginIdentifier = userRequest.getUserRecord().getEmail();
					if (null != updatingUser && updatingUser.equalsIgnoreCase(userRequest.getUserRecord().getEmail())) {
						userUpdateforSameUser = true;
					}

				} else if (((null != updatingUser) ? emailValidator.validate(updatingUser) : true)
						&& (null != userRequest.getUserRecord().getEmail())
						&& (!userRequest.getUserRecord().getEmail().isEmpty())) {
					user.setMail(userRequest.getUserRecord().getEmail());
					openAmReq.getInput().getUser().setIdmsuid(userRequest.getUserRecord().getEmail());
					hotpService = UserConstants.HOTP_EMAIL_UPDATE;
					identifierType = UserConstants.EMAIL;
					loginIdentifier = userRequest.getUserRecord().getEmail();
					if (null != updatingUser && updatingUser.equalsIgnoreCase(userRequest.getUserRecord().getEmail())) {
						userUpdateforSameUser = true;
					}
				} else if ((null != userRequest.getUserRecord().getMobilePhone())
						&& (!userRequest.getUserRecord().getMobilePhone().isEmpty())) {
					if (null != userRequest.getUserRecord().getIDMS_Profile_update_source__c() && !UserConstants.UIMS
							.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Profile_update_source__c())) {
						if (null != updatingUser) {// added if socialLogin id is
													// empty
							legthValidator.validate(UserConstants.MOBILE_PHONE, updatingUser);
						}
					}
					user.setMobile(userRequest.getUserRecord().getMobilePhone());
					openAmReq.getInput().getUser()
							.setIdmsuid(userRequest.getUserRecord().getMobilePhone() + "bridge-fo.com");
					hotpService = UserConstants.HOTP_MOBILE_UPDATE;
					identifierType = UserConstants.MOBILE;
					loginIdentifier = userRequest.getUserRecord().getMobilePhone();
					if (null != updatingUser
							&& updatingUser.equalsIgnoreCase(userRequest.getUserRecord().getMobilePhone())) {
						userUpdateforSameUser = true;
					}
				}

				String userExists = productService.checkUserExistsWithEmailMobile(
						UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, "loginid eq " + "\"" + loginIdentifier
								+ "\" or login_mobile eq " + "\"" + loginIdentifier + "\"");

				productDocCtx = JsonPath.using(conf).parse(userExists);
				Integer resultCount = productDocCtx.read(JsonConstants.RESULT_COUNT);
				LOGGER.info("resultCount=" + resultCount);
				if (resultCount.intValue() > 0 && ((null != loginIdentifier) && (null != updatingUser)
						&& (!loginIdentifier.equalsIgnoreCase(updatingUser)))) {
					userResponse.setStatus(errorStatus);
					userResponse.setMessage(UserConstants.NEW_USER_EXISTS);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.error("Error is " + userResponse.getMessage());
					LOGGER.info("Time taken by updateUser() : " + elapsedTime);
					return Response.status(Response.Status.CONFLICT).entity(userResponse).build();

				}

				if ((UserConstants.EMAIL.equalsIgnoreCase(identifierType) && !userUpdateforSameUser)) {

					// Step 3:
					/**
					 * Check password exits and assign to openAM
					 */

					// Commenting the below code(Line Num - 3505 To 3570)
					// because OTP Generating from Java Only
					/*
					 * openAmReq.getInput().getUser().setUserPassword(
					 * generateRamdomPassWord()); // Step 4:
					 *//**
						 * Generate Random login ID and map it to Open AM Login
						 * ID attribute
						 *//*
						 * // HOTP Mobile and Email Verification String
						 * prefferedLanguage =
						 * getValue(productDocCtxUser.read("$.preferredlanguage"
						 * ).toString());
						 * 
						 * String cn =
						 * getValue(productDocCtxUser.read("$.cn").toString());
						 * 
						 * if (userRequest.getUserRecord().getEmail() != null) {
						 * openAmReq.getInput().getUser()
						 * .setHotpEmailVerification(userRequest.getUserRecord()
						 * .getEmail() + ":" + userName + ":" +
						 * prefferedLanguage + ":" +
						 * userRequest.getUserRecord().
						 * getIDMS_Profile_update_source__c() + ":" + cn); }
						 * else if (userRequest.getUserRecord().getMobilePhone()
						 * != null) { openAmReq.getInput().getUser()
						 * .setHotpMobileVerification(userRequest.getUserRecord(
						 * ).getMobilePhone() + ":" + userName + ":" +
						 * prefferedLanguage + ":" +
						 * userRequest.getUserRecord().
						 * getIDMS_Profile_update_source__c() + ":" + cn);
						 * 
						 * } // userName = UUID.randomUUID().toString() +
						 * ".tmp"; userName = userName + ".tmp";
						 * openAmReq.getInput().getUser().setUsername(userName);
						 * jsonRequset =
						 * objMapper.writeValueAsString(openAmReq); jsonRequset
						 * = jsonRequset.replace("\"\"", "[]");
						 * 
						 * LOGGER.info(AUDIT_REQUESTING_USER + userName +
						 * AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN +
						 * AUDIT_OPENAM_API +
						 * AUDIT_OPENAM_USER_REGISTRATION_PROVISIONAL_CALL +
						 * userAction + AUDIT_LOG_CLOSURE); // Check for the
						 * user already exist or not
						 * 
						 * try { LOGGER.info(
						 * "UserServiceImpl:updateUser -> : provisionalService.userRegistration : Requset -> "
						 * ,jsonRequset); jsonResponse =
						 * provisionalService.userRegistration(
						 * UserConstants.IPLANET_DIRECTORY_PRO +
						 * iPlanetDirectoryKey, userAction, jsonRequset);
						 * LOGGER.info(
						 * "UserServiceImpl:updateUser -> : provisionalService.userRegistration : Response -> "
						 * ,jsonResponse); } catch (ClientErrorException e) { if
						 * (UserConstants.EMAIL.equalsIgnoreCase(identifierType)
						 * ) { String jsonString = "{" + "\"mail\": \"" +
						 * userRequest.getUserRecord().getEmail() +
						 * "\",\"updateSource\": \"" +
						 * userRequest.getUserRecord().
						 * getIDMS_Profile_update_source__c() +
						 * "\",\"userPassword\": \"" +
						 * openAmReq.getInput().getUser().getUserPassword() +
						 * "\",\"hotpEmailVerification\": \"" +
						 * openAmReq.getInput().getUser().
						 * getHotpEmailVerification() + "\"" + "}";
						 * 
						 * LOGGER.info(
						 * "UserServiceImpl:updateUser -> : provisionalService.updateUser : Request -> "
						 * ,jsonString);
						 * provisionalService.updateUser(UserConstants.
						 * IPLANET_DIRECTORY_PRO + iPlanetDirectoryKey,
						 * userName, jsonString);
						 * 
						 * } else if
						 * (UserConstants.MOBILE.equalsIgnoreCase(identifierType
						 * )) { String jsonString = "{" + "\"mobile\": \"" +
						 * userRequest.getUserRecord().getMobilePhone() +
						 * "\",\"updateSource\": \"" +
						 * userRequest.getUserRecord().
						 * getIDMS_Profile_update_source__c() +
						 * "\",\"userPassword\": \"" +
						 * openAmReq.getInput().getUser().getUserPassword() +
						 * "\",\"hotpMobileVerification\": \"" +
						 * openAmReq.getInput().getUser().
						 * getHotpMobileVerification() + "\"" + "}";
						 * LOGGER.info(
						 * "UserServiceImpl:updateUser -> : provisionalService.updateUser : Request -> "
						 * ,jsonString);
						 * provisionalService.updateUser(UserConstants.
						 * IPLANET_DIRECTORY_PRO + iPlanetDirectoryKey,
						 * userName, jsonString); }
						 * //productService.sessionLogout(UserConstants.
						 * IPLANET_DIRECTORY_PRO+iPlanetDirectoryKey, "logout");
						 * 
						 * }
						 */

					/*
					 * String PRODUCT_JSON_STRING = sendOtp(hotpService,
					 * userName,
					 * openAmReq.getInput().getUser().getUserPassword(),
					 * UserConstants.UPDATE_USER_SERVICE);
					 */
					// + "\",\"hotpEmailVerification\": \""+
					// openAmReq.getInput().getUser().getHotpEmailVerification()
					String product_json_string = "{" + "\"newmail\": \"" + userRequest.getUserRecord().getEmail() + "\""
							+ "}";

					LOGGER.info("Start: updateUser() of OpenAMService to update new email for userid:" + userId);
					productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userId,
							product_json_string);
					LOGGER.info("End: updateUser() of OpenAMService to update new email finished for userid:" + userId);
					/**
					 * Adding the below condition for social Login
					 */

					// if(!isUserFromSocialLogin){
					String otp = sendEmail.generateOtp(userId);
					LOGGER.info("Successfully OTP generated for " + userId);
					sendEmail.sendOpenAmEmail(otp, EmailConstants.UPDATEUSERRECORD_OPT_TYPE, userId,
							userRequest.getUserRecord().getIDMS_Profile_update_source__c());
					// }
					// LOGGER.info("UserServiceImpl:updateUser -> : sendOtp :
					// Response -> ",PRODUCT_JSON_STRING);
					/**
					 * To update authId in openAM extended attribute
					 */
					/*
					 * if (null != PRODUCT_JSON_STRING &&
					 * !PRODUCT_JSON_STRING.isEmpty()) { LOGGER.info(
					 * "To update authId in openAM extended attribute :: updateUser -> "
					 * + PRODUCT_JSON_STRING); LOGGER.info(AUDIT_REQUESTING_USER
					 * + userName + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN +
					 * AUDIT_OPENAM_API + AUDIT_OPENAM_UPDATE_PROVISIONAL_CALL +
					 * userAction + AUDIT_LOG_CLOSURE);
					 * provisionalService.updateUser(UserConstants.
					 * IPLANET_DIRECTORY_PRO + iPlanetDirectoryKey, userName,
					 * PRODUCT_JSON_STRING);
					 */

					if (UserConstants.EMAIL.equalsIgnoreCase(identifierType) && null != updatingUser) {
						String prefferedLanguage = null != productDocCtxUser.read("$.preferredlanguage")
								? getValue(productDocCtxUser.read("$.preferredlanguage").toString()) : getDelimeter();

						String subject = null;
						/**
						 * Adding for social lgoin
						 */

						if (isUserFromSocialLogin) {
							prefferedLanguage = UserConstants.LANGUAGE_CHINA;
						}
						if (prefferedLanguage.equalsIgnoreCase(UserConstants.LANGUAGE_CHINA)) {
							subject = UserConstants.UPDATE_EMAIL_NOTIFICATION_ZH;
						} else {
							subject = UserConstants.UPDATE_EMAIL_NOTIFICATION;
						}

						// cal send email
						Response applicationDetails   = openDJService.getUser(djUserName, djUserPwd, userRequest.getUserRecord().getIDMS_Profile_update_source__c());
						productDJData = JsonPath.using(conf).parse(IOUtils.toString((InputStream) applicationDetails.getEntity()));
						if (null != applicationDetails && 200 == applicationDetails.getStatus()) {
							String userNameFormatOpenDJ = productDJData.read("userNameFormat");
							LOGGER.info("update user userNameFormatOpenDJ:"+userNameFormatOpenDJ);
							LOGGER.info("update user defaultUserNameFormat:"+defaultUserNameFormat);
							if(null != userNameFormatOpenDJ && !userNameFormatOpenDJ.isEmpty()){
								emailUserNameFormat = userNameFormatOpenDJ;
							} else{
								emailUserNameFormat = defaultUserNameFormat;
							}
						}
						if (null != applicationDetails && 200 != applicationDetails.getStatus()) {
							emailUserNameFormat = defaultUserNameFormat;
						}
						if(emailUserNameFormat.equalsIgnoreCase(UserConstants.FIRST_NAME))
							firstName=productDocCtxUser.read("$.givenName[0]");
						else if(emailUserNameFormat.equalsIgnoreCase(UserConstants.LAST_NAME))
							firstName=productDocCtxUser.read("$.sn[0]");
						else if(emailUserNameFormat.equalsIgnoreCase(UserConstants.FULL_NAME))
							firstName=productDocCtxUser.read("$.cn[0]");
						else
							firstName=productDocCtxUser.read("$.givenName[0]");
						LOGGER.info("Update user Email format Name:"+firstName);
						
						/*firstName = null != productDocCtxUser.read("$.givenName")
								? getValue(productDocCtxUser.read("$.givenName").toString()) : getDelimeter();
						if (null == firstName) { // added for socialLogin issue
							firstName = null != productDocCtxUser.read("$.cn")
									? getValue(productDocCtxUser.read("$.cn").toString()) : getDelimeter();
						}*/
						
						contentBuilder = getContentFromTemplate(UserConstants.UPDATE_EMAIL_NOTIFICATION,
								prefferedLanguage);
						int startName = contentBuilder.indexOf("{!User.FirstName},");
						int endName = startName + "{!User.FirstName}".length();
						contentBuilder.replace(startName, endName, firstName);// Need
																				// to
																				// check
																				// whether
																				// we
																				// need
																				// to
																				// pass
																				// FirstName
																				// of
																				// loginId
						try {
							// sending email to old user
							sendEmail.emailReadyToSendEmail(updatingUser, fromUserName, subject,
									contentBuilder.toString());
						} catch (Exception e) {							
							LOGGER.error("Exception while sending email to old User :: -> " + e.getMessage(),e);
						}
					}
					// }

				} else if (UserConstants.MOBILE.equalsIgnoreCase(identifierType) && !userUpdateforSameUser) {
					// for mobile scenarios

					String product_json_string = "{" + "\"newmobile\": \""
							+ userRequest.getUserRecord().getMobilePhone() + "\"" + "}";
					LOGGER.info("Start: updateUser() of OpenAMService to update new mobile for userid:" + userId);
					productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userId,
							product_json_string);
					LOGGER.info(
							"End: updateUser() of OpenAMService to update new mobile finished for userid:" + userId);

					String otp = sendEmail.generateOtp(userId);
					LOGGER.info("Successfully OTP generated for " + userId);
					sendEmail.sendSMSNewGateway(otp, EmailConstants.UPDATEUSERRECORD_OPT_TYPE, userName,
							userRequest.getUserRecord().getIDMS_Registration_Source__c());

					sendEmail.sendOpenAmMobileEmail(otp, EmailConstants.UPDATEUSERRECORD_OPT_TYPE, userId,
							userRequest.getUserRecord().getIDMS_Profile_update_source__c());
					if (null != updatingUser) {
						// Need to check whether do we need to send message to
						// existing mobile number
					}
				}

				openAmReq = mapper.map(userRequest, OpenAmUserRequest.class);

				if (UserConstants.MOBILE.equalsIgnoreCase(identifierType) && !userUpdateforSameUser) {
					openAmReq.getInput().getUser().setMobile(null);
					userRecord = new IFWUser();
					userRecord.setMobilePhone(userRequest.getUserRecord().getMobilePhone());
					// jsonObject.put("MobilePhone",
					// userRecord.getMobilePhone());
				} else if (UserConstants.EMAIL.equalsIgnoreCase(identifierType) && !userUpdateforSameUser) {
					openAmReq.getInput().getUser().setMail(null);
					userRecord = new IFWUser();
					userRecord.setEmail(userRequest.getUserRecord().getEmail());
					// jsonObject.put("Email", userRecord.getEmail());
				} /*
					 * else { openAmReq.getInput().getUser().setMobile(null);
					 * openAmReq.getInput().getUser().setMail(null); }
					 */

			}
			// convert Ifw to open am
			//Senthil if IDMSMarketserved field is not null set in OpenAMUser Object
			 if (null != userRequest.getUserRecord().getIDMSCompanyMarketServed__c()
						&& !userRequest.getUserRecord().getIDMSCompanyMarketServed__c().isEmpty()){
				LOGGER.info("inside IDMSCompanyMarketServed__c block");
				openAmReq.getInput().getUser().setIndustries(userRequest.getUserRecord().getIDMSCompanyMarketServed__c());

			}
			jsonRequset = objMapper.writeValueAsString(openAmReq.getInput().getUser());
			jsonRequset = jsonRequset.replace("\"\"", "[]");

			/**
			 * Call updateuser for all attributes except email and mobile
			 */
			// LOGGER.info(" UserServiceImpl :: updateUser
			// productService.updateUser ");
			LOGGER.info(AUDIT_REQUESTING_USER + userId + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN + AUDIT_OPENAM_API
					+ AUDIT_OPENAM_UPDATE_CALL + userId + AUDIT_LOG_CLOSURE);
			LOGGER.info("Json  Request  for  update  user ------------->" + jsonRequset);
			jsonResponse = productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userId,
					jsonRequset);
			productDocCtx = JsonPath.using(conf).parse(jsonResponse);
			String openamVnew = null != productDocCtx.read("$.V_New[0]") ? getValue(productDocCtx.read("$.V_New[0]"))
					: getDelimeter();
			if (null != openamVnew) {
				vNewCntValue = Integer.parseInt(openamVnew) + 1;
			}
			String version = "{\"V_New\": \"" + vNewCntValue + "\"" + "}";

			LOGGER.info("Json Response " + ChinaIdmsUtil.printOpenAMInfo(jsonResponse));

			if (booleanTrue == userRequest.getUserRecord().isActive() && UserConstants.UIMS
					.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Profile_update_source__c())) {
				ActivateUserRequest activateUserRequest = new ActivateUserRequest();
				ActivateUser activateUser = new ActivateUser();
				activateUserRequest.setUserRecord(activateUser);
				activateUser.setIDMS_Federated_ID__c(userRequest.getUserRecord().getIDMS_Federated_ID__c());
				activateUser.setIDMS_Registration_Source__c(UserConstants.UIMS);
				activateUser(iPlanetDirectoryKey, clientId, clientSecret, activateUserRequest);
			}

			// calling UIMS update user
			if ((!isUserFromSocialLogin)
					&& (null != userRequest.getUserRecord().getIDMS_Profile_update_source__c() && !UserConstants.UIMS
							.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Profile_update_source__c()))) {
				// Adding V_New
				LOGGER.info("Start: updateUser() of OpenAMService for updating version for userid=" + userId);
				productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userId, version);
				LOGGER.info("End: updateUser() of OpenAMService for updating version finished for userid=" + userId);
				// mapping IFW request to UserCompany
				CompanyV3 company = mapper.map(userRequest, CompanyV3.class);
				if (null != company.getLanguageCode()) {
					company.setLanguageCode(company.getLanguageCode().toLowerCase());
				}
				company.setFederatedId(companyFedIdInOpenAM);

				// Setting publicVisibility value to company.publicVisibility
				if (null != userRequest.getAttributes() && userRequest.getAttributes().size() > 0) {
					List<RegistrationAttributes> attributeList = userRequest.getAttributes();
					for (int i = 0; i < attributeList.size(); i++) {
						String KeyName = attributeList.get(i).getKeyName();
						String KeyValue = attributeList.get(i).getKeyValue();
						LOGGER.info("KeyName = " + KeyName + " and KeyValue = " + KeyValue);

						if (KeyName.equalsIgnoreCase("publicVisibility") && null != KeyValue && !KeyValue.isEmpty()) {
							company.setPublicVisibility(Boolean.valueOf(KeyValue));
							if (null == attributeText || attributeText.isEmpty()) {
								attributeText = "{" + "\"publicVisibility\": \"" + KeyValue + "\"" + "}";
								LOGGER.info("Start: updateUser() of openam to update publicVisibility for userid="
										+ userId);
								productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userId,
										attributeText);
								LOGGER.info(
										"End: updateUser() of openam to update publicVisibility finished for userid="
												+ userId);
							}
						}
						if (KeyName.equalsIgnoreCase("pvtRegPRMCompFedID") && null != KeyValue && !KeyValue.isEmpty()) {
							companyFedIdInRequest = KeyValue;
							attributeText = null;
							if (null == attributeText || attributeText.isEmpty()) {
								attributeText = "{" + "\"companyFederatedID\": \"" + KeyValue + "\"" + "}";
								LOGGER.info("Start: updateUser() of openam to update companyFederatedID for userid="
										+ userId);
								productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userId,
										attributeText);
								LOGGER.info(
										"End: updateUser() of openam to update companyFederatedID finished for userid="
												+ userId);
							}
						}
					}
				}

				com.se.uims.usermanager.UserV6 identity = mapper.map(userRequest, com.se.uims.usermanager.UserV6.class);
				if (null != identity.getLanguageCode()) {
					identity.setLanguageCode(identity.getLanguageCode().toLowerCase());
				}
				// calling Async method updateUIMSUserAndCompany
				LOGGER.info("Start: ASYNC updateUIMSUserAndCompany() for userId:" + userId);
				uimsUserManagerSoapService.updateUIMSUserAndCompany(fedId, identity,
						userRequest.getUserRecord().getIDMS_User_Context__c(), company, vNewCntValue.toString(),
						productService, UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userId,
						companyFedIdInRequest, usermail);
				LOGGER.info("End: ASYNC updateUIMSUserAndCompany() finished for userId:" + userId);
			} else {
				// productService.sessionLogout(UserConstants.IPLANET_DIRECTORY_PRO+iPlanetDirectoryKey,
				// "logout");
			}

			Response response = getUser(userId);
			Object responseObject = response.getEntity();

			sucessRespone = new UpdateUserResponse();
			sucessRespone.setStatus(successStatus);
			sucessRespone.setMessage(UserConstants.UPDATE_USER_SUCCESS_MESSAGE);
			userRequest.getUserRecord().setId(userId);
			sucessRespone.setIDMSUserRecord(responseObject);

		} catch (BadRequestException e) {
			userResponse.setMessage(UserConstants.ERROR_UPDATE_USER);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by updateUser() : " + elapsedTime);
			LOGGER.error("BadRequestException in Updating the User :: -> " + e.getMessage(),e);
			// productService.sessionLogout(UserConstants.IPLANET_DIRECTORY_PRO+iPlanetDirectoryKey,
			// "logout");
			if(UserConstants.UIMS.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Profile_update_source__c())){
				return handleUIMSError(Response.Status.BAD_REQUEST, UserConstants.ERROR_UPDATE_USER);
			}
			return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
		} catch (NotAuthorizedException e) {
			userResponse.setMessage("Session expired or invalid");
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by updateUser() : " + elapsedTime);
			LOGGER.error("NotAuthorizedException in Updating the User :: -> " + e.getMessage(),e);
			// productService.sessionLogout(UserConstants.IPLANET_DIRECTORY_PRO+iPlanetDirectoryKey,
			// "logout");
			if(UserConstants.UIMS.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Profile_update_source__c())){
				return handleUIMSError(Response.Status.UNAUTHORIZED, "Session expired or invalid");
			}
			return Response.status(Response.Status.UNAUTHORIZED).entity(userResponse).build();
		} catch (ClientErrorException e) {
			userResponse.setMessage(UserConstants.NEW_USER_EXISTS);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.updateUser() : " + elapsedTime);
			LOGGER.error("ClientErrorException in updating the User :: -> " + userResponse.getMessage(),e);
			// productService.sessionLogout(UserConstants.IPLANET_DIRECTORY_PRO+iPlanetDirectoryKey,
			// "logout");
			if(UserConstants.UIMS.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Profile_update_source__c())){
				return handleUIMSError(Response.Status.CONFLICT, UserConstants.NEW_USER_EXISTS);
			}
			return Response.status(Response.Status.CONFLICT).entity(userResponse).build();
		} catch (Exception e) {
			userResponse.setMessage(UserConstants.ERROR_UPDATE_USER);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by updateUser() : " + elapsedTime);
			LOGGER.error("Exception in Updating the User :: -> " + e.getMessage(),e);
			// productService.sessionLogout(UserConstants.IPLANET_DIRECTORY_PRO+iPlanetDirectoryKey,
			// "logout");
			if(UserConstants.UIMS.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Profile_update_source__c())){
				return handleUIMSError(Response.Status.INTERNAL_SERVER_ERROR, UserConstants.ERROR_UPDATE_USER);
			}
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(userResponse).build();
		}
		LOGGER.info(" UserServiceImpl :: updateUser End");
		elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		LOGGER.info("Time taken by updateUser() : " + elapsedTime);
		if(UserConstants.UIMS
				.equalsIgnoreCase(userRequest.getUserRecord().getIDMS_Profile_update_source__c())){
			UIMSResponse response= new UIMSResponse();
			response.setHasErrors("false");
			response.setStatus("Success");
			UIMSStatusInfo userResponse=new UIMSStatusInfo();
			userResponse.setStatusCode(String.valueOf(Response.Status.OK.getStatusCode()));
			userResponse.setMessage(UserConstants.UPDATE_USER_SUCCESS_MESSAGE);
			response.setResults(userResponse);
			LOGGER.info("updateUser -> : UIMS Response -> " + response);
			return Response.status(Response.Status.OK).entity(response).build();
		}
		return Response.status(Response.Status.OK).entity(sucessRespone).build();
	}

	private Response passwordRecoverySuccessResponse(String userName, long startTime, String userData) {
		LOGGER.info("Entered passwordRecoverySuccessResponse() -> Start");
		LOGGER.info("Parameter userName -> " + userName);
		LOGGER.info("Parameter userData -> " + ChinaIdmsUtil.printOpenAMInfo(userData));
		PasswordRecoveryResponse passwordRecoveryResponse;
		Attributes attributes = new Attributes();
		IDMSUserRecord idmsUserRecord = new IDMSUserRecord();

		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		DocumentContext productDocCtx = JsonPath.using(conf).parse(userData);

		idmsUserRecord.setAttributes(attributes);
		idmsUserRecord.setId(userName);

		String idmsPreferredLanguage = null != productDocCtx.read("$.preferredlanguage")
				? getValue(productDocCtx.read("$.preferredlanguage").toString()) : getDelimeter();
		String idmsProfileUpdateSource = null != productDocCtx.read("$.updateSource")
				? getValue(productDocCtx.read("$.updateSource").toString()) : getDelimeter();
		String idmsRegistrationSource = null != productDocCtx.read("$.registerationSource")
				? getValue(productDocCtx.read("$.registerationSource").toString()) : getDelimeter();
		String username = null != productDocCtx.read("$.uid") ? getValue(productDocCtx.read("$.uid").toString())
				: getDelimeter();
		String name = null != productDocCtx.read("$.givenName") ? getValue(productDocCtx.read("$.givenName").toString())
				: getDelimeter();
		String mobilePhone = null != productDocCtx.read("$.mobile_reg")
				? getValue(productDocCtx.read("$.mobile_reg").toString()) : getDelimeter();
		String email = null != productDocCtx.read("$.mail") ? getValue(productDocCtx.read("$.mail").toString())
				: getDelimeter();
		String federationID = null != productDocCtx.read("$.federationID")
				? getValue(productDocCtx.read("$.federationID").toString()) : getDelimeter();
		// String idmsIdentityType = null !=
		// productDocCtx.read("$.IDMSIdentityType__c")?
		// getValue(productDocCtx.read("$.IDMSIdentityType__c").toString()) :
		// getDelimeter();

		idmsUserRecord.setIdmsPreferredLanguage(idmsPreferredLanguage);
		idmsUserRecord.setIdmsProfileUpdateSource(idmsProfileUpdateSource);
		idmsUserRecord.setIdmsRegistrationSource(idmsRegistrationSource);
		idmsUserRecord.setUsername(username);
		idmsUserRecord.setName(name);
		idmsUserRecord.setMobilePhone(mobilePhone);
		idmsUserRecord.setEmail(email);
		idmsUserRecord.setIdmsIdentityType("");
		idmsUserRecord.setIDMS_Federated_ID__c(username);

		passwordRecoveryResponse = new PasswordRecoveryResponse(idmsUserRecord);
		passwordRecoveryResponse.setStatus(successStatus);
		passwordRecoveryResponse.setMessage("Reset Password Done successfully.");

		long elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		LOGGER.info("Time taken by UserServiceImpl.passwordRecovery() : " + elapsedTime);
		return Response.status(Response.Status.OK).entity(passwordRecoveryResponse).build();
	}

	private Response passwordRecoveryErrorResponse() {
		LOGGER.info("Entered passwordRecoveryErrorResponse() -> Start");
		PasswordRecoveryResponse serviceResponse = new PasswordRecoveryResponse();
		serviceResponse.setStatus(errorStatus);
		serviceResponse.setMessage("User not found based on Email/Mobile");
		LOGGER.error(serviceResponse.getMessage());
		return Response.status(Response.Status.BAD_REQUEST).entity(serviceResponse).build();
	}

	/*
	 * private void requestHotp(String hotpService, String userName, String
	 * iPlanetDirectoryKey, String updateSource) throws Exception { String
	 * password = generateRamdomPassWord(); String PRODUCT_JSON_STRING = "{" +
	 * "\"userPassword\": \"" + password + "\"" + "," + "\"updateSource\": \"" +
	 * updateSource + "\"" + "}"; LOGGER.info(AUDIT_REQUESTING_USER + userName +
	 * AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN + AUDIT_OPENAM_API +
	 * AUDIT_OPENAM_UPDATE_CALL + userName + AUDIT_LOG_CLOSURE); String
	 * updateUser =
	 * productService.updateUser(UserConstants.IPLANET_DIRECTORY_PRO +
	 * iPlanetDirectoryKey, userName, PRODUCT_JSON_STRING);
	 * LOGGER.info("updateUser=" + updateUser);
	 * 
	 * // callback logic for sending otp
	 * 
	 * 
	 * productDocCtx = sendOtp(hotpService, userName, password,
	 * UserConstants.CREATE_USER_SERVICE); String authId =
	 * productDocCtx.read(JsonConstants.AUTH_ID);
	 * 
	 * PRODUCT_JSON_STRING = "{" + "\"authId\": \"" + authId + "\"" + "}";
	 *//**
		 * To update authId in openAM extended attribute
		 */
	/*
	 * if (null != productDocCtx.read(JsonConstants.AUTH_ID)) {
	 * LOGGER.info(AUDIT_REQUESTING_USER + userName + AUDIT_IMPERSONATING_USER +
	 * AUDIT_API_ADMIN + AUDIT_OPENAM_API + AUDIT_OPENAM_UPDATE_CALL + userName
	 * + AUDIT_LOG_CLOSURE);
	 * productService.updateUser(UserConstants.IPLANET_DIRECTORY_PRO +
	 * iPlanetDirectoryKey, userName, PRODUCT_JSON_STRING); }
	 * 
	 * PRODUCT_JSON_STRING = sendOtp(hotpService, userName, password,
	 * UserConstants.CREATE_USER_SERVICE);
	 *//**
		 * To update authId in openAM extended attribute
		 *//*
		 * if (null != PRODUCT_JSON_STRING && !PRODUCT_JSON_STRING.isEmpty()) {
		 * LOGGER.info(AUDIT_REQUESTING_USER + userName +
		 * AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN + AUDIT_OPENAM_API +
		 * AUDIT_OPENAM_UPDATE_CALL + userName + AUDIT_LOG_CLOSURE);
		 * productService.updateUser(UserConstants.IPLANET_DIRECTORY_PRO +
		 * iPlanetDirectoryKey, userName, PRODUCT_JSON_STRING); } }
		 */

	/**
	 * This method will generate the random password based on langUtils with
	 * string characters
	 * 
	 */

	private String generateRamdomPassWord() {
		LOGGER.info("Entered generateRamdomPassWord() -> Start");
		String tmpPr = RandomStringUtils.random(10, UserConstants.RANDOM_PR_CHARS);
		return tmpPr;
	}

	/**
	 * This method will verify the user password against the regex provided by
	 * the user.
	 * 
	 */
	private boolean checkPasswordPolicy(String userPassword, String firstName, String lastName, String email, String mobile) {
		LOGGER.info("Entered checkPasswordPolicy() -> Start");
		LOGGER.info("Parameter firstName -> " + firstName + " , lastName -> " + lastName);
		LOGGER.info("Parameter email -> " + email + " , mobile -> " + mobile);

		if (userPassword.length() < UserConstants.PASSWORD_LENGTH || userPassword.contains(firstName) || userPassword.contains(lastName)
				|| !userPassword.matches(UserConstants.PASSWORD_REGEX ) || ChinaIdmsUtil.passwordCheck(userPassword,email,mobile))
			return false;
		else
			return true;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.idms.service.UserServiceImpl#setProductService(com.idms.product.
	 * client.OpenAMService)
	 */
	public void setProductService(OpenAMService productService) {
		this.productService = productService;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.idms.service.UserServiceImpl#setOpenAMTokenService(com.idms.product.
	 * client.OpenAMTokenService)
	 */
	public void setOpenAMTokenService(OpenAMTokenService openAMTokenService) {
		this.openAMTokenService = openAMTokenService;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.idms.service.UserServiceImpl#setMapper(com.idms.mapper.IdmsMapper)
	 */
	public void setMapper(IdmsMapper mapper) {
		this.mapper = mapper;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.idms.service.UserServiceImpl#setPickListValidator(com.se.idms.cache.
	 * validate.IValidator)
	 */
	public void setPickListValidator(IValidator pickListValidator) {
		this.pickListValidator = pickListValidator;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.idms.service.UserServiceImpl#setMultiPickListValidator(com.se.idms.
	 * cache.validate.IValidator)
	 */
	public void setMultiPickListValidator(IValidator multiPickListValidator) {
		this.multiPickListValidator = multiPickListValidator;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.idms.service.UserServiceImpl#setLegthValidator(com.se.idms.cache.
	 * validate.IValidator)
	 */
	public void setLegthValidator(IValidator legthValidator) {
		this.legthValidator = legthValidator;
	}

	/**
	 * For mobile scenario
	 */
	@SuppressWarnings("unchecked")
	@Override
	public Response resendPIN(String token, ResendPinRequest resendPinRequest) {
		LOGGER.info("Entered resendPIN() -> Start");
		LOGGER.info("Parameter token -> " + token);

		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		JSONObject response = new JSONObject();
		DocumentContext productDocCtx = null;
		String userData = null;
		String loginIdentifier = null;
		String tmpPR = null;
		String PRODUCT_JSON_STRING = null;
		String iPlanetDirectoryKey = null;
		String sendEmailOptType = "";
		String resendId = "";
		ObjectMapper objMapper = new ObjectMapper();

		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		response.put(UserConstants.STATUS, UserConstants.STATUS_FAILD);
		try {
			LOGGER.info("resendPinRequest  -> " + objMapper.writeValueAsString(resendPinRequest));
			try {
				iPlanetDirectoryKey = getSSOToken();
			} catch (IOException ioExp) {
				LOGGER.error("Unable to get SSO Token " + ioExp.getMessage(),ioExp);
				iPlanetDirectoryKey = "";
			}

			if ((null == resendPinRequest.getIdmsUserId() || resendPinRequest.getIdmsUserId().isEmpty())
					&& (null == resendPinRequest.getIDMS_Federated_ID__c()
							|| resendPinRequest.getIDMS_Federated_ID__c().isEmpty())
					&& (null == resendPinRequest.getFederationIdentifier()
							|| resendPinRequest.getFederationIdentifier().isEmpty())
					&& (null == resendPinRequest.getFederationId() || resendPinRequest.getFederationId().isEmpty())) {
				response.put(UserConstants.STATUS, UserConstants.STATUS_FAILD);
				response.put(UserConstants.MESSAGE, UserConstants.RESPONSE_MESSAGE_NULL);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error(UserConstants.RESPONSE_MESSAGE_NULL);
				LOGGER.info("Time taken by resendPIN() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();

			} else {

				if (null != resendPinRequest.getIdmsUserId() && !resendPinRequest.getIdmsUserId().isEmpty()) {
					resendId = resendPinRequest.getIdmsUserId();
				} else if (null != resendPinRequest.getIDMS_Federated_ID__c()
						&& !resendPinRequest.getIDMS_Federated_ID__c().isEmpty()) {
					resendId = resendPinRequest.getIDMS_Federated_ID__c();
				} else if (null != resendPinRequest.getFederationId()
						&& !resendPinRequest.getFederationId().isEmpty()) {
					resendId = resendPinRequest.getFederationId();
				} else {
					resendId = resendPinRequest.getFederationIdentifier();
				}
				/*
				 * if((null == resendId)|| (resendId.isEmpty()) ||
				 * "".equalsIgnoreCase(resendId)){ resendId =
				 * resendPinRequest.getIDMS_Federated_ID__c(); }
				 */
			}
			// Federation Identifier
			/*
			 * if (null == resendPinRequest.getFederationId() ||
			 * resendPinRequest.getFederationId().isEmpty()) {
			 * response.put(UserConstants.STATUS, UserConstants.STATUS_FAILD);
			 * response.put(UserConstants.MESSAGE,
			 * UserConstants.MANDATORY_FEDERATION_ID); elapsedTime =
			 * UserConstants.TIME_IN_MILLI_SECONDS - startTime; LOGGER.info(
			 * "Time taken by UserServiceImpl.setPassword() : " + elapsedTime);
			 * return
			 * Response.status(Response.Status.BAD_REQUEST).entity(response).
			 * build(); }
			 */

			if (null != resendId) {
				LOGGER.info(AUDIT_REQUESTING_USER + AUDIT_TECHNICAL_USER + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN
						+ AUDIT_OPENAM_API + AUDIT_OPENAM_GET_CALL + AUDIT_LOG_CLOSURE);
				LOGGER.info("Start: getUser() of OpenAMService in resendPIN for resendId:" + resendId);
				userData = productService.getUser(iPlanetDirectoryKey, resendId);
				LOGGER.info("End: getUser() of OpenAMService in resendPIN finished for resendId:" + resendId);
				LOGGER.info("user data from Openam: " + ChinaIdmsUtil.printOpenAMInfo(userData));

				productDocCtx = JsonPath.using(conf).parse(userData);

				if (UserConstants.USER_REGISTRATION.equalsIgnoreCase(resendPinRequest.getOperation())) {
					loginIdentifier = productDocCtx.read("$.mobile_reg[0]");
				} else if (UserConstants.UPDATE_USER_RECORD.equals(resendPinRequest.getOperation())) {
					loginIdentifier = productDocCtx.read("$.newmobile[0]");
				} else {
					loginIdentifier = productDocCtx.read(JsonConstants.LOGIN_ID_LOWER_0);
					if (null == loginIdentifier) {
						loginIdentifier = productDocCtx.read(JsonConstants.LOGIN_ID_UPPER_0);
					}
					if (null == loginIdentifier) {
						loginIdentifier = productDocCtx.read(JsonConstants.LOGIN_MOBILE_0);
					}
				}
				if (null != loginIdentifier && validateMobile(loginIdentifier)) {

					tmpPR = productDocCtx.read("$.tmp_password[0]");
					if (null == tmpPR || tmpPR.isEmpty()) {
						tmpPR = generateRamdomPassWord();
					} else {
						tmpPR = new String(Base64.decodeBase64(tmpPR));
					}

					String hotpService = null;
					String userService = null;
					if (UserConstants.USER_REGISTRATION.equalsIgnoreCase(resendPinRequest.getOperation())) {
						hotpService = UserConstants.HOTP_MOBILE_USER_REGISTRATION;
						userService = UserConstants.CREATE_USER_SERVICE;
						sendEmailOptType = EmailConstants.USERREGISTRATION_OPT_TYPE;
					} else if (UserConstants.UPDATE_USER_RECORD.equalsIgnoreCase(resendPinRequest.getOperation())) {
						hotpService = UserConstants.HOTP_MOBILE_UPDATE;
						userService = UserConstants.UPDATE_USER_SERVICE;
						sendEmailOptType = EmailConstants.UPDATEUSERRECORD_OPT_TYPE;
					} else {
						hotpService = UserConstants.HOTP_MOBILE_RESET_PR;
						userService = UserConstants.CREATE_USER_SERVICE;
						sendEmailOptType = EmailConstants.SETUSERPWD_OPT_TYPE;
					}

					// PRODUCT_JSON_STRING = "{" + "\"userPassword\": \"" +
					// tmpPR + "\"" + "}";

					// LOGGER.info("UserServiceImpl:resendPIN :
					// productService.updateUser : Request -> " +
					// PRODUCT_JSON_STRING);
					/*
					 * productService.updateUser(UserConstants.
					 * IPLANET_DIRECTORY_PRO + iPlanetDirectoryKey,
					 * resendPinRequest.getIdmsUserId(), PRODUCT_JSON_STRING);
					 */

					// To update authId in openAM extended attribute
					// PRODUCT_JSON_STRING = sendOtp(hotpService,
					// resendPinRequest.getIdmsUserId(), tmpPR, userService);

					String regestrationSource = productDocCtx.read("$.registerationSource[0]");
					if ((EmailConstants.UPDATEUSERRECORD_OPT_TYPE.equalsIgnoreCase(sendEmailOptType)
							&& null != productDocCtx.read("$.newmail[0]"))
							|| EmailConstants.USERREGISTRATION_OPT_TYPE.equalsIgnoreCase(sendEmailOptType)
							|| EmailConstants.SETUSERPWD_OPT_TYPE.equalsIgnoreCase(sendEmailOptType)) {
						String otp = sendEmail.generateOtp(resendId);
						LOGGER.info("Successfully OTP generated for resendId:" + resendId);
						sendEmail.sendSMSNewGateway(otp, sendEmailOptType, resendId, regestrationSource);

						sendEmail.sendOpenAmMobileEmail(otp, sendEmailOptType, resendId, regestrationSource);
						// sendEmail.sendOpenAmEmail(otp, sendEmailOptType,
						// resendId, regestrationSource);
					} else {
						response.put(UserConstants.STATUS, UserConstants.STATUS_FAILD);
						response.put(UserConstants.MESSAGE, UserConstants.RESEND_UPDATEOPTTYPE_ERROR);
						elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
						LOGGER.error(UserConstants.RESEND_UPDATEOPTTYPE_ERROR);
						LOGGER.info("Time taken by resendPIN() : " + elapsedTime);
						return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(response).build();
					}
					// LOGGER.info("UserServiceImpl:resendPIN : sendOtp :
					// Response -> " + PRODUCT_JSON_STRING);
					// To update authId in openAM extended attribute
					/*
					 * if (null != PRODUCT_JSON_STRING &&
					 * !PRODUCT_JSON_STRING.isEmpty()) { LOGGER.info(
					 * "To update authId in openAM extended attribute :: updateUser -> "
					 * + PRODUCT_JSON_STRING); LOGGER.info(AUDIT_REQUESTING_USER
					 * + AUDIT_TECHNICAL_USER + AUDIT_IMPERSONATING_USER +
					 * AUDIT_API_ADMIN + AUDIT_OPENAM_API +
					 * AUDIT_OPENAM_UPDATE_CALL + AUDIT_LOG_CLOSURE);
					 * 
					 * productService.updateUser(UserConstants.
					 * IPLANET_DIRECTORY_PRO + iPlanetDirectoryKey,
					 * resendPinRequest.getIdmsUserId(), PRODUCT_JSON_STRING); }
					 */
				} else {
					response.put(UserConstants.STATUS, UserConstants.STATUS_FAILD);
					response.put(UserConstants.MESSAGE, UserConstants.RESEND_ONLYMOBILE_ERROR_MESSAGE);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.error(UserConstants.RESEND_ONLYMOBILE_ERROR_MESSAGE);
					LOGGER.info("Time taken by resendPIN() : " + elapsedTime);
					return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(response).build();
				}

			} else {
				response.put(UserConstants.STATUS, UserConstants.STATUS_FAILD);
				response.put(UserConstants.MESSAGE, UserConstants.RESPONSE_MESSAGE);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error(UserConstants.RESPONSE_MESSAGE);
				LOGGER.info("Time taken by resendPIN() : " + elapsedTime);
				return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(response).build();
			}
		} catch (NotFoundException e) {
			response.put(UserConstants.MESSAGE, UserConstants.ERROR_RESEND_PIN);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by resendPIN() : " + elapsedTime);
			LOGGER.error("NotFoundException in Resending User PIN :: -> " + e.getMessage(),e);
			return Response.status(Response.Status.NOT_FOUND).entity(response).build();
		} catch (BadRequestException e) {
			response.put(UserConstants.MESSAGE, UserConstants.ERROR_RESEND_PIN);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by resendPIN() : " + elapsedTime);
			LOGGER.error("BadRequestException in Resending User PIN :: -> " + e.getMessage(),e);
			return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
		} catch (Exception e) {
			response.put(UserConstants.MESSAGE, UserConstants.ERROR_RESEND_PIN);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.resendPIN() : " + elapsedTime);
			LOGGER.error("Exception in Resending User PIN :: -> " + e.getMessage(),e);
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(response).build();
		}
		response.put(UserConstants.STATUS, successStatus);
		response.put(UserConstants.MESSAGE, "Pin Code has been sent successfully");
		elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		LOGGER.info("Time taken by UserServiceImpl.resendPIN() : " + elapsedTime);
		return Response.status(Response.Status.OK).entity(response).build();
	}

	/**
	 * This method will update the existing user password to new password
	 * 
	 */
	@Override
	public Response updatePassword(String token, UpdatePasswordRequest updatePasswordRequest) {
		LOGGER.info("Entered updatePassword() -> Start");
		LOGGER.info("Parameter token -> " + token);
		LOGGER.info("Source -> " + updatePasswordRequest.getIDMS_Profile_update_source());
		LOGGER.info("UIFlag -> " + updatePasswordRequest.getUIFlag());

		DocumentContext productDocCtx = null;
		String iPlanetDirectoryKey = null;
		String PRODUCT_JSON_STRING = null;
		String userId = null;
		String newPassword = "";
		String userData = "";
		String userEmail = null, userMobile = null;
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		String openamVnew = null;
		String fedId = null;
		Integer vNewCntValue = 0;
		ObjectMapper objMapper = new ObjectMapper();
		ErrorResponse errorResponse = new ErrorResponse();
		Response passwordOpenAMResponse = null;
		boolean isPasswordUpdatedInUIMS = false;
		try {
			// LOGGER.info("UserServiceImpl:updatePassword : sendOtp : Request
			// -> "+ objMapper.writeValueAsString(updatePasswordRequest));
			// Fetching the userid from the Authorization Token

			if ((null == updatePasswordRequest.getUIFlag()
					|| !UserConstants.TRUE.equalsIgnoreCase(updatePasswordRequest.getUIFlag()))
					&& (!UserConstants.UIMS.equalsIgnoreCase(updatePasswordRequest.getIDMS_Profile_update_source()))) {
				errorResponse.setStatus(errorStatus);
				errorResponse.setMessage(UserConstants.UPDATE_USER_REC_BLCOKED);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error is " + errorResponse.getMessage());
				LOGGER.info("Time taken by updatePassword() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
			}

			if (null == token || token.isEmpty()) {
				errorResponse.setStatus(errorStatus);
				errorResponse.setMessage("Authorization token is null or missing");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Authorization token is null or missing");
				LOGGER.info("Time taken by updatePassword() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
			}

			LOGGER.info("Start: getUserInfoByAccessToken() of openam");
			String userInfoByAccessToken = openAMTokenService.getUserInfoByAccessToken(token, "/se");
			LOGGER.info("End: getUserInfoByAccessToken() of openam finished");
			// Get fedid from openAMTokenService service

			Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
			productDocCtx = JsonPath.using(conf).parse(userInfoByAccessToken);
			userId = productDocCtx.read("$.sub");
			fedId = userId; // productDocCtx.read("$.federationID"); //There is
							// no federationID in the token response
			LOGGER.info("UserId or FedID = " + fedId);
			
			// Authenticating the User
			String oldPassword = updatePasswordRequest.getExistingPwd();
			newPassword = updatePasswordRequest.getNewPwd();
			String updateSource = updatePasswordRequest.getIDMS_Profile_update_source();

			// Evaluating the input parameters
			if (updateSource == null || updateSource.isEmpty()) {
				errorResponse.setStatus(errorStatus);
				errorResponse.setMessage("Update source not found");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error is " + errorResponse.getMessage());
				LOGGER.info("Time taken by updatePassword() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST.getStatusCode()).entity(errorResponse).build();
			}

			if (oldPassword == null || oldPassword.isEmpty() || newPassword == null || newPassword.isEmpty()) {
				PasswordRecoveryResponse passwordEmptyResponse = new PasswordRecoveryResponse();
				passwordEmptyResponse.setStatus(errorStatus);
				passwordEmptyResponse.setMessage("Existing Password and  New Password are mandatory values.");
				passwordEmptyResponse.setIdmsUserRecord(null);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error is " + passwordEmptyResponse.getMessage());
				LOGGER.info("Time taken by updatePassword() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST.getStatusCode()).entity(passwordEmptyResponse)
						.build();
			}

			if ((null != oldPassword && null != newPassword) && (oldPassword.equals(newPassword))) {
				errorResponse.setStatus(errorStatus);
				errorResponse.setMessage(UserConstants.UPDATE_PR_EQUAL);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error is " + errorResponse.getMessage());
				LOGGER.info("Time taken by updatePassword() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
			}

			// Pattern pswNamePtrn =
			// Pattern.compile("((?=.*\\d)(?=.*[a-z])(?=.*[A-Z])(?=.*[@#$%]).{6,15})");
			/*Pattern pswNamePtrn = Pattern.compile(UserConstants.PASSWORD_REGEX);
			if (null != newPassword && !newPassword.isEmpty()) {
				Matcher mtch = pswNamePtrn.matcher(newPassword);
				if (!mtch.matches()) {
					errorResponse.setStatus(errorStatus);
					errorResponse.setMessage("Password does not match with password policy.");
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.error("Error is " + errorResponse.getMessage());
					LOGGER.info("Time taken by updatePassword() : " + elapsedTime);
					return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
				}
			}*/

			// Fetching the Username i.e IDMSUID

			try {
				iPlanetDirectoryKey = getSSOToken();
			} catch (IOException ioExp) {
				LOGGER.error("Unable to get SSO Token " + ioExp.getMessage(),ioExp);
				iPlanetDirectoryKey = "";
			}

			LOGGER.info("AUDIT:requestingUser->" + userId + "," + "impersonatingUser : amadmin,"
					+ "openAMApi:GET/se/users/getUser{userId}");
			if (null != userId) {
				LOGGER.info("Start: retrieving getUser() of OpenAMService for userId:" + userId);
				userData = productService.getUser(iPlanetDirectoryKey, userId);
				LOGGER.info("End: getUser() of OpenAMService finished for userId:" + userId);
				LOGGER.info("userData:" + ChinaIdmsUtil.printOpenAMInfo(userData));
			}
			conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
			productDocCtx = JsonPath.using(conf).parse(userData);

			userEmail = productDocCtx.read("$.Loginid[0]");
			if (null == userEmail || userEmail.isEmpty()) {
				userEmail = productDocCtx.read("$.loginid[0]");
			}
			
			userMobile = productDocCtx.read("$.login_mobile[0]");
			if (null == userMobile || userMobile.isEmpty()) {
				userMobile = productDocCtx.read("$.mobile_reg[0]");
			}
			LOGGER.info("userEmail = " + userEmail +" , userMobile = "+userMobile);

			// Authenticate the given credentials
			try {
				LOGGER.info("Start: Authenticating oldpassword in OpenDJ for userid=" + userId);
				productService.authenticateIdmsChinaUser(userId, oldPassword, UserConstants.REALM);
				LOGGER.info("End: Authenticating oldpassword in OpenDJ finished for userid=" + userId);
			} catch (Exception e) {
				LOGGER.error(e.getMessage(),e);
				errorResponse.setStatus(errorStatus);
				errorResponse.setMessage("Existing Password is not correct");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error is " + errorResponse.getMessage());
				LOGGER.info("Time taken by updatePassword() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
			}
			// Building query to update new password
			if (null != updatePasswordRequest.getUIFlag() && UserConstants.TRUE.equalsIgnoreCase(updatePasswordRequest.getUIFlag())) {
				if (((null != newPassword && !newPassword.isEmpty())) && !checkPasswordPolicy(newPassword.trim(),
						productDocCtx.read("$.givenName[0]"), productDocCtx.read("$.sn[0]"), userEmail, userMobile)) {
					errorResponse.setStatus(errorStatus);
					errorResponse.setMessage(UserConstants.PR_POLICY);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;					
					LOGGER.error("Error is " + errorResponse.getMessage());
					LOGGER.info("Time taken by updatePassword() : " + elapsedTime);
					return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
				}
			}

			PRODUCT_JSON_STRING = "{" + "\"userPassword\": \"" + newPassword.trim() + "\"" + "}";
			openamVnew = null != productDocCtx.read("$.V_New[0]") ? getValue(productDocCtx.read("$.V_New[0]"))
					: getDelimeter();

			if (null != vNewCntValue && null != openamVnew) {
				vNewCntValue = Integer.parseInt(openamVnew) + 1;
			}
			String version = "{\"V_New\": \"" + vNewCntValue + "\"" + "}";

			// Adding V_New
			LOGGER.info("Start: UpdatePassword - Updating version in openam for userId=" + userId);
			productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userId, version);
			LOGGER.info("End: UpdatePassword - Updating version in openam finished for userId=" + userId);

			// updating new password in openAM
			LOGGER.info("Start: updating new password in openam for userId=" + userId);
			passwordOpenAMResponse = updatePasswordHistory(iPlanetDirectoryKey, userId, PRODUCT_JSON_STRING);
			if (200 != passwordOpenAMResponse.getStatus()) {
				return passwordOpenAMResponse;
			}
			LOGGER.info("End: updating new password in openam finished for userId=" + userId);

			// check UIMSPasswordSync to call sync or Async method
			if (pickListValidator.validate(UserConstants.UIMSPasswordSync, UserConstants.TRUE)) {
				LOGGER.info(
						"Start: SYNC method of updateUIMSPassword() of UimsSetPasswordSoapService for userId" + userId);
				isPasswordUpdatedInUIMS = uimsSetPasswordSoapService.updateUIMSPassword(fedId, userId,
						updatePasswordRequest.getExistingPwd(), updatePasswordRequest.getNewPwd(),
						vNewCntValue.toString(), UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey);
				LOGGER.info(
						"End: SYNC method of updateUIMSPassword() of UimsSetPasswordSoapService finished for userId="
								+ userId);
			} else {
				// Calling Async method of setUIMSPassword
				LOGGER.info("Start: ASYNC method of updateUIMSPassword() of UIMSUserManagerSoapService for userId="
						+ userId);
				uimsUserManagerSoapService.updateUIMSPassword(fedId, userId, updatePasswordRequest.getExistingPwd(),
						updatePasswordRequest.getNewPwd(), vNewCntValue.toString(),
						UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey);
				LOGGER.info(
						"End: ASYNC method of updateUIMSPassword() of UIMSUserManagerSoapService finished for userId="
								+ userId);
			}
			if (isPasswordUpdatedInUIMS) {
				userResponse.setStatus(successStatus);
				userResponse.setMessage("User Password updated successfully in IDMS China and UIMS");
				LOGGER.info("User Password updated successfully in IDMS China and UIMS");
				return Response.status(Response.Status.OK).entity(userResponse).build();
			} else {
				userResponse.setStatus(successStatus);
				userResponse.setMessage("User Password updated successfully in IDMS China");
				LOGGER.info("User Password updated successfully in IDMS China");
				return Response.status(Response.Status.OK).entity(userResponse).build();
			}
		} catch (NotAuthorizedException e) {
			LOGGER.error( e.getMessage(),e);
			userResponse.setStatus("INVALID_SESSION_ID");
			userResponse.setMessage("Session expired or invalid");
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.error("NotAuthorizedException in updatePassword():" + userResponse.getMessage());
			LOGGER.info("Time taken by updatePassword() : " + elapsedTime);
			return Response.status(Response.Status.UNAUTHORIZED).entity(userResponse).build();
		} catch (MalformedURLException me) {
			errorResponse.setStatus(errorStatus);
			errorResponse.setMessage(me.getMessage());
			LOGGER.error("MalformedURLException in updatePassword():: ->" + me.getMessage(),me);
			return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
		} catch (Exception e) {

			errorResponse.setStatus(errorStatus);
			errorResponse.setMessage(e.getMessage());
			LOGGER.error("Exception in updatePassword():" + e.getMessage(),e);
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(errorResponse).build();
		}
		// return updatePasswordSuccessResponse(userId, userName, startTime);
	}

	/*
	 * private Response updatePasswordErrorResponse(long startTime) {
	 * LOGGER.info("Entered updatePasswordErrorResponse() -> Start");
	 * LOGGER.info("Parameter startTime -> " + startTime); long elapsedTime;
	 * ErrorResponse errorResponse = new ErrorResponse();
	 * errorResponse.setStatus(errorStatus); errorResponse.setMessage(
	 * "Error in Updating User Password."); elapsedTime =
	 * UserConstants.TIME_IN_MILLI_SECONDS - startTime; LOGGER.error("Error is "
	 * +errorResponse.getMessage()); LOGGER.info(
	 * "Time taken by UserServiceImpl.updatePassword() : " + elapsedTime);
	 * return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(
	 * errorResponse).build(); }
	 */

	/*
	 * private Response updatePasswordSuccessResponse(String userId, String
	 * userName, long startTime) { LOGGER.info(
	 * "Entered updatePasswordSuccessResponse() -> Start"); LOGGER.info(
	 * "Parameter userId -> " + userId+" ,userName -> "+userName); LOGGER.info(
	 * "Parameter startTime -> " + startTime); UpdatePasswordResponse
	 * updatePasswordResponse; Attributes attributes = new Attributes();
	 * IDMSUserRecordUpdatePassword idmsUserRecord = new
	 * IDMSUserRecordUpdatePassword(); idmsUserRecord.setAttributes(attributes);
	 * idmsUserRecord.setId(userId); idmsUserRecord.setUserName(userName);
	 * idmsUserRecord.setIDMS_Federated_ID__c(""); updatePasswordResponse = new
	 * UpdatePasswordResponse(idmsUserRecord);
	 * updatePasswordResponse.setStatus(successStatus);
	 * updatePasswordResponse.setMessage("Password Updated successfully"); long
	 * elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
	 * LOGGER.info(updatePasswordResponse.getMessage()); LOGGER.info(
	 * "Time taken by UserServiceImpl.updatePassword() : " + elapsedTime);
	 * return
	 * Response.status(Response.Status.OK).entity(updatePasswordResponse).build(
	 * ); }
	 */

	private boolean validateMobile(String mobileNumber) {
		LOGGER.info("Entered validateMobile() -> Start");
		LOGGER.info("Parameter mobileNumber -> " + mobileNumber);

		if (mobileNumber.matches("\\d{11}")) {
			return true;
		}
		return false;
	}

	/**
	 * From UIMS side when user want to activate
	 */
	@Override
	public Response setPassword(String authorizedToken, String clientId, String clientSecret,
			SetPasswordRequest setPasswordRequest) {
		LOGGER.info("Entered setPassword() -> Start");
		// LOGGER.info("Parameter authorizedToken -> " + authorizedToken);
		LOGGER.info("id -> " + setPasswordRequest.getId() + " ,FederationIdentifier -> "
				+ setPasswordRequest.getFederationIdentifier());
		LOGGER.info("IDMS_Federated_ID__c -> " + setPasswordRequest.getIDMS_Federated_ID__c()
				+ " ,IDMS_Profile_update_source -> " + setPasswordRequest.getIDMS_Profile_update_source());
		LOGGER.info("Token -> " + setPasswordRequest.getToken() + " ,UIFlag -> " + setPasswordRequest.getUIFlag());

		SetPasswordErrorResponse response = new SetPasswordErrorResponse();
		DocumentContext productDocCtx = null;
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;

		// String userId = "d4b0079f-135e-4c2f-8689-8672f6453ed4";
		String userId = "";
		String userData = "";
		String authId = "";
		String amlbcookieValue = null;
		String openamVnew = null;
		Integer vNewCntValue = 0;
		String version = null;
		String iPlanetDirectoryKey = null;
		String usermail = "";
		boolean validPinStatus = false;
		ObjectMapper objMapper = new ObjectMapper();
		String federationID = null;
		String emailOrMobile = null;
		String loginIdentifierType = null;
		String PRODUCT_JSON_STRING = null;

		try {
			// LOGGER.info("UserServiceImpl:updatePassword : setPassword :
			// Request -> " + objMapper.writeValueAsString(setPasswordRequest));

			if ((!UserConstants.UIMS.equalsIgnoreCase(setPasswordRequest.getIDMS_Profile_update_source()))
					&& (null == setPasswordRequest.getUIFlag()
							|| !UserConstants.TRUE.equalsIgnoreCase(setPasswordRequest.getUIFlag()))) {

				response.setStatus(errorStatus);
				response.setMessage(UserConstants.OPERATION_BLCOKED);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error in setPassword():" + response.getMessage());
				LOGGER.info("Time taken by setPassword() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}

			// Evaluating the input parameters
			if (null == setPasswordRequest.getIDMS_Profile_update_source()
					|| setPasswordRequest.getIDMS_Profile_update_source().isEmpty()) {
				response.setStatus(errorStatus);
				response.setMessage(UserConstants.PROFILE_UPDATE_SOURCE);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error in setPassword():" + response.getMessage());
				LOGGER.info("Time taken by setPassword() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}
			if ((null != setPasswordRequest.getIDMS_Profile_update_source()
					&& !setPasswordRequest.getIDMS_Profile_update_source().isEmpty())
					&& (!pickListValidator.validate(UserConstants.UPDATE_SOURCE,
							setPasswordRequest.getIDMS_Profile_update_source()))) {
				response.setMessage(UserConstants.INVALID_VALUE_IDMS + UserConstants.UPDATE_SOURCE);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}
			if (null != setPasswordRequest.getIDMS_Profile_update_source()
					&& UserConstants.UIMS.equalsIgnoreCase(setPasswordRequest.getIDMS_Profile_update_source())) {

				// Federation Identifier
				if ((null == setPasswordRequest.getIDMS_Federated_ID__c()
						|| setPasswordRequest.getIDMS_Federated_ID__c().isEmpty())
						&& (null == setPasswordRequest.getFederationIdentifier()
								|| setPasswordRequest.getFederationIdentifier().isEmpty())) {
					response.setStatus(errorStatus);
					response.setMessage(UserConstants.MANDATORY_FEDERATION_ID);
					response.setId(userId);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.error("Error in setPassword():" + response.getMessage());
					LOGGER.info("Time taken by setPassword() : " + elapsedTime);
					return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
				}
				if (null == clientId || null == clientSecret) {
					response.setStatus(errorStatus);
					response.setMessage(UserConstants.UIMS_CLIENTID_SECRET);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.error("Error in setPassword():" + response.getMessage());
					LOGGER.info("Time taken by setPassword() : " + elapsedTime);
					return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
				}
				if ((null != clientId && !clientId.equalsIgnoreCase(uimsClientId))
						|| (null != clientSecret && !clientSecret.equalsIgnoreCase(uimsClientSecret))) {
					response.setStatus(errorStatus);
					response.setMessage(UserConstants.INVALID_UIMS_CREDENTIALS);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.error("Error in setPassword() :" + response.getMessage());
					LOGGER.info("Time taken by setPassword() : " + elapsedTime);
					return Response.status(Response.Status.UNAUTHORIZED).entity(response).build();
				}
			}

			if (null != setPasswordRequest.getIDMS_Profile_update_source()
					&& !UserConstants.UIMS.equalsIgnoreCase(setPasswordRequest.getIDMS_Profile_update_source())) {
				// Federation Identifier
				if ((null == setPasswordRequest.getId() || setPasswordRequest.getId().isEmpty())
						|| (null == setPasswordRequest.getIDMS_Federated_ID__c()
								|| setPasswordRequest.getIDMS_Federated_ID__c().isEmpty())) {
					response.setStatus(errorStatus);
					response.setMessage(UserConstants.MANDATORY_FEDERATION_ID);
					response.setId(userId);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.error("Error in setPassword() : " + response.getMessage());
					LOGGER.info("Time taken by setPassword() : " + elapsedTime);
					return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
				}

				userId = setPasswordRequest.getId();
				if (null == userId || userId.isEmpty()) {
					userId = setPasswordRequest.getIDMS_Federated_ID__c();
				}
				if (!userId.startsWith("cn00")) {
					response.setStatus(errorStatus);
					response.setMessage("User Id should start with cn00");
					response.setId(userId);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.error("Error in setPassword():" + response.getMessage());
					LOGGER.info("Time taken by setPassword() : " + elapsedTime);
					return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
				}

				if (null == userId || userId.isEmpty()) {
					SetPasswordResponse setPasswordResponse;
					setPasswordResponse = new SetPasswordResponse(null);
					setPasswordResponse.setStatus(errorStatus);
					setPasswordResponse.setMessage("User not found based on user Id");
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.error("Error in setPassword():" + response.getMessage());
					LOGGER.info("Time taken by setPassword() : " + elapsedTime);
					return Response.status(Response.Status.BAD_REQUEST).entity(setPasswordResponse).build();
				}

				// Checking the newPassword any policy

				Pattern pswNamePtrn = Pattern.compile(UserConstants.PASSWORD_REGEX);
				Matcher mtch = pswNamePtrn.matcher(setPasswordRequest.getNewPwd());
				if (!mtch.matches()) {
					response.setStatus(errorStatus);
					response.setMessage("New password is not following password policy");
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.error("Error in setPassword():" + response.getMessage());
					LOGGER.info("Time taken by setPassword() : " + elapsedTime);
					return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
				}

			}
			// Profile Update Source
			if (null == setPasswordRequest.getIDMS_Profile_update_source()
					|| setPasswordRequest.getIDMS_Profile_update_source().isEmpty()
					|| "null".equalsIgnoreCase(setPasswordRequest.getIDMS_Profile_update_source())) {
				response.setStatus(errorStatus);
				response.setMessage("Update source not found");
				response.setId(userId);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error in setPassword():" + response.getMessage());
				LOGGER.info("Time taken by setPassword() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}

			// NewPwd
			if (null == setPasswordRequest.getNewPwd() || setPasswordRequest.getNewPwd().isEmpty()) {
				response.setStatus(errorStatus);
				response.setMessage("NewPwd is mandatory, can not be blank");
				response.setId(userId);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error in setPassword():" + response.getMessage());
				LOGGER.info("Time taken by setPassword() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}

			// UerId Check
			// Token
			if (null != setPasswordRequest.getIDMS_Profile_update_source()
					&& !UserConstants.UIMS.equalsIgnoreCase(setPasswordRequest.getIDMS_Profile_update_source())) {
				if (null == setPasswordRequest.getToken() || setPasswordRequest.getToken().isEmpty()) {
					SetPasswordResponse setPasswordResponse;
					setPasswordResponse = new SetPasswordResponse(null);
					setPasswordResponse.setStatus(errorStatus);
					setPasswordResponse.setMessage(" Token cannot be blank or null");
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.error("Error in setPassword():" + setPasswordResponse.getMessage());
					LOGGER.info("Time taken by setPassword() : " + elapsedTime);
					return Response.status(Response.Status.BAD_REQUEST).entity(setPasswordResponse).build();
				}
			}
			// Get iPlanetDirectory Pro Admin token for admin

			// LOGGER.info(" UserServiceImpl :: ActivateUser getSSOToken ");
			try {
				iPlanetDirectoryKey = getSSOToken();
			} catch (IOException ioExp) {
				LOGGER.error("Unable to get SSO Token " + ioExp.getMessage(),ioExp);
				iPlanetDirectoryKey = "";
			}

			if (null != setPasswordRequest.getIDMS_Profile_update_source()
					&& UserConstants.UIMS.equalsIgnoreCase(setPasswordRequest.getIDMS_Profile_update_source())
					&& (null != setPasswordRequest.getIDMS_Federated_ID__c()
							|| null != setPasswordRequest.getFederationIdentifier())) {

				if (null != setPasswordRequest.getFederationIdentifier()) {
					userId = setPasswordRequest.getFederationIdentifier();
				} else {
					userId = setPasswordRequest.getIDMS_Federated_ID__c();
				}

				Callable<String> callableGetUser = new Callable<String>() {
					public String call() throws Exception {
						LOGGER.info("Start: getUser() of OpenAMService");
						userIdExistInUIMS = productService.getUser(getSSOToken(),
								(null != setPasswordRequest.getFederationIdentifier()
										? setPasswordRequest.getFederationIdentifier()
										: setPasswordRequest.getIDMS_Federated_ID__c()));
						LOGGER.info(
								"End: getUser() of OpenAMService finished ... userIdExistInUIMS= " + userIdExistInUIMS);
						return userIdExistInUIMS;
					}
				};
				Retryer<String> retryerGetUser = RetryerBuilder.<String> newBuilder()
						.retryIfResult(Predicates.<String> isNull()).retryIfExceptionOfType(Exception.class)
						.withWaitStrategy(WaitStrategies.fixedWait(18000L, TimeUnit.MILLISECONDS))
						.withStopStrategy(StopStrategies.stopAfterAttempt(5)).build();

				try {
					retryerGetUser.call(callableGetUser);
				} catch (Exception e) {
					LOGGER.error("Exception in getting user details= " + e.getMessage(),e);

				}

				Response fedResponse = checkUserExistsWithFederationID(iPlanetDirectoryKey, userId, startTime);
				if (fedResponse.getStatus() == 200) {
					JSONObject uimsResponse = (JSONObject) fedResponse.getEntity();
					userId = (String) uimsResponse.get("userId");
					PRODUCT_JSON_STRING = "{" + "\"userPassword\": \"" + setPasswordRequest.getNewPwd().trim() + "\""
							+ "}";
					LOGGER.info("Start: updateUser() of openam to update new password for userid=" + userId);
					productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userId,
							PRODUCT_JSON_STRING);
					LOGGER.info("End: updateUser() of openam to update new password finished for userid=" + userId);
				} else {
					return fedResponse;
				}

			} else {
				// Fetching the AuthID from the UserId
				LOGGER.info("AUDIT:requestingUser->" + userId + "," + "impersonatingUser : amadmin,"
						+ "openAMApi:GET/se/users/getUser{userId}");
				if (null != userId) {
					try {
						LOGGER.info("Start: getUser() of OpenAm for userId=" + userId);
						userData = productService.getUser(iPlanetDirectoryKey, userId);
						LOGGER.info("End: getUser() of OpenAm finished for userId=" + userId);
					} catch (NotFoundException e) {
						LOGGER.error( e.getMessage(),e);
						SetPasswordResponse setPasswordResponse;
						setPasswordResponse = new SetPasswordResponse(null);
						setPasswordResponse.setStatus(errorStatus);
						setPasswordResponse.setMessage("User not found based on user Id");
						elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
						LOGGER.error("Error is " + setPasswordResponse.getMessage());
						LOGGER.info("Time taken by UserServiceImpl.setPassword() : " + elapsedTime);
						return Response.status(Response.Status.NOT_FOUND).entity(setPasswordResponse).build();
					} catch (Exception e) {
						LOGGER.error(e.getMessage(),e);
						SetPasswordResponse setPasswordResponse;
						setPasswordResponse = new SetPasswordResponse(null);
						setPasswordResponse.setStatus(errorStatus);
						setPasswordResponse.setMessage("User not found based on user Id");
						elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
						LOGGER.error("Error is " + setPasswordResponse.getMessage());
						LOGGER.info("Time taken by UserServiceImpl.setPassword() : " + elapsedTime);
						return Response.status(Response.Status.BAD_REQUEST).entity(setPasswordResponse).build();
					}

				}
				Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
				productDocCtx = JsonPath.using(conf).parse(userData);

				usermail = productDocCtx.read("$.mail[0]");

				if ("[]".equalsIgnoreCase(productDocCtx.read("$.AuthID[0]"))
						|| "[]".equalsIgnoreCase(productDocCtx.read("$.authId[0]"))) {
					throw new Exception("Pin got expired or invalid!!");
				} else {
					authId = null != productDocCtx.read("$.AuthID[0]") ? getValue(productDocCtx.read("$.AuthID[0]"))
							: getDelimeter();
					if (authId == null || authId.isEmpty()) {
						authId = null != productDocCtx.read("$.authId[0]") ? getValue(productDocCtx.read("$.authId[0]"))
								: getDelimeter();
					}
				}

				federationID = productDocCtx.read("$.federationID[0]");

				emailOrMobile = productDocCtx.read("$.loginid[0]");

				emailOrMobile = productDocCtx.read("$.mail[0]");
				loginIdentifierType = UserConstants.EMAIL;
				if (null == emailOrMobile) {
					emailOrMobile = productDocCtx.read("$.mobile_reg[0]");
					loginIdentifierType = UserConstants.MOBILE;
				}

				amlbcookieValue = null != productDocCtx.read("$.amlbcookie")
						? getValue(productDocCtx.read("$.amlbcookie").toString()) : getDelimeter();
				// amlbcookieValue = UserConstants.AMLB_COOKIE+amlbcookieValue;

				openamVnew = null != productDocCtx.read("$.V_New[0]") ? getValue(productDocCtx.read("$.V_New[0]"))
						: getDelimeter();
				if (null != vNewCntValue && null != openamVnew) {
					vNewCntValue = Integer.parseInt(openamVnew) + 1;
				}
				version = "{\"V_New\": \"" + vNewCntValue + "\"" + "}";

				if (null == userData || userData.isEmpty()) {
					response.setStatus(errorStatus);
					response.setMessage("User not found");
					response.setId(userId);
					return Response.status(Response.Status.NOT_FOUND).entity(response).build();
				}

				authId = authId.replaceAll("\\[", "");
				authId = authId.replaceAll("\\]", "");
				LOGGER.info("Authorisation Id ------------->" + authId);
				productDocCtx = JsonPath.using(conf).parse(userData);
				productDocCtx = JsonPath.using(conf).parse(UserConstants.OPT_SUBMIT_REQUEST);
				productDocCtx.set(JsonConstants.AUTH_ID, authId);
				productDocCtx.set("$.callbacks[0].input[0].value", setPasswordRequest.getToken());
				productDocCtx.set("$.callbacks[1].input[0].value", 0);

				String userRequest = productDocCtx.jsonString();
				userRequest = userRequest.replace("\\\"", "");
				// String hotpValidate = "";
				Response hotpValidate;
				try {
					/*
					 * hotpValidate =
					 * provisionalService.otpAuthentication(amlbcookieValue,
					 * UserConstants.HOTP_EMAIL, UserConstants.HOTP_SERVICE,
					 * UserConstants.HOTP_EMAIL, userRequest);
					 */
					validPinStatus = sendEmail.validatePin(setPasswordRequest.getToken(), userId);
					if (!validPinStatus) {
						throw new Exception("Pin got expired or invalid!!");
					}
					PRODUCT_JSON_STRING = "{" + "\"userPassword\": \"" + setPasswordRequest.getNewPwd().trim() + "\""
							+ "}";
					/**
					 * Commenting below line updateuser since we are updating
					 * after uims sync
					 */
					/*
					 * productService.updateUser(UserConstants.CHINA_IDMS_TOKEN
					 * + iPlanetDirectoryKey, userId, PRODUCT_JSON_STRING);
					 */
				} catch (Exception e) {
					LOGGER.error( e.getMessage(),e);
					response.setStatus(errorStatus);
					response.setMessage(UserConstants.PIN_INVALID);
					response.setId(userId);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.error("Error is " + response.getMessage());
					LOGGER.info("Time taken by UserServiceImpl.setPassword() : " + elapsedTime);
					// productService.sessionLogout(UserConstants.IPLANET_DIRECTORY_PRO+iPlanetDirectoryKey,
					// "logout");
					return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
				}
				/*
				 * LOGGER.info("AUDIT:requestingUser" + userId + "," +
				 * "impersonatingUser : amadmin," +
				 * "openAMApi:POST/se/users/updateUser{userId}"); if
				 * (hotpValidate.getStatus() == 200) { // if (hotpValidate !=
				 * null && !hotpValidate.isEmpty()) { String PRODUCT_JSON_STRING
				 * = "{" + "\"userPassword\": \"" +
				 * setPasswordRequest.getNewPwd().trim() + "\"" + "}";
				 * productService.updateUser(UserConstants.IPLANET_DIRECTORY_PRO
				 * + iPlanetDirectoryKey, userId, PRODUCT_JSON_STRING); }else{
				 * userData = IOUtils.toString((InputStream)
				 * hotpValidate.getEntity()); productDocCtx =
				 * JsonPath.using(conf).parse(userData);
				 * response.setStatus(errorStatus);
				 * response.setMessage(productDocCtx.read("$.message"));
				 * response.setId(userId); elapsedTime =
				 * UserConstants.TIME_IN_MILLI_SECONDS - startTime; LOGGER.info(
				 * "Time taken by UserServiceImpl.setPassword() : " +
				 * elapsedTime); return
				 * Response.status(Response.Status.REQUEST_TIMEOUT).entity(
				 * response).build(); }
				 */
			}
			try {
				if (null != setPasswordRequest.getIDMS_Profile_update_source()
						&& !UserConstants.UIMS.equalsIgnoreCase(setPasswordRequest.getIDMS_Profile_update_source())) {
					// Adding V_New
					LOGGER.info("Start: updateUser() of OpenAMService for version update");
					productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
							setPasswordRequest.getId(), version);
					LOGGER.info("End: updateUser() of OpenAMService for version update");

					PRODUCT_JSON_STRING = PRODUCT_JSON_STRING.substring(0, PRODUCT_JSON_STRING.length() - 1)
							.concat(",\"authId\":\"" + "[]" + "\"}");
					// check UIMSPasswordSync to call sync or Async method
					if (pickListValidator.validate(UserConstants.UIMSPasswordSync, UserConstants.TRUE)) {
						LOGGER.info("Start: SYNC method of setUIMSPassword() of UimsSetPasswordSoapService");
						uimsSetPasswordSoapService.setUIMSPassword(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
								setPasswordRequest.getId(), federationID, setPasswordRequest.getNewPwd(),
								vNewCntValue.toString(), loginIdentifierType, emailOrMobile);
						updateOpenamDetails(iPlanetDirectoryKey, federationID, PRODUCT_JSON_STRING);
						LOGGER.info("End: SYNC method of setUIMSPassword() of UimsSetPasswordSoapService finished");
					} else {
						// Calling Async method of setUIMSPassword
						LOGGER.info("Start: ASYNC method of setUIMSPassword() of UIMSUserManagerSoapService");
						uimsUserManagerSoapService.setUIMSPassword(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
								setPasswordRequest.getId(), federationID, setPasswordRequest.getNewPwd(),
								vNewCntValue.toString(), loginIdentifierType, emailOrMobile);
						updateOpenamDetails(iPlanetDirectoryKey, federationID, PRODUCT_JSON_STRING);
						LOGGER.info("End: ASYNC method of setUIMSPassword() of UIMSUserManagerSoapService finished");
					}
				} else {
					// productService.sessionLogout(UserConstants.IPLANET_DIRECTORY_PRO+iPlanetDirectoryKey,
					// "logout");
				}
			} catch (Exception e) {
				LOGGER.error("Exception in setUIMSPassword UIMS API:: ->" + e.getMessage(),e);
			}

			SetPasswordResponse setPasswordResponse;
			Attributes attributes = new Attributes();
			IDMSUserRecord idmsUserRecord = new IDMSUserRecord();
			idmsUserRecord.setAttributes(attributes);
			idmsUserRecord.setId(userId);
			idmsUserRecord.setIDMS_Federated_ID__c(userId);
			setPasswordResponse = new SetPasswordResponse(idmsUserRecord);
			setPasswordResponse.setStatus(successStatus);
			setPasswordResponse.setMessage("Password Updated successfully");
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info(setPasswordResponse.getMessage());
			LOGGER.info("Time taken by UserServiceImpl.setPassword() : " + elapsedTime);
			return Response.status(Response.Status.OK).entity(setPasswordResponse).build();
		} catch (WebApplicationException wae) {
			String errorString=null;
			try {
				errorString = IOUtils.toString((InputStream) wae.getResponse().getEntity());
			} catch (IOException e) {
				LOGGER.error("IOException is " + e.getMessage(),e);
			}
			response.setStatus(errorStatus);
			response.setMessage(errorString);
			response.setId(userId);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.error("Exception is :: " + errorString);
			LOGGER.info("Time taken by UserServiceImpl.setPassword() : " + elapsedTime);
			return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
		} catch (Exception e) {
			response.setStatus(errorStatus);
			response.setMessage("Error in Setting User Password");
			response.setId(userId);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.error("Exception is " + e.getMessage(),e);
			LOGGER.info("Time taken by UserServiceImpl.setPassword() : " + elapsedTime);
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(response).build();
		}

	}

	/**
	 * To activate user if he lost registeration email, Admin will active, we
	 * update login Id identifier
	 */
	@SuppressWarnings("unchecked")
	@Override
	public Response activateUser(String token, String clientId, String clientSecret,
			ActivateUserRequest activateUserRequest) {
		LOGGER.info("Entered activateUser() -> Start");
		LOGGER.info("Parameter token -> " + token);
		// LOGGER.info("Parameter clientId -> " + clientId+" ,clientSecret ->
		// "+clientSecret);
		// LOGGER.info("Parameter activateUserRequest -> " +
		// activateUserRequest);
		String userData = null;
		String userId = null;
		JSONObject response = new JSONObject();
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		DocumentContext productDocCtx = null;
		String loginIdentifier = null;
		String registrationSource = null;
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		String openamVnew = null;
		Integer vNewCntValue = 0;
		String iPlanetDirectoryKey = null;
		String usermail = "";
		ObjectMapper objMapper = new ObjectMapper();
		String emailOrMobile = null;
		String loginIdentifierType = null;
		String federationID = null;
		try {
			LOGGER.info("activateUser : Request   -> " + objMapper.writeValueAsString(activateUserRequest));

			if (null == activateUserRequest.getUserRecord().getIDMS_Registration_Source__c()
					|| activateUserRequest.getUserRecord().getIDMS_Registration_Source__c().isEmpty()) {
				response.put(UserConstants.STATUS, UserConstants.STATUS_ERROR);
				response.put(UserConstants.MESSAGE, UserConstants.REGISTRATION_SOURCE_MISSING);
				LOGGER.error("Error is " + UserConstants.REGISTRATION_SOURCE_MISSING);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}

			if (null != activateUserRequest.getUserRecord().getIDMS_Registration_Source__c() && !UserConstants.UIMS
					.equalsIgnoreCase(activateUserRequest.getUserRecord().getIDMS_Registration_Source__c())) {
				if (null == activateUserRequest.getUserRecord().getId()
						|| activateUserRequest.getUserRecord().getId().isEmpty()) {
					response.put(UserConstants.STATUS, UserConstants.STATUS_ERROR);
					response.put(UserConstants.MESSAGE, UserConstants.MANDATORY_FEDERATION_ID);
					LOGGER.error("Error is " + UserConstants.MANDATORY_FEDERATION_ID);
					return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
				}
			}

			if (null != activateUserRequest.getUserRecord().getIDMS_Registration_Source__c() && UserConstants.UIMS
					.equalsIgnoreCase(activateUserRequest.getUserRecord().getIDMS_Registration_Source__c())) {
				if (null == activateUserRequest.getUserRecord().getIDMS_Federated_ID__c()
						|| activateUserRequest.getUserRecord().getIDMS_Federated_ID__c().isEmpty()) {
					response.put(UserConstants.STATUS, UserConstants.STATUS_ERROR);
					response.put(UserConstants.MESSAGE, UserConstants.MANDATORY_FEDERATION_ID);
					LOGGER.error("Error is " + UserConstants.MANDATORY_FEDERATION_ID);
					return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
				}

				if (null == clientId || null == clientSecret) {
					response.put(UserConstants.STATUS, UserConstants.STATUS_ERROR);
					response.put(UserConstants.MESSAGE, UserConstants.UIMS_CLIENTID_SECRET);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.error("Error is " + UserConstants.UIMS_CLIENTID_SECRET);
					LOGGER.info("Time taken by activateUser() : " + elapsedTime);
					return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
				}

				if ((null != clientId && !clientId.equalsIgnoreCase(uimsClientId))
						|| (null != clientSecret && !clientSecret.equalsIgnoreCase(uimsClientSecret))) {
					response.put(UserConstants.STATUS, UserConstants.STATUS_ERROR);
					response.put(UserConstants.MESSAGE, UserConstants.INVALID_UIMS_CREDENTIALS);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.error("Error is " + UserConstants.INVALID_UIMS_CREDENTIALS);
					LOGGER.info("Time taken by activateUser() : " + elapsedTime);
					return Response.status(Response.Status.UNAUTHORIZED).entity(response).build();
				}
			}

			/**
			 * Get iPlanetDirectory Pro Admin token for admin
			 */
			try {
				iPlanetDirectoryKey = getSSOToken();
			} catch (IOException ioExp) {
				LOGGER.error("Unable to get SSO Token" + ioExp.getMessage(),ioExp);
				iPlanetDirectoryKey = "";
			}

			if (null != activateUserRequest.getUserRecord().getIDMS_Registration_Source__c() && UserConstants.UIMS
					.equalsIgnoreCase(activateUserRequest.getUserRecord().getIDMS_Registration_Source__c())) {

				Response fedResponse = checkUserExistsWithFederationID(iPlanetDirectoryKey,
						activateUserRequest.getUserRecord().getIDMS_Federated_ID__c(), startTime);
				if (fedResponse.getStatus() == 200) {
					JSONObject uimsResponse = (JSONObject) fedResponse.getEntity();
					userId = (String) uimsResponse.get("userId");
				} else {
					return fedResponse;
				}
				response.put(UserConstants.FEDERATION_IDENTIFIER,
						activateUserRequest.getUserRecord().getIDMS_Federated_ID__c());
			} else {

				userId = activateUserRequest.getUserRecord().getId();
			}

			if (null == userId || userId.isEmpty()) {
				SetPasswordResponse setPasswordResponse;
				setPasswordResponse = new SetPasswordResponse(null);
				setPasswordResponse.setStatus(errorStatus);
				setPasswordResponse.setMessage("User not found based on user Id");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error is " + setPasswordResponse.getMessage());
				LOGGER.info("Time taken by ActivateUser() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(setPasswordResponse).build();
			} else {

				LOGGER.info(AUDIT_REQUESTING_USER + userId + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN
						+ AUDIT_OPENAM_API + AUDIT_OPENAM_GET_CALL + userId + AUDIT_LOG_CLOSURE);
				LOGGER.info("Start: getUser() of openam for userid=" + userId);
				userData = productService.getUser(iPlanetDirectoryKey, userId);
				LOGGER.info("End: getUser() of openam finished for userid=" + userId);
				LOGGER.info(" productService.getUser :: " + ChinaIdmsUtil.printOpenAMInfo(userData));
				productDocCtx = JsonPath.using(conf).parse(userData);

				usermail = productDocCtx.read("$.mail[0]");

				emailOrMobile = productDocCtx.read("$.mail[0]");
				loginIdentifierType = UserConstants.EMAIL;
				if (null == emailOrMobile) {
					emailOrMobile = productDocCtx.read("$.mobile_reg[0]");
					loginIdentifierType = UserConstants.MOBILE;
				}

				federationID = productDocCtx.read("$.federationID[0]");

				registrationSource = null != productDocCtx.read(JsonConstants.REGISTRATION_SOURCE)
						? getValue(productDocCtx.read(JsonConstants.REGISTRATION_SOURCE).toString()) : null;

				if ((null == registrationSource || registrationSource.isEmpty()) || (!activateUserRequest
						.getUserRecord().getIDMS_Registration_Source__c().equalsIgnoreCase(registrationSource))) {
					response.put(UserConstants.STATUS, UserConstants.STATUS_ERROR);
					response.put(UserConstants.MESSAGE, UserConstants.REGISTRATION_SOURCE_NOT_MATCHING);
					response.put(UserConstants.ID, activateUserRequest.getUserRecord().getId());
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.error("Error is " + UserConstants.REGISTRATION_SOURCE_NOT_MATCHING);
					LOGGER.info("Time taken by ActivateUser() : " + elapsedTime);
					return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
				}

				loginIdentifier = null != productDocCtx.read(JsonConstants.MAIL)
						? getValue(productDocCtx.read(JsonConstants.MAIL).toString()) : null;
				String PRODUCT_JSON_STRING = "{" + "\"loginid\": \"" + loginIdentifier + "\"" + "}";
				if (null == loginIdentifier || loginIdentifier.isEmpty()) {
					loginIdentifier = null != productDocCtx.read(JsonConstants.MOBILE_REG)
							? getValue(productDocCtx.read(JsonConstants.MOBILE_REG).toString()) : null;
					PRODUCT_JSON_STRING = "{" + "\"login_mobile\": \"" + loginIdentifier + "\"" + "}";
				}

				if (null != loginIdentifier && !loginIdentifier.isEmpty()) {
					LOGGER.info(AUDIT_REQUESTING_USER + userId + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN
							+ AUDIT_OPENAM_API + AUDIT_OPENAM_UPDATE_CALL + userId + AUDIT_LOG_CLOSURE);
					LOGGER.info("Start: updateUser() of OpenAMService to update login for userid:" + userId);
					productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, userId,
							PRODUCT_JSON_STRING);
					LOGGER.info("End: updateUser() of OpenAMService to update login finished for userid:" + userId);
				}
			}

			openamVnew = null != productDocCtx.read("$.V_New[0]") ? getValue(productDocCtx.read("$.V_New[0]"))
					: getDelimeter();
			if (null != vNewCntValue && null != openamVnew) {
				vNewCntValue = Integer.parseInt(openamVnew) + 1;
			}
			String version = "{\"V_New\": \"" + vNewCntValue + "\"" + "}";

			if (null != activateUserRequest.getUserRecord().getIDMS_Registration_Source__c() && !UserConstants.UIMS
					.equalsIgnoreCase(activateUserRequest.getUserRecord().getIDMS_Registration_Source__c())) {
				// Adding V_New
				LOGGER.info("Start: updateUser() of OpenAMService to update version");
				productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
						activateUserRequest.getUserRecord().getId(), version);
				LOGGER.info("End: updateUser() of OpenAMService to update version");
				// call uims activate user
				/*
				 * LOGGER.info(
				 * "In UserServiceImpl.activateUser().activateUIMSUser():--> " +
				 * "calling Async UIMS Usermanager methods of activateIdentity/activateIdentityWithNoPassword"
				 * );
				 */

				activateUserRequest.getUserRecord().setIDMS_Federated_ID__c(federationID);
				LOGGER.info("Start: activateIdentityNoPassword() of UIMS for emailOrMobile:" + emailOrMobile);
				uimsUserManagerSoapService.activateIdentityNoPassword(activateUserRequest.getUserRecord().getId(),
						activateUserRequest.getUserRecord().getIDMS_Federated_ID__c(), vNewCntValue.toString(),
						UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, loginIdentifierType, emailOrMobile);
				LOGGER.info("End: activateIdentityNoPassword() of UIMS finished for emailOrMobile:" + emailOrMobile);
			} else {
				// productService.sessionLogout(UserConstants.IPLANET_DIRECTORY_PRO+iPlanetDirectoryKey,
				// "logout");
			}
		} catch (BadRequestException e) {

			response.put(UserConstants.STATUS, errorStatus);
			response.put(UserConstants.MESSAGE, UserConstants.USER_NOT_FOUND);
			response.put(UserConstants.ID, activateUserRequest.getUserRecord().getId());
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by ActivateUser() : " + elapsedTime);
			LOGGER.error("BadRequestException in ActivateUser the User :: -> " + UserConstants.USER_NOT_FOUND);
			return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
		} catch (NotAuthorizedException e) {

			response.put(UserConstants.STATUS, errorStatus);
			response.put(UserConstants.MESSAGE, UserConstants.USER_NOT_FOUND);
			response.put(UserConstants.ID, activateUserRequest.getUserRecord().getId());
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by ActivateUser() : " + elapsedTime);
			LOGGER.error("NotAuthorizedException in ActivateUser :: -> " + UserConstants.USER_NOT_FOUND);
			return Response.status(Response.Status.UNAUTHORIZED).entity(response).build();
		} catch (NotFoundException e) {

			response.put(UserConstants.STATUS, errorStatus);
			response.put(UserConstants.MESSAGE, UserConstants.USER_NOT_FOUND);
			response.put(UserConstants.ID, activateUserRequest.getUserRecord().getId());
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by ActivateUser() : " + elapsedTime);
			LOGGER.error("NotFoundException in ActivateUser :: -> " + UserConstants.USER_NOT_FOUND);
			return Response.status(Response.Status.NOT_FOUND).entity(response).build();
		} catch (Exception e) {
			LOGGER.error( e.getMessage(),e);
			response.put(UserConstants.STATUS, errorStatus);
			response.put(UserConstants.MESSAGE, UserConstants.USER_NOT_FOUND);
			response.put(UserConstants.ID, activateUserRequest.getUserRecord().getId());
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by UserServiceImpl.ActivateUser() : " + elapsedTime);
			LOGGER.error("Executing while ActivateUser the User :: -> " + UserConstants.USER_NOT_FOUND);
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(response).build();
		}
		response.put(UserConstants.STATUS, successStatus);
		response.put(UserConstants.MESSAGE, UserConstants.USER_ACTIVATED);
		response.put(UserConstants.ID, userId);
		elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		LOGGER.info("Time taken by ActivateUser() : " + elapsedTime);
		return Response.status(Response.Status.OK).entity(response).build();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.idms.service.UserServiceImpl#getUserByLoginIdentifier(java.lang.
	 * String)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public Response getUserByLoginIdentifier(String loginIdentifier) {
		LOGGER.info("Entered getUserByLoginIdentifier() -> Start");
		LOGGER.info("Parameter loginIdentifier -> " + loginIdentifier);

		DocumentContext productDocCtx = null;
		String iPlanetDirectoryKey = null;
		String userExists = null;
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		JSONObject response = new JSONObject();

		try {
			iPlanetDirectoryKey = getSSOToken();
		} catch (IOException ioExp) {
			LOGGER.error("Unable to get SSO Token " + ioExp.getMessage(),ioExp);
			iPlanetDirectoryKey = "";
		}

		if (null != loginIdentifier) {
			LOGGER.info(AUDIT_REQUESTING_USER + AUDIT_TECHNICAL_USER + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN
					+ AUDIT_OPENAM_API + AUDIT_OPENAM_USER_EXISTS_CALL + loginIdentifier + AUDIT_LOG_CLOSURE);

			try {
				LOGGER.info("Start: checkUserExistsWithEmailMobile() of openam for loginIdentifier=" + loginIdentifier);
				userExists = productService.checkUserExistsWithEmailMobile(
						UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
						"mail eq " + "\"" + URLEncoder.encode(URLDecoder.decode(loginIdentifier, "UTF-8"), "UTF-8")
								+ "\" or mobile_reg eq " + "\""
								+ URLEncoder.encode(URLDecoder.decode(loginIdentifier, "UTF-8"), "UTF-8") + "\"");
				LOGGER.info("End: checkUserExistsWithEmailMobile() of openam finished for loginIdentifier="
						+ loginIdentifier);
			} catch (UnsupportedEncodingException e) {

				LOGGER.error("UnsupportedEncodingException in  getUserByLoginIdentifier():" + e.getMessage(),e);
			}

			productDocCtx = JsonPath.using(conf).parse(userExists);
			Integer resultCount = productDocCtx.read("$.resultCount");
			LOGGER.info("resultCount=" + resultCount);
			if (resultCount.intValue() > 0) {
				response.put("userId", productDocCtx.read("$.result[0].username"));
				response.put("fedId", productDocCtx.read("$.result[0].federationID[0]"));
				response.put("regSource", productDocCtx.read("$.result[0].registerationSource[0]"));
				return Response.status(Response.Status.OK).entity(response).build();

			} else {
				return Response.status(Response.Status.OK).entity(response).build();
			}
		}
		return null;
	}

	private String getIDMSAclType(String aclType) {
		if (UserConstants.ACLTYPE_APPLICATION.equalsIgnoreCase(aclType))
			return UserConstants.ACLTYPE_APPLICATIONS;
		else if (UserConstants.ACLTYPE_PROGRAM.equalsIgnoreCase(aclType))
			return UserConstants.ACLTYPE_PROGRAMS;
		else if (UserConstants.ACLTYPE_FEATURE.equalsIgnoreCase(aclType))
			return UserConstants.ACLTYPE_FEATURES;

		return null;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.idms.service.UserServiceImpl#getUserByOauth(java.lang.String)
	 */
	@Override
	public Response getUserByOauth(String token) {
		return getUserbyToken(token);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.idms.service.UserServiceImpl#getUserByOauthFromUI(java.lang.String)
	 */
	@Override
	public Response getUserByOauthFromUI(String token, String appName) {
		return getUserbyTokenUI(token, appName);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.idms.service.UserServiceImpl#activateToken(java.lang.String)
	 */
	@Override
	public Response activateToken(String userTokenId) {
		LOGGER.info("Entered activateToken() -> Start");
		LOGGER.info("Parameter userTokenId -> " + userTokenId);
		String activeToken = null;
		String amAdminToken = null;
		try {
			amAdminToken = getSSOToken();
		} catch (IOException ioExp) {
			LOGGER.error("Unable to get SSO Token" + ioExp.getMessage(),ioExp);
			amAdminToken = "";
		}
		try {
			LOGGER.info("Start: activeToken() of openam");
			activeToken = productService.activeToken(UserConstants.CHINA_IDMS_TOKEN + amAdminToken, "isActive",
					userTokenId);
			LOGGER.info("End: activeToken() of openam");
		} catch (NotAuthorizedException e) {
			e.getStackTrace();
			LOGGER.info("NotAuthorizedException in activateToken():" + e.getMessage());
			return Response.status(Response.Status.UNAUTHORIZED).entity(activeToken).build();
		}
		return Response.status(Response.Status.OK).entity(activeToken).build();
	}

	/**
	 * IFW is calling
	 * 
	 * @param iPlanetDirectoryToken
	 * @param federationId
	 * @param startTime
	 * @return
	 */
	@SuppressWarnings({ "unchecked" })
	private Response checkUserExistsWithFederationID(String iPlanetDirectoryToken, String federationId,
			long startTime) {
		LOGGER.info("Entered checkUserExistsWithFederationID() -> Start");
		LOGGER.info("Parameter iPlanetDirectoryToken -> " + iPlanetDirectoryToken);
		LOGGER.info("Parameter federationId -> " + federationId);
		// LOGGER.info("Parameter startTime -> " + startTime);
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		DocumentContext productDocCtx = null;
		JSONObject uimsResponse = new JSONObject();
		long elapsedTime;
		String userId = null;
		String loginIdentifierEmail = "", loginIdentifierMobile = "";

		LOGGER.info("Start: checkUserExistsWithEmailMobile() of openam for federationId:" + federationId);
		String userExists = productService.checkUserExistsWithEmailMobile(
				UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryToken,
				"federationID eq " + "\"" + federationId + "\" or uid eq " + "\"" + federationId + "\"");
		LOGGER.info("End: checkUserExistsWithEmailMobile() of openam finished for federationId:" + federationId);
		// LOGGER.info("User Record with fed ID= " + userExists);

		productDocCtx = JsonPath.using(conf).parse(userExists);
		LOGGER.info("productDocCtx = " + ChinaIdmsUtil.printOpenAMInfo(productDocCtx.jsonString()));
		Integer resultCount = productDocCtx.read(JsonConstants.RESULT_COUNT);
		LOGGER.info("resultCount=" + resultCount);

		if (resultCount.intValue() == 0) {
			uimsResponse = new JSONObject();
			uimsResponse.put(UserConstants.STATUS, errorStatus);
			uimsResponse.put(UserConstants.MESSAGE, UserConstants.USER_NOT_EXISTS);
			uimsResponse.put(UserConstants.FEDERATION_IDENTIFIER, federationId);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.error("Error is -> " + UserConstants.USER_NOT_EXISTS);
			LOGGER.info("Time taken by checkUserExistsWithFederationID() : " + elapsedTime);
			return Response.status(Response.Status.NOT_FOUND).entity(uimsResponse).build();
		} else {
			userId = productDocCtx.read(JsonConstants.RESULT);
			loginIdentifierEmail = productDocCtx.read(JsonConstants.RESULT_Loginid);
			loginIdentifierMobile = productDocCtx.read(JsonConstants.RESULT_LOGIN_MOBILE);
			if (null != loginIdentifierEmail && !loginIdentifierEmail.isEmpty()) {
				if (emailValidator.validate(loginIdentifierEmail)) {
					uimsResponse.put("loginIdentity", "Email");
				} else if (null != loginIdentifierMobile && !loginIdentifierMobile.isEmpty()) {
					if (ChinaIdmsUtil.mobileValidator(loginIdentifierMobile))
						uimsResponse.put("loginIdentity", "Mobile");
				}
			}
			uimsResponse.put("userId", userId);
		}
		return Response.status(Response.Status.OK).entity(uimsResponse).build();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.idms.service.UserServiceImpl#getContentFromTemplate(java.lang.String,
	 * java.lang.String)
	 */
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

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.idms.service.UserServiceImpl#activateBulkUser()
	 */
	@Override
	public Response activateBulkUser() {
		LOGGER.info("Entered activateBulkUser() -> Start");

		String hostname = "https://identity-stg.schneider-electric.com";
		String csvFile = "C:\\JsonRequestURLs\\GoDigitalCert\\UserData.csv";
		ActivateUsers activate = new ActivateUsers();

		try {
			activate.readDataFromFile(hostname, csvFile);
		} catch (NoSuchAlgorithmException e) {

			LOGGER.error(e.getMessage(),e);
		}
		return null;
	}

	/**
	 * IFW calling
	 */
	@SuppressWarnings("unchecked")
	@Override
	public Response getUserByFederationId(String authorizationToken, String federationId) {
		LOGGER.info("Entered getUserByFederationId() -> Start");
		LOGGER.info("Parameter federationId -> " + federationId);
		JSONObject errorResponse = new JSONObject();
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		String userId = null;

		if (!getTechnicalUserDetails(authorizationToken)) {
			errorResponse.put(UserConstants.MESSAGE, ErrorCodeConstants.BADREQUEST_MESSAGE);
			return Response.status(Response.Status.UNAUTHORIZED).entity(errorResponse).build();
		}
		/**
		 * Get iPlanetDirectory Pro Admin token for admin
		 */
		String iPlanetDirectoryKey = null;
		try {
			iPlanetDirectoryKey = getSSOToken();
		} catch (IOException ioExp) {
			LOGGER.error("Unable to get SSO Token" + ioExp.getMessage(),ioExp);
			iPlanetDirectoryKey = "";
		}

		Response fedResponse = checkUserExistsWithFederationID(iPlanetDirectoryKey, federationId, startTime);
		LOGGER.info("fedResponse status in checkUserExistsWithFederationID(): " + fedResponse.getStatus());
		if (fedResponse.getStatus() == 200) {
			JSONObject uimsResponse = (JSONObject) fedResponse.getEntity();
			userId = (String) uimsResponse.get("userId");
		}
		return getUser(userId);
	}

	/**
	 * Invite someone by email
	 */
	@Override
	public Response sendInvitation(String authorizedToken, SendInvitationRequest sendInvitaionRequest) {
		LOGGER.info("Entered sendInvitation() -> Start");
		// LOGGER.info("Parameter authorizedToken -> " + authorizedToken);
		LOGGER.info("Parameter sendInvitaionRequest -> " + sendInvitaionRequest);

		long elapsedTime;
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;

		try {
			if ((null == sendInvitaionRequest.getEmail() || sendInvitaionRequest.getEmail().isEmpty())
					|| (null == sendInvitaionRequest.getInvitationId()
							|| sendInvitaionRequest.getInvitationId().isEmpty())
					|| (null == sendInvitaionRequest.getRedirectUrl()
							|| sendInvitaionRequest.getRedirectUrl().isEmpty())) {
				userResponse.setStatus(errorStatus);
				userResponse.setMessage("Email, InvitationId and RedirectUrl are mandatory");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error is -> " + userResponse.getMessage());
				LOGGER.info("Time taken by sendInvitation() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
			} else {
				sendEmail.sendInvitationEmail(EmailConstants.SENDINVITATION_OPT_TYPE,
						sendInvitaionRequest.getRedirectUrl(), sendInvitaionRequest.getEmail(),
						sendInvitaionRequest.getInvitationId());
			}
		} catch (Exception e) {

			LOGGER.error("Exception occured!!!!" + e.getMessage(),e);
			return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
		}
		userResponse.setStatus(successStatus);
		userResponse.setMessage(UserConstants.SET_INVITATION_SUCCESS_MESSAGE);
		return Response.status(Response.Status.OK).entity(userResponse).build();
	}

	/**
	 * Resending email to user with token
	 */
	@Override
	public Response resendRegEmail(ResendRegEmailRequest resendRegEmail) {
		LOGGER.info("Entered resendRegEmail() -> Start");
		long elapsedTime;
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;

		String iPlanetDirectoryKey = null;
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		DocumentContext productDocCtx = null;
		String userId = null, userType = null, userCName = null, userExistsQuery = null;
		Integer resultCount = 0;
		ObjectMapper objMapper = new ObjectMapper();

		try {
			LOGGER.info("Parameter userRequest -> " + objMapper.writeValueAsString(resendRegEmail));

			if ((null == resendRegEmail.getEmail() || resendRegEmail.getEmail().isEmpty())
					&& (null == resendRegEmail.getMobile() || resendRegEmail.getMobile().isEmpty())) {
				userResponse.setStatus(errorStatus);
				userResponse.setMessage("Email or mobile is mandatory");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error is -> " + userResponse.getMessage());
				LOGGER.info("Time taken by resendRegEmail() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
			}
			if (null == resendRegEmail.getFirstName() || resendRegEmail.getFirstName().isEmpty()) {
				userResponse.setStatus(errorStatus);
				userResponse.setMessage("FirstName is mandatory");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error is -> " + userResponse.getMessage());
				LOGGER.info("Time taken by resendRegEmail() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
			}
			if (null == resendRegEmail.getLastName() || resendRegEmail.getLastName().isEmpty()) {
				userResponse.setStatus(errorStatus);
				userResponse.setMessage("LastName is mandatory");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error is -> " + userResponse.getMessage());
				LOGGER.info("Time taken by resendRegEmail() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
			}
			try {
				iPlanetDirectoryKey = getSSOToken();
			} catch (IOException ioExp) {
				LOGGER.error("Unable to get SSO Token " + ioExp.getMessage(),ioExp);
				iPlanetDirectoryKey = "";
			}

			if (null != resendRegEmail.getEmail() && !resendRegEmail.getEmail().isEmpty()) {
				userId = resendRegEmail.getEmail();
				userType = "mail";
			} else if (null != resendRegEmail.getMobile() && !resendRegEmail.getMobile().isEmpty()) {
				userId = resendRegEmail.getMobile();
				userType = "mobile";
			}

			if (null != userId && !userId.isEmpty()) {
				LOGGER.info(AUDIT_REQUESTING_USER + AUDIT_TECHNICAL_USER + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN
						+ AUDIT_OPENAM_API + AUDIT_OPENAM_USER_EXISTS_CALL + resendRegEmail.getEmail()
						+ AUDIT_LOG_CLOSURE);

				if (userType.equalsIgnoreCase("mail")) {
					LOGGER.info("Start: checkUserExistsWithEmailMobile() of openam for email=" + userId);
					userExistsQuery = productService.checkUserExistsWithEmailMobile(
							UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
							"mail eq " + "\"" + URLEncoder.encode(URLDecoder.decode(userId, "UTF-8"), "UTF-8") + "\"");
					LOGGER.info("End: checkUserExistsWithEmailMobile() of openam for email=" + userId);
				}
				if (userType.equalsIgnoreCase("mobile")) {
					LOGGER.info("Start: checkUserExistsWithEmailMobile() of openam for mobile=" + userId);
					userExistsQuery = productService.checkUserExistsWithEmailMobile(
							UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, "mobile_reg eq " + "\""
									+ URLEncoder.encode(URLDecoder.decode(userId, "UTF-8"), "UTF-8") + "\"");
					LOGGER.info("End: checkUserExistsWithEmailMobile() of openam for mobile=" + userId);
				}

				productDocCtx = JsonPath.using(conf).parse(userExistsQuery);
				resultCount = productDocCtx.read("$.resultCount");
				LOGGER.info("resultCount=" + resultCount);

				if (resultCount.intValue() == 0) {
					userResponse.setStatus(errorStatus);
					userResponse.setMessage("User not found based on given email/mobile");
					userResponse.setId(userCName);
					LOGGER.error("User not found based on given email/mobile");
					return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
				}

				if (UserConstants.TRUE.equalsIgnoreCase(productDocCtx.read("$.result[0].isActivated[0]"))) {
					userResponse.setStatus(errorStatus);
					userResponse.setMessage("You have already registered");
					LOGGER.error("Error is -> " + userResponse.getMessage());
					return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
				} else {

					userCName = productDocCtx.read("$.result[0].username");
					String regSource = productDocCtx.read("$.result[0].registerationSource[0]");
					String fName = null != productDocCtx.read("$.result[0].givenName")
							? getValue(productDocCtx.read("$.result[0].givenName").toString()) : getDelimeter();
					String lName = null != productDocCtx.read("$.result[0].sn")
							? getValue(productDocCtx.read("$.result[0].sn").toString()) : getDelimeter();

					if ((null != fName && !fName.isEmpty()) && (fName.equalsIgnoreCase(resendRegEmail.getFirstName()))
							&& ((null != lName && !lName.isEmpty())
									&& (lName.equalsIgnoreCase(resendRegEmail.getLastName())))) {
						String otp = sendEmail.generateOtp(userCName);
						LOGGER.info("Successfully OTP generated for " + userCName);

						if (userType.equalsIgnoreCase("mail")) {
							sendEmail.sendOpenAmEmail(otp, EmailConstants.USERREGISTRATION_OPT_TYPE, userCName,
									regSource);
						}
						if (userType.equalsIgnoreCase("mobile")) {
							LOGGER.info("Start: sendSMSMessage() for mobile userName:" + userCName);
							sendEmail.sendSMSNewGateway(otp, EmailConstants.USERREGISTRATION_OPT_TYPE, userCName,
									regSource);
							LOGGER.info("End: sendSMSMessage() finished for  mobile userName:" + userCName);
							LOGGER.info("Start: sendOpenAmMobileEmail() for mobile userName:" + userCName);
							sendEmail.sendOpenAmMobileEmail(otp, EmailConstants.USERREGISTRATION_OPT_TYPE, userCName,
									regSource);
							LOGGER.info("End: sendOpenAmMobileEmail() finsihed for  mobile userName:" + userCName);
						}
					} else {
						userResponse.setStatus(errorStatus);
						userResponse.setMessage("FirstName and LastName are not matched with Email/mobile given!!");
						LOGGER.error("Error is -> " + userResponse.getMessage());
						return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
					}
				}
			}
		} catch (Exception e) {
			LOGGER.error("Exception in resendRegEmail :" + e.getMessage(), e);
			userResponse.setStatus(errorStatus);
			userResponse.setMessage(UserConstants.RESEND_REGEMAIL_ERROR_MESSAGE);
			return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
		}
		userResponse.setStatus(successStatus);
		userResponse.setMessage(UserConstants.RESEND_REGEMAIL_SUCCESS_MESSAGE);
		userResponse.setId(userCName);
		return Response.status(Response.Status.OK).entity(userResponse).build();
	}

	/**
	 * from UI, this method called
	 */
	@Override
	public Response idmsIdpChaning(String idToken1, String idToken2, String idButton, String gotoUrl, String gotoOnFail,
			String sunQueryParamsString, String encoded, String errorMessage, String gxCharset) {
		LOGGER.info("Entered idmsIdpChaning() -> Start");
		LOGGER.info("Parameter idToken1 -> " + idToken1);
		LOGGER.info("Parameter idButton -> " + idButton + " ,gotoUrl -> " + gotoUrl);
		LOGGER.info("Parameter gotoOnFail -> " + gotoOnFail + " ,sunQueryParamsString -> " + sunQueryParamsString);
		LOGGER.info("Parameter encoded -> " + encoded + " ,errorMessage -> " + errorMessage);
		LOGGER.info("Parameter gxCharset -> " + gxCharset);

		long elapsedTime;
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		Response response = null;

		try {
			if ((null == gotoUrl || gotoUrl.isEmpty())) {
				userResponse.setStatus(errorStatus);
				userResponse.setMessage("goto value is mandatory");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error is -> " + userResponse.getMessage());
				LOGGER.info("Time taken by idmsIdpChaning() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
			} else if ((null == sunQueryParamsString || sunQueryParamsString.isEmpty())) {
				userResponse.setStatus(errorStatus);
				userResponse.setMessage("sunQueryParamsString value is mandatory");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error is -> " + userResponse.getMessage());
				LOGGER.info("Time taken by idmsIdpChaning() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
			} else {
				// String url = Goto.substring(4);
				byte[] valueDecoded = Base64.decodeBase64(gotoUrl);
				String urlresponse = new String(valueDecoded);

				String afterUrlDecoded = decode(urlresponse);
				LOGGER.info("Decoded goto url value is::: " + urlresponse);
				String substr = "RelayState=";

				StringBuffer urlHeaderValue = new StringBuffer(
						afterUrlDecoded.substring(afterUrlDecoded.indexOf(substr) + substr.length()));

				// Checking Error Message

				/*
				 * if((null != errorMessage &&
				 * !errorMessage.isEmpty())&&(UserConstants.AUTH_FAILED.
				 * equalsIgnoreCase(errorMessage))) {
				 * urlHeaderValue.append(UserConstants.ERROR_MESSAGE).append(
				 * errorMessage); }
				 */

				if (null != errorMessage && !errorMessage.isEmpty()) {
					urlHeaderValue.append(UserConstants.ERROR_MESSAGE).append(errorMessage);
				}

				urlHeaderValue.append(UserConstants.GOTO).append(gotoUrl);
				urlHeaderValue.append(UserConstants.SUNQUERY_PARAM_STRING).append(sunQueryParamsString);

				urlHeaderValue.append(UserConstants.IDBUTTON).append(idButton);
				urlHeaderValue.append(UserConstants.GOTO_ONFAIL).append(gotoOnFail);
				urlHeaderValue.append(UserConstants.ENCODED).append(encoded);
				urlHeaderValue.append(UserConstants.GX_CHARSET).append(gxCharset);

				LOGGER.info("Decoded goto url value is :: " + urlHeaderValue);
				Response.ResponseBuilder rb = Response.status(Response.Status.MOVED_PERMANENTLY);

				response = rb.header("Location", urlHeaderValue).build();
			}
		} catch (Exception e) {
			LOGGER.error("Exception occured!!!!" + e.getMessage(),e);

			return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
		}
		return response;
	}

	/**
	 * In update user, resending email
	 */
	@Override
	public Response resendChangeEmail(ResendEmailChangeRequest emailChangeRequest) {
		LOGGER.info("Entered resendChangeEmail() -> Start");
		LOGGER.info("Parameter emailChangeRequest -> " + emailChangeRequest);
		long elapsedTime;
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;

		String iPlanetDirectoryKey = "";
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		DocumentContext productDocCtx = null;
		String userName = null;
		ObjectMapper objMapper = new ObjectMapper();

		try {
			LOGGER.info("Parameter emailChangeRequest -> " + objMapper.writeValueAsString(emailChangeRequest));
			if ((null == emailChangeRequest.getOldEmail() || emailChangeRequest.getOldEmail().isEmpty())
					|| (null == emailChangeRequest.getNewEmail() || emailChangeRequest.getNewEmail().isEmpty())
					|| (null == emailChangeRequest.getFirstName() || emailChangeRequest.getFirstName().isEmpty())
					|| (null == emailChangeRequest.getLastName() || emailChangeRequest.getLastName().isEmpty())) {
				userResponse.setStatus(errorStatus);
				userResponse.setMessage("OldEmail,NewEmail, FirstNmae and LastName are mandatory");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error is -> " + userResponse.getMessage());
				LOGGER.info("Time taken by resendRegEmail() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
			} else {

				try {
					iPlanetDirectoryKey = getSSOToken();
				} catch (IOException ioExp) {
					LOGGER.error("Unable to get SSO Token" + ioExp.getMessage(),ioExp);
					iPlanetDirectoryKey = "";
				}

				if (null != emailChangeRequest.getOldEmail()) {
					LOGGER.info(AUDIT_REQUESTING_USER + AUDIT_TECHNICAL_USER + AUDIT_IMPERSONATING_USER
							+ AUDIT_API_ADMIN + AUDIT_OPENAM_API + AUDIT_OPENAM_USER_EXISTS_CALL
							+ emailChangeRequest.getOldEmail() + AUDIT_LOG_CLOSURE);
					LOGGER.info("Start: checkUserExistsWithEmailMobile() of openam for email = "
							+ emailChangeRequest.getOldEmail());
					String userExists = productService
							.checkUserExistsWithEmailMobile(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
									"mail eq " + "\""
											+ URLEncoder.encode(
													URLDecoder.decode(emailChangeRequest.getOldEmail(), "UTF-8"),
													"UTF-8")
											+ "\"");

					LOGGER.info("End: checkUserExistsWithEmailMobile() of openam finished for email = "
							+ emailChangeRequest.getOldEmail());
					productDocCtx = JsonPath.using(conf).parse(userExists);

					Integer resultCount = productDocCtx.read(JsonConstants.RESULT_COUNT);
					LOGGER.info("resultCount=" + resultCount);
					if (resultCount.intValue() > 0) {
						userName = productDocCtx.read("$.result[0].username");
						String regSource = productDocCtx.read("$.result[0].registerationSource[0]");
						String firstName = null != productDocCtx.read("$.result[0].givenName")
								? getValue(productDocCtx.read("$.result[0].givenName").toString()) : getDelimeter();
						String lastName = null != productDocCtx.read("$.result[0].sn")
								? getValue(productDocCtx.read("$.result[0].sn").toString()) : getDelimeter();
						String email = productDocCtx.read("$.result[0].mail[0]");
						String newEmail = productDocCtx.read("$.result[0].newmail[0]");

						if ((null != firstName && !firstName.isEmpty()
								&& firstName.equalsIgnoreCase(emailChangeRequest.getFirstName()))
								&& (null != lastName && !lastName.isEmpty()
										&& lastName.equalsIgnoreCase(emailChangeRequest.getLastName()))
								&& (null != email && !email.isEmpty()
										&& email.equalsIgnoreCase(emailChangeRequest.getOldEmail()))
								&& (null != newEmail && !newEmail.isEmpty()
										&& newEmail.equalsIgnoreCase(emailChangeRequest.getNewEmail()))) {
							String otp = sendEmail.generateOtp(userName);
							LOGGER.info("Successfully OTP generated for " + userName);
							sendEmail.sendOpenAmEmail(otp, EmailConstants.UPDATEUSERRECORD_OPT_TYPE, userName,
									regSource);
						} else {
							userResponse.setStatus(errorStatus);
							userResponse
									.setMessage("newEmail or FirstName or LastName are not mached with Email given!!");
							userResponse.setId(userName);
							LOGGER.error("Error is -> " + userResponse.getMessage());
							return Response.status(Response.Status.UNAUTHORIZED).entity(userResponse).build();
						}
					} else {
						userResponse.setStatus(errorStatus);
						userResponse.setMessage("oldEmail is not valid to fetch the user");
						userResponse.setId(userName);
						LOGGER.error("Error is -> " + userResponse.getMessage());
						return Response.status(Response.Status.UNAUTHORIZED).entity(userResponse).build();
					}
				}
			}
		} catch (Exception e) {
			LOGGER.error("Exception occured:" + e.getMessage(),e);

			userResponse.setStatus(errorStatus);
			userResponse.setMessage("Exception occured in resendChangeEmail");
			userResponse.setId(userName);
			return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
		}
		userResponse.setStatus(successStatus);
		userResponse.setMessage(UserConstants.RESEND_REGEMAIL_SUCCESS_MESSAGE);
		userResponse.setId(userName);
		return Response.status(Response.Status.OK).entity(userResponse).build();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.idms.service.UserServiceImpl#initSocialLogin(java.lang.String)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public Response initSocialLogin(String service) {
		LOGGER.info("Entered initSocialLogin() -> Start");
		LOGGER.info("Parameter service -> " + service);
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		DocumentContext productDocCtx = null;
		String authId = null;
		String redirectUrl = null;
		String proxyUrl = null;
		String origUrl = null;
		String ntId = null;
		String state = null;
		String amlbcookie = null;
		SocialLoginResponse loginResponse = new SocialLoginResponse();
		JSONObject errorResponse = new JSONObject();
		errorResponse.put(UserConstants.STATUS, errorStatus);
		Response response = null;
		String userData = null;
		try {

			if (UserConstants.SOCIAL_LOGIN_SERVICE.equalsIgnoreCase(service)) {
				LOGGER.info("Start: initSocialLogin() of openam");
				response = productService.initSocialLogin(UserConstants.SOCIAL_LOGIN_SERVICE, "service",
						UserConstants.SOCIAL_LOGIN_SERVICE);
				LOGGER.info("End: initSocialLogin() of openam");
				userData = IOUtils.toString((InputStream) response.getEntity());
				productDocCtx = JsonPath.using(conf).parse(userData);
				authId = productDocCtx.read("$.authId");
				redirectUrl = productDocCtx.read("$.callbacks[0]$.output[0]$.value");
				List<Object> cookies = response.getHeaders().get("Set-Cookie");

				for (Object object : cookies) {
					String cookie = (String.valueOf(object));
					if (cookie.contains(UserConstants.PROXY_URL)) {
						proxyUrl = cookie.substring(UserConstants.PROXY_URL.length(), cookie.length());
					} else if (cookie.contains(UserConstants.ORIG_URL)) {
						origUrl = cookie.substring(UserConstants.ORIG_URL.length(), cookie.length());
					} else if (cookie.contains(UserConstants.NTID)) {
						ntId = cookie.substring(UserConstants.NTID.length(), cookie.length());
					} else if (cookie.contains(UserConstants.AMLBCOOKIE)) {
						amlbcookie = cookie.substring(UserConstants.AMLBCOOKIE.length(), cookie.length());
					}
				}

				state = redirectUrl.substring(redirectUrl.lastIndexOf("state=") + 6, redirectUrl.length());

				if (null == authId || authId.isEmpty()) {
					errorResponse.put(UserConstants.MESSAGE, UserConstants.AUTH_ID_EMPTY);
					return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(errorResponse).build();
				} else if (null == state || state.isEmpty()) {
					errorResponse.put(UserConstants.MESSAGE, UserConstants.STATE_EMPTY);
					return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(errorResponse).build();
				} else if (null == proxyUrl || proxyUrl.isEmpty()) {
					errorResponse.put(UserConstants.MESSAGE, UserConstants.PROXY_URL_EMPTY);
					return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(errorResponse).build();
				} else if (null == origUrl || origUrl.isEmpty()) {
					errorResponse.put(UserConstants.MESSAGE, UserConstants.ORIG_URL_EMPTY);
					return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(errorResponse).build();
				} else if (null == ntId || ntId.isEmpty()) {
					errorResponse.put(UserConstants.MESSAGE, UserConstants.NTID_EMPTY);
					return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(errorResponse).build();
				} else if (null == amlbcookie || amlbcookie.isEmpty()) {
					errorResponse.put(UserConstants.MESSAGE, UserConstants.AMLBCOOKIE_EMPTY);
					return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(errorResponse).build();
				}
				loginResponse.setAuthId(authId);
				loginResponse.setState(state);
				loginResponse.setProxyUrl(proxyUrl);
				loginResponse.setOrigUrl(origUrl);
				loginResponse.setNtId(ntId);
				loginResponse.setAmlbcookie(amlbcookie);
			} else {
				errorResponse.put(UserConstants.MESSAGE, UserConstants.SOCIAL_LOGIN_SERVICE_NAME);
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
			}
		} catch (IOException e) {
			errorResponse.put(UserConstants.MESSAGE, UserConstants.AMLBCOOKIE_EMPTY);
			LOGGER.error("Error is -> " + e.getMessage(),e);
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(errorResponse).build();
		} catch (Exception e) {
			LOGGER.error("Error is -> " + e.getMessage(),e);
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(errorResponse).build();
		}
		return Response.status(Response.Status.OK).entity(loginResponse).build();
	}

	private String decode(String url) {
		LOGGER.info("Entered decode() -> Start");
		LOGGER.info("Parameter url -> " + url);
		try {
			String prevURL = "";
			String decodeURL = url;
			while (!prevURL.equals(decodeURL)) {
				prevURL = decodeURL;
				decodeURL = URLDecoder.decode(decodeURL, "UTF-8");
			}
			return decodeURL;
		} catch (UnsupportedEncodingException e) {
			return "Issue while decoding" + e.getMessage();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.idms.service.UserServiceImpl#transliterator(java.lang.String)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public Response transliterator(String jsonAsString) {
		LOGGER.info("Entered transliterator() -> Start");
		LOGGER.info("Parameter jsonAsString -> " + jsonAsString);
		String srcNtargetId = null;
		String result = "";
		JSONObject errorResponse = null;// new JSONObject();
		ArrayList<Object> listResponse = null;
		TransliteratorResponse response = null;
		List<String> sourceLanguagesList = new ArrayList<String>();
		List<String> supportedSourceLanguagesList = new ArrayList<String>();
		List<String> supportedTargetLanguagesList = new ArrayList<String>();
		try {

			List<TransliteratorRequest> requestList = new ObjectMapper().readValue(jsonAsString,
					new TypeReference<List<TransliteratorRequest>>() {
					});

			List<String> supportedLanguages = LangSupportUtil.getTransilatorLanguages();
			for (String supportedLanguage : supportedLanguages) {
				String[] split = supportedLanguage.split("-");
				supportedSourceLanguagesList.add(split[0]);
				supportedTargetLanguagesList.add(split[1]);
			}

			if (null != requestList && requestList.size() > 0) {

				listResponse = new ArrayList<Object>();

				for (int index = 0; index < requestList.size(); index++) {

					if (null != requestList.get(index).getSourceLanguage()) {
						sourceLanguagesList.add(requestList.get(index).getSourceLanguage());
					}

					srcNtargetId = requestList.get(index).getSourceLanguage() + "-"
							+ requestList.get(index).getTargetLanguage();
					if (null == requestList.get(index).getSource() || requestList.get(index).getSource().isEmpty()) {
						errorResponse = new JSONObject();
						errorResponse.put("code", "MISSING_SOURCE");
						errorResponse.put("message", "Source is missing");
						listResponse.add(errorResponse);
					} else if (null == requestList.get(index).getSourceLanguage()
							|| requestList.get(index).getSourceLanguage().isEmpty()) {
						errorResponse = new JSONObject();
						errorResponse.put("code", "MISSING_SOURCE_LANGUAGE");
						errorResponse.put("message", "SourceLanguage is missing");
						listResponse.add(errorResponse);
					} else if (null == requestList.get(index).getTargetLanguage()
							|| requestList.get(index).getTargetLanguage().isEmpty()) {
						errorResponse = new JSONObject();
						errorResponse.put("code", "MISSING_TARGET_LANGUAGE");
						errorResponse.put("message", "TargetLanguage is missing");
						listResponse.add(errorResponse);
					} else if (supportedLanguages.contains(srcNtargetId) && (null != requestList.get(index).getSource()
							&& !requestList.get(index).getSource().isEmpty())) {
						result = Transliterator.getInstance(srcNtargetId).transform(requestList.get(index).getSource());

						response = new TransliteratorResponse();
						response.setSource(requestList.get(index).getSource());
						response.setTarget(result);
						response.setSourceLanguage(requestList.get(index).getSourceLanguage());
						response.setTargetLanguage(requestList.get(index).getTargetLanguage());
						listResponse.add(response);

					} else {
						errorResponse = new JSONObject();
						errorResponse.put("code", "INVALID_LANGUAGE");
						errorResponse.put("message", "Language is invalid");
						listResponse.add(errorResponse);
					}

				}
			} else {
				errorResponse = new JSONObject();
				errorResponse.put("code", "MISSING_INPUT");
				errorResponse.put("message", "Missing Input");
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
			}
		}

		catch (JsonMappingException e) {
			errorResponse = new JSONObject();
			errorResponse.put("code", "INVALID_REQUEST");
			errorResponse.put(UserConstants.MESSAGE, "Invalid request format");
			return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
		}

		catch (Exception e) {
			errorResponse = new JSONObject();
			errorResponse.put("code", "SERVER_ERROR");
			errorResponse.put(UserConstants.MESSAGE, "Failed to transliterate");
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(errorResponse).build();
		}
		return Response.status(Response.Status.OK).entity(listResponse).build();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.idms.service.UserServiceImpl#idmsDirectLogin(java.lang.String,
	 * java.lang.String, java.lang.String, java.lang.String, java.lang.String)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public Response idmsDirectLogin(String startUrl, String idToken1, String idToken2, String submitted,
			String loginbutton) {
		LOGGER.info("Entered idmsDirectLogin() -> Start");
		LOGGER.info("Parameter startUrl -> " + startUrl + " ,idToken1 -> " + idToken1);
		LOGGER.info("Parameter submitted -> " + submitted);
		LOGGER.info("Parameter loginbutton -> " + loginbutton);
		long elapsedTime;
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		JSONObject jsonObject = new JSONObject();
		Configuration conf = null;
		Response response = null;
		String token = null;
		StringBuffer prefix = new StringBuffer();
		String valueToFind = "goto=";
		Response.ResponseBuilder rb = null;
		try {
			if ((null == startUrl || startUrl.isEmpty())) {
				userResponse.setStatus(errorStatus);
				userResponse.setMessage("StartUrl value is mandatory");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by idmsDirectLogin() : " + elapsedTime);
				LOGGER.error("Error in idmsDirectLogin is " + userResponse.getMessage());
				return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
			} else if ((null == idToken1 || idToken1.isEmpty())) {
				userResponse.setStatus(errorStatus);
				userResponse.setMessage("idToken1 value is mandatory");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error in idmsDirectLogin is " + userResponse.getMessage());
				LOGGER.info("Time taken by idmsDirectLogin() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
			} else if ((null == idToken2 || idToken2.isEmpty())) {
				userResponse.setStatus(errorStatus);
				userResponse.setMessage("idToken2 value is mandatory");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error in idmsDirectLogin is " + userResponse.getMessage());
				LOGGER.info("Time taken by idmsDirectLogin() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
			} else if ((null == submitted || submitted.isEmpty())) {
				userResponse.setStatus(errorStatus);
				userResponse.setMessage("Submitted value is mandatory");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error in idmsDirectLogin is " + userResponse.getMessage());
				LOGGER.info("Time taken by idmsDirectLogin() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
			} else if ((null == loginbutton || loginbutton.isEmpty())) {
				userResponse.setStatus(errorStatus);
				userResponse.setMessage("Loginbutton value is mandatory");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error in idmsDirectLogin is " + userResponse.getMessage());
				LOGGER.info("Time taken by idmsDirectLogin() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
			} else {

				rb = Response.status(Response.Status.MOVED_PERMANENTLY);

				LOGGER.info("Start: authenticateIdmsChinaUser() of openam");
				String tokenResponse = productService.authenticateIdmsChinaUser(idToken1, idToken2,
						UserConstants.SE_REALM);
				LOGGER.info("End: authenticateIdmsChinaUser() of openam finished");
				conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
				DocumentContext productDocCtx = JsonPath.using(conf).parse(tokenResponse);
				token = productDocCtx.read(JsonConstants.TOKEN_ID);

				Cookie cookie = new Cookie("iPlanetDirectoryPro", token, "/", ".schneider-electric.com");
				NewCookie newCookie = new NewCookie(cookie);
				rb.cookie(newCookie);

				LOGGER.info("New Cookievalue:" + newCookie);
				LOGGER.info("Request builder:" + rb);
				LOGGER.info("Token:" + token);

				// startUrl = startUrl.substring(0,
				// startUrl.indexOf(valueToFind)+valueToFind.length()).concat(URLEncoder.encode(valueToFind.substring(valueToFind.indexOf(valueToFind)+5,
				// valueToFind.length()), "UTF-8" ));
				if (startUrl.contains(valueToFind)) {
					prefix.append(prefixIdentityUrl).append("/ui/#!")
							.append(startUrl.substring(0, startUrl.indexOf(valueToFind) + valueToFind.length()))
							.append(URLEncoder.encode(startUrl.substring(
									startUrl.indexOf(valueToFind) + valueToFind.length(), startUrl.length()), "UTF-8"));
				} else {
					prefix.append(prefixIdentityUrl).append(startUrl);
				}

				response = rb.header("Location", prefix.toString()).build();

			}

		} catch (Exception e) {
			LOGGER.error("idmsDirectLogin Exception occured!!!!" + e.getMessage(), e);

			jsonObject.put("error_code", "L9101");
			jsonObject.put("error_message", "Invalid username or password");
			/*
			 * prefix.append(UserConstants.redirectUrl_Option3)
			 * .append("?startUrl=")
			 */
			/*
			 * prefix.append("startUrl=") .append(startUrl)
			 */

			try {
				prefix = new StringBuffer();
				if (startUrl.contains(valueToFind)) {
					prefix.append(prefixIdentityUrl).append("/ui/#!")
							.append(startUrl.substring(0, startUrl.indexOf(valueToFind) + valueToFind.length()))
							.append(URLEncoder.encode(startUrl.substring(
									startUrl.indexOf(valueToFind) + valueToFind.length(), startUrl.length()), "UTF-8"));
				} else {
					prefix.append(prefixIdentityUrl).append(startUrl).append("&login_error=L9101");
				}
			} catch (UnsupportedEncodingException e1) {

			}
			/*
			 * prefix.append(prefixStartUrl) .append("/ui/#!")
			 */
			// .append("/login?login_error=L9101");

			response = rb.header("Location", prefix.toString()).build();
			// return
			// Response.status(Response.Status.UNAUTHORIZED).entity(jsonObject).build();
		}
		return response;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.idms.service.UserServiceImpl#idmsCheckUserExists(com.idms.model.
	 * CheckUserExistsRequest)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public Response idmsCheckUserExists(CheckUserExistsRequest request) {
		LOGGER.info("Entered idmsCheckUserExists() -> Start");
		DocumentContext productDocCtx = null, productDocApp = null;
		String iPlanetDirectoryKey = null;
		String ifwAccessToken = null, userExists = null;
		JSONObject response = new JSONObject();
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		Response ifwResponse = null, appDetails = null;
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		String loginId = null, mobileNum = null, fieldType = null;
		ObjectMapper objMapper = new ObjectMapper();
		Integer resultCount = 0;
		ArrayList<String> varList = new ArrayList<String>();
		String appname = null, enableTestMailStatus = null;

		try {
			LOGGER.info("Parameter request -> " + objMapper.writeValueAsString(request));
			if(null == request){
				response.put(UserConstants.MESSAGE_L, "Request body is empty or null");
				LOGGER.error("Mandatory check: Request body is empty or null");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by idmsCheckUserExists() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}
						
			if(null != request.getEmail() && !request.getEmail().isEmpty()){
				varList.add("true");
			}
			if(null != request.getMobile() && !request.getMobile().isEmpty()){
				varList.add("true");
			}
			if(null != request.getLoginID() && !request.getLoginID().isEmpty()){
				varList.add("true");
			}
			if(null != request.getIdmsFederatedId() && !request.getIdmsFederatedId().isEmpty()){
				varList.add("true");
			}
			LOGGER.info("Parameters size(email,mobile,loginid,idmsFedid): " + varList.size());
			if(varList.size()>1){
				response.put(UserConstants.MESSAGE_L, "Only one identifier is allowed");
				LOGGER.error("Mandatory check: Only one identifier is allowed");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by idmsCheckUserExists() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}
			if ((null == request.getEmail() || request.getEmail().isEmpty())
					&& (null == request.getMobile() || request.getMobile().isEmpty())
					&& (null == request.getLoginID() || request.getLoginID().isEmpty())
					&& (null == request.getIdmsFederatedId() || request.getIdmsFederatedId().isEmpty())) {
				response.put(UserConstants.MESSAGE_L, "Either one Email/Mobile/LoginID/FederatedId should have value");
				LOGGER.error("Mandatory check: Either one Email/Mobile/LoginID/FederatedId should have value");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by idmsCheckUserExists() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}
			if ((null != request.getWithGlobalUsers() && !request.getWithGlobalUsers().isEmpty())
					&& (!UserConstants.TRUE.equalsIgnoreCase(request.getWithGlobalUsers())
							&& !UserConstants.FALSE.equalsIgnoreCase(request.getWithGlobalUsers()))) {
				response.put(UserConstants.MESSAGE_L, UserConstants.GLOBAL_USER_BOOLEAN);
				LOGGER.error("Error in idmsCheckUserExists is " + UserConstants.GLOBAL_USER_BOOLEAN);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by idmsCheckUserExists() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}
			if (null != request.getEmail() && !request.getEmail().isEmpty()) {
				if (!emailValidator.validate(request.getEmail())) {
					response.put(UserConstants.MESSAGE_L, "Email validation failed");
					LOGGER.error("Error in idmsCheckUserExists is :: Email validation failed.");
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by idmsCheckUserExists() : " + elapsedTime);
					return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
				}
			}
			if (null != request.getMobile() && !request.getMobile().isEmpty()) {
				mobileNum = ChinaIdmsUtil.mobileTransformation(request.getMobile());
				if (!ChinaIdmsUtil.mobileValidator(mobileNum)) {
					response.put(UserConstants.MESSAGE_L, "Mobile validation failed");
					LOGGER.error("Error in idmsCheckUserExists is :: Mobile validation failed.");
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by idmsCheckUserExists() : " + elapsedTime);
					return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
				}
			}
			if (null != request.getLoginID() && !request.getLoginID().isEmpty()) {
				String loginString = request.getLoginID().trim();
				if (loginString.contains("@")) {
					if (!emailValidator.validate(loginString)) {
						response.put(UserConstants.MESSAGE_L, "LoginID validation failed");
						LOGGER.error("Error in idmsCheckUserExists is :: LoginID validation failed.");
						elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
						LOGGER.info("Time taken by idmsCheckUserExists() : " + elapsedTime);
						return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
					}
				} else if (!ChinaIdmsUtil.mobileValidator(loginString)) {
					response.put(UserConstants.MESSAGE_L, "LoginID validation failed");
					LOGGER.error("Error in idmsCheckUserExists is :: LoginID validation failed.");
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by idmsCheckUserExists() : " + elapsedTime);
					return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
				}
			}
			
			if (null != request.getEmail() && !request.getEmail().isEmpty()) {
				loginId = request.getEmail().trim();
				fieldType = "email";
			} else if (null != request.getMobile() && !request.getMobile().isEmpty()) {
				loginId = mobileNum;
				fieldType = "mobile";
			} else if (null != request.getLoginID() && !request.getLoginID().isEmpty()) {
				loginId = request.getLoginID().trim();
				fieldType = "loginID";
			} else if (null != request.getIdmsFederatedId() && !request.getIdmsFederatedId().isEmpty()) {
				loginId = request.getIdmsFederatedId().trim();
				fieldType = "idmsFederatedId";
			}
			LOGGER.info("loginId : " + loginId +" ,fieldType = "+fieldType);
			//check appname to allow testMailDomain			
			if (null != request.getApplicationName() && !request.getApplicationName().isEmpty()) {
				appname = request.getApplicationName().trim();
				LOGGER.info("Start: getUser() of openDJ for appname=" + appname);
				appDetails = openDJService.getUser(djUserName, djUserPwd, appname);
				LOGGER.info("End: finished getUser() of openDJ for appname=" + appname);
				LOGGER.info("Response code from OpenDJ for appDetails: " + appDetails.getStatus());
			}
			
			if (null != appDetails && 200 == appDetails.getStatus()) {
				productDocApp = JsonPath.using(conf)
						.parse(IOUtils.toString((InputStream) appDetails.getEntity()));
				String testMailStatusOpenAM = productDocApp.read("_enableTestMailDomain");
				if(null != testMailStatusOpenAM && !testMailStatusOpenAM.isEmpty()){
					enableTestMailStatus = testMailStatusOpenAM;
				} else{
					enableTestMailStatus = enableTestMailDomain;
				}
			}
			
			/*if (null != appDetails && 404 != appDetails.getStatus()) {
				enableTestMailStatus = enableTestMailDomain;
			}*/
			
			try {
				iPlanetDirectoryKey = getSSOToken();
				
			} //// No Exception handling
			catch (IOException ioExp) {
				LOGGER.error("Unable to get SSO Token " + ioExp.getMessage(),ioExp);
			}
			LOGGER.info(AUDIT_REQUESTING_USER + AUDIT_TECHNICAL_USER + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN
					+ AUDIT_OPENAM_API + AUDIT_OPENAM_USER_EXISTS_CALL + loginId + AUDIT_LOG_CLOSURE);
			
			if(fieldType.equalsIgnoreCase("email") || fieldType.equalsIgnoreCase("mobile") || fieldType.equalsIgnoreCase("loginID")){
				LOGGER.info("Start: checkUserExistsWithEmailMobile() of openam for loginId=" + loginId);
				userExists = productService.checkUserExistsWithEmailMobile(
						UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
						"mail eq " + "\"" + URLEncoder.encode(URLDecoder.decode(loginId, "UTF-8"), "UTF-8")
								+ "\" or mobile_reg eq " + "\""
								+ URLEncoder.encode(URLDecoder.decode(loginId, "UTF-8"), "UTF-8") + "\"");
				LOGGER.info("End: checkUserExistsWithEmailMobile() of openam finished for loginId=" + loginId);
			}
			
			if(fieldType.equalsIgnoreCase("idmsFederatedId")){
				LOGGER.info("Start: checkUserExistsWithEmailMobile() of openam for federationId=" + loginId);
				userExists = productService.checkUserExistsWithEmailMobile(
						UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
						"federationID  eq " + "\"" + URLEncoder.encode(URLDecoder.decode(loginId, "UTF-8"), "UTF-8")
								+ "\"");
				LOGGER.info("End: checkUserExistsWithEmailMobile() of openam finished for federationId=" + loginId);
			}

			productDocCtx = JsonPath.using(conf).parse(userExists);
			resultCount = productDocCtx.read("$.resultCount");
			LOGGER.info("resultCount=" + resultCount);
			if (resultCount.intValue() == 1) {
				response.put(UserConstants.MESSAGE_L, UserConstants.TRUE);
				response.put("idmsFederatedId", productDocCtx.read("$.result[0].federationID[0]"));
				if(Boolean.valueOf(productDocCtx.read("$.result[0].isActivated[0]"))){
					response.put("userStatus", UserConstants.USER_ACTIVE);
				} else {
					response.put("userStatus", UserConstants.USER_INACTIVE);
				}
				response.put("countryCode", UserConstants.CHINA_CODE);
				LOGGER.info("User found in IDMS China");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by idmsCheckUserExists() : " + elapsedTime);
				return Response.status(Response.Status.OK).entity(response).build();
			}
			if (resultCount.intValue() > 1) {
				response.put(UserConstants.MESSAGE_L, UserConstants.USER_MULTIPLE_EXIST);
				LOGGER.error("Error in idmsCheckUserExists is :: "+UserConstants.USER_MULTIPLE_EXIST);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by idmsCheckUserExists() : " + elapsedTime);
				return Response.status(Response.Status.CONFLICT).entity(response).build();
			}
			
			LOGGER.info("enableTestMailStatus value = " + enableTestMailStatus);
			if(resultCount.intValue() == 0){
				if (null != enableTestMailStatus && !Boolean.parseBoolean(enableTestMailStatus) && loginId.contains("@")) {
					String mailDomain = loginId.substring(loginId.indexOf("@") + 1);
					LOGGER.info("mailDomain = " + mailDomain);
					if (pickListValidator.validate(UserConstants.TestMailDomain, mailDomain)) {
						response.put(UserConstants.MESSAGE_L, "This Email Domain is not allowed - "+mailDomain);
						LOGGER.error("Error in idmsCheckUserExists is :: "+"This Email Domain is not allowed - "+mailDomain);
						elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
						LOGGER.info("Time taken by idmsCheckUserExists() : " + elapsedTime);
						return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
					}
				}
			}
			
			if (resultCount.intValue() == 0 && UserConstants.TRUE.equalsIgnoreCase(request.getWithGlobalUsers())
					&& (loginId.contains("@") || fieldType.equalsIgnoreCase("idmsFederatedId"))) {
				LOGGER.info("Start: getIFWToken() of IFWService");
				ifwAccessToken = ifwService.getIFWToken(UserConstants.CONTENT_TYPE_URL_FROM,
						UserConstants.IFW_GRANT_TYPE, ifwClientId, ifwClientSecret);
				LOGGER.info("End: getIFWToken() of IFWService finished");
				productDocCtx = null;
				productDocCtx = JsonPath.using(conf).parse(ifwAccessToken);
				String accessToken = productDocCtx.read("$.access_token");
				String bfoAuthorizationToken = sfSyncServiceImpl.getSFToken();
				String authorization = "Bearer " + accessToken;
				LOGGER.info("IFW dynamic Token = "+authorization);

				if (loginId.contains("@")) {
					LOGGER.info("Start: checkUserExistsWithEmail() of IFWService for email:" + loginId);
					ifwResponse = ifwService.checkUserExistsWithEmail(bfoAuthorizationToken, UserConstants.APPLICATION_NAME,
							UserConstants.COUNTRY_CODE, UserConstants.LANGUAGE_CODE, UserConstants.REQUEST_ID,
							authorization, loginId, false);
					LOGGER.info("End: checkUserExistsWithEmail() of IFWService finished for email:" + loginId);
				}
				if (fieldType.equalsIgnoreCase("idmsFederatedId")) {
					LOGGER.info("Start: checkUserExistsWithEmail() of IFWService for fedID:" + loginId);
					ifwResponse = ifwService.checkUserExistsWithFedId(bfoAuthorizationToken, UserConstants.APPLICATION_NAME,
							UserConstants.COUNTRY_CODE, UserConstants.LANGUAGE_CODE, UserConstants.REQUEST_ID,
							authorization, loginId, false);
					LOGGER.info("End: checkUserExistsWithEmail() of IFWService finished for fedID:" + loginId);
				}
	
				LOGGER.info("ifwResponse response status code for checkUserExist -> " + ifwResponse.getStatus());
				
				if (null != ifwResponse && 200 == ifwResponse.getStatus()) {
					productDocCtx = null;
					productDocCtx = JsonPath.using(conf)
							.parse(IOUtils.toString((InputStream) ifwResponse.getEntity()));
					LOGGER.info("ifwResponse == "+productDocCtx.jsonString());
					
					response.put("userStatus", productDocCtx.read("$.userStatus"));
					response.put("idmsFederatedId", productDocCtx.read("$.idmsFederatedId"));
					response.put("countryCode", productDocCtx.read("$.countryCode"));
					response.put(UserConstants.MESSAGE_L, UserConstants.TRUE);
					LOGGER.info("User found in IDMS Global");
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by idmsCheckUserExists() : " + elapsedTime);
					return Response.status(ifwResponse.getStatus()).entity(response).build();
				} else if (null != ifwResponse && 404 == ifwResponse.getStatus()) {
					response.put(UserConstants.MESSAGE_L, UserConstants.FALSE);
					LOGGER.info("User NOT found in IDMS Global");
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by idmsCheckUserExists() : " + elapsedTime);
					return Response.status(ifwResponse.getStatus()).entity(response).build();
				} else if (null != ifwResponse && 400 == ifwResponse.getStatus()) {
					response.put(UserConstants.MESSAGE_L, UserConstants.BAD_REQUEST);
					LOGGER.error("Error in idmsCheckUserExists is :: " + UserConstants.BAD_REQUEST);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by idmsCheckUserExists() : " + elapsedTime);
					return Response.status(ifwResponse.getStatus()).entity(response).build();
				} else if (null != ifwResponse && 409 == ifwResponse.getStatus()) {
					response.put(UserConstants.MESSAGE_L, UserConstants.USER_MULTIPLE_EXIST);
					LOGGER.error("Error in idmsCheckUserExists is :: " + UserConstants.USER_MULTIPLE_EXIST);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by idmsCheckUserExists() : " + elapsedTime);
					return Response.status(ifwResponse.getStatus()).entity(response).build();
				} else if (null != ifwResponse && 500 == ifwResponse.getStatus()) {
					response.put(UserConstants.MESSAGE_L, UserConstants.SERVER_ERROR_IFW);
					LOGGER.error("Error in idmsCheckUserExists is :: " + UserConstants.SERVER_ERROR_IFW);
					return Response.status(ifwResponse.getStatus()).entity(response).build();
				} else if (null != ifwResponse && 401 == ifwResponse.getStatus()) {
					response.put(UserConstants.MESSAGE_L, UserConstants.AUTHENTICATION_ERROR_IFW);
					LOGGER.error("Error in idmsCheckUserExists is :: " + UserConstants.AUTHENTICATION_ERROR_IFW);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by idmsCheckUserExists() : " + elapsedTime);
					return Response.status(ifwResponse.getStatus()).entity(response).build();
				}

				response.put(UserConstants.MESSAGE_L, "Global idmsCheckUserExist failed to perform");
				LOGGER.error("Error in idmsCheckUserExists is :: Global checkUserExist failed to perform");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by idmsCheckUserExists() : " + elapsedTime);
				return Response.status(Response.Status.SERVICE_UNAVAILABLE).entity(response).build();
			} else {
				response.put(UserConstants.MESSAGE_L, UserConstants.FALSE);
				LOGGER.error("User not found");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by idmsCheckUserExists() : " + elapsedTime);
				return Response.status(Response.Status.NOT_FOUND).entity(response).build();
			}
		} catch (BadRequestException e) {
			response.put(UserConstants.MESSAGE_L, UserConstants.BAD_REQUEST);
			LOGGER.error("BadRequestException in idmsCheckUserExists :: -> " + e.getMessage(),e);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by idmsCheckUserExists() : " + elapsedTime);
			return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
		} catch (NotAuthorizedException e) {
			response.put(UserConstants.MESSAGE_L, "Authorization Failed");
			LOGGER.error("NotAuthorizedException in idmsCheckUserExists :: -> " + e.getMessage(),e);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by idmsCheckUserExists() : " + elapsedTime);
			return Response.status(Response.Status.UNAUTHORIZED).entity(response).build();
		} catch (NotFoundException e) {
			response.put(UserConstants.MESSAGE_L, UserConstants.USER_NOT_FOUND);
			LOGGER.error("NotFoundException in idmsCheckUserExists :: -> " + e.getMessage(),e);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by idmsCheckUserExists() : " + elapsedTime);
			return Response.status(Response.Status.NOT_FOUND).entity(response).build();
		} catch (Exception e) {
			response.put(UserConstants.MESSAGE_L, e.getMessage());
			LOGGER.error("Exception in idmsCheckUserExists :: -> " + e.getMessage(),e);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by idmsCheckUserExists() : " + elapsedTime);
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(response).build();
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public Response sendRemainderEmail(List<String> remainderUsersForActivation) {
		LOGGER.info("Entered sendRemainderEmail() -> Start");
		LOGGER.info("Parameter remainderUsersForActivation -> " + remainderUsersForActivation);
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		JSONObject response = new JSONObject();
		DocumentContext productDocCtx = null;
		String userData = null;
		String loginIdentifier = null;
		String iPlanetDirectoryKey = null;
		String loginId = null;
		String uniqueIdentifier = null;
		String product_json_string = null;
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		response.put(UserConstants.STATUS, UserConstants.STATUS_FAILD);
		Set<String> userNotSendEmail = new HashSet<String>();
		int mailCount = 0;
		ObjectMapper objMapper = new ObjectMapper();

		for (String federationId : remainderUsersForActivation) {
			try {
				LOGGER.info("federationId= " + federationId);
				loginId = null;
				product_json_string = null;
				try {
					iPlanetDirectoryKey = getSSOToken();
				} catch (IOException ioExp) {
					LOGGER.error("Unable to get SSO Token" + ioExp.getMessage(),ioExp);
				}

				LOGGER.info(AUDIT_REQUESTING_USER + AUDIT_TECHNICAL_USER + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN
						+ AUDIT_OPENAM_API + AUDIT_OPENAM_GET_CALL + AUDIT_LOG_CLOSURE);
				LOGGER.info("Start: getUser() of openam for federationId:" + federationId);
				userData = productService.getUser(iPlanetDirectoryKey, federationId);
				LOGGER.info("End: getUser() of openam finished for federationId:" + federationId);
				LOGGER.info("user data from Openam: " + ChinaIdmsUtil.printOpenAMInfo(userData));

				productDocCtx = JsonPath.using(conf).parse(userData);

				loginId = productDocCtx.read(JsonConstants.LOGIN_ID_LOWER_0);
				if (null == loginId) {
					loginId = productDocCtx.read(JsonConstants.LOGIN_ID_UPPER_0);
				}

				if (null == loginId || loginId.isEmpty()) {

					String lName = null != productDocCtx.read("$.emailcount[0]")
							? getValue(productDocCtx.read("$.emailcount[0]").toString()) : getDelimeter();

					if (null != lName && Integer.valueOf(lName).intValue() < 4) {
						mailCount = Integer.valueOf(lName).intValue();
						uniqueIdentifier = productDocCtx.read("$.mail[0]");

						if (null == uniqueIdentifier || uniqueIdentifier.isEmpty()) {
							uniqueIdentifier = productDocCtx.read("$.mobile[0]");
						}

						String regestrationSource = productDocCtx.read("$.registerationSource[0]");
						String otp = sendEmail.generateOtp(federationId);
						LOGGER.info("Successfully OTP generated for " + federationId);

						if (!emailValidator.validate(uniqueIdentifier)) {
							sendEmail.sendOpenAmMobileEmail(otp, EmailConstants.USERREGISTRATION_OPT_TYPE, federationId,
									regestrationSource);

							sendEmail.sendSMSNewGateway(otp, EmailConstants.USERREGISTRATION_OPT_TYPE, federationId,
									regestrationSource);
						} else {
							sendEmail.sendOpenAmEmail(otp, EmailConstants.USERREGISTRATION_OPT_TYPE, federationId,
									regestrationSource);
						}

						mailCount = mailCount + 1;
						product_json_string = "{" + "\"emailcount\": \"" + mailCount + "\"}";
						LOGGER.info(
								"Start: updateUser() of openam to update email count for federationId:" + federationId);
						productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, federationId,
								product_json_string);
						LOGGER.info("End: updateUser() of openam to update email count finished for federationId:"
								+ federationId);
					} else {
						userNotSendEmail.add(federationId);
					}
				} else {
					userNotSendEmail.add(federationId);
				}
			} catch (Exception e) {
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by sendRemainderEmail() : " + elapsedTime);
				LOGGER.error("Exception in sendRemainderEmail() :: -> " + e.getMessage(),e);
				userNotSendEmail.add(federationId);
			}
		}
		response.put(UserConstants.STATUS, successStatus);
		response.put(UserConstants.MESSAGE,
				"Remainder Email sent successfuly and Failed to send Users are :: " + userNotSendEmail);
		elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		LOGGER.info("Time taken by sendRemainderEmail() : " + elapsedTime);
		return Response.status(Response.Status.OK).entity(response).build();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.idms.service.UserServiceImpl#transliteratorConversion(java.lang.
	 * String)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public Response transliteratorConversion(String jsonAsString) {
		// LOGGER.info("Entered transliteratorConversion() -> Start");
		// LOGGER.info("Parameter jsonAsString -> " + jsonAsString);

		String result = "";
		String srcNtargetId = null;
		JSONObject errorResponse = null;// new JSONObject();
		ArrayList<Object> listResponse = null;
		List<String> sourceLanguagesList = new ArrayList<String>();
		List<String> supportedSourceLanguagesList = new ArrayList<String>();
		List<String> supportedTargetLanguagesList = new ArrayList<String>();
		TransliteratorConversionResponse response = null;
		TransliteratorErrorResponse transErrorResponse = null;
		try {

			List<TransliteratorConversionRequest> requestList = new ObjectMapper().readValue(jsonAsString,
					new TypeReference<List<TransliteratorConversionRequest>>() {
					});

			List<String> supportedLanguages = LangSupportUtil.getTransilatorLanguages();
			for (String supportedLanguage : supportedLanguages) {
				String[] split = supportedLanguage.split("-");
				supportedSourceLanguagesList.add(split[0]);
				supportedTargetLanguagesList.add(split[1]);
			}

			if (null != requestList && requestList.size() > 0) {

				listResponse = new ArrayList<Object>();

				List<TransliteratorConversionRequest> conversionList = requestList;

				for (int index = 0; index < conversionList.size(); index++) {

					if (null != conversionList.get(index).getSourceLanguage()) {
						sourceLanguagesList.add(conversionList.get(index).getSourceLanguage());
					}

					srcNtargetId = conversionList.get(index).getSourceLanguage() + "-"
							+ conversionList.get(index).getTargetLanguage();

					if (null == conversionList.get(index).getIdentifier()
							|| conversionList.get(index).getIdentifier().isEmpty()) {
						transErrorResponse = new TransliteratorErrorResponse();
						transErrorResponse.setCode("MISSING_IDENTIFIER");
						transErrorResponse.setMessage("Identifier is missing");
						listResponse.add(transErrorResponse);
					} else if (null == conversionList.get(index).getSourceLanguage()
							|| conversionList.get(index).getSourceLanguage().isEmpty()) {
						transErrorResponse = new TransliteratorErrorResponse();
						transErrorResponse.setKey(conversionList.get(index).getIdentifier());
						transErrorResponse.setCode("MISSING_SOURCE_LANGUAGE");
						transErrorResponse.setMessage("SourceLanguage is missing");
						listResponse.add(transErrorResponse);
					} else if (null == conversionList.get(index).getTargetLanguage()
							|| conversionList.get(index).getTargetLanguage().isEmpty()) {
						transErrorResponse = new TransliteratorErrorResponse();
						transErrorResponse.setKey(conversionList.get(index).getIdentifier());
						transErrorResponse.setCode("MISSING_TARGET_LANGUAGE");
						transErrorResponse.setMessage("TargetLanguage is missing");
						listResponse.add(transErrorResponse);
					} else if (null == conversionList.get(index).getAttributes()
							|| conversionList.get(index).getAttributes().isEmpty()) {
						transErrorResponse = new TransliteratorErrorResponse();
						transErrorResponse.setKey(conversionList.get(index).getIdentifier());
						transErrorResponse.setCode("MISSING_ATTRIBUTES");
						transErrorResponse.setMessage("Attributes are missing");
						listResponse.add(errorResponse);
					} else if ((null != conversionList.get(index).getAttributes()
							&& conversionList.get(index).getAttributes().size() > 0)
							&& (supportedLanguages.contains(srcNtargetId))) {

						// List<TransliteratorAttributes> attributes = new
						// ArrayList<TransliteratorAttributes>();
						List<Object> attributes = new ArrayList<Object>();
						response = new TransliteratorConversionResponse();
						response.setIdentifier(conversionList.get(index).getIdentifier());
						response.setSourceLanguage(conversionList.get(index).getSourceLanguage());
						response.setTargetLanguage(conversionList.get(index).getTargetLanguage());

						response.setAttributes(attributes);

						listResponse.add(response);
						for (TransliteratorAttributes attribute : conversionList.get(index).getAttributes()) {

							TransliteratorAttributes attribueResponse = null;// new
																				// TransliteratorAttributes();
							transErrorResponse = null;
							/*
							 * if ((null == attribute.getKey() ||
							 * attribute.getKey().isEmpty()) && (null ==
							 * attribute.getValue() ||
							 * attribute.getValue().isEmpty())) {
							 * transErrorResponse = new JSONObject();
							 * transErrorResponse.put("code",
							 * "KEY_VALUE are missing");
							 * transErrorResponse.put("message",
							 * "Key Value are missing"); } else
							 */
							if (null == attribute.getKey() || attribute.getKey().isEmpty()) {
								transErrorResponse = new TransliteratorErrorResponse();
								transErrorResponse.setCode("MISSING_KEY");
								transErrorResponse.setMessage("Key is missing");
								/*
								 * transErrorResponse.put("code",
								 * "MISSING_KEY");
								 * transErrorResponse.put("message",
								 * "Key is missing");
								 */
							} else if (null == attribute.getValue() || attribute.getValue().isEmpty()) {
								transErrorResponse = new TransliteratorErrorResponse();
								transErrorResponse.setKey(attribute.getKey());
								transErrorResponse.setCode("MISSING_VALUE");
								transErrorResponse.setMessage("Value is missing");
							} else {
								result = Transliterator.getInstance(srcNtargetId).transform(attribute.getValue());
								attribueResponse = new TransliteratorAttributes();
								attribueResponse.setTarget(result);
								attribueResponse.setKey(attribute.getKey());
								attribueResponse.setValue(attribute.getValue());
							}
							if (null == transErrorResponse) {
								attributes.add(attribueResponse);
							} else {
								attributes.add(transErrorResponse);
							}
						}
						/*
						 * if (supportedLanguages.contains(srcNtargetId) &&
						 * (null != conversionList.get(index).getSource() &&
						 * !conversionList.get(index).getSource().isEmpty())) {
						 * result =
						 * Transliterator.getInstance(srcNtargetId).transform(
						 * conversionList.get(index).getSource());
						 * 
						 * response = new TransliteratorResponse();
						 * response.setSource(requestList.get(index).getSource()
						 * ); response.setTarget(result);
						 * response.setSourceLanguage(requestList.get(index).
						 * getSourceLanguage());
						 * response.setTargetLanguage(requestList.get(index).
						 * getTargetLanguage()); listResponse.add(response);
						 * 
						 * } else { errorResponse = new JSONObject();
						 * errorResponse.put("code", "INVALID_LANGUAGE");
						 * errorResponse.put("message", "Language is invalid");
						 * listResponse.add(errorResponse); }
						 */

					} /*
						 * else if (supportedLanguages.contains(srcNtargetId) &&
						 * (null != conversionList.get(index).getSource() &&
						 * !conversionList.get(index).getSource().isEmpty())) {
						 * result =
						 * Transliterator.getInstance(srcNtargetId).transform(
						 * conversionList.get(index).getSource());
						 * 
						 * response = new TransliteratorResponse();
						 * response.setSource(requestList.get(index).getSource()
						 * ); response.setTarget(result);
						 * response.setSourceLanguage(requestList.get(index).
						 * getSourceLanguage());
						 * response.setTargetLanguage(requestList.get(index).
						 * getTargetLanguage()); listResponse.add(response);
						 * 
						 * }
						 */ else {
						transErrorResponse = new TransliteratorErrorResponse();
						transErrorResponse.setKey(conversionList.get(index).getIdentifier());
						transErrorResponse.setCode("INVALID_LANGUAGE");
						transErrorResponse.setMessage("Language is invalid");
						listResponse.add(transErrorResponse);
					}
				}
			} else {
				errorResponse = new JSONObject();
				errorResponse.put("code", "MISSING_INPUT");
				errorResponse.put("message", "Missing Input");
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
			}

		} catch (JsonMappingException e) {
			errorResponse = new JSONObject();
			errorResponse.put("code", "INVALID_REQUEST");
			errorResponse.put(UserConstants.MESSAGE, "Invalid request format");
			// LOGGER.error("Error in transliteratorConversion is
			// "+e.getMessage());
			return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
		}

		catch (Exception e) {
			errorResponse = new JSONObject();
			errorResponse.put("code", "SERVER_ERROR");
			errorResponse.put(UserConstants.MESSAGE, "Failed to transliterate");
			// LOGGER.error("Error in transliteratorConversion is
			// "+e.getMessage());
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(errorResponse).build();
		}

		return Response.status(Response.Status.OK).entity(listResponse).build();
	}

	// private String getSaleforceToken() {
	// String bfoAuthorizationToken = sfSyncServiceImpl.getSFToken();
	// return "Bearer " + bfoAuthorizationToken;
	// }

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.idms.service.UserServiceImpl#oauthToIplanet(java.lang.String)
	 */
	@SuppressWarnings({ "unchecked" })
	public Response oauthToIplanet(String token) {
		LOGGER.info("Entered oauthToIplanet() -> Start");
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		JSONObject response = new JSONObject();
		JSONObject errorResponse = null;
		Response oauth2iplanetResponse = null;
		String tokenId = null;
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		try {
			LOGGER.info("Start: otpAuthentication() of openam");
			Response authenticate = productService.otpAuthentication("", "OAuth2IPlanet", UserConstants.HOTP_SERVICE,
					"OAuth2IPlanet", "");
			LOGGER.info("End: otpAuthentication() of openam");
			String cookieOath = ChinaIdmsUtil.getCookie(authenticate, ha_mode);
			LOGGER.info("cookieOath=" + cookieOath);
			String authResponseAsString = authenticate.readEntity(String.class);
			LOGGER.info("authenticate JSON request: " + authResponseAsString);
			LOGGER.info("Start: oauth2iplanet() of openam");
			if (token.contains("Bearer")) {
				LOGGER.info("token contains bearer.");
				String[] tokenSplit = token.split("Bearer ");
				LOGGER.info("Start: oauth2iplanet() of openam");
				oauth2iplanetResponse = productService.oauth2iplanet(cookieOath, "no-cache", tokenSplit[1],
						"OAuth2IPlanet", UserConstants.HOTP_SERVICE, "OAuth2IPlanet", authResponseAsString);
				LOGGER.info("End: oauth2iplanet() of openam");
			} else {
				LOGGER.info("Start: oauth2iplanet() of openam");
				oauth2iplanetResponse = productService.oauth2iplanet(cookieOath, "no-cache", token, "OAuth2IPlanet",
						UserConstants.HOTP_SERVICE, "OAuth2IPlanet", authResponseAsString);
				LOGGER.info("End: oauth2iplanet() of openam");
			}
			LOGGER.info("End: oauth2iplanet() of openam finished");
			if (Response.Status.UNAUTHORIZED.getStatusCode() == oauth2iplanetResponse.getStatus()) {
				errorResponse = new JSONObject();
				errorResponse.put("message", "token invalid");
				LOGGER.error("Error in oauth2iplanet: " + "UNAUTHORIZED");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by oauthToIplanet() : " + elapsedTime);
				return Response.status(Response.Status.UNAUTHORIZED).entity(errorResponse).build();
			}
			String oauth2iplanetResponseAsString = oauth2iplanetResponse.readEntity(String.class);
			LOGGER.info("authenticate JSON request: " + oauth2iplanetResponseAsString);
			DocumentContext productDocCtx = JsonPath.using(conf).parse(oauth2iplanetResponseAsString);
			tokenId = productDocCtx.read("$.tokenId");
		} catch (NotAuthorizedException e) {
			errorResponse = new JSONObject();
			errorResponse.put("message", "token invalid");
			LOGGER.error("NotAuthorizedException in oauth2iplanet: " + e.getMessage(),e);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by oauthToIplanet() : " + elapsedTime);
			return Response.status(Response.Status.UNAUTHORIZED).entity(errorResponse).build();
		} catch (Exception e) {
			errorResponse = new JSONObject();
			errorResponse.put("code", "SERVER_ERROR");
			errorResponse.put(UserConstants.MESSAGE, "oauth2iplanet failed.");
			LOGGER.error("Error in oauthToIplanet(): " + e.getMessage(),e);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by oauthToIplanet() : " + elapsedTime);
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(errorResponse).build();
		}
		response.put("iPlanetDirectoryPro", tokenId);
		elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		LOGGER.info("Time taken by oauthToIplanet() : " + elapsedTime);
		return Response.status(Response.Status.OK).entity(response).build();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.idms.service.UserServiceImpl#checkCompanyMappedOtherUsers(java.lang.
	 * String)
	 */
	public Integer checkCompanyMappedOtherUsers(String companyId) {
		LOGGER.info("Entered checkCompanyMappedOtherUsers() -> Start");
		LOGGER.info("Parameter companyId -> "+companyId);
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		DocumentContext productDocCtxCheck = null;
		String iPlanetDirectoryKey = "";

		try {
			iPlanetDirectoryKey = getSSOToken();
		} catch (IOException ioExp) {
			LOGGER.error("Unable to get SSO Token" + ioExp.getMessage(),ioExp);
			return 0;
		}

		LOGGER.info("Start: checkUserExistsWithEmailMobile() for companyId:" + companyId);
		String companyMapped = productService.checkUserExistsWithEmailMobile(
				UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
				"companyFederatedID eq " + "\"" + companyId + "\"");
		LOGGER.info("End: checkUserExistsWithEmailMobile() for companyId:" + companyId);
		productDocCtxCheck = JsonPath.using(conf).parse(companyMapped);
		Integer resultCountCheck = productDocCtxCheck.read(JsonConstants.RESULT_COUNT);
		LOGGER.info("resultCountCheck=" + resultCountCheck);

		return resultCountCheck;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.idms.service.UserServiceImpl#userRegistration_4_1(java.lang.String,
	 * java.lang.String, com.idms.model.CreateUserRequest)
	 */
	@Override
	public Response userRegistration_4_1(String clientId, String clientSecret, CreateUserRequest userRequest) {
		return this.userRegistration(clientId, clientSecret, userRequest);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.idms.service.UserServiceImpl#updateIDMSUserService(java.lang.String,
	 * java.lang.String, java.lang.String, com.idms.model.UpdateUserRequest)
	 */
	@Override
	public Response updateIDMSUserService(String authorizedToken, String clientId, String clientSecret,
			UpdateUserRequest userRequest) {
		return this.updateUser(authorizedToken, clientId, clientSecret, userRequest);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.idms.service.UserServiceImpl#executeCreateUserAndCompany(com.idms.
	 * model.CreateUserRequest)
	 */
	public void executeCreateUserAndCompany(CreateUserRequest userRequest) {
		LOGGER.info("Entered executeCreateUserAndCompany() -> Start");
		Integer resultCountCheck = 0;
		String iPlanetDirectoryKey;
		try {
			iPlanetDirectoryKey = getSSOToken();
		} catch (IOException ioExp) {
			LOGGER.error("Unable to get SSO Token" + ioExp.getMessage(),ioExp);
			iPlanetDirectoryKey = "";
		}
		// mapping IFW request to UserCompany
		CompanyV3 company = mapper.map(userRequest, CompanyV3.class);
		if (null != company.getLanguageCode() && !company.getLanguageCode().isEmpty()) {
			company.setLanguageCode(company.getLanguageCode().toLowerCase());
		}
		// Setting publicVisibility value to company.publicVisibility
		if (null != userRequest.getAttributes() && userRequest.getAttributes().size() > 0) {
			List<RegistrationAttributes> attributeList = userRequest.getAttributes();
			for (int i = 0; i < attributeList.size(); i++) {
				String KeyName = attributeList.get(i).getKeyName();
				String KeyValue = attributeList.get(i).getKeyValue();
				if (KeyName.equalsIgnoreCase("publicVisibility") && null != KeyValue && !KeyValue.isEmpty()) {
					company.setPublicVisibility(Boolean.parseBoolean(KeyValue));
				}
			}
		}

		UserV6 identity = mapper.map(userRequest, UserV6.class);
		if (null != identity.getLanguageCode() && !identity.getLanguageCode().isEmpty()) {
			identity.setLanguageCode(identity.getLanguageCode().toLowerCase());
		}

		/**
		 * In case mobile registration
		 */
		if ((null == userRequest.getUserRecord().getEmail() || userRequest.getUserRecord().getEmail().isEmpty())
				&& (null != userRequest.getUserRecord().getMobilePhone()
						&& !userRequest.getUserRecord().getMobilePhone().isEmpty())) {
			identity.setPhoneId(userRequest.getUserRecord().getMobilePhone());
		}

		if (null != userRequest.getUserRecord().getIDMSCompanyFederationIdentifier__c()
				&& !userRequest.getUserRecord().getIDMSCompanyFederationIdentifier__c().isEmpty()) {
			resultCountCheck = checkCompanyMappedOtherUsers(
					userRequest.getUserRecord().getIDMSCompanyFederationIdentifier__c());
			LOGGER.info("resultCount:" + resultCountCheck + " for Company id="
					+ userRequest.getUserRecord().getIDMSCompanyFederationIdentifier__c());
		}

		// forcedFederatedId = "cn00"+ UUID.randomUUID().toString();
		if (pickListValidator.validate(UserConstants.UIMSCreateUserSync, UserConstants.TRUE)) {
			// Calling SYNC method createUIMSUserAndCompany
			LOGGER.info("Start: Sync createUIMSUserAndCompany() for userName:"
					+ userRequest.getUserRecord().getIDMS_Federated_ID__c());
			uimsUserManagerSync.createUIMSUserAndCompany(CALLER_FID, identity,
					userRequest.getUserRecord().getIDMS_User_Context__c(), company,
					userRequest.getUserRecord().getIDMS_Federated_ID__c(),
					UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, UserConstants.V_NEW,
					userRequest.getPassword(), userRequest.getUserRecord().getIDMS_Federated_ID__c(), userRequest,
					resultCountCheck.intValue());
			LOGGER.info("End: Sync createUIMSUserAndCompany() finished for userName:"
					+ userRequest.getUserRecord().getIDMS_Federated_ID__c());

		} else {
			// Calling Async method createUIMSUserAndCompany
			LOGGER.info("Start: Async createUIMSUserAndCompany() of UIMSUserManagerSoapService for userName:"
					+ userRequest.getUserRecord().getIDMS_Federated_ID__c());
			uimsUserManagerSoapService.createUIMSUserAndCompany(CALLER_FID, identity,
					userRequest.getUserRecord().getIDMS_User_Context__c(), company,
					userRequest.getUserRecord().getIDMS_Federated_ID__c(),
					UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, UserConstants.V_NEW,
					userRequest.getPassword(), userRequest.getUserRecord().getIDMS_Federated_ID__c(), userRequest,
					resultCountCheck.intValue());
			LOGGER.info("End: Async createUIMSUserAndCompany() of UIMSUserManagerSoapService finished for userName:"
					+ userRequest.getUserRecord().getIDMS_Federated_ID__c());
			userRequest.getUserRecord().setIDMS_Federated_ID__c(userRequest.getUserRecord().getIDMS_Federated_ID__c());// federated
			// IDMS
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.idms.service.UserServiceImpl#getOIDCAutoDiscoveryConfig()
	 */
	@Override
	@SuppressWarnings({ "unchecked" })
	public Response getOIDCAutoDiscoveryConfig() {
		LOGGER.info("Entered getOIDCAutoDiscoveryConfig() -> Start");

		ObjectMapper oMapper = new ObjectMapper();
		LOGGER.info("Start: getOIDCAutoDiscoveryConfig() of openam");
		Response oidcAutoDiscoveryConfig = openAMTokenService.getOIDCAutoDiscoveryConfig();
		LOGGER.info("End: getOIDCAutoDiscoveryConfig() of openam");

		if (oidcAutoDiscoveryConfig.getStatus() == Response.Status.OK.getStatusCode()) {
			JsonNode jsonNode = null;
			Object entity = oidcAutoDiscoveryConfig.getEntity();
			try {
				String respString = IOUtils.toString((InputStream) entity);
				jsonNode = oMapper.readTree(respString);

				/*
				 * OpenAM OIDC discovery well-know REST response is missing the
				 * revoke endpoint hence, adding it explicitly. Note: This has
				 * to be taken care when OAM is upgraded to next versions, and
				 * if the OIDC discovery result is already having the revocation
				 * endpoint, this code should be removed Sample revocation
				 * endpoint URL for OpenAM
				 * https://<server-host>/accessmanager/oauth2/se/token/revoke
				 */

				String issuerUrl = ((ObjectNode) jsonNode).get("issuer").asText();
				((ObjectNode) jsonNode).put("revocation_endpoint", issuerUrl + "/token/revoke");

				String jsonString = jsonNode.toString();
				String tempJsonString = jsonString.replaceAll(prefixStartUrl, prefixIdentityUrl);
				JsonNode actualObj = oMapper.readTree(tempJsonString);

				return Response.status(Response.Status.OK).entity(actualObj).build();
			} catch (JsonProcessingException e) {
				LOGGER.error("JsonProcessingException in getOIDCAutoDiscoveryConfig() ::" + e.getMessage(),e);
			} catch (IOException e) {
				LOGGER.error("IOException in getOIDCAutoDiscoveryConfig() ::" + e.getMessage(),e);
			}
		} else {
			try {
				LOGGER.error("Received error from OpenAM OIDC discovery endpoint: "
						+ IOUtils.toString((InputStream) oidcAutoDiscoveryConfig.getEntity()));
			} catch (IOException e) {
				LOGGER.error("Error reading data stream from OpenAM OIDC discovery endpoint" + e.getMessage(),e);
			}
		}

		JSONObject errorResponse = new JSONObject();
		errorResponse.put("code", "SERVER_ERROR");
		errorResponse.put(UserConstants.MESSAGE, "Error generating OIDC Discovery data");
		return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(errorResponse).build();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.idms.service.UserServiceImpl#getTechnicalUserDetails(java.lang.
	 * String)
	 */
	public boolean getTechnicalUserDetails(String authorizationToken) {
		LOGGER.info("Entered getTechnicalUserDetails() -> Start");
		try {
			String userInfo = openAMTokenService.getUserDetails(authorizationToken);
			Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
			DocumentContext productDocCtx = JsonPath.using(conf).parse(userInfo);

			String userSubject = productDocCtx.read("$.userGroup");

			if (null != userSubject && userSubject.contains(DirectApiConstants.TECHNICAL_USER)) {
				return true;
			}
		} catch (Exception e) {
			LOGGER.error("Exception in getTechnicalUserDetails() ::" + e.getMessage(), e);
			return false;
		}
		return false;
	}

	/*
	 * Stage 1 Login
	 * User credential Validation
	 */
	@SuppressWarnings({ "unchecked" })
	@Override
	public Response securedLogin(String userName, String password, String realm, String app) {
		LOGGER.info("Entered securedLogin() -> Start");
		LOGGER.info("Parameter userName -> " + userName + " ,realm -> " + realm);
		LOGGER.info("Parameter app -> " + app);
		long startTime = System.currentTimeMillis();
		long elapsedTime;
		String successResponse = null;
		String regSource = app;
		List<String> accssControlList =null;
		ErrorResponse errorResponse = new ErrorResponse();
		boolean maintenanceMode=false;
		DocumentContext productDocCtx = null;
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		
		if ((app == null || app.equalsIgnoreCase("undefined"))) {
			regSource = UserConstants.LOGZ_IO_DEFAULT_APP;
		} else if ((app != null && app.contains("partner"))) {
			regSource = UserConstants.PRM_DEFAULT_SP_LOGIN;
		}
		Response checkUserExistsResponse = null;
		JSONObject jsonObject = new JSONObject();
		JSONObject jsonObjectResponse = new JSONObject();
		LOGGER.info(AUDIT_REQUESTING_USER.concat(userName).concat(AUDIT_IMPERSONATING_USER).concat(AUDIT_API_ADMIN)
				.concat(AUDIT_OPENAM_API).concat(AUDIT_OPENAM_AUTHENTICATE_CALL).concat(AUDIT_LOG_CLOSURE));
		cache = (EhCacheCache) cacheManager.getCache("iPlanetToken");
		if (null != cache) {
			LOGGER.info("cacahe NotNull");
		}

		try {
			LOGGER.info("Access Control List:"+maintenanceModeGlobal);
			if(maintenanceModeGlobal!=null)
				accssControlList = Arrays.asList(maintenanceModeGlobal.split(","));
			if(accssControlList!=null && accssControlList.size()>0 && !(accssControlList.contains("False"))){
				if(accssControlList.contains(UserConstants.MAINTENANCE_MODE_COMPLETE) || accssControlList.contains(UserConstants.MAINTENANCE_MODE_LOGIN) ){
					errorResponse.setStatus(errorStatus);
					errorResponse.setMessage(UserConstants.MAINTENANCE_MODE_MESSAGE);
					LOGGER.error("Error :: Maintenance mode in progress");
					maintenanceMode=true;
				}
				//Consider  exclusions for maintenance mode as below
				if(maintenanceMode){
					maintenanceMode = excludeMaintenanceMode(app,  UserConstants.MAINTENANCE_MODE_LOGIN);
				}
				if(maintenanceMode){
					return Response.status(Response.Status.SERVICE_UNAVAILABLE).entity(errorResponse).build();
				}
			}
			LOGGER.info("Start: aunthenticate User of OPENAMService for username=" + userName);
			Response authenticateResponse = ChinaIdmsUtil.executeHttpClient(prefixStartUrl, realm, userName, password);
			LOGGER.info("End: aunthenticate User of OPENAMService for username=" + userName);
			successResponse = (String) authenticateResponse.getEntity();
			LOGGER.info("Response code from OPENAMService: " + authenticateResponse.getStatus());
			
			productDocCtx = JsonPath.using(conf).parse(successResponse);
			String authIdSecuredLogin = productDocCtx.read("$.authId");
			String stage = productDocCtx.read("$.stage");
			
			if (401 == authenticateResponse.getStatus() && successResponse.contains(UserConstants.ACCOUNT_BLOCKED)) {
				jsonObjectResponse.put("message", UserConstants.ACCOUNT_BLOCKED);
				elapsedTime = (System.currentTimeMillis() - startTime);
				AsyncUtil.generateCSV(authCsvPath, new Date() + "," + userName + "," + errorStatus + "," + regSource
						+ "," + elapsedTime + "ms" + "," + UserConstants.ACCOUNT_BLOCKED);
				LOGGER.info("Time taken by securedLogin() : " + elapsedTime);
				return Response.status(Response.Status.UNAUTHORIZED.getStatusCode()).entity(jsonObjectResponse).build();

			} else if (401 == authenticateResponse.getStatus()) {
				LOGGER.info("Checking checkUserExists China");
				checkUserExistsResponse = checkUserExists(userName, UserConstants.FALSE);
				jsonObject =  (JSONObject) checkUserExistsResponse.getEntity();
				LOGGER.info("Response from checkUserExists China: " + jsonObject.toString());
				
				if(400 == checkUserExistsResponse.getStatus()){
					elapsedTime = (System.currentTimeMillis() - startTime);
					LOGGER.info("Time taken by securedLogin() : " + elapsedTime);
					return Response.status(Response.Status.BAD_REQUEST.getStatusCode()).entity(jsonObject).build();
				}

				if (UserConstants.TRUE.equalsIgnoreCase(jsonObject.get(UserConstants.MESSAGE_L).toString())) {
					if(UserConstants.CN_USER_ACTIVE.equalsIgnoreCase(jsonObject.get(UserConstants.USER_INFO).toString())){
						jsonObjectResponse.put("user_store", "CN");
						jsonObjectResponse.put("user_status", "Registered-Active");
						elapsedTime = (System.currentTimeMillis() - startTime);
						AsyncUtil.generateCSV(authCsvPath, new Date() + "," + userName + "," + errorStatus + "," + regSource
								+ "," + elapsedTime + "ms" + "," + UserConstants.INCORRECT_PASSWORD);
						LOGGER.info("Time taken by securedLogin() : " + elapsedTime);
						return Response.status(Response.Status.UNAUTHORIZED.getStatusCode()).entity(jsonObjectResponse).build();
					}
					if(UserConstants.CN_USER_INACTIVE.equalsIgnoreCase(jsonObject.get(UserConstants.USER_INFO).toString())){
						jsonObjectResponse.put("user_store", "CN");
						jsonObjectResponse.put("user_status", "Registered-Not-Active");
						elapsedTime = (System.currentTimeMillis() - startTime);
						AsyncUtil.generateCSV(authCsvPath, new Date() + "," + userName + "," + errorStatus + "," + regSource
								+ "," + elapsedTime + "ms" + "," + UserConstants.USER_INACTIVE);
						LOGGER.info("Time taken by securedLogin() : " + elapsedTime);
						return Response.status(Response.Status.UNAUTHORIZED.getStatusCode()).entity(jsonObjectResponse).build();
					}
					if(UserConstants.CN_USER_MULTIPLE_EXIST.equalsIgnoreCase(jsonObject.get(UserConstants.USER_INFO).toString())){
						jsonObjectResponse.put("user_store", "CN");
						jsonObjectResponse.put("user_status", "Multiple Registered Users");
						elapsedTime = (System.currentTimeMillis() - startTime);
						AsyncUtil.generateCSV(authCsvPath, new Date() + "," + userName + "," + errorStatus + "," + regSource
								+ "," + elapsedTime + "ms" + "," + UserConstants.CN_USER_MULTIPLE_EXIST);
						LOGGER.info("Time taken by securedLogin() : " + elapsedTime);
						return Response.status(Response.Status.UNAUTHORIZED.getStatusCode()).entity(jsonObjectResponse).build();
					}					
				} else {
					LOGGER.info("Checking checkUserExists Global");
					checkUserExistsResponse = checkUserExists(userName, UserConstants.TRUE);
					jsonObject =  (JSONObject) checkUserExistsResponse.getEntity();
					LOGGER.info("Response from checkUserExists Global: " + jsonObject.toString());

					if (UserConstants.TRUE.equalsIgnoreCase(jsonObject.get(UserConstants.MESSAGE_L).toString())) {
						jsonObjectResponse.put("user_store", "GLOBAL");
						elapsedTime = (System.currentTimeMillis() - startTime);
						AsyncUtil.generateCSV(authCsvPath, new Date() + "," + userName + "," + errorStatus + ","
								+ regSource + "," + elapsedTime + "ms" + "," + UserConstants.INCORRECT_PASSWORD);
						LOGGER.info("Time taken by securedLogin() : " + elapsedTime);
						return Response.status(Response.Status.UNAUTHORIZED.getStatusCode()).entity(jsonObjectResponse).build();
					} else {
						jsonObjectResponse.put("user_store", "None");
						elapsedTime = (System.currentTimeMillis() - startTime);
						AsyncUtil.generateCSV(authCsvPath, new Date() + "," + userName + "," + errorStatus + ","
								+ regSource + "," + elapsedTime + "ms" + "," + UserConstants.USER_NOT_EXISTS);
						LOGGER.info("Time taken by securedLogin() : " + elapsedTime);
						return Response.status(Response.Status.UNAUTHORIZED.getStatusCode()).entity(jsonObjectResponse).build();
					}
				}
			} else if (200 == authenticateResponse.getStatus() && (null != stage && !stage.isEmpty()) 
					&& stage.equalsIgnoreCase(UserConstants.STAGE_DEVICEIDMATCH2)) {
				LOGGER.info("authIdSecuredLogin : " + authIdSecuredLogin);
				LOGGER.info("stage : " + stage);
				JSONObject response = new JSONObject();
				response.put("authID", authIdSecuredLogin);
				response.put("stage", stage);
				LOGGER.info("securedLogin() -> Ending");
				return Response.status(Response.Status.OK.getStatusCode()).entity(response).build();
			}
		} catch (Exception e) {
			LOGGER.error("Problem in securedLogin():" + e.getMessage(),e);
			jsonObjectResponse.put("message", UserConstants.LOGIN_ERROR);
			elapsedTime = (System.currentTimeMillis() - startTime);
			AsyncUtil.generateCSV(authCsvPath, new Date() + "," + userName + "," + errorStatus + "," + regSource + ","
					+ elapsedTime + "ms" + "," + UserConstants.SERVER_ERROR);
			LOGGER.info("Time taken by securedLogin() : " + elapsedTime);
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode()).entity(jsonObjectResponse).build();
		}

		elapsedTime = (System.currentTimeMillis() - startTime);
		AsyncUtil.generateCSV(authCsvPath, new Date() + "," + userName + "," + successStatus + "," + regSource + ","
				+ elapsedTime + "ms" + "," + UserConstants.LOGIN_SUCCESS);
		LOGGER.info("securedLogin() -> Ending");
		return Response.status(Response.Status.OK.getStatusCode()).entity(successResponse).build();
	}
	
	/**
	 * Stage 2 & Stage 3 Login
	 * DeviceInfo or OTPInfo validation
	 */
	@SuppressWarnings("unchecked")
	@Override
	public Response securedLoginNext(UserMFADataRequest userMFADataRequest) {
		LOGGER.info("Entered securedLoginNext() -> Start");
		LOGGER.info("Parameter loginUser -> " + userMFADataRequest.getLoginUser());
		LOGGER.info("Parameter appName -> " + userMFADataRequest.getAppName());
		LOGGER.info("Parameter stageName -> " + userMFADataRequest.getStageName());
		
		ErrorResponse errorResponse = new ErrorResponse();
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		String successResponse = null, stage = null;
		DocumentContext productDocCtx = null;
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		String authIdSecuredLogin = null, header = null, stageNameFromUI = null, fileName = null;
		String fileNameDevice = "DeviceDataInformation.txt";
		String fileNameOTP = "DeviceOTPInformation.txt";

		try {
			if (null == userMFADataRequest.getAuthId() || userMFADataRequest.getAuthId().isEmpty()) {
				errorResponse.setStatus(ErrorCodeConstants.ERROR);
				errorResponse.setMessage(UserConstants.AUTHID_EMPTY);
				LOGGER.error(UserConstants.AUTHID_EMPTY);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by securedLoginNext() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
			}
			if (null == userMFADataRequest.getStageData() || userMFADataRequest.getStageData().isEmpty()) {
				errorResponse.setStatus(ErrorCodeConstants.ERROR);
				errorResponse.setMessage(UserConstants.DEVICEDATA_EMPTY);
				LOGGER.error(UserConstants.DEVICEDATA_EMPTY);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by securedLoginNext() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
			}
			if (null == userMFADataRequest.getLoginUser() || userMFADataRequest.getLoginUser().isEmpty()) {
				errorResponse.setStatus(ErrorCodeConstants.ERROR);
				errorResponse.setMessage(UserConstants.USER_EMPTY);
				LOGGER.error(UserConstants.USER_EMPTY);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by securedLoginNext() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
			}
			if (null == userMFADataRequest.getAppName() || userMFADataRequest.getAppName().isEmpty()) {
				errorResponse.setStatus(ErrorCodeConstants.ERROR);
				errorResponse.setMessage(UserConstants.APPNAME_EMPTY);
				LOGGER.error(UserConstants.APPNAME_EMPTY);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by securedLoginNext() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
			}
			if (null == userMFADataRequest.getStageName() || userMFADataRequest.getStageName().isEmpty()) {
				errorResponse.setStatus(ErrorCodeConstants.ERROR);
				errorResponse.setMessage(UserConstants.STAGENAME_EMPTY);
				LOGGER.error(UserConstants.STAGENAME_EMPTY);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by securedLoginNext() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
			}
			
			stageNameFromUI = userMFADataRequest.getStageName();
			if(!(stageNameFromUI.equals("deviceStage") || stageNameFromUI.equals("OTPStage"))){
				errorResponse.setStatus(ErrorCodeConstants.ERROR);
				errorResponse.setMessage(UserConstants.STAGENAME_INCORRECT);
				LOGGER.error(UserConstants.STAGENAME_INCORRECT);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by securedLoginNext() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
			}
			
			if(stageNameFromUI.equalsIgnoreCase("deviceStage")){
				fileName = fileNameDevice;
			}
			if(stageNameFromUI.equalsIgnoreCase("OTPStage")){
				fileName = fileNameOTP;
			}
			
			LOGGER.info("Start: checkDeviceInfo of OPENAMService for username="+userMFADataRequest.getLoginUser());
			Response authenticateResponse = ChinaIdmsUtil.executeHttpDeviceClient(prefixStartUrl, "se", userMFADataRequest.getAuthId(), 
					userMFADataRequest.getStageData(), fileName);
			LOGGER.info("End: checkDeviceInfo of OPENAMService for username="+userMFADataRequest.getLoginUser());
			successResponse = (String) authenticateResponse.getEntity();
			LOGGER.info("Response code from OPENAMService: " + authenticateResponse.getStatus());
			productDocCtx = JsonPath.using(conf).parse(successResponse);
			authIdSecuredLogin = productDocCtx.read("$.authId");
			stage = productDocCtx.read("$.stage");
			header = productDocCtx.read("$.header");

			if (200 != authenticateResponse.getStatus()) {
				LOGGER.error("Problem in securedLoginDevice():" + successResponse);
				JSONObject jsonObjectResponse = new JSONObject();
				jsonObjectResponse.put("message", productDocCtx.read("$.message"));
				elapsedTime = (System.currentTimeMillis() - startTime);
				LOGGER.info("Time taken by securedLoginNext() : " + elapsedTime);
				return Response.status(authenticateResponse.getStatus()).entity(jsonObjectResponse).build();
			}
			if (200 == authenticateResponse.getStatus() && (null != stage && !stage.isEmpty())
					&& stage.equalsIgnoreCase(UserConstants.STAGE_HOTP2)) {
				LOGGER.info("authIdSecuredLogin : " + authIdSecuredLogin);
				LOGGER.info("stage : " + stage);
				LOGGER.info("header : " + header);
				JSONObject response = new JSONObject();
				response.put("authID", authIdSecuredLogin);
				response.put("stage", stage);
				response.put("header", header);
				LOGGER.info("securedLoginNext() -> Ending");
				return Response.status(Response.Status.OK.getStatusCode()).entity(response).build();
			}
		} catch (Exception e) {
			LOGGER.error("Problem in securedLoginDevice():" + e.getMessage(), e);
			JSONObject jsonObjectResponse = new JSONObject();
			jsonObjectResponse.put("message", UserConstants.LOGIN_ERROR);
			elapsedTime = (System.currentTimeMillis() - startTime);
			AsyncUtil.generateCSV(authCsvPath, new Date() + "," + userMFADataRequest.getLoginUser() + "," + errorStatus
					+ "," + userMFADataRequest.getAppName() + "," + elapsedTime + "ms" + "," + e.getMessage());
			LOGGER.info("Time taken by securedLoginNext() : " + elapsedTime);
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR.getStatusCode()).entity(jsonObjectResponse)
					.build();
		}

		elapsedTime = (System.currentTimeMillis() - startTime);
		AsyncUtil.generateCSV(authCsvPath, new Date() + "," + userMFADataRequest.getLoginUser() + "," + successStatus + ","
				+ userMFADataRequest.getAppName() + "," + elapsedTime + "ms" + "," + UserConstants.LOGIN_SUCCESS);
		LOGGER.info("securedLoginNext() -> Ending");
		return Response.status(Response.Status.OK.getStatusCode()).entity(successResponse).build();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.idms.service.UserServiceImpl#updateOpenamDetails(java.lang.String,
	 * java.lang.String, java.lang.String)
	 */
	public void updateOpenamDetails(String iPlanetDirectoryKey, String federationId, String jsonData) {
		LOGGER.info("Entered updateOpenamDetails() -> Start");
		LOGGER.info("Parameter federationId -> " + federationId);
		LOGGER.info("Parameter jsonData -> " + ChinaIdmsUtil.printOpenAMInfo(jsonData));

		try {
			LOGGER.info("Start: updateUserForPassword() of openam for federatioId=" + federationId);
			Response updateResponse = productService.updateUserForPassword(
					UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, federationId, jsonData);
			LOGGER.info("End: updateUserForPassword() of openam finished for federatioId=" + federationId);
			//LOGGER.info("Information from OPENAM=" + IOUtils.toString((InputStream) updateResponse.getEntity()));
		} catch (Exception e) {
			LOGGER.error("Error in updateOpenamDetails() -> " + e.getMessage(),e);
		}
		LOGGER.info("Ended updateOpenamDetails()");
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see
	 * com.idms.service.UserServiceImpl#updatePasswordHistory(java.lang.String,
	 * java.lang.String, java.lang.String)
	 */
	public Response updatePasswordHistory(String iPlanetDirectoryKey, String federatioId, String jsonData) {
		LOGGER.info("Entered updatePasswordHistory() -> Start");
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		ErrorResponse errorResponse = new ErrorResponse();
		String message = null;
		Response jsonResponse = null;
		try {
			LOGGER.info("Start: updateUserForPassword() of openam for federatioId=" + federatioId);
			jsonResponse = productService.updateUserForPassword(UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
					federatioId, jsonData);
			LOGGER.info("End: updateUserForPassword() of openam finished for federatioId=" + federatioId);
			message = IOUtils.toString((InputStream) jsonResponse.getEntity());
			LOGGER.info("Message from OpenAM=" + ChinaIdmsUtil.printOpenAMInfo(message));
			if (200 != jsonResponse.getStatus()) {
				errorResponse.setStatus(errorStatus);
				errorResponse.setMessage("New password has been used previously.");
				LOGGER.error("Error in updatePasswordHistory()=" + message);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by updatePasswordHistory() : " + elapsedTime);
				return Response.status(Response.Status.PRECONDITION_FAILED).entity(errorResponse).build();
			}
		} catch (Exception e) {
			errorResponse.setStatus(errorStatus);
			errorResponse.setMessage(e.getMessage());
			LOGGER.error("Exception in updatePasswordHistory()=" + e.getMessage(),e);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by updatePasswordHistory() : " + elapsedTime);
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(errorResponse).build();
		}
		return jsonResponse;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.idms.service.UserServiceImpl#getUser(java.lang.String,
	 * java.lang.String)
	 */
	@SuppressWarnings("unchecked")
	@Override
	public Response getUser(String authorizationToken, String userId) {
		LOGGER.info("Entered getUser() -> Start");
		LOGGER.info("Parameter userId -> " + userId);
		JSONObject errorResponse = new JSONObject();
		Response response = null;

		if (!getTechnicalUserDetails(authorizationToken)) {
			errorResponse.put(UserConstants.MESSAGE, "Unauthorized or session expired");
			return Response.status(Response.Status.UNAUTHORIZED).entity(errorResponse).build();
		}
		response = getUser(userId);
		return response;
	}

	/**
	 * Dual Identifier Requirement given by Prasenjit
	 */
	@Override
	public Response verifyPIN(VerifyPinRequest verifyPinInfo) {
		LOGGER.info("Entered verifyPIN() -> Start");
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		ObjectMapper objMapper = new ObjectMapper();
		ErrorResponse errorResponse = new ErrorResponse();
		String mobileNum = null, pin = null, pinInOpenDJ = null;
		String otpStatus = null, otpValidityTime = null;
		try {
			LOGGER.info("Parameter userRequest -> " + objMapper.writeValueAsString(verifyPinInfo));
			if (null == verifyPinInfo.getMobileRegNumber() || verifyPinInfo.getMobileRegNumber().isEmpty()) {
				errorResponse.setStatus(ErrorCodeConstants.ERROR);
				errorResponse.setMessage(UserConstants.MOBILE_EMPTY);
				LOGGER.error(UserConstants.MOBILE_EMPTY);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by verifyPIN() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
			}
			if (null == verifyPinInfo.getPin() || verifyPinInfo.getPin().isEmpty()) {
				errorResponse.setStatus(ErrorCodeConstants.ERROR);
				errorResponse.setMessage(UserConstants.OTP_EMPTY);
				LOGGER.error(UserConstants.OTP_EMPTY);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by verifyPIN() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
			}
			mobileNum = verifyPinInfo.getMobileRegNumber().trim();
			pin = verifyPinInfo.getPin().trim();

			LOGGER.info("Start: getMobileOTPDetails() of OpenDjService for mobile=" + mobileNum);
			Response otpDetails = openDJService.getMobileOTPDetails(djUserName, djUserPwd, mobileNum);
			LOGGER.info("End: getMobileOTPDetails() of OpenDjService finished for mobile=" + mobileNum);
			LOGGER.info("Response code from OpenDJ for get call: " + otpDetails.getStatus());

			if (null != otpDetails && 200 == otpDetails.getStatus()) {
				Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
				DocumentContext productDocCtx = JsonPath.using(conf)
						.parse(IOUtils.toString((InputStream) otpDetails.getEntity()));
				pinInOpenDJ = productDocCtx.read("otpToken");
				otpStatus = productDocCtx.read("tokenStatus");
				otpValidityTime = productDocCtx.read("tokenExpirationTstamp");
			}
			if (null != pinInOpenDJ && !pinInOpenDJ.isEmpty()
					&& otpStatus.equalsIgnoreCase(UserConstants.PIN_NOT_VERIFIED)
					&& pinInOpenDJ.equalsIgnoreCase(pin)) {
				if (Long.parseLong(otpValidityTime) > System.currentTimeMillis()) {
					LOGGER.info("Pin verified and now changing pin status");

					PostMobileRecord postMobileRecord = new PostMobileRecord();
					postMobileRecord.set_id(mobileNum);
					postMobileRecord.setMobileNumber(mobileNum);
					postMobileRecord.setOtpToken(pinInOpenDJ);
					postMobileRecord.setTokenStatus(UserConstants.PIN_VERIFIED);
					postMobileRecord.setTokenExpirationTstamp(otpValidityTime);

					String json = objMapper.writeValueAsString(postMobileRecord);
					json = json.replace("\"\"", "[]");

					LOGGER.info("Start: putMobileOTPDetails() of OpenDjService for mobile=" + mobileNum);
					Response resPut = openDJService.putMobileOTPDetails("application/json", "*", djUserName, djUserPwd,
							mobileNum, json);
					LOGGER.info("End: putMobileOTPDetails() of OpenDjService finished for mobile=" + mobileNum);
					LOGGER.info("Response code from OpenDJ for put call=" + resPut.getStatus());
					if (200 == resPut.getStatus()) {
						LOGGER.info("otp details updated into OpenDJ for mobile : " + mobileNum);
					} else if (200 != resPut.getStatus()) {
						LOGGER.info("Bad request.. Record not updated in OpenDJ");
						errorResponse.setStatus(errorStatus);
						errorResponse.setMessage("Server issue, please raise a ticket");
						elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
						LOGGER.info("Time taken by sendOTP() : " + elapsedTime);
						return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(errorResponse).build();
					}
					errorResponse.setStatus(successStatus);
					errorResponse.setMessage(UserConstants.OTP_VALIDATED_SUCCESS);
					LOGGER.info(UserConstants.OTP_VALIDATED_SUCCESS + " for mobileNum ::" + mobileNum);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by verifyPIN() : " + elapsedTime);
					return Response.status(Response.Status.OK).entity(errorResponse).build();
				}
				if (Long.parseLong(otpValidityTime) <= System.currentTimeMillis()) {
					errorResponse.setStatus(errorStatus);
					errorResponse.setMessage(UserConstants.OTP_EXPIRED);
					LOGGER.error(UserConstants.OTP_EXPIRED + " for mobileNum ::" + mobileNum);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by verifyPIN() : " + elapsedTime);
					return Response.status(Response.Status.UNAUTHORIZED).entity(errorResponse).build();
				}
			} else if (null != pinInOpenDJ && !pinInOpenDJ.isEmpty()
					&& pinInOpenDJ.equalsIgnoreCase(UserConstants.PIN_VERIFIED)) {
				errorResponse.setStatus(errorStatus);
				errorResponse.setMessage(UserConstants.OTP_EXPIRED);
				LOGGER.error(UserConstants.OTP_EXPIRED + " for mobileNum ::" + mobileNum);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by verifyPIN() : " + elapsedTime);
				return Response.status(Response.Status.UNAUTHORIZED).entity(errorResponse).build();
			} else {
				errorResponse.setStatus(errorStatus);
				errorResponse.setMessage(UserConstants.OTP_INVALID);
				LOGGER.error(UserConstants.OTP_INVALID + " for mobileNum ::" + mobileNum);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by verifyPIN() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
			}
		} catch (Exception e) {
			errorResponse.setStatus(errorStatus);
			errorResponse.setMessage(e.getMessage());
			LOGGER.error("Exception in verifyPIN():: -> " + e.getMessage(), e);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by verifyPIN() : " + elapsedTime);
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(errorResponse).build();
		}
		errorResponse.setStatus(errorStatus);
		errorResponse.setMessage(UserConstants.OTP_INVALID);
		LOGGER.error(UserConstants.OTP_INVALID + " for mobileNum ::" + mobileNum);
		elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		LOGGER.info("Time taken by verifyPIN() : " + elapsedTime);
		return Response.status(Response.Status.BAD_REQUEST).entity(errorResponse).build();
	}

	/**
	 * This method is used to build cookies in PRM registration page redirection
	 */
	@Override
	public Response buildQueryParam(String relayState, String samlRequest, int length) {
		LOGGER.info("Entered buildQueryParam() -> Start");
		LOGGER.info("Parameter relayState -> " + relayState + " ,SAMLRequest  -> " + samlRequest
				+ " ,content length  -> " + length);
		ErrorResponse errorResponse = new ErrorResponse();
		String message = null;
		Response jsonResponse = null;
		Response.ResponseBuilder rb = null;
		String strQueryParam = null;
		int index = 0;
		String openAMHost = prefixStartUrl.substring(8);
		LOGGER.info("openAM part URL: " + openAMHost);
		String identityServiceHost = prefixIdentityUrl.substring(8);
		LOGGER.info("identityServiceHost URL: " + identityServiceHost);
		try {
			// jsonResponse =
			// identityService.buildQueryParam(relayState,samlRequest,registerPRMUserIdp,length);
			jsonResponse = openAMTokenService.buildQueryParam(relayState, samlRequest, registerPRMUserIdp, length);
			message = jsonResponse.getStatusInfo().getReasonPhrase();
			LOGGER.info("Message from OpenAM=" + message);
			LOGGER.info("HTTP status code from OpenAM=" + jsonResponse.getStatus());
			String location = jsonResponse.getLocation().toString();
			LOGGER.info("Location info from OpenAM=" + location);
			if (!openAMHost.equals(identityServiceHost))
				location = location.replaceAll(openAMHost, identityServiceHost);
			LOGGER.info("modifiedLocationUrl: " + location);
			if (302 != jsonResponse.getStatus()) {// Verifying redirect URL
				errorResponse.setStatus(errorStatus);
				errorResponse.setMessage("Error in building Query Param.");
				LOGGER.error("Error in buildQueryParam()=" + message);
				return Response.status(Response.Status.PRECONDITION_FAILED).entity(errorResponse).build();
			} else {
				/*
				 * String queryParam []=relayState.split("\\?"); for (String
				 * name:queryParam) { LOGGER.info("Relay state Query Params: " +
				 * name); }
				 */
				rb = Response.status(jsonResponse.getStatus()).entity(jsonResponse.getEntity()).header("Location",
						location);
				if (relayState != null) {
					index = relayState.indexOf("?");
				}
				LOGGER.info("index:" + index);
				if (relayState != null & index > -1) {
					strQueryParam = relayState.substring(index + 1);
					LOGGER.info("Relay state Query Params:" + strQueryParam);
					// if(queryParam.length> 1 && queryParam[1]!=null){
					// rb =
					// Response.status(Response.Status.FOUND).entity(jsonResponse.getEntity()).header("Location",jsonResponse.getLocation().toString());
					// rb =
					// Response.status(Response.Status.FOUND).entity(jsonResponse.getEntity()).header("Location",location);
					Cookie cookie = new Cookie("regQueryParams", strQueryParam, "/", domainName);
					NewCookie newCookie = new NewCookie(cookie);
					String amlbcookieArray[] = jsonResponse.getHeaderString("Set-Cookie").split(",");
					for (String responseCookie : amlbcookieArray) {
						LOGGER.info("cookie* " + responseCookie);
						rb = rb.header("Set-Cookie", responseCookie);
					}
					rb = rb.cookie(newCookie);// Adding new cookie to the
												// response
					/*
					 * jsonResponse.getCookies().put("regQueryParams",newCookie)
					 * ; jsonResponse=Response.status(Response.Status.FOUND).
					 * entity(jsonResponse.getEntity()).header("Location",
					 * jsonResponse.getLocation().toString()).cookie(newCookie).
					 * build();
					 * jsonResponse=Response.status(Response.Status.FOUND).
					 * header("Set-Cookie",newCookie).build();
					 */
					// jsonResponse=rb.build();
				}
				jsonResponse = rb.build();
			}
		} catch (Exception e) {
			errorResponse.setStatus(errorStatus);
			//errorResponse.setMessage(jsonResponse.getStatusInfo().getReasonPhrase());
			LOGGER.error("Exception in buildQueryParam()=" + e.getMessage(), e);
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(errorResponse).build();
		}
		return jsonResponse;

	}

	@SuppressWarnings("unchecked")
	@Override
	public Response idmsCheckIdentity(CheckUserIdentityRequest userRequest) {
		LOGGER.info("Entered idmsCheckIdentity() -> Start");

		DocumentContext productDocCtx = null;
		String iPlanetDirectoryKey = null;
		String ifwAccessToken = null;
		JSONObject response = new JSONObject();
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		Response ifwResponse = null;
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		String loginId = null;
		ObjectMapper objMapper = new ObjectMapper();

		try {
			LOGGER.info("Parameter request -> " + objMapper.writeValueAsString(userRequest));

			if (null == userRequest.getEmailOrMobile() || userRequest.getEmailOrMobile().isEmpty()) {
				response.put(UserConstants.STATUS, errorStatus);
				response.put(UserConstants.MESSAGE, "email/mobile is null or empty");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by idmsCheckIdentity() : " + elapsedTime);
				LOGGER.error("Error in idmsCheckIdentity is :: email/mobile is null or empty");
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}
			if (null != userRequest.getEmailOrMobile() && !userRequest.getEmailOrMobile().isEmpty()) {
				if (userRequest.getEmailOrMobile().contains("@")) {
					if (!emailValidator.validate(userRequest.getEmailOrMobile().trim())) {
						response.put(UserConstants.STATUS, errorStatus);
						response.put(UserConstants.MESSAGE, "Email validation failed.");
						elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
						LOGGER.info("Time taken by idmsCheckIdentity() : " + elapsedTime);
						LOGGER.error("Error in idmsCheckIdentity is :: Email validation failed.");
						return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
					}
				} else {
					String id = userRequest.getEmailOrMobile().trim();
					id = ChinaIdmsUtil.mobileTransformation(id);
					if (StringUtils.isNumeric(id)) {
						if (id.length() < 11) {
							response.put(UserConstants.STATUS, errorStatus);
							response.put(UserConstants.MESSAGE, "Mobile validation failed.");
							elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
							LOGGER.info("Time taken by idmsCheckIdentity() : " + elapsedTime);
							LOGGER.error("Error in idmsCheckIdentity is :: Mobile validation failed.");
							return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
						}
					} else {
						response.put(UserConstants.STATUS, errorStatus);
						response.put(UserConstants.MESSAGE, "Not valid email or mobile");
						elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
						LOGGER.info("Time taken by idmsCheckIdentity() : " + elapsedTime);
						LOGGER.error("Error in idmsCheckIdentity is :: Not valid email or mobile.");
						return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
					}
				}
			}

			loginId = userRequest.getEmailOrMobile().trim();
			try {
				iPlanetDirectoryKey = getSSOToken();
			} catch (IOException ioExp) {
				LOGGER.error("Unable to get SSO Token" + ioExp.getMessage(),ioExp);
				iPlanetDirectoryKey = "";
			}

			LOGGER.info(AUDIT_REQUESTING_USER + AUDIT_TECHNICAL_USER + AUDIT_IMPERSONATING_USER + AUDIT_API_ADMIN
					+ AUDIT_OPENAM_API + AUDIT_OPENAM_USER_EXISTS_CALL + loginId + AUDIT_LOG_CLOSURE);
			LOGGER.info("Start: checkUserExistsWithEmailMobile() of openam for loginId=" + loginId);
			String userExists = productService.checkUserExistsWithEmailMobile(
					UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
					"mail eq " + "\"" + URLEncoder.encode(URLDecoder.decode(loginId, "UTF-8"), "UTF-8")
							+ "\" or mobile_reg eq " + "\""
							+ URLEncoder.encode(URLDecoder.decode(loginId, "UTF-8"), "UTF-8") + "\"");
			LOGGER.info("End: checkUserExistsWithEmailMobile() of openam finished for loginId=" + loginId);

			productDocCtx = JsonPath.using(conf).parse(userExists);
			Integer resultCount = productDocCtx.read("$.resultCount");
			LOGGER.info("resultCount=" + resultCount);
			if (resultCount.intValue() > 0) {
				response.put(UserConstants.STATUS, successStatus);
				response.put("user_store", "CN");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by idmsCheckIdentity() : " + elapsedTime);
				return Response.status(Response.Status.OK).entity(response).build();

			} else {
				LOGGER.info("Start: getIFWToken() of IFWService");
				ifwAccessToken = ifwService.getIFWToken(UserConstants.CONTENT_TYPE_URL_FROM,
						UserConstants.IFW_GRANT_TYPE, ifwClientId, ifwClientSecret);
				LOGGER.info("End: getIFWToken() of IFWService finished");

				productDocCtx = JsonPath.using(conf).parse(ifwAccessToken);
				String accessToken = productDocCtx.read("$.access_token");

				String bfoAuthorizationToken = sfSyncServiceImpl.getSFToken();
				String authorization = "Bearer " + accessToken;

				if (loginId.contains("@")) {
					LOGGER.info("Start: checkUserExistsWithEmail() of IFWService for loginId:" + loginId);
					ifwResponse = ifwService.checkUserExistsWithEmail(bfoAuthorizationToken,
							UserConstants.APPLICATION_NAME, UserConstants.COUNTRY_CODE, UserConstants.LANGUAGE_CODE,
							UserConstants.REQUEST_ID, authorization, loginId, false);
					LOGGER.info("End: checkUserExistsWithEmail() of IFWService finished for loginId:" + loginId);
				} else {
					LOGGER.info("Start: checkUserExistsWithMobile() of IFWService for loginId:" + loginId);
					ifwResponse = ifwService.checkUserExistsWithMobile(bfoAuthorizationToken,
							UserConstants.APPLICATION_NAME, UserConstants.COUNTRY_CODE, UserConstants.LANGUAGE_CODE,
							UserConstants.REQUEST_ID, authorization, loginId, false);
					LOGGER.info("End: checkUserExistsWithMobile() of IFWService finished for loginId:" + loginId);
				}

				LOGGER.info("checkUserExist status from Global = " + ifwResponse.getStatus());
				if (null != ifwResponse && 200 == ifwResponse.getStatus()) {
					response.put(UserConstants.STATUS, successStatus);
					response.put("user_store", "GLOBAL");
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by idmsCheckIdentity() : " + elapsedTime);
					return Response.status(ifwResponse.getStatus()).entity(response).build();
				} else if (null != ifwResponse && 404 == ifwResponse.getStatus()) {
					response.put(UserConstants.STATUS, errorStatus);
					response.put(UserConstants.MESSAGE, UserConstants.FALSE);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by idmsCheckIdentity() : " + elapsedTime);
					return Response.status(ifwResponse.getStatus()).entity(response).build();
				} else if (null != ifwResponse && 400 == ifwResponse.getStatus()) {
					response.put(UserConstants.STATUS, errorStatus);
					response.put(UserConstants.MESSAGE, UserConstants.BAD_REQUEST);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by idmsCheckIdentity() : " + elapsedTime);
					return Response.status(ifwResponse.getStatus()).entity(response).build();
				} else if (null != ifwResponse && 500 == ifwResponse.getStatus()) {
					response.put(UserConstants.STATUS, errorStatus);
					response.put(UserConstants.MESSAGE, UserConstants.SERVER_ERROR_IFW);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by idmsCheckIdentity() : " + elapsedTime);
					return Response.status(ifwResponse.getStatus()).entity(response).build();
				} else if (null != ifwResponse && 409 == ifwResponse.getStatus()) {
					response.put(UserConstants.STATUS, errorStatus);
					response.put(UserConstants.MESSAGE, UserConstants.USER_EXISTS);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by idmsCheckIdentity() : " + elapsedTime);
					return Response.status(ifwResponse.getStatus()).entity(response).build();
				} else if (null != ifwResponse && 401 == ifwResponse.getStatus()) {
					response.put(UserConstants.STATUS, errorStatus);
					response.put(UserConstants.MESSAGE, UserConstants.AUTHENTICATION_ERROR_IFW);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by idmsCheckIdentity() : " + elapsedTime);
					return Response.status(ifwResponse.getStatus()).entity(response).build();
				}
				response.put(UserConstants.STATUS, "Error");
				response.put("user_store", "None");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by idmsCheckIdentity() : " + elapsedTime);
				return Response.status(Response.Status.NOT_FOUND).entity(response).build();
			}
		} catch (BadRequestException e) {
			response.put(UserConstants.STATUS, errorStatus);
			response.put(UserConstants.MESSAGE, UserConstants.USER_NOT_FOUND);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by idmsCheckIdentity() : " + elapsedTime);
			LOGGER.error("BadRequestException in idmsCheckIdentity() :: -> " + e.getMessage(),e);
			return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
		} catch (NotAuthorizedException e) {
			response.put(UserConstants.STATUS, errorStatus);
			response.put(UserConstants.MESSAGE, UserConstants.USER_NOT_FOUND);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by idmsCheckIdentity() : " + elapsedTime);
			LOGGER.error("NotAuthorizedException in idmsCheckIdentity() :: -> " + e.getMessage(),e);
			return Response.status(Response.Status.UNAUTHORIZED).entity(response).build();
		} catch (NotFoundException e) {
			response.put(UserConstants.STATUS, errorStatus);
			response.put(UserConstants.MESSAGE, UserConstants.USER_NOT_FOUND);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by idmsCheckIdentity() : " + elapsedTime);
			LOGGER.error("NotFoundException in idmsCheckIdentity() :: -> " + e.getMessage(),e);
			return Response.status(Response.Status.NOT_FOUND).entity(response).build();
		} catch (Exception e) {
			response.put(UserConstants.STATUS, errorStatus);
			response.put(UserConstants.MESSAGE, UserConstants.USER_NOT_FOUND);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by idmsCheckIdentity() : " + elapsedTime);
			LOGGER.error("Exception in idmsCheckIdentity() :: -> " + e.getMessage(),e);
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(response).build();
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public Response sendOTP(SendOTPRequest otpRequest) throws Exception {
		LOGGER.info("Entered sendOTP() -> Start");
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;

		ObjectMapper objMapper = new ObjectMapper();
		String otpMobile = null, otpStatus = null, otpValidityTime = null;
		//Thread.sleep(5000);
		String mobile = null;
		JSONObject response = new JSONObject();

		try {
			LOGGER.info("Parameter request -> " + objMapper.writeValueAsString(otpRequest));
			if (null == otpRequest.getMobile() || otpRequest.getMobile().isEmpty()) {
				response.put(UserConstants.STATUS, errorStatus);
				response.put(UserConstants.MESSAGE, UserConstants.MOBILE_EMPTY);
				LOGGER.error("Error in sendOTP() is ::" + UserConstants.MOBILE_EMPTY);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by sendOTP() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}
			if (null != otpRequest.getMobile() && !otpRequest.getMobile().isEmpty()) {
				mobile = ChinaIdmsUtil.mobileTransformation(otpRequest.getMobile().trim());
				if (!ChinaIdmsUtil.mobileValidator(mobile)) {
					response.put(UserConstants.STATUS, errorStatus);
					response.put(UserConstants.MESSAGE, "Mobile validation failed.");
					LOGGER.error("Error in sendOTP() is :: Mobile validation failed.");
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by sendOTP() : " + elapsedTime);
					return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
				}
			}

			LOGGER.info("Start: getMobileOTPDetails() of OpenDjService for mobile=" + mobile);
			Response otpDetails = openDJService.getMobileOTPDetails(djUserName, djUserPwd, mobile);
			LOGGER.info("End: getMobileOTPDetails() of OpenDjService finished for mobile=" + mobile);
			LOGGER.info("Response code from OpenDJ for get call: " + otpDetails.getStatus());

			if (null != otpDetails && 200 == otpDetails.getStatus()) {
				Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
				DocumentContext productDocCtx = JsonPath.using(conf)
						.parse(IOUtils.toString((InputStream) otpDetails.getEntity()));
				otpMobile = productDocCtx.read("otpToken");
				otpStatus = productDocCtx.read("tokenStatus");
				otpValidityTime = productDocCtx.read("tokenExpirationTstamp");
			}

			if (null != otpMobile && !otpMobile.isEmpty() && otpStatus.equalsIgnoreCase(UserConstants.PIN_NOT_VERIFIED)
					&& Long.parseLong(otpValidityTime) > System.currentTimeMillis()) {
				LOGGER.info("Got valid otp from OpenDJ");
			} else {
				LOGGER.info("creating new otp for mobile : " + mobile);
				otpMobile = RandomStringUtils.random(6, UserConstants.RANDOM_PIN_CHARS);
				Calendar now = Calendar.getInstance();
				now.add(Calendar.MINUTE, Integer.parseInt(otpvalidationtimeinminute));
				long validityTimeStamp = now.getTimeInMillis();

				PostMobileRecord postMobileRecord = new PostMobileRecord();
				postMobileRecord.set_id(mobile);
				postMobileRecord.setMobileNumber(mobile);
				postMobileRecord.setOtpToken(otpMobile);
				postMobileRecord.setTokenStatus(UserConstants.PIN_NOT_VERIFIED);
				postMobileRecord.setTokenExpirationTstamp(String.valueOf(validityTimeStamp));

				String json = objMapper.writeValueAsString(postMobileRecord);
				json = json.replace("\"\"", "[]");

				if (404 == otpDetails.getStatus()) {
					LOGGER.info("Start: postMobileOTPDetails() of OpenDjService for mobile=" + mobile);
					Response resPost = openDJService.postMobileOTPDetails("application/json", djUserName, djUserPwd,
							"create", json);
					LOGGER.info("End: postMobileOTPDetails() of OpenDjService finished for mobile=" + mobile);
					LOGGER.info("Response code from OpenDJ for post call=" + resPost.getStatus());

					if (201 == resPost.getStatus()) {
						LOGGER.info("OTP details saved into OpenDJ for mobile : " + mobile);
					} else if (412 == resPost.getStatus()) {
						LOGGER.info("duplicate insertion of mobile is denied by OpenDJ");
					} else {
						LOGGER.info("Exception in saving OTP details .. StatusCode: " + resPost.getStatus()
								+ " sent by OpenDJ");
						response.put(UserConstants.STATUS, errorStatus);
						response.put(UserConstants.MESSAGE, "Server issue, please raise a ticket");
						elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
						LOGGER.info("Time taken by sendOTP() : " + elapsedTime);
						return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(response).build();
					}
				} else {
					LOGGER.info("Start: putMobileOTPDetails() of OpenDjService for mobile=" + mobile);
					Response resPut = openDJService.putMobileOTPDetails("application/json", "*", djUserName, djUserPwd,
							mobile, json);
					LOGGER.info("End: putMobileOTPDetails() of OpenDjService finished for mobile=" + mobile);
					LOGGER.info("Response code from OpenDJ for put call=" + resPut.getStatus());
					if (200 == resPut.getStatus()) {
						LOGGER.info("otp details updated into OpenDJ for mobile : " + mobile);
					} else if (200 != resPut.getStatus()) {
						LOGGER.info("Bad request.. Record not updated in OpenDJ");
						response.put(UserConstants.STATUS, errorStatus);
						response.put(UserConstants.MESSAGE, "Server issue, please raise a ticket");
						elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
						LOGGER.info("Time taken by sendOTP() : " + elapsedTime);
						return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(response).build();
					}
				}
			}
			LOGGER.info("Start: sendSMS() for mobile user:"+mobile);
			sendEmail.sendSMS(otpMobile, mobile);
			LOGGER.info("End: sendSMS() finished for  mobile user:"+mobile);
			if(Boolean.valueOf(sendOTPOverEmail)){
				LOGGER.info("Start: sendMobileEmail() for mobile userName:" + mobile);
				sendEmail.sendMobileEmail(otpMobile, mobile);
				LOGGER.info("End: sendMobileEmail() finished for  mobile user:" + mobile);
			}
			else{
				LOGGER.info("Send Mobile OTP over Email() for mobile userName:" + mobile+" disallowed");
				/**LOGGER.info("Start: sendMobileEmail() for mobile userName:" + mobile);
				sendEmail.sendMobileEmail(otpMobile, mobile);
				LOGGER.info("End: sendMobileEmail() finished for  mobile user:" + mobile);*/
			}
			response.put(UserConstants.STATUS, successStatus);
			response.put(UserConstants.MESSAGE, UserConstants.PIN_SEND_SUCCESS);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by sendOTP() : " + elapsedTime);
			return Response.status(Response.Status.OK).entity(response).build();
		} catch (Exception e) {
			LOGGER.error("Exception in sendOTP() :: -> " + e.getMessage(),e);
			response.put(UserConstants.STATUS, errorStatus);
			response.put(UserConstants.MESSAGE, e.getMessage());
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by sendOTP() : " + elapsedTime);
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(response).build();
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public Response addMobile(AddMobileRequest addMobileRequest) {
		LOGGER.info("Entered addMobile() -> Start");
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		ObjectMapper objMapper = new ObjectMapper();
		String mobile = null, fedid = null, otpStoredStatus = null, regSource = null;
		JSONObject response = new JSONObject();
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		String openamVnew = null, userIdFromToken = null;
		Integer vNewCntValue = 0;

		try {
			LOGGER.info("Parameter request -> " + objMapper.writeValueAsString(addMobileRequest));

			if (null == addMobileRequest.getMobile() || addMobileRequest.getMobile().isEmpty()) {
				response.put(UserConstants.STATUS_L, errorStatus);
				response.put(UserConstants.MESSAGE_L, UserConstants.MOBILE_EMPTY);
				LOGGER.error("Error in addMobile() is ::" + UserConstants.MOBILE_EMPTY);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by addMobile() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}
			if (null != addMobileRequest.getMobile() && !addMobileRequest.getMobile().isEmpty()) {
				mobile = ChinaIdmsUtil.mobileTransformation(addMobileRequest.getMobile().trim());
				if (!ChinaIdmsUtil.mobileValidator(mobile)) {
					response.put(UserConstants.STATUS_L, errorStatus);
					response.put(UserConstants.MESSAGE_L, "Mobile validation failed.");
					LOGGER.error("Error in addMobile() is :: Mobile validation failed.");
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by addMobile() : " + elapsedTime);
					return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
				}
			}
			if (null == addMobileRequest.getFedId() || addMobileRequest.getFedId().isEmpty()) {
				response.put(UserConstants.STATUS_L, errorStatus);
				response.put(UserConstants.MESSAGE_L, UserConstants.FEDID_EMPTY);
				LOGGER.error("Error in addMobile() is ::" + UserConstants.FEDID_EMPTY);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by addMobile() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}
			if (null == addMobileRequest.getProfileUpdateSource()
					|| addMobileRequest.getProfileUpdateSource().isEmpty()) {
				response.put(UserConstants.STATUS_L, errorStatus);
				response.put(UserConstants.MESSAGE_L, UserConstants.PROFILE_UPDATE_SOURCE);
				LOGGER.error("Error in addMobile() is ::" + UserConstants.PROFILE_UPDATE_SOURCE);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by addMobile() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}
			if(null != addMobileRequest.getProfileUpdateSource() && !addMobileRequest.getProfileUpdateSource().isEmpty()){
				regSource = addMobileRequest.getProfileUpdateSource().trim();
			}
			
			if (null == addMobileRequest.getAccesstoken() || addMobileRequest.getAccesstoken().isEmpty()) {
				response.put(UserConstants.STATUS_L, errorStatus);
				response.put(UserConstants.MESSAGE_L, "User token is null or missing");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("User token is null or missing");
				LOGGER.info("Time taken by addMobile() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}
			
			LOGGER.info("Start: getUserInfoByAccessToken() of openam");
			String userInfoByAccessToken = openAMTokenService.getUserInfoByAccessToken(addMobileRequest.getAccesstoken(), "/se");
			LOGGER.info("End: getUserInfoByAccessToken() of openam finished");
			
			// Get fedid from openAMTokenService service
			DocumentContext productDocCtx = JsonPath.using(conf).parse(userInfoByAccessToken);
			userIdFromToken = productDocCtx.read("$.sub");
			LOGGER.info("userIdFromToken = " + userIdFromToken);
			
			if(!userIdFromToken.equals(addMobileRequest.getFedId().trim())){
				response.put(UserConstants.STATUS_L, errorStatus);
				response.put(UserConstants.MESSAGE_L, "User token is invalid...suspicious activity observed");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("User token is invalid...suspicious activity observed");
				LOGGER.info("Time taken by addMobile() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}

			CheckUserExistsRequest checkRequest = new CheckUserExistsRequest();
			checkRequest.setMobile(mobile);
			checkRequest.setWithGlobalUsers("false");
			checkRequest.setApplicationName(regSource);
			Response checkUserExist = idmsCheckUserExists(checkRequest);
			LOGGER.info("idmsCheckUserExists reponse in addmobile()::" + objMapper.writeValueAsString(checkUserExist));

			org.json.simple.JSONObject checkUserJson = (org.json.simple.JSONObject) checkUserExist.getEntity();
			String messageUser = checkUserJson.get(UserConstants.MESSAGE_L).toString();
			if (!messageUser.equalsIgnoreCase(UserConstants.FALSE)) {
				if (200 != checkUserExist.getStatus()) {
					response.put(UserConstants.STATUS_L, errorStatus);
					response.put(UserConstants.MESSAGE_L, messageUser);
					LOGGER.error("Error while idmsCheckUserExists in addMobile() ->  " + messageUser);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by addMobile() : " + elapsedTime);
					return Response.status(checkUserExist.getStatus()).entity(response).build();
				}
				if (200 == checkUserExist.getStatus()) {
					response.put(UserConstants.STATUS_L, errorStatus);
					response.put(UserConstants.MESSAGE_L, UserConstants.USER_EXISTS);
					LOGGER.error("User exists/registered in OpenAM");
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by addMobile() : " + elapsedTime);
					return Response.status(Response.Status.CONFLICT).entity(response).build();
				}
			}

			LOGGER.info("Start: getMobileOTPDetails() of OpenDjService for mobile=" + mobile);
			Response otpDetails = openDJService.getMobileOTPDetails(djUserName, djUserPwd, mobile);
			LOGGER.info("End: getMobileOTPDetails() of OpenDjService finished for mobile=" + mobile);
			LOGGER.info("Response code from OpenDJ for get call: " + otpDetails.getStatus());

			if (null != otpDetails && 200 == otpDetails.getStatus()) {
				productDocCtx = JsonPath.using(conf).parse(IOUtils.toString((InputStream) otpDetails.getEntity()));
				otpStoredStatus = productDocCtx.read("tokenStatus");
				productDocCtx = null;
			}

			if (null != otpStoredStatus && !otpStoredStatus.isEmpty()
					&& otpStoredStatus.equalsIgnoreCase(UserConstants.PIN_VERIFIED)) {
				LOGGER.info("User mobile verified and now registering as dual identifier");
			} else {
				response.put(UserConstants.STATUS_L, errorStatus);
				response.put(UserConstants.MESSAGE_L, "User mobile is not verified");
				LOGGER.info("User mobile is not verified.");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by addMobile() : " + elapsedTime);
				return Response.status(Response.Status.NOT_FOUND).entity(response).build();
			}

			fedid = addMobileRequest.getFedId().trim();

			String ssoToken = null;
			try {
				ssoToken = getSSOToken();
			} catch (IOException ioExp) {
				LOGGER.error("Unable to get SSO Token" + ioExp.getMessage(),ioExp);
				ssoToken = "";
			}
			LOGGER.info("Start: checkUserExistsWithEmailMobile() of openam for fedid = " + fedid);
			String userExistsInOpenam = productService.checkUserExistsWithEmailMobile(
					UserConstants.CHINA_IDMS_TOKEN + ssoToken,
					"federationID eq " + "\"" + fedid + "\" or uid eq " + "\"" + fedid + "\"");
			LOGGER.info("End: checkUserExistsWithEmailMobile() of openam for fedid = " + fedid);
			productDocCtx = JsonPath.using(conf).parse(userExistsInOpenam);
			Integer resultCount = productDocCtx.read(JsonConstants.RESULT_COUNT);
			LOGGER.info("resultCount = " + resultCount);
			
			openamVnew = null != productDocCtx.read("$.result[0].V_New[0]") ? getValue(productDocCtx.read("$.result[0].V_New[0]"))
					: getDelimeter();
			if (null != vNewCntValue && null != openamVnew) {
				vNewCntValue = Integer.parseInt(openamVnew) + 1;
			}
			
			if (resultCount.intValue() == 1) {
				String addMobileString = "{" + "\"mobile\": \"" + mobile + "\",\"mobile_reg\": \"" + mobile
						+ "\",\"login_mobile\": \"" + mobile + "\",\"V_New\": \"" + vNewCntValue + "\"" + "}";
				LOGGER.info(
						"Start: updateUser() of openamservice to add mobile as dual indentifier for userId:" + fedid);
				productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + ssoToken, fedid, addMobileString);
				LOGGER.info("End: updateUser() of openamservice to add mobile as dual indentifier finished for userId:"
						+ fedid);

				SendOTPRequest sendOTPRequest = new SendOTPRequest();
				sendOTPRequest.setMobile(mobile);
				deleteMobile(sendOTPRequest);

				if (null != regSource && !UserConstants.UIMS.equalsIgnoreCase(regSource)) {
					LOGGER.info("Start: ASYNC updateChangeEmailOrMobile() of UIMSService for federationID=" + fedid);
					uimsUserManagerSoapService.updateChangeEmailOrMobile(ssoToken, fedid, fedid, String.valueOf(vNewCntValue), "mobile", mobile);
					LOGGER.info("End: ASYNC updateChangeEmailOrMobile() of UIMSService finished for federationID=" + fedid);
				}
				response.put(UserConstants.STATUS_L, successStatus);
				response.put(UserConstants.MESSAGE_L, UserConstants.ADD_MOBILE_IDENTIFIER);
				return Response.status(Response.Status.OK).entity(response).build();
			}
			if (resultCount.intValue() > 1) {
				response.put(UserConstants.STATUS_L, errorStatus);
				response.put(UserConstants.MESSAGE_L, UserConstants.USER_MULTIPLE_EXIST+" federationID");
				LOGGER.error("Error in idmsCheckUserExists is :: "+UserConstants.USER_MULTIPLE_EXIST+" federationID");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by idmsCheckUserExists() : " + elapsedTime);
				return Response.status(Response.Status.CONFLICT).entity(response).build();
			}
			if (resultCount.intValue() == 0) {
				response.put(UserConstants.STATUS_L, errorStatus);
				response.put(UserConstants.MESSAGE_L, "User not found with fedID : " + fedid);
				LOGGER.error("Error in addMobile() is -> Adding mobile terminated, no user exist with fedID " + fedid);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by addMobile() : " + elapsedTime);
				return Response.status(Response.Status.NOT_FOUND).entity(response).build();
			}
		} catch (Exception e) {
			LOGGER.error("Exception in addMobile() :: -> " + e.getMessage(),e);
			response.put(UserConstants.STATUS_L, errorStatus);
			response.put(UserConstants.MESSAGE_L, e.getMessage());
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by addMobile() : " + elapsedTime);
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(response).build();
		}
		response.put(UserConstants.STATUS_L, errorStatus);
		response.put(UserConstants.MESSAGE_L, "User not found with fedID : " + fedid);
		LOGGER.error("Error in addMobile() is -> no user exist with fedID " + fedid);
		elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		LOGGER.info("Time taken by addMobile() : " + elapsedTime);
		return Response.status(Response.Status.NOT_FOUND).entity(response).build();
	}

	/**
	 * Adding new email to user
	 */
	@SuppressWarnings("unchecked")
	@Override
	public Response addEmail(AddEmailRequest addEmailRequest) {
		LOGGER.info("Entered addEmail() -> Start");
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		ObjectMapper objMapper = new ObjectMapper();
		String email = null, fedid = null, source = null, userIdFromToken = null;
		JSONObject response = new JSONObject();
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		DocumentContext productDocCtx = null;
		try {
			LOGGER.info("Parameter request -> " + objMapper.writeValueAsString(addEmailRequest));

			if (null == addEmailRequest.getEmail() || addEmailRequest.getEmail().isEmpty()) {
				response.put(UserConstants.STATUS_L, errorStatus);
				response.put(UserConstants.MESSAGE_L, UserConstants.EMAIL_EMPTY);
				LOGGER.error("Error in addEmail() is ::" + UserConstants.EMAIL_EMPTY);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by addEmail() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}
			if (null != addEmailRequest.getEmail() && !addEmailRequest.getEmail().isEmpty()) {
				if (!emailValidator.validate(addEmailRequest.getEmail().trim())) {
					response.put(UserConstants.STATUS_L, errorStatus);
					response.put(UserConstants.MESSAGE_L, "Email validation failed.");
					LOGGER.error("Error in addEmail() is :: Email validation failed.");
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by addEmail() : " + elapsedTime);
					return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
				}
			}
			if (null == addEmailRequest.getFedId() || addEmailRequest.getFedId().isEmpty()) {
				response.put(UserConstants.STATUS_L, errorStatus);
				response.put(UserConstants.MESSAGE_L, UserConstants.FEDID_EMPTY);
				LOGGER.error("Error in addEmail() is ::" + UserConstants.FEDID_EMPTY);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by addEmail() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}
			if (null == addEmailRequest.getProfileUpdateSource()
					|| addEmailRequest.getProfileUpdateSource().isEmpty()) {
				response.put(UserConstants.STATUS_L, errorStatus);
				response.put(UserConstants.MESSAGE_L, UserConstants.PROFILE_UPDATE_SOURCE);
				LOGGER.error("Error in addEmail() is ::" + UserConstants.PROFILE_UPDATE_SOURCE);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by addEmail() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}
			
			if (null == addEmailRequest.getAccesstoken() || addEmailRequest.getAccesstoken().isEmpty()) {
				response.put(UserConstants.STATUS_L, errorStatus);
				response.put(UserConstants.MESSAGE_L, "User token is null or missing");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("User token is null or missing");
				LOGGER.info("Time taken by addEmail() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}
			
			LOGGER.info("Start: getUserInfoByAccessToken() of openam");
			String userInfoByAccessToken = openAMTokenService.getUserInfoByAccessToken(addEmailRequest.getAccesstoken(), "/se");
			LOGGER.info("End: getUserInfoByAccessToken() of openam finished");
			
			// Get fedid from openAMTokenService service
			productDocCtx = JsonPath.using(conf).parse(userInfoByAccessToken);
			userIdFromToken = productDocCtx.read("$.sub");
			LOGGER.info("userIdFromToken = " + userIdFromToken);
			
			if(!userIdFromToken.equals(addEmailRequest.getFedId().trim())){
				response.put(UserConstants.STATUS_L, errorStatus);
				response.put(UserConstants.MESSAGE_L, "User token is invalid...suspicious activity observed");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("User token is invalid...suspicious activity observed");
				LOGGER.info("Time taken by addEmail() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}

			email = addEmailRequest.getEmail().trim();
			fedid = addEmailRequest.getFedId().trim();
			source = addEmailRequest.getProfileUpdateSource().trim();

			CheckUserExistsRequest checkRequest = new CheckUserExistsRequest();
			checkRequest.setEmail(email);
			checkRequest.setWithGlobalUsers("true");
			checkRequest.setApplicationName(source);
			Response checkUserExist = idmsCheckUserExists(checkRequest);
			LOGGER.info("idmsCheckUserExists reponse in addEmail()::" + objMapper.writeValueAsString(checkUserExist));

			org.json.simple.JSONObject checkUserJson = (org.json.simple.JSONObject) checkUserExist.getEntity();
			String messageUser = checkUserJson.get(UserConstants.MESSAGE_L).toString();
			if (!messageUser.equalsIgnoreCase(UserConstants.FALSE)) {
				if (200 != checkUserExist.getStatus()) {
					response.put(UserConstants.STATUS_L, errorStatus);
					response.put(UserConstants.MESSAGE_L, messageUser);
					LOGGER.error("Error while idmsCheckUserExists in addEmail() ->  " + messageUser);
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by addEmail() : " + elapsedTime);
					return Response.status(checkUserExist.getStatus()).entity(response).build();
				}
				if (200 == checkUserExist.getStatus()) {
					response.put(UserConstants.STATUS_L, errorStatus);
					response.put(UserConstants.MESSAGE_L, UserConstants.USER_EXISTS);
					LOGGER.error("User exists/registered in OpenAM");
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by addEmail() : " + elapsedTime);
					return Response.status(Response.Status.CONFLICT).entity(response).build();
				}
			}

			String ssoToken = null;
			try {
				ssoToken = getSSOToken();
			} catch (IOException ioExp) {
				LOGGER.error("Unable to get SSO Token" + ioExp.getMessage(),ioExp);
				ssoToken = "";
			}

			LOGGER.info("Start: checkUserExistsWithEmailMobile() of openam for fedid = " + fedid);
			String userExistsInOpenam = productService.checkUserExistsWithEmailMobile(
					UserConstants.CHINA_IDMS_TOKEN + ssoToken,
					"federationID eq " + "\"" + fedid + "\" or uid eq " + "\"" + fedid + "\"");
			LOGGER.info("End: checkUserExistsWithEmailMobile() of openam for fedid = " + fedid);
			productDocCtx = JsonPath.using(conf).parse(userExistsInOpenam);
			Integer resultCount = productDocCtx.read(JsonConstants.RESULT_COUNT);
			LOGGER.info("resultCount = " + resultCount);
			if (resultCount.intValue() == 1) {
				String addEmailString = "{" + "\"mail\": \"" + email + "\"}";
				LOGGER.info(
						"Start: updateUser() of openamservice to add email as dual indentifier for userId:" + fedid);
				productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + ssoToken, fedid, addEmailString);
				LOGGER.info("End: updateUser() of openamservice to add email as dual indentifier finished for userId:"
						+ fedid);
				String otp = sendEmail.generateOtp(fedid);
				LOGGER.info("sending mail notification to added email");
				sendEmail.sendOpenAmEmail(otp, EmailConstants.ADDEMAILUSERRECORD_OPT_TYPE, fedid, source);

				response.put(UserConstants.STATUS_L, successStatus);
				response.put(UserConstants.MESSAGE_L, UserConstants.ADD_EMAIL_PROFILE);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by addEmail() : " + elapsedTime);
				return Response.status(Response.Status.OK).entity(response).build();
			} 
			if (resultCount.intValue() > 1) {
				response.put(UserConstants.STATUS_L, errorStatus);
				response.put(UserConstants.MESSAGE_L, UserConstants.USER_MULTIPLE_EXIST + " federationID");
				LOGGER.error("Error in addEmail() is :: "+UserConstants.USER_MULTIPLE_EXIST + " federationID");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by addEmail() : " + elapsedTime);
				return Response.status(Response.Status.CONFLICT).entity(response).build();
			}			
			if (resultCount.intValue() == 0)  {
				response.put(UserConstants.STATUS_L, errorStatus);
				response.put(UserConstants.MESSAGE_L, "User not found with fedID : " + fedid);
				LOGGER.error("Error in addEmail() is -> Adding email terminated, no user exist with fedID " + fedid);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by addEmail() : " + elapsedTime);
				return Response.status(Response.Status.NOT_FOUND).entity(response).build();
			}
		} catch (Exception e) {
			LOGGER.error("Exception in addEmail() :: -> " + e.getMessage(),e);
			response.put(UserConstants.STATUS_L, errorStatus);
			response.put(UserConstants.MESSAGE_L, e.getMessage());
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by addEmail() : " + elapsedTime);
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(response).build();
		}
		response.put(UserConstants.STATUS_L, errorStatus);
		response.put(UserConstants.MESSAGE_L, "User not found with fedID : " + fedid);
		LOGGER.error("Error in addEmail() is -> user does not exist with fedID " + fedid);
		elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		LOGGER.info("Time taken by addEmail() : " + elapsedTime);
		return Response.status(Response.Status.NOT_FOUND).entity(response).build();
	}

	/**
	 * Verify Email from email link Updating loginid to user
	 */
	@SuppressWarnings("unchecked")
	@Override
	public Response addEmailToUser(AddEmailRequest addEmailRequest) {
		LOGGER.info("Entered addEmailToUser() -> Start");
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		ObjectMapper objMapper = new ObjectMapper();
		String email = null, fedid = null, source = null;
		String optType = null;
		JSONObject response = new JSONObject();
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();

		try {
			LOGGER.info("Parameter request -> " + objMapper.writeValueAsString(addEmailRequest));

			if (null == addEmailRequest.getFedId() || addEmailRequest.getFedId().isEmpty()) {
				response.put(UserConstants.STATUS_L, errorStatus);
				response.put(UserConstants.MESSAGE_L, UserConstants.FEDID_EMPTY);
				LOGGER.error("Error in addEmailToUser() is ::" + UserConstants.FEDID_EMPTY);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by addEmailToUser() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}
			if (null == addEmailRequest.getProfileUpdateSource()
					|| addEmailRequest.getProfileUpdateSource().isEmpty()) {
				response.put(UserConstants.STATUS_L, errorStatus);
				response.put(UserConstants.MESSAGE_L, UserConstants.PROFILE_UPDATE_SOURCE);
				LOGGER.error("Error in addEmailToUser() is ::" + UserConstants.PROFILE_UPDATE_SOURCE);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by addEmailToUser() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}
			if (null == addEmailRequest.getOperationType() || addEmailRequest.getOperationType().isEmpty()) {
				response.put(UserConstants.STATUS_L, errorStatus);
				response.put(UserConstants.MESSAGE_L, UserConstants.MANDATORY_ADD_EMAIL_OPT_TYPE);
				LOGGER.error("Error in addEmailToUser() is ::" + UserConstants.MANDATORY_ADD_EMAIL_OPT_TYPE);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by addEmailToUser() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}

			fedid = addEmailRequest.getFedId().trim();
			source = addEmailRequest.getProfileUpdateSource().trim();
			optType = addEmailRequest.getOperationType().trim();

			String ssoToken = null;
			try {
				ssoToken = getSSOToken();
			} catch (IOException ioExp) {
				LOGGER.error("Unable to get SSO Token" + ioExp.getMessage(),ioExp);
				ssoToken = "";
			}

			LOGGER.info("Start: checkUserExistsWithEmailMobile() of openam for fedid = " + fedid);
			String userExistsInOpenam = productService.checkUserExistsWithEmailMobile(
					UserConstants.CHINA_IDMS_TOKEN + ssoToken,
					"federationID eq " + "\"" + fedid + "\" or uid eq " + "\"" + fedid + "\"");
			LOGGER.info("End: checkUserExistsWithEmailMobile() of openam for fedid = " + fedid);
			DocumentContext productDocCtx = JsonPath.using(conf).parse(userExistsInOpenam);
			Integer resultCount = productDocCtx.read(JsonConstants.RESULT_COUNT);
			LOGGER.info("resultCount = " + resultCount);
			
			if (optType.equalsIgnoreCase(UserConstants.ADD_EMAIL_USER_RECORD))
				email = productDocCtx.read("$.result[0].mail[0]");
			LOGGER.info("email in openam = " + email);

			if (resultCount.intValue() == 1) {
				String addEmailString = "{" + "\"mail\": \"" + email + "\",\"loginid\": \"" + email + "\"" + "}";
				LOGGER.info(
						"Start: updateUser() of openamservice to add email as dual indentifier for userId:" + fedid);
				productService.updateUser(UserConstants.CHINA_IDMS_TOKEN + ssoToken, fedid, addEmailString);
				LOGGER.info("End: updateUser() of openamservice to add email as dual indentifier finished for userId:"
						+ fedid);

				response.put(UserConstants.STATUS_L, successStatus);
				response.put(UserConstants.MESSAGE_L, UserConstants.ADD_EMAIL_PROFILE_SUCCESS);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by addEmailToUser() : " + elapsedTime);
				return Response.status(Response.Status.OK).entity(response).build();
			}
			if (resultCount.intValue() > 1) {
				response.put(UserConstants.STATUS_L, errorStatus);
				response.put(UserConstants.MESSAGE_L, UserConstants.USER_MULTIPLE_EXIST+" federationID");
				LOGGER.error("Error in addEmailToUser is :: "+UserConstants.USER_MULTIPLE_EXIST+" federationID");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by addEmailToUser() : " + elapsedTime);
				return Response.status(Response.Status.CONFLICT).entity(response).build();
			}
			if (resultCount.intValue() == 0) {
				response.put(UserConstants.STATUS_L, errorStatus);
				response.put(UserConstants.MESSAGE_L, "User not found with fedID : " + fedid);
				LOGGER.error("Error in addEmailToUser is :: "+"User not found with fedID : " + fedid);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by addEmailToUser() : " + elapsedTime);
				return Response.status(Response.Status.CONFLICT).entity(response).build();
			}
		} catch (Exception e) {
			LOGGER.error("Exception in addEmailToUser() :: -> " + e.getMessage(),e);
			response.put(UserConstants.STATUS_L, errorStatus);
			response.put(UserConstants.MESSAGE_L, e.getMessage());
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by addEmailToUser() : " + elapsedTime);
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(response).build();
		}
		response.put(UserConstants.STATUS_L, errorStatus);
		response.put(UserConstants.MESSAGE_L, "User not found with fedID : " + fedid);
		LOGGER.error(
				"Error in addEmailToUser() is -> Adding email terminated, no user exist with fedID " + fedid);
		elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		LOGGER.info("Time taken by addEmailToUser() : " + elapsedTime);
		return Response.status(Response.Status.NOT_FOUND).entity(response).build();
	}

	@SuppressWarnings("unchecked")
	@Override
	public Response deleteMobile(SendOTPRequest deleteRequest) {
		LOGGER.info("Entered deleteMobile() -> Start");
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		ObjectMapper objMapper = new ObjectMapper();
		String mobile = null;
		JSONObject response = new JSONObject();
		try {
			LOGGER.info("Parameter request -> " + objMapper.writeValueAsString(deleteRequest));
			if (null == deleteRequest.getMobile() || deleteRequest.getMobile().isEmpty()) {
				response.put(UserConstants.STATUS_L, errorStatus);
				response.put(UserConstants.MESSAGE_L, UserConstants.MOBILE_EMPTY);
				LOGGER.error("Error in deleteMobile() is ::" + UserConstants.MOBILE_EMPTY);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by deleteMobile() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}
			if (null != deleteRequest.getMobile() && !deleteRequest.getMobile().isEmpty()) {
				mobile = ChinaIdmsUtil.mobileTransformation(deleteRequest.getMobile().trim());
				if (!ChinaIdmsUtil.mobileValidator(mobile)) {
					response.put(UserConstants.STATUS_L, errorStatus);
					response.put(UserConstants.MESSAGE_L, "Mobile validation failed.");
					LOGGER.error("Error in deleteMobile() is :: Mobile validation failed.");
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by deleteMobile() : " + elapsedTime);
					return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
				}
			}

			LOGGER.info("Start: deleteMobileOTPDetails() of OpenDjService for mobile=" + mobile);
			Response otpDetails = openDJService.deleteMobileOTPDetails("application/json", djUserName, djUserPwd,
					mobile);
			LOGGER.info("End: deleteMobileOTPDetails() of OpenDjService finished for mobile=" + mobile);
			LOGGER.info("Response code from OpenDJ for get call: " + otpDetails.getStatus());

			if (null != otpDetails && 200 == otpDetails.getStatus()) {
				LOGGER.info("Mobile record deleted successfully from OpenDJ : " + mobile);
				response.put(UserConstants.STATUS_L, successStatus);
				response.put(UserConstants.MESSAGE_L, UserConstants.DELETE_MOBILE_IDENTIFIER);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by deleteMobile() : " + elapsedTime);
				return Response.status(Response.Status.OK).entity(response).build();
			} else if (null != otpDetails && 404 == otpDetails.getStatus()) {
				LOGGER.info("No mobile record found in OpenDJ : " + mobile);
				response.put(UserConstants.STATUS_L, errorStatus);
				response.put(UserConstants.MESSAGE_L, "Mobile record not found");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by deleteMobile() : " + elapsedTime);
				return Response.status(Response.Status.NOT_FOUND).entity(response).build();
			} else {
				LOGGER.info("Problem in deleting record from OpenDJ" + otpDetails.getStatus());
			}
		} catch (Exception e) {
			LOGGER.error("Exception in deleteMobile() :: -> " + e.getMessage(),e);
		}

		response.put(UserConstants.STATUS_L, errorStatus);
		response.put(UserConstants.MESSAGE_L, UserConstants.SERVER_ERROR);
		elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		LOGGER.info("Time taken by deleteMobile() : " + elapsedTime);
		return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(response).build();
	}
	
	@Override
	public Response getUserDetailByApplication(String authorizationToken, String type,
			UserDetailByApplicationRequest userDetailByApplicationRequest) {
		LOGGER.info("Entered getUserDetailByApplication() -> Start");
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		ObjectMapper objMapper = new ObjectMapper();
		String mobileNum = null, appNameInOpenDJ = null;
		String loginId = null, fieldType = null, userExists = null, iPlanetDirectoryKey = null;
		Integer resultCount = 0;
		ArrayList<String> varList = new ArrayList<String>();
		Response applicationDetails = null;
		Configuration confg = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		
		GetUserByApplicationResponse userResponse = new GetUserByApplicationResponse();
		DocumentContext productDocCtx = null;
		try {
			LOGGER.info("Parameter request -> " + objMapper.writeValueAsString(userDetailByApplicationRequest));

			if(null == userDetailByApplicationRequest){
				userResponse.setStatus(errorStatus);
				userResponse.setMessage("Request body is null or empty");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error is -> Request body is null or empty");
				LOGGER.info("Time taken by getUserDetailByApplication() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
			}
			if (null == authorizationToken || authorizationToken.isEmpty()) {
				userResponse.setStatus(errorStatus);
				userResponse.setMessage(UserConstants.ADMIN_TOKEN_MANDATORY);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error is " + userResponse.getMessage());
				LOGGER.info("Time taken by getUserDetailByApplication() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
			}
			if (null == userDetailByApplicationRequest.getAppHash()
					|| userDetailByApplicationRequest.getAppHash().isEmpty()) {
				userResponse.setStatus(errorStatus);
				userResponse.setMessage("Please provide Hashcode");
				LOGGER.error("Mandatory check: Please provide Hashcode");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by getUserDetailByApplication() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
			}
			if ((null == userDetailByApplicationRequest.getEmail()
					|| userDetailByApplicationRequest.getEmail().isEmpty())
					&& (null == userDetailByApplicationRequest.getMobile()
							|| userDetailByApplicationRequest.getMobile().isEmpty())
					&& (null == userDetailByApplicationRequest.getIdmsFederatedId()
							|| userDetailByApplicationRequest.getIdmsFederatedId().isEmpty())) {
				userResponse.setStatus(errorStatus);
				userResponse.setMessage("Either one Email/ Mobile/ FederatedId should have value");
				LOGGER.error("Mandatory check: Either one Email/ Mobile/ FederatedId should have value");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by getUserDetailByApplication() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
			}

			if (null != userDetailByApplicationRequest.getEmail()
					&& !userDetailByApplicationRequest.getEmail().isEmpty()) {
				varList.add("true");
			}
			if (null != userDetailByApplicationRequest.getMobile()
					&& !userDetailByApplicationRequest.getMobile().isEmpty()) {
				varList.add("true");
			}
			if (null != userDetailByApplicationRequest.getIdmsFederatedId()
					&& !userDetailByApplicationRequest.getIdmsFederatedId().isEmpty()) {
				varList.add("true");
			}
			LOGGER.info("Parameters size(email,mobile,idmsFedid): " + varList.size());

			if (varList.size() > 1) {
				userResponse.setStatus(errorStatus);
				userResponse.setMessage("Only one identifier is allowed");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by getUserDetailByApplication() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
			}
			if (null != userDetailByApplicationRequest.getEmail()
					&& !userDetailByApplicationRequest.getEmail().isEmpty()) {
				if (!emailValidator.validate(userDetailByApplicationRequest.getEmail())) {
					userResponse.setStatus(errorStatus);
					userResponse.setMessage("Email validation failed");
					LOGGER.error("Error in getUserDetailByApplication() is :: Email validation failed.");
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by getUserDetailByApplication() : " + elapsedTime);
					return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
				}
			}
			if (null != userDetailByApplicationRequest.getMobile()
					&& !userDetailByApplicationRequest.getMobile().isEmpty()) {
				mobileNum = ChinaIdmsUtil.mobileTransformation(userDetailByApplicationRequest.getMobile());
				if (!ChinaIdmsUtil.mobileValidator(mobileNum)) {
					userResponse.setStatus(errorStatus);
					userResponse.setMessage("Mobile validation failed");
					LOGGER.error("Error in getUserDetailByApplication is :: Mobile validation failed.");
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by getUserDetailByApplication() : " + elapsedTime);
					return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
				}
			}

			if (!getTechnicalUserDetails(authorizationToken)) {
				userResponse.setStatus(errorStatus);
				userResponse.setMessage("Unauthorized or session expired");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.error("Error is " + userResponse.getMessage());
				LOGGER.info("Time taken by getUserDetailByApplication() : " + elapsedTime);
				return Response.status(Response.Status.UNAUTHORIZED).entity(userResponse).build();
			}
			if (null != userDetailByApplicationRequest.getAppHash()
					&& !userDetailByApplicationRequest.getAppHash().isEmpty()) {
				String appHash = userDetailByApplicationRequest.getAppHash().trim();
				LOGGER.info("Start: getAppInfo() of OpenDjService for appHash="
						+ userDetailByApplicationRequest.getAppHash().trim());
				applicationDetails = openDJService.getAppInfo(djUserName, djUserPwd,
						"_Application_Hash eq " + "\"" + appHash + "\"");
				LOGGER.info("End: getAppInfo() of OpenDjService finished for appHash="
						+ userDetailByApplicationRequest.getAppHash().trim());
				LOGGER.info("Response code from OpenDJ for get call: " + applicationDetails.getStatus());
			}

			if (null != applicationDetails && 200 == applicationDetails.getStatus()) {
				productDocCtx = JsonPath.using(confg)
						.parse(IOUtils.toString((InputStream) applicationDetails.getEntity()));
				resultCount = productDocCtx.read("$.resultCount");
				LOGGER.info("resultCount=" + resultCount);
			}
			if (resultCount.intValue() == 1) {
				appNameInOpenDJ = productDocCtx.read(JsonConstants.APP_ID);
				LOGGER.info("App name/id = " + appNameInOpenDJ);
			}
			if (resultCount.intValue() == 0) {
				userResponse.setStatus(errorStatus);
				userResponse.setMessage("Invalid Application Hash Code");
				LOGGER.error("Error is :: Invalid Application Hash Code.");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by getUserDetailByApplication() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
			}

			if (null != userDetailByApplicationRequest.getEmail()
					&& !userDetailByApplicationRequest.getEmail().isEmpty()) {
				loginId = userDetailByApplicationRequest.getEmail().trim();
				fieldType = "email";
			} else if (null != userDetailByApplicationRequest.getMobile()
					&& !userDetailByApplicationRequest.getMobile().isEmpty()) {
				loginId = mobileNum;
				fieldType = "mobile";
			} else if (null != userDetailByApplicationRequest.getIdmsFederatedId()
					&& !userDetailByApplicationRequest.getIdmsFederatedId().isEmpty()) {
				loginId = userDetailByApplicationRequest.getIdmsFederatedId().trim();
				fieldType = "idmsFederatedId";
			}
			LOGGER.info("loginId : " + loginId + " ,fieldType = " + fieldType);
			// checking user and getting info
			try {
				iPlanetDirectoryKey = getSSOToken();
			} //// No Exception handling
			catch (IOException ioExp) {
				LOGGER.error("Unable to get SSO Token " + ioExp.getMessage(), ioExp);
			}

			if (fieldType.equalsIgnoreCase("email") || fieldType.equalsIgnoreCase("mobile")) {
				LOGGER.info("Start: checkUserExistsWithEmailMobile() of openam for loginId=" + loginId);
				userExists = productService.checkUserExistsWithEmailMobile(
						UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey,
						"mail eq " + "\"" + URLEncoder.encode(URLDecoder.decode(loginId, "UTF-8"), "UTF-8")
								+ "\" or mobile_reg eq " + "\""
								+ URLEncoder.encode(URLDecoder.decode(loginId, "UTF-8"), "UTF-8") + "\"");
				LOGGER.info("End: checkUserExistsWithEmailMobile() of openam finished for loginId=" + loginId);
			}
			if (fieldType.equalsIgnoreCase("idmsFederatedId")) {
				LOGGER.info("Start: checkUserExistsWithEmailMobile() of openam for federationId=" + loginId);
				userExists = productService.checkUserExistsWithEmailMobile(
						UserConstants.CHINA_IDMS_TOKEN + iPlanetDirectoryKey, "federationID  eq " + "\""
								+ URLEncoder.encode(URLDecoder.decode(loginId, "UTF-8"), "UTF-8") + "\"");
				LOGGER.info("End: checkUserExistsWithEmailMobile() of openam finished for federationId=" + loginId);
			}
			productDocCtx = null; resultCount =0;
			productDocCtx = JsonPath.using(confg).parse(userExists);
			resultCount = productDocCtx.read("$.resultCount");
			LOGGER.info("User resultCount=" + resultCount);

			if (resultCount.intValue() > 1) {
				userResponse.setStatus(errorStatus);
				userResponse.setMessage("Critical Error: Multiple user record exists");
				LOGGER.error("Error is :: " + UserConstants.USER_MULTIPLE_EXIST);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by getUserDetailByApplication() : " + elapsedTime);
				return Response.status(Response.Status.CONFLICT).entity(userResponse).build();
			}
			if (resultCount.intValue() == 0) {
				userResponse.setStatus(errorStatus);
				userResponse.setMessage("User not found based on "+fieldType);
				LOGGER.error("Error is :: " + "User not found based on "+fieldType);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by getUserDetailByApplication() : " + elapsedTime);
				return Response.status(Response.Status.NOT_FOUND).entity(userResponse).build();
			}
			if (resultCount.intValue() == 1) {
				boolean appFound = false;
				String appAIL = productDocCtx.read("$.result[0].IDMSAIL_Applications_c[0]");
				if(null != appAIL && !appAIL.isEmpty()){
					List<String> appAILList = Arrays.asList(appAIL.split(","));
					for(String appName:appAILList){
						if(appName.toLowerCase().equalsIgnoreCase(appNameInOpenDJ.toLowerCase())){
							LOGGER.info("application is found in User AIL");
							appFound = true;
							break;
						}
					}
				}
							
				LOGGER.info("is app associated with user = "+appFound);
				
				if(!appFound){
					userResponse.setStatus(errorStatus);
					userResponse.setMessage("User not found based on AIL");
					LOGGER.error("Error is :: User not found based on AIL");
					elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
					LOGGER.info("Time taken by getUserDetailByApplication() : " + elapsedTime);
					return Response.status(Response.Status.BAD_REQUEST).entity(userResponse).build();
				}
				
				String userStatusInChina = null;
				if (Boolean.valueOf(productDocCtx.read("$.result[0].isActivated[0]"))) {
					userStatusInChina = UserConstants.USER_ACTIVE;
				} else {
					userStatusInChina = UserConstants.USER_INACTIVE;
				}
				LOGGER.info("userStatusInChina = "+userStatusInChina);
				GetUserRecordResponse userRecordResponse = new GetUserRecordResponse();
				ParseValuesByOauthHomeWorkContextDto userInfoMapper = new ParseValuesByOauthHomeWorkContextDto();
				userInfoMapper.parseValuesForGetUserByApplication(userRecordResponse, productDocCtx);
				
				userResponse.setStatus(successStatus);
				userResponse.setMessage(userStatusInChina);
				userResponse.setIDMSUserRecord(userRecordResponse);
				LOGGER.info("Response given");
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by getUserDetailByApplication() : " + elapsedTime);
				return Response.status(Response.Status.OK).entity(userResponse).build();
			}
		} catch (Exception e) {
			LOGGER.error("Error in getUserDetailByApplication is ::" + e.getMessage(),e);
			userResponse.setStatus(errorStatus);
			userResponse.setMessage("Internal Server Error");
			LOGGER.error("Error in getUserDetailByApplication is :: Internal Server Error.");
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by getUserDetailByApplication() : " + elapsedTime);
			return Response.status(Response.Status.INTERNAL_SERVER_ERROR).entity(userResponse).build();
		}

		userResponse.setStatus(errorStatus);
		userResponse.setMessage("User Not Found");
		LOGGER.error("Error in getUserDetailByApplication is :: User Not Found.");
		elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		LOGGER.info("Time taken by getUserDetailByApplication() : " + elapsedTime);
		return Response.status(Response.Status.NOT_FOUND).entity(userResponse).build();
	}
	
	@SuppressWarnings("unchecked")
	@Override
	public Response sendDemoMail(CheckUserIdentityRequest emailRequest) {
		LOGGER.info("Entered sendDemoMail() -> Start");
		long startTime = UserConstants.TIME_IN_MILLI_SECONDS;
		long elapsedTime;
		JSONObject response = new JSONObject();
		LinkedHashMap<String, String> lhm = new LinkedHashMap<String, String>();
		ObjectMapper objMapper = new ObjectMapper();
		boolean status = false;

		try {
			LOGGER.info("emailIds = " + objMapper.writeValueAsString(emailRequest));

			if (null == emailRequest.getEmailOrMobile() || emailRequest.getEmailOrMobile().isEmpty()
					|| emailRequest.getEmailOrMobile().split(";").length < 1) {
				response.put(UserConstants.STATUS_L, errorStatus);
				response.put(UserConstants.MESSAGE_L, UserConstants.EMAIL_EMPTY);
				LOGGER.error("Error in addMobile() is ::" + UserConstants.EMAIL_EMPTY);
				elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
				LOGGER.info("Time taken by sendDemoMail() : " + elapsedTime);
				return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
			}
			String[] listEmailIds = emailRequest.getEmailOrMobile().split(";");
			LOGGER.info("number of email ids = " + listEmailIds.length);
			// changing user mailid to testmail
			for (int i = 0; i < listEmailIds.length; i++) {
				if (null != listEmailIds[i] && !listEmailIds[i].isEmpty()) {
					String orgMail = listEmailIds[i].trim();
					if(orgMail.contains("@")){
						orgMail = "testmail"+orgMail.substring(orgMail.indexOf('@'));
					}
					if (emailValidator.validate(orgMail)) {
						lhm.put(orgMail, "Email validation passed. Sending email...");
						try {
							status = sendEmail.sendDemoEmail(orgMail);
						} catch (Exception e) {
							LOGGER.error("Exception as == " + e.getMessage());
							lhm.put(orgMail,lhm.get(orgMail) + "Sending email failed. " + e.getMessage());
						}
						if (status)
							lhm.put(orgMail,lhm.get(orgMail) + "Sending email finished.");
					} else
						lhm.put(orgMail, "Email validation failed.");
				}
			}
			return Response.status(Response.Status.ACCEPTED).entity(lhm).build();
		} catch (JsonProcessingException e) {
			response.put(UserConstants.STATUS_L, errorStatus);
			response.put(UserConstants.MESSAGE_L, e.getMessage());
			LOGGER.error("JsonProcessingException in sendDemoMail() ::" + e.getMessage(), e);
			elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
			LOGGER.info("Time taken by sendDemoMail() : " + elapsedTime);
			return Response.status(Response.Status.BAD_REQUEST).entity(response).build();
		}
	}
    
	private Response handleUIMSError(Status statusCode, String message){
		UIMSResponse response= new UIMSResponse();
		response.setHasErrors("true");
		response.setStatus("Error");
		UIMSStatusInfo userResponse=new UIMSStatusInfo();
		userResponse.setStatusCode(String.valueOf(statusCode.getStatusCode()));
		userResponse.setMessage(message);
		response.setResults(userResponse);
		return Response.status(statusCode).entity(response).build();
	}
	public void setEMAIL_TEMPLATE_DIR(String eMAIL_TEMPLATE_DIR) {
		EMAIL_TEMPLATE_DIR = eMAIL_TEMPLATE_DIR;
	}

	public void setLOGIN_ERROR(String lOGIN_ERROR) {
		LOGIN_ERROR = lOGIN_ERROR;
	}

	public void setAuthCsvPath(String authCsvPath) {
		this.authCsvPath = authCsvPath;
	}

	public void setRegistrationCsvPath(String registrationCsvPath) {
		this.registrationCsvPath = registrationCsvPath;
	}

	public void setAdminUserName(String adminUserName) {
		this.adminUserName = adminUserName;
	}

	public void setAdminPassword(String adminPassword) {
		this.adminPassword = adminPassword;
	}

	public void setIfwClientId(String ifwClientId) {
		this.ifwClientId = ifwClientId;
	}

	public void setIfwClientSecret(String ifwClientSecret) {
		this.ifwClientSecret = ifwClientSecret;
	}

	public void setSalesForceClientId(String salesForceClientId) {
		this.salesForceClientId = salesForceClientId;
	}

	public void setSalesForceClientSecret(String salesForceClientSecret) {
		this.salesForceClientSecret = salesForceClientSecret;
	}

	public void setSalesForceUserName(String salesForceUserName) {
		this.salesForceUserName = salesForceUserName;
	}

	public void setSalesForcePassword(String salesForcePassword) {
		this.salesForcePassword = salesForcePassword;
	}

	public void setHa_mode(String ha_mode) {
		this.ha_mode = ha_mode;
	}

	public void setFromUserName(String fromUserName) {
		this.fromUserName = fromUserName;
	}

	public void setGoDitalToken(String goDitalToken) {
		this.goDitalToken = goDitalToken;
	}

	public void setGoDigitalValue(String goDigitalValue) {
		this.goDigitalValue = goDigitalValue;
	}

	public void setUimsClientId(String uimsClientId) {
		this.uimsClientId = uimsClientId;
	}

	public void setUimsClientSecret(String uimsClientSecret) {
		this.uimsClientSecret = uimsClientSecret;
	}

	public void setRedirectUri(String redirectUri) {
		this.redirectUri = redirectUri;
	}

	public void setPrefixStartUrl(String prefixStartUrl) {
		this.prefixStartUrl = prefixStartUrl;
	}

	public void setPrefixIdentityUrl(String prefixIdentityUrl) {
		this.prefixIdentityUrl = prefixIdentityUrl;
	}

	public void setRegisterPRMUserIdp(String registerPRMUserIdp) {
		this.registerPRMUserIdp = registerPRMUserIdp;
	}

	public void setOtpvalidationtimeinminute(String otpvalidationtimeinminute) {
		this.otpvalidationtimeinminute = otpvalidationtimeinminute;
	}

	public void setDjUserName(String djUserName) {
		this.djUserName = djUserName;
	}

	public void setDjUserPwd(String djUserPwd) {
		this.djUserPwd = djUserPwd;
	}

	public void setCALLER_FID(String cALLER_FID) {
		CALLER_FID = cALLER_FID;
	}

	public String getCALLER_FID() {
		return CALLER_FID;
	}

	public void setSendOTPOverEmail(String sendOTPOverEmail) {
		this.sendOTPOverEmail = sendOTPOverEmail;
	}
    
    public void setEnableTestMailDomain(String enableTestMailDomain) {
		this.enableTestMailDomain = enableTestMailDomain;
	}

	public String getLOGIN_ERROR() {
		return LOGIN_ERROR;
	}

	public String getEMAIL_TEMPLATE_DIR() {
		return EMAIL_TEMPLATE_DIR;
	}

	public String getAuthCsvPath() {
		return authCsvPath;
	}

	public String getRegistrationCsvPath() {
		return registrationCsvPath;
	}

	public String getAdminUserName() {
		return adminUserName;
	}

	public String getAdminPassword() {
		return adminPassword;
	}

	public String getIfwClientId() {
		return ifwClientId;
	}

	public String getIfwClientSecret() {
		return ifwClientSecret;
	}

	public String getSalesForceClientId() {
		return salesForceClientId;
	}

	public String getSalesForceClientSecret() {
		return salesForceClientSecret;
	}

	public String getSalesForceUserName() {
		return salesForceUserName;
	}

	public String getSalesForcePassword() {
		return salesForcePassword;
	}

	public String getHa_mode() {
		return ha_mode;
	}

	public String getFromUserName() {
		return fromUserName;
	}

	public String getGoDitalToken() {
		return goDitalToken;
	}

	public String getGoDigitalValue() {
		return goDigitalValue;
	}

	public String getUimsClientId() {
		return uimsClientId;
	}

	public String getUimsClientSecret() {
		return uimsClientSecret;
	}

	public String getRedirectUri() {
		return redirectUri;
	}

	public String getPrefixStartUrl() {
		return prefixStartUrl;
	}

	public String getDjUserName() {
		return djUserName;
	}

	public String getDjUserPwd() {
		return djUserPwd;
	}

	public String getEnableTestMailDomain() {
		return enableTestMailDomain;
	}

	public String getPrefixIdentityUrl() {
		return prefixIdentityUrl;
	}

	public String getRegisterPRMUserIdp() {
		return registerPRMUserIdp;
	}

	public String getOtpvalidationtimeinminute() {
		return otpvalidationtimeinminute;
	}

	public String getSendOTPOverEmail() {
		return sendOTPOverEmail;
	}
	
	public String getDefaultUserNameFormat() {
		return defaultUserNameFormat;
	}


	public void setDefaultUserNameFormat(String defaultUserNameFormat) {
		this.defaultUserNameFormat = defaultUserNameFormat;
	}

	public String getMaintenanceModeGlobal() {
		return maintenanceModeGlobal;
	}

	public void setMaintenanceModeGlobal(String maintenanceModeGlobal) {
		this.maintenanceModeGlobal = maintenanceModeGlobal;
	}
	
	
}

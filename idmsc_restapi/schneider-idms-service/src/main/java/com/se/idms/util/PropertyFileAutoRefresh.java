package com.se.idms.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import javax.inject.Inject;

import org.hibernate.validator.internal.constraintvalidators.LengthValidator;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.idms.service.SendEmail;
import com.idms.service.UIMSCompanyManagerSoapService;
import com.idms.service.UIMSUserManagerSoapService;
import com.idms.service.UimsSetPasswordSoapService;
import com.idms.service.UserService;
import com.idms.service.UserServiceImpl;
import com.idms.service.digital.GoDigitalUserService;
import com.idms.service.digital.impl.GoDigitalUserServiceImpl;
import com.schneider.idms.salesforce.service.SaleforceServiceImpl;
import com.schneider.idms.salesforce.service.SalesforceSyncServiceImpl;
import com.schneider.idms.service.impl.CreateUserServiceImpl;
import com.schneider.idms.service.impl.IdmsCommonServiceImpl;
import com.schneider.uims.service.DirectUIMSUserManagerSoapService;
import com.se.idms.cache.validate.impl.FieldsMappingValidatorImpl;
import com.se.idms.cache.validate.impl.LengthValidatorImpl;
import com.se.idms.cache.validate.impl.MandatoryValidatorImpl;
import com.se.idms.cache.validate.impl.MultiPickListValidatorImpl;
import com.se.idms.cache.validate.impl.PickListValidatorImpl;
import static com.se.idms.util.UserConstants.APP_ROOT_PLACE_HOLDER;;

@Service("propertyFileAutoRefresh")
public class PropertyFileAutoRefresh {

	private final static PropertyFileAutoRefresh INSTANCE = new PropertyFileAutoRefresh();
	

	/**
	 * Logger instance.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(PropertyFileAutoRefresh.class);


	@Value("${app.properties.file}")
	private String PROPERTY_FILE;

	@Value("${idms.env}")
	private String IDMS_DEPLOY_ENV;

	@Inject
	private UserService userService;
	@Inject
	private GoDigitalUserService goDigitalUserService;
	@Inject
	private SendEmail emailService;
	@Inject
	UIMSCompanyManagerSoapService uimsCompManagSoapService;
	@Inject
	UimsSetPasswordSoapService uimsSetPasswordSoapService;
	@Inject
	UIMSUserManagerSoapService uimsUserManagSoapService;
	@Inject
	SaleforceServiceImpl saleforceService;
	@Inject
	SalesforceSyncServiceImpl saleforceSynService;
	@Inject
	CreateUserServiceImpl createUserService;
	@Inject
	IdmsCommonServiceImpl commonService;
	@Inject
	DirectUIMSUserManagerSoapService directUIMSUserManagerSoapService;
	@Inject
	SamlAssertionTokenGenerator samlAssertionTokenService;
	@Inject
	PickListValidatorImpl pickListValidator ;
	@Inject
	MultiPickListValidatorImpl multiPickListValidator;
	@Inject
	MandatoryValidatorImpl mandatoryValidator;
	@Inject
	LengthValidatorImpl legthValidator;
	@Inject
	FieldsMappingValidatorImpl fieldsMappingValidator;
	
	private String CALLER_FID;
	
	private String LOGIN_ERROR;
	
	private String EMAIL_TEMPLATE_DIR;
	
	private String authCsvPath;
	
	private String registrationCsvPath;

	private String adminUserName;

	private String adminPassword;

	private String ifwClientId;
	
	private String ifwClientSecret;
	
	private String salesForceClientId;
	
	private String salesForceClientSecret;
	
	private String salesForceUserName;
	
	private String salesForcePassword;
	
	private String ha_mode;

	private String fromUserName;
	
	private String goDitalToken;
	
	private String goDigitalValue;
	
	private String uimsClientId;
	
	private String uimsClientSecret;

	private String redirectUri;
	
	private String prefixStartUrl;
	
	private String prefixIdentityUrl;
	
	private String registerPRMUserIdp;

	private String otpvalidationtimeinminute;
	
	private String djUserName;
	
	private String djUserPwd;
	
	private String sendOTPOverEmail;
	
	private String enableTestMailDomain;
		
	private String supportUser;
	
	private String hotpEmailVerificationURL;
	
	private String IDMS_USER_REST_PASSWORD_EMAILTEMPLATE_CN;
	
	private String IDMS_USER_REST_PASSWORD_EMAILTEMPLATE_EN;
	
	private String IDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_CN;

	private String IDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_EN;

	private String IDMS_USER_UPDATE_EMAILTEMPLATE_CN;
	
	private String IDMS_USER_UPDATE_EMAILTEMPLATE_EN;
	
	private String IDMS_USER_DEFAULT_EMAILTEMPLATE_CN;
	
	private String IDMS_USER_DEFAULT_EMAILTEMPLATE_EN;	
	
	private String IDMS_SEND_INVITATION_EMAILTEMPLATE_EN;
	
	private String IDMS_SEND_INVITATION_EMAILTEMPLATE_CN;
	
	private String IDMS_USER_ADD_EMAILTEMPLATE_CN;
	
	private String IDMS_USER_ADD_EMAILTEMPLATE_EN;
	
	private String sftokentimeinminute;
	
	private String directApiSecretToken;
		
	private String samlAssertionSigningKeystore;
	
	private String samlAssertionKeystorePassword;
		
	private String samlAssertionKeyPassword;
	
	private String samlAssertionSigningCert;
		
	private String samlAssertionSigningAlgo;
	
	private String IDMS_FIELDSPICKLIST_PROPERTIES_PATH;
		
	private String IDMS_FIELDSMULTI_PICKLIST_PROPERTIES_PATH;
	
	private String IDMS_FIELDSMANDATORY_PROPERTIES_PATH;
	
	private String IDMS_FIELDSLENGTH_PROPERTIES_PATH;

	private String IDMS_FIELDSMAPPING_PROPERTIES_PATH;
	
	public static PropertyFileAutoRefresh getInstance() {
		return INSTANCE;
	}

	private static Properties configuration = new Properties();

	private static Properties getConfiguration() {
		return configuration;
	}

	public void initilize(final String file) {
		InputStream in = null;
		try {
			LOGGER.info("initilize(final String file) start");
			in = new FileInputStream(new File(file));
			configuration.load(in);
			LOGGER.info("PROPERTY_FILE"+PROPERTY_FILE);
			//configuration.forEach((key, value) -> LOGGER.info(key + " : " + value));
			//String DEFAULT_APP_ROOT_LOCATION="${idmsc.app_root.location}";
			String appRootLocation=System.getProperty("idmsc.app_root.location");
			LOGGER.info("System app root property:"+appRootLocation);
			configuration.forEach((type, value) -> {
			String strValue=(String)value;
			   if(strValue.contains(APP_ROOT_PLACE_HOLDER)){
				   //configuration.setProperty((String)type, strValue.replace(DEFAULT_APP_ROOT_LOCATION, appRootLocation));
				   strValue=strValue.replace(APP_ROOT_PLACE_HOLDER, appRootLocation);
				   if(IDMS_DEPLOY_ENV.equalsIgnoreCase("DEV")){
				   //configuration.setProperty((String)type, strValue.replaceAll("/", "\\\\"));
				    strValue=strValue.replaceAll("/", "\\\\");
				   }
				   configuration.setProperty((String)type, strValue);
			   }
			  });
			configuration.setProperty("app.properties.file",configuration.getProperty("app.properties.file").replace("${idms.env}", IDMS_DEPLOY_ENV));
			LOGGER.info("\nAfter conversion:");
			configuration.forEach((key, value) -> LOGGER.info(key + " : " + value));
			
			initializeProperties();
			
			// Update all properties file entries
			if(!((UserServiceImpl) userService).getCALLER_FID().equals(CALLER_FID) && CALLER_FID!=null && !CALLER_FID.isEmpty())
				((UserServiceImpl) userService).setCALLER_FID(CALLER_FID);
			
			if(!((UserServiceImpl) userService).getLOGIN_ERROR().equals(LOGIN_ERROR) && LOGIN_ERROR!=null && !LOGIN_ERROR.isEmpty())
			((UserServiceImpl) userService).setLOGIN_ERROR(LOGIN_ERROR);
			
			LOGGER.info("Userservice email template dir:##"+((UserServiceImpl) userService).getEMAIL_TEMPLATE_DIR());
			LOGGER.info("Config  email template dir:##"+EMAIL_TEMPLATE_DIR);
			
			if(!((UserServiceImpl) userService).getEMAIL_TEMPLATE_DIR().equals(EMAIL_TEMPLATE_DIR) && EMAIL_TEMPLATE_DIR!=null && !EMAIL_TEMPLATE_DIR.isEmpty())
			((UserServiceImpl) userService).setEMAIL_TEMPLATE_DIR(EMAIL_TEMPLATE_DIR);
			
			if(!((UserServiceImpl) userService).getAuthCsvPath().equals(authCsvPath) && authCsvPath!=null && !authCsvPath.isEmpty())
			((UserServiceImpl) userService).setAuthCsvPath(authCsvPath);
			
			if(!((UserServiceImpl) userService).getRegistrationCsvPath().equals(registrationCsvPath) && registrationCsvPath!=null && !registrationCsvPath.isEmpty())
			((UserServiceImpl) userService).setRegistrationCsvPath(registrationCsvPath);
			
			if(!((UserServiceImpl) userService).getAdminUserName().equals(adminUserName) && adminUserName!=null && !adminUserName.isEmpty())
			((UserServiceImpl) userService).setAdminUserName(adminUserName);
			
			if(!((UserServiceImpl) userService).getAdminPassword().equals(adminPassword) && adminPassword!=null && !adminPassword.isEmpty())
			((UserServiceImpl) userService).setAdminPassword(adminPassword);
			
			if(!((UserServiceImpl) userService).getIfwClientId().equals(ifwClientId) && ifwClientId!=null && !ifwClientId.isEmpty())
			((UserServiceImpl) userService).setIfwClientId(ifwClientId);
			
			if(!((UserServiceImpl) userService).getIfwClientSecret().equals(ifwClientSecret) && ifwClientSecret!=null && !ifwClientSecret.isEmpty())
			((UserServiceImpl) userService).setIfwClientSecret(ifwClientSecret);
			
			if(!((UserServiceImpl) userService).getSalesForceClientId().equals(salesForceClientId) && salesForceClientId!=null && !salesForceClientId.isEmpty())
			((UserServiceImpl) userService).setSalesForceClientId(salesForceClientId);
			
			if(!((UserServiceImpl) userService).getSalesForceClientSecret().equals(salesForceClientSecret) && salesForceClientSecret!=null && !salesForceClientSecret.isEmpty())
			((UserServiceImpl) userService)
					.setSalesForceClientSecret(salesForceClientSecret);
			
			if(!((UserServiceImpl) userService).getSalesForceUserName().equals(salesForceUserName) && salesForceUserName!=null && !salesForceUserName.isEmpty())
			((UserServiceImpl) userService).setSalesForceUserName(salesForceUserName);
			
			if(!((UserServiceImpl) userService).getSalesForcePassword().equals(salesForcePassword) && salesForcePassword!=null && !salesForcePassword.isEmpty())
			((UserServiceImpl) userService).setSalesForcePassword(salesForcePassword);
			
			if(!((UserServiceImpl) userService).getHa_mode().equals(ha_mode) && ha_mode!=null && !ha_mode.isEmpty())
			((UserServiceImpl) userService).setHa_mode(ha_mode);
			
			if(!((UserServiceImpl) userService).getFromUserName().equals(fromUserName) && fromUserName!=null && !fromUserName.isEmpty())
			((UserServiceImpl) userService).setFromUserName(fromUserName);
			
			if(!((UserServiceImpl) userService).getGoDitalToken().equals(goDitalToken) && goDitalToken!=null && !goDitalToken.isEmpty())
			((UserServiceImpl) userService).setGoDitalToken(goDitalToken);
			
			if(!((UserServiceImpl) userService).getGoDigitalValue().equals(goDigitalValue) && goDigitalValue!=null && !goDigitalValue.isEmpty())
			((UserServiceImpl) userService).setGoDigitalValue(goDigitalValue);
			
			if(!((UserServiceImpl) userService).getUimsClientId().equals(uimsClientId) && uimsClientId!=null && !uimsClientId.isEmpty())
			((UserServiceImpl) userService).setUimsClientId(configuration.getProperty("uimsClientId"));
			
			if(!((UserServiceImpl) userService).getUimsClientSecret().equals(uimsClientSecret) && uimsClientSecret!=null && !uimsClientSecret.isEmpty())
			((UserServiceImpl) userService).setUimsClientSecret(configuration.getProperty("uimsClientSecret"));
			
			if(!((UserServiceImpl) userService).getRedirectUri().equals(redirectUri) && redirectUri!=null && !redirectUri.isEmpty())
			((UserServiceImpl) userService).setRedirectUri(configuration.getProperty("redirect.uri"));
			
			if(!((UserServiceImpl) userService).getPrefixStartUrl().equals(prefixStartUrl) && prefixStartUrl!=null && !prefixStartUrl.isEmpty())
			((UserServiceImpl) userService).setPrefixStartUrl(configuration.getProperty("openAMService.url"));
			
			if(!((UserServiceImpl) userService).getPrefixIdentityUrl().equals(prefixIdentityUrl) && prefixIdentityUrl!=null && !prefixIdentityUrl.isEmpty())
			((UserServiceImpl) userService).setPrefixIdentityUrl(configuration.getProperty("identityService.url"));
			
			if(!((UserServiceImpl) userService).getRegisterPRMUserIdp().equals(registerPRMUserIdp) && registerPRMUserIdp!=null && !registerPRMUserIdp.isEmpty())
			((UserServiceImpl) userService).setRegisterPRMUserIdp(configuration.getProperty("register.prmUser.idp"));
			
			if(!((UserServiceImpl) userService).getOtpvalidationtimeinminute().equals(otpvalidationtimeinminute) && otpvalidationtimeinminute!=null && !otpvalidationtimeinminute.isEmpty())
			((UserServiceImpl) userService)
					.setOtpvalidationtimeinminute(configuration.getProperty("otpvalidationtimeinminute"));
			
			if(!((UserServiceImpl) userService).getDjUserName().equals(djUserName) && djUserName!=null && !djUserName.isEmpty())
			((UserServiceImpl) userService).setDjUserName(configuration.getProperty("openDJUserName"));
			
			if(!((UserServiceImpl) userService).getDjUserPwd().equals(djUserPwd) && djUserPwd!=null && !djUserPwd.isEmpty())
			((UserServiceImpl) userService).setDjUserPwd(configuration.getProperty("openDJUserPassword"));
			
			if(!((UserServiceImpl) userService).getSendOTPOverEmail().equals(sendOTPOverEmail) && sendOTPOverEmail!=null && !sendOTPOverEmail.isEmpty())
			((UserServiceImpl) userService).setSendOTPOverEmail(configuration.getProperty("enable.sendOtpOverEmail"));
			
			if(!((UserServiceImpl) userService).getEnableTestMailDomain().equals(enableTestMailDomain) && enableTestMailDomain!=null && !enableTestMailDomain.isEmpty())
			((UserServiceImpl) userService).setEnableTestMailDomain(configuration.getProperty("enableTestMailDomain"));
			
			if(!(goDigitalUserService.getFromUserName().equals(fromUserName)) && fromUserName!=null && !fromUserName.isEmpty())
			goDigitalUserService.setFromUserName(configuration.getProperty("fromUserName"));
			
			if(!(goDigitalUserService.getSupportUser().equals(supportUser)) && supportUser!=null && !supportUser.isEmpty())
			goDigitalUserService.setSupportUser(configuration.getProperty("supportUser"));
			
			if(!(emailService.getFrom().equals(fromUserName)) && fromUserName!=null && !fromUserName.isEmpty())
			emailService.setFrom(configuration.getProperty("fromUserName"));
			
			if(!(emailService.getDjUserName().equals(djUserName)) && djUserName!=null && !djUserName.isEmpty())
			emailService.setDjUserName(configuration.getProperty("openDJUserName"));
			
			if(!(emailService.getDjUserPwd().equals(djUserPwd)) && djUserPwd!=null && !djUserPwd.isEmpty())
			emailService.setDjUserPwd(configuration.getProperty("openDJUserPassword"));
			
			if(!(emailService.getHotpEmailVerificationURL().equals(hotpEmailVerificationURL)) && hotpEmailVerificationURL!=null && !hotpEmailVerificationURL.isEmpty())
			emailService.setHotpEmailVerificationURL(configuration.getProperty("hotpEmailVerificationURL"));
			
			if(!(emailService.getIDMS_USER_REST_PASSWORD_EMAILTEMPLATE_CN().equals(IDMS_USER_REST_PASSWORD_EMAILTEMPLATE_CN)) && IDMS_USER_REST_PASSWORD_EMAILTEMPLATE_CN!=null && !IDMS_USER_REST_PASSWORD_EMAILTEMPLATE_CN.isEmpty())
			emailService.setIDMS_USER_REST_PASSWORD_EMAILTEMPLATE_CN(
					configuration.getProperty("user.reset.password.email.template.cn"));
			
			if(!(emailService.getIDMS_USER_REST_PASSWORD_EMAILTEMPLATE_EN().equals(IDMS_USER_REST_PASSWORD_EMAILTEMPLATE_EN)) && IDMS_USER_REST_PASSWORD_EMAILTEMPLATE_EN!=null && !IDMS_USER_REST_PASSWORD_EMAILTEMPLATE_EN.isEmpty())
			emailService.setIDMS_USER_REST_PASSWORD_EMAILTEMPLATE_EN(
					configuration.getProperty("user.reset.password.email.template.en"));
			
			if(!(emailService.getIDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_CN().equals(IDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_CN)) && IDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_CN!=null && !IDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_CN.isEmpty())
			emailService.setIDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_CN(
					configuration.getProperty("user.registration.withpwd.email.template.cn"));
			
			if(!(emailService.getIDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_EN().equals(IDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_EN)) && IDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_EN!=null && !IDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_EN.isEmpty())
			emailService.setIDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_EN(
					configuration.getProperty("user.registration.withpwd.email.template.en"));
			
			if(!(emailService.getIDMS_USER_UPDATE_EMAILTEMPLATE_CN().equals(IDMS_USER_UPDATE_EMAILTEMPLATE_CN)) && IDMS_USER_UPDATE_EMAILTEMPLATE_CN!=null && !IDMS_USER_UPDATE_EMAILTEMPLATE_CN.isEmpty())
			emailService
					.setIDMS_USER_UPDATE_EMAILTEMPLATE_CN(configuration.getProperty("user.update.email.template.cn"));
			
			if(!(emailService.getIDMS_USER_UPDATE_EMAILTEMPLATE_EN().equals(IDMS_USER_UPDATE_EMAILTEMPLATE_EN)) && IDMS_USER_UPDATE_EMAILTEMPLATE_EN!=null && !IDMS_USER_UPDATE_EMAILTEMPLATE_EN.isEmpty())
			emailService
					.setIDMS_USER_UPDATE_EMAILTEMPLATE_EN(configuration.getProperty("user.update.email.template.en"));
			
			if(!(emailService.getIDMS_USER_DEFAULT_EMAILTEMPLATE_CN().equals(IDMS_USER_DEFAULT_EMAILTEMPLATE_CN)) && IDMS_USER_DEFAULT_EMAILTEMPLATE_CN!=null && !IDMS_USER_DEFAULT_EMAILTEMPLATE_CN.isEmpty())
			emailService
					.setIDMS_USER_DEFAULT_EMAILTEMPLATE_CN(configuration.getProperty("user.default.email.template.cn"));
			
			if(!(emailService.getIDMS_USER_DEFAULT_EMAILTEMPLATE_EN().equals(IDMS_USER_DEFAULT_EMAILTEMPLATE_EN)) && IDMS_USER_DEFAULT_EMAILTEMPLATE_EN!=null && !IDMS_USER_DEFAULT_EMAILTEMPLATE_EN.isEmpty())
			emailService
					.setIDMS_USER_DEFAULT_EMAILTEMPLATE_EN(configuration.getProperty("user.default.email.template.en"));
			
			if(!(emailService.getIDMS_SEND_INVITATION_EMAILTEMPLATE_EN().equals(IDMS_SEND_INVITATION_EMAILTEMPLATE_EN)) && IDMS_SEND_INVITATION_EMAILTEMPLATE_EN!=null && !IDMS_SEND_INVITATION_EMAILTEMPLATE_EN.isEmpty())
			emailService.setIDMS_SEND_INVITATION_EMAILTEMPLATE_EN(
					configuration.getProperty("send.invitation.email.template.en"));
			
			if(!(emailService.getIDMS_SEND_INVITATION_EMAILTEMPLATE_CN().equals(IDMS_SEND_INVITATION_EMAILTEMPLATE_CN)) && IDMS_SEND_INVITATION_EMAILTEMPLATE_CN!=null && !IDMS_SEND_INVITATION_EMAILTEMPLATE_CN.isEmpty())
			emailService.setIDMS_SEND_INVITATION_EMAILTEMPLATE_CN(
					configuration.getProperty("send.invitation.email.template.cn"));
			
			if(!(emailService.getIDMS_USER_ADD_EMAILTEMPLATE_CN().equals(IDMS_USER_ADD_EMAILTEMPLATE_CN)) && IDMS_USER_ADD_EMAILTEMPLATE_CN!=null && !IDMS_USER_ADD_EMAILTEMPLATE_CN.isEmpty())
			emailService.setIDMS_USER_ADD_EMAILTEMPLATE_CN(configuration.getProperty("user.add.email.template.cn"));
			
			if(!(emailService.getIDMS_USER_ADD_EMAILTEMPLATE_EN().equals(IDMS_USER_ADD_EMAILTEMPLATE_EN)) && IDMS_USER_ADD_EMAILTEMPLATE_EN!=null && !IDMS_USER_ADD_EMAILTEMPLATE_EN.isEmpty())
			emailService.setIDMS_USER_ADD_EMAILTEMPLATE_EN(configuration.getProperty("user.add.email.template.en"));
			
			if(!(uimsCompManagSoapService.getCALLER_FID().equals(CALLER_FID)) && CALLER_FID!=null && !CALLER_FID.isEmpty())
			uimsCompManagSoapService.setCALLER_FID(configuration.getProperty("caller.fid"));
			
			if(!(uimsSetPasswordSoapService.getCALLER_FID().equals(CALLER_FID)) && CALLER_FID!=null && !CALLER_FID.isEmpty())
			uimsSetPasswordSoapService.setCALLER_FID(configuration.getProperty("caller.fid"));
			
			if(!(uimsSetPasswordSoapService.getFromUserName().equals(fromUserName)) && fromUserName!=null && !fromUserName.isEmpty())
			uimsSetPasswordSoapService.setFromUserName(configuration.getProperty("fromUserName"));
			
			if(!(uimsSetPasswordSoapService.getSupportUser().equals(supportUser)) && supportUser!=null && !supportUser.isEmpty())
			uimsSetPasswordSoapService.setSupportUser(configuration.getProperty("supportUser"));
			
			if(!(uimsUserManagSoapService.getCALLER_FID().equals(CALLER_FID)) && CALLER_FID!=null && !CALLER_FID.isEmpty())
			uimsUserManagSoapService.setCALLER_FID(configuration.getProperty("caller.fid"));
			
			if(!(uimsUserManagSoapService.getFromUserName().equals(fromUserName)) && fromUserName!=null && !fromUserName.isEmpty())
			uimsUserManagSoapService.setFromUserName(configuration.getProperty("fromUserName"));
			
			if(!(uimsUserManagSoapService.getSupportUser().equals(supportUser)) && supportUser!=null && !supportUser.isEmpty())
			uimsUserManagSoapService.setSupportUser(configuration.getProperty("supportUser"));
			
			if(!(uimsUserManagSoapService.getGoDigitalValue().equals(goDigitalValue)) && goDigitalValue!=null && !goDigitalValue.isEmpty())
			uimsUserManagSoapService.setGoDigitalValue(configuration.getProperty("goDigitalValue"));
			
			if(!(uimsUserManagSoapService.getGoDitalToken().equals(goDitalToken)) && goDitalToken!=null && !goDitalToken.isEmpty())
			uimsUserManagSoapService.setGoDitalToken(configuration.getProperty("goDitalToken"));
			
			if(!(saleforceService.getSalesForceClientId().equals(salesForceClientId)) && salesForceClientId!=null && !salesForceClientId.isEmpty())
			saleforceService.setSalesForceClientId(configuration.getProperty("salesForceClientId"));
			
			if(!(saleforceService.getSalesForceClientSecret().equals(salesForceClientSecret)) && salesForceClientSecret!=null && !salesForceClientSecret.isEmpty())
			saleforceService.setSalesForceClientSecret(configuration.getProperty("salesForceClientSecret"));
			
			if(!(saleforceService.getSalesForcePassword().equals(salesForcePassword)) && salesForcePassword!=null && !salesForcePassword.isEmpty())
			saleforceService.setSalesForcePassword(configuration.getProperty("salesForcePassword"));
			
			if(!(saleforceService.getSalesForceUserName().equals(salesForceUserName)) && salesForceUserName!=null && !salesForceUserName.isEmpty())
			saleforceService.setSalesForceUserName(configuration.getProperty("salesForceUserName"));
			
			if(!(saleforceSynService.getSalesForceClientId().equals(salesForceClientId)) && salesForceClientId!=null && !salesForceClientId.isEmpty())
			saleforceSynService.setSalesForceClientId(configuration.getProperty("salesForceClientId"));
			
			if(!(saleforceSynService.getSalesForceClientSecret().equals(salesForceClientSecret)) && salesForceClientSecret!=null && !salesForceClientSecret.isEmpty())
			saleforceSynService.setSalesForceClientSecret(configuration.getProperty("salesForceClientSecret"));
			
			if(!(saleforceSynService.getSalesForcePassword().equals(salesForcePassword)) && salesForcePassword!=null && !salesForcePassword.isEmpty())
			saleforceSynService.setSalesForcePassword(configuration.getProperty("salesForcePassword"));
			
			if(!(saleforceSynService.getSalesForceUserName().equals(salesForceUserName)) && salesForceUserName!=null && !salesForceUserName.isEmpty())
			saleforceSynService.setSalesForceUserName(configuration.getProperty("salesForceUserName"));
			
			if(!(saleforceSynService.getSftokentimeinminute().equals(sftokentimeinminute)) && sftokentimeinminute!=null && !sftokentimeinminute.isEmpty())
			saleforceSynService.setSftokentimeinminute(configuration.getProperty("sftokentimeinminute"));
			
			if(!(createUserService.getCALLER_FID().equals(CALLER_FID)) && CALLER_FID!=null && !CALLER_FID.isEmpty())
			createUserService.setCALLER_FID(configuration.getProperty("caller.fid"));
			
			if(!(commonService.getSalesForceClientId().equals(salesForceClientId)) && salesForceClientId!=null && !salesForceClientId.isEmpty())
			commonService.setSalesForceClientId(configuration.getProperty("salesForceClientId"));
			
			if(!(commonService.getSalesForceClientSecret().equals(salesForceClientSecret)) && salesForceClientSecret!=null && !salesForceClientSecret.isEmpty())
			commonService.setSalesForceClientSecret(configuration.getProperty("salesForceClientSecret"));
			
			if(!(commonService.getSalesForcePassword().equals(salesForcePassword)) && salesForcePassword!=null && !salesForcePassword.isEmpty())
			commonService.setSalesForcePassword(configuration.getProperty("salesForcePassword"));
			
			if(!(commonService.getSalesForceUserName().equals(salesForceUserName)) && salesForceUserName!=null && !salesForceUserName.isEmpty())
			commonService.setSalesForceUserName(configuration.getProperty("salesForceUserName"));
			
			if(!(commonService.getAdminUserName().equals(adminUserName)) && adminUserName!=null && !adminUserName.isEmpty())
			commonService.setAdminUserName(configuration.getProperty("adminUserName"));
			
			if(!(commonService.getAdminPassword().equals(adminPassword)) && adminPassword!=null && !adminPassword.isEmpty())
			commonService.setAdminPassword(configuration.getProperty("adminPassword"));
			
			if(!(commonService.getAuthCsvPath().equals(authCsvPath)) && authCsvPath!=null && !authCsvPath.isEmpty())
			commonService.setAuthCsvPath(configuration.getProperty("authCsvPath"));
			
			if(!(commonService.getRegistrationCsvPath().equals(registrationCsvPath)) && registrationCsvPath!=null && !registrationCsvPath.isEmpty())
			commonService.setRegistrationCsvPath(configuration.getProperty("registrationCsvPath"));
			
			if(!(commonService.getDirectApiSecretToken().equals(directApiSecretToken)) && directApiSecretToken!=null && !directApiSecretToken.isEmpty())
			commonService.setDirectApiSecretToken(configuration.getProperty("directApiSecretToken"));
			
			if(!(commonService.getEMAIL_TEMPLATE_DIR().equals(EMAIL_TEMPLATE_DIR)) && EMAIL_TEMPLATE_DIR!=null && !EMAIL_TEMPLATE_DIR.isEmpty())
			commonService.setEMAIL_TEMPLATE_DIR(configuration.getProperty("email.template.dir"));
			
			if(!commonService.getFromUserName().equals(fromUserName) && fromUserName!=null && !fromUserName.isEmpty())
			commonService.setFromUserName(configuration.getProperty("fromUserName"));
			
			if(!commonService.getGoDigitalValue().equals(goDigitalValue) && goDigitalValue!=null && !goDigitalValue.isEmpty())
			commonService.setGoDigitalValue(configuration.getProperty("goDigitalValue"));
			
			if(!commonService.getGoDitalToken().equals(goDitalToken) && goDitalToken!=null && !goDitalToken.isEmpty())
			commonService.setGoDitalToken(configuration.getProperty("goDitalToken"));
			
			if(!commonService.getHa_mode().equals(ha_mode) && ha_mode!=null && !ha_mode.isEmpty())
			commonService.setHa_mode(configuration.getProperty("ha_mode"));
			
			if(!commonService.getIfwClientId().equals(ifwClientId) && ifwClientId!=null && !ifwClientId.isEmpty())
			commonService.setIfwClientId(configuration.getProperty("ifwClientId"));
			
			if(!commonService.getIfwClientSecret().equals(ifwClientSecret) && ifwClientSecret!=null && !ifwClientSecret.isEmpty())
			commonService.setIfwClientSecret(configuration.getProperty("ifwClientSecret"));
			
			if(!(commonService.getOpenDJUserName().equals(djUserName)) && djUserName!=null && !djUserName.isEmpty())
			commonService.setOpenDJUserName(configuration.getProperty("openDJUserName"));
			
			if(!(commonService.getOpenDJUserPassword().equals(djUserPwd)) && djUserPwd!=null && !djUserPwd.isEmpty())
			commonService.setOpenDJUserPassword(configuration.getProperty("openDJUserPassword"));
			
			if(!commonService.getRedirectUri().equals(redirectUri) && redirectUri!=null && !redirectUri.isEmpty())
			commonService.setRedirectUri(configuration.getProperty("redirect.uri"));
			
			if(!commonService.getUimsClientId().equals(uimsClientId) && uimsClientId!=null && !uimsClientId.isEmpty())
			commonService.setUimsClientId(configuration.getProperty("uimsClientId"));
			
			if(!commonService.getUimsClientSecret().equals(uimsClientSecret) && uimsClientSecret!=null && !uimsClientSecret.isEmpty())
			commonService.setUimsClientSecret(configuration.getProperty("uimsClientSecret"));
			
			if(!commonService.getPrefixStartUrl().equals(prefixStartUrl) && prefixStartUrl!=null && !prefixStartUrl.isEmpty())
			commonService.setPrefixStartUrl(configuration.getProperty("openAMService.url"));
			
			if(!(directUIMSUserManagerSoapService.getCALLER_FID().equals(CALLER_FID)) && CALLER_FID!=null && !CALLER_FID.isEmpty())
			directUIMSUserManagerSoapService.setCALLER_FID(configuration.getProperty("caller.fid"));
			
			if(!directUIMSUserManagerSoapService.getFromUserName().equals(fromUserName) && fromUserName!=null && !fromUserName.isEmpty())
			directUIMSUserManagerSoapService.setFromUserName(configuration.getProperty("fromUserName"));
			
			if(!directUIMSUserManagerSoapService.getGoDigitalValue().equals(goDigitalValue) && goDigitalValue!=null && !goDigitalValue.isEmpty())
			directUIMSUserManagerSoapService.setGoDigitalValue(configuration.getProperty("goDigitalValue"));
			
			if(!directUIMSUserManagerSoapService.getGoDitalToken().equals(goDitalToken) && goDitalToken!=null && !goDitalToken.isEmpty())
			directUIMSUserManagerSoapService.setGoDitalToken(configuration.getProperty("goDitalToken"));
			
			if(!(directUIMSUserManagerSoapService.getSupportUser().equals(supportUser)) && supportUser!=null && !supportUser.isEmpty())
			directUIMSUserManagerSoapService.setSupportUser(configuration.getProperty("supportUser"));
			
			if(!samlAssertionTokenService.getSamlAssertionKeyPassword().equals(samlAssertionKeyPassword) && samlAssertionKeyPassword!=null && !samlAssertionKeyPassword.isEmpty())
			samlAssertionTokenService.setSamlAssertionKeyPassword(
					configuration.getProperty("keystore.samlAssertionSigning.keystore.privateKey.password"));
			
			if(!samlAssertionTokenService.getSamlAssertionKeystorePassword().equals(samlAssertionKeystorePassword)&& samlAssertionKeystorePassword!=null && !samlAssertionKeystorePassword.isEmpty() )
			samlAssertionTokenService.setSamlAssertionKeystorePassword(
					configuration.getProperty("keystore.samlAssertionSigning.keystore.password"));
			
			if(!samlAssertionTokenService.getSamlAssertionSigningAlgo().equals(samlAssertionSigningAlgo) && samlAssertionSigningAlgo!=null && !samlAssertionSigningAlgo.isEmpty())
			samlAssertionTokenService
					.setSamlAssertionSigningAlgo(configuration.getProperty("crypto.algo.samlAssertionSigning"));
			
			if(!samlAssertionTokenService.getSamlAssertionSigningCert().equals(samlAssertionSigningCert) && samlAssertionSigningCert!=null && !samlAssertionSigningCert.isEmpty())
			samlAssertionTokenService.setSamlAssertionSigningCert(
					configuration.getProperty("keystore.samlAssertionSigning.keystore.certAlias"));
			
			if(!samlAssertionTokenService.getSamlAssertionSigningKeystore().equals(samlAssertionSigningKeystore) && samlAssertionSigningKeystore!=null && !samlAssertionSigningKeystore.isEmpty())
			samlAssertionTokenService
					.setSamlAssertionSigningKeystore(configuration.getProperty("keystore.samlAssertionSigning.path"));
			
			//pickListValidator.setIDMS_DEPLOY_ENV(configuration.getProperty("idms.env"));
			if(!pickListValidator.getIDMS_FIELDSPICKLIST_PROPERTIES_PATH().equals(IDMS_FIELDSPICKLIST_PROPERTIES_PATH) && IDMS_FIELDSPICKLIST_PROPERTIES_PATH!=null && !IDMS_FIELDSPICKLIST_PROPERTIES_PATH.isEmpty())
				pickListValidator.setIDMS_FIELDSPICKLIST_PROPERTIES_PATH(configuration.getProperty("fields.picklist.props.path"));
			
			//multiPickListValidator.setIDMS_DEPLOY_ENV(configuration.getProperty("idms.env"));
			if(!multiPickListValidator.getIDMS_FIELDSMULTI_PICKLIST_PROPERTIES_PATH().equals(IDMS_FIELDSMULTI_PICKLIST_PROPERTIES_PATH) && IDMS_FIELDSMULTI_PICKLIST_PROPERTIES_PATH!=null && !IDMS_FIELDSMULTI_PICKLIST_PROPERTIES_PATH.isEmpty())
				multiPickListValidator.setIDMS_FIELDSMULTI_PICKLIST_PROPERTIES_PATH(configuration.getProperty("fields.multi.picklist.props.path"));
			
			//mandatoryValidator.setIDMS_DEPLOY_ENV(configuration.getProperty("idms.env"));
			if(!mandatoryValidator.getIDMS_FIELDSMANDATORY_PROPERTIES_PATH().equals(IDMS_FIELDSMANDATORY_PROPERTIES_PATH) && IDMS_FIELDSMANDATORY_PROPERTIES_PATH!=null && !IDMS_FIELDSMANDATORY_PROPERTIES_PATH.isEmpty())
				mandatoryValidator.setIDMS_FIELDSMANDATORY_PROPERTIES_PATH(configuration.getProperty("fields.mandatory.props.path"));
			
			//legthValidator.setIDMS_DEPLOY_ENV(configuration.getProperty("idms.env"));
			if(!legthValidator.getIDMS_FIELDSLENGTH_PROPERTIES_PATH().equals(IDMS_FIELDSLENGTH_PROPERTIES_PATH) && IDMS_FIELDSLENGTH_PROPERTIES_PATH!=null && !IDMS_FIELDSLENGTH_PROPERTIES_PATH.isEmpty())
				legthValidator.setIDMS_FIELDSLENGTH_PROPERTIES_PATH(configuration.getProperty("fields.length.props.path"));
			
			//fieldsMappingValidator.setIDMS_DEPLOY_ENV(configuration.getProperty("idms.env"));
			if(!fieldsMappingValidator.getIDMS_FIELDSMAPPING_PROPERTIES_PATH().equals(IDMS_FIELDSMAPPING_PROPERTIES_PATH) && IDMS_FIELDSMAPPING_PROPERTIES_PATH!=null && !IDMS_FIELDSMAPPING_PROPERTIES_PATH.isEmpty())
				fieldsMappingValidator.setIDMS_FIELDSMAPPING_PROPERTIES_PATH(configuration.getProperty("fields.mapping.props.path"));
			
			LOGGER.info("initilize(final String file) end");

		} catch (IOException e) {
			LOGGER.error("IOException in property file  initilizing"+e.getMessage(), e);
		}
		catch (Exception ex) {
			LOGGER.error("Exception in property file  initilizing"+ex.getMessage(), ex);
		}
	}

	private void initializeProperties() {
		CALLER_FID=configuration.getProperty("caller.fid");
		
		LOGIN_ERROR=configuration.getProperty("caller.fid");
		
		EMAIL_TEMPLATE_DIR=configuration.getProperty("email.template.dir");
		
		authCsvPath=configuration.getProperty("authCsvPath");
		
		registrationCsvPath=configuration.getProperty("registrationCsvPath");

		adminUserName=configuration.getProperty("adminUserName");

		adminPassword=configuration.getProperty("adminPassword");

		ifwClientId=configuration.getProperty("ifwClientId");
		
		ifwClientSecret=configuration.getProperty("ifwClientSecret");
		
		salesForceClientId=configuration.getProperty("salesForceClientId");
		
		salesForceClientSecret=configuration.getProperty("salesForceClientSecret");
		
		salesForceUserName=configuration.getProperty("salesForceUserName");
		
		salesForcePassword=configuration.getProperty("salesForcePassword");
		
		ha_mode=configuration.getProperty("ha_mode");

		fromUserName=configuration.getProperty("fromUserName");
		
		goDitalToken=configuration.getProperty("goDitalToken");
		
		goDigitalValue=configuration.getProperty("goDigitalValue");
		
		uimsClientId=configuration.getProperty("uimsClientId");
		
		uimsClientSecret=configuration.getProperty("uimsClientSecret");

		redirectUri=configuration.getProperty("redirect.uri");
		
		prefixStartUrl=configuration.getProperty("openAMService.url");
		
		prefixIdentityUrl=configuration.getProperty("identityService.url");
		
		registerPRMUserIdp=configuration.getProperty("register.prmUser.idp");

		otpvalidationtimeinminute=configuration.getProperty("otpvalidationtimeinminute");
		
		djUserName=configuration.getProperty("openDJUserName");
		
		djUserPwd=configuration.getProperty("openDJUserPassword");
		
		sendOTPOverEmail=configuration.getProperty("enable.sendOtpOverEmail");
		
		enableTestMailDomain=configuration.getProperty("enableTestMailDomain");
			
		supportUser=configuration.getProperty("supportUser");
		
		hotpEmailVerificationURL=configuration.getProperty("hotpEmailVerificationURL");
		
		IDMS_USER_REST_PASSWORD_EMAILTEMPLATE_CN=configuration.getProperty("user.reset.password.email.template.cn");
		
		IDMS_USER_REST_PASSWORD_EMAILTEMPLATE_EN=configuration.getProperty("user.reset.password.email.template.en");
		
		IDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_CN=configuration.getProperty("user.registration.withpwd.email.template.cn");

		IDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_EN=configuration.getProperty("user.registration.withpwd.email.template.en");

		IDMS_USER_UPDATE_EMAILTEMPLATE_CN=configuration.getProperty("user.update.email.template.cn");
		
		IDMS_USER_UPDATE_EMAILTEMPLATE_EN=configuration.getProperty("user.update.email.template.en");
		
		IDMS_USER_DEFAULT_EMAILTEMPLATE_CN=configuration.getProperty("user.default.email.template.cn");
		
		IDMS_USER_DEFAULT_EMAILTEMPLATE_EN=	configuration.getProperty("user.default.email.template.en");
		
		IDMS_SEND_INVITATION_EMAILTEMPLATE_EN=configuration.getProperty("send.invitation.email.template.en");
		
		IDMS_SEND_INVITATION_EMAILTEMPLATE_CN=configuration.getProperty("send.invitation.email.template.cn");
		
		IDMS_USER_ADD_EMAILTEMPLATE_CN=configuration.getProperty("user.add.email.template.cn");
		
		IDMS_USER_ADD_EMAILTEMPLATE_EN=configuration.getProperty("user.add.email.template.en");
		
		sftokentimeinminute=configuration.getProperty("sftokentimeinminute");
		
		directApiSecretToken=configuration.getProperty("directApiSecretToken");
			
		samlAssertionSigningKeystore=configuration.getProperty("keystore.samlAssertionSigning.path");
		
		samlAssertionKeystorePassword=configuration.getProperty("keystore.samlAssertionSigning.keystore.password");
			
		samlAssertionKeyPassword=configuration.getProperty("keystore.samlAssertionSigning.keystore.privateKey.password");
		
		samlAssertionSigningCert=configuration.getProperty("keystore.samlAssertionSigning.keystore.certAlias");
			
		samlAssertionSigningAlgo=configuration.getProperty("crypto.algo.samlAssertionSigning");
		
		IDMS_FIELDSPICKLIST_PROPERTIES_PATH=configuration.getProperty("fields.picklist.props.path");
			
		IDMS_FIELDSMULTI_PICKLIST_PROPERTIES_PATH=configuration.getProperty("fields.multi.picklist.props.path");
		
		IDMS_FIELDSMANDATORY_PROPERTIES_PATH=configuration.getProperty("fields.mandatory.props.path");
		
		IDMS_FIELDSLENGTH_PROPERTIES_PATH=configuration.getProperty("fields.length.props.path");

		IDMS_FIELDSMAPPING_PROPERTIES_PATH=configuration.getProperty("fields.mapping.props.path");
	}

	public void initilize() throws Exception {
		InputStream in = null;
		try {
			LOGGER.info("PropertyFileAutoRefresh::initilize() called");
			in = new FileInputStream(new File(PROPERTY_FILE));
			configuration.load(in);
			//String DEFAULT_APP_ROOT_LOCATION="${idmsc.app_root.location}";
			String appRootLocation=System.getProperty("idmsc.app_root.location");
			LOGGER.info("system property:"+appRootLocation);
			configuration.forEach((type, value) -> {
			   String strValue=(String)value;
			   if(strValue.contains(APP_ROOT_PLACE_HOLDER)){
				   //configuration.setProperty((String)type, strValue.replace(DEFAULT_APP_ROOT_LOCATION, appRootLocation));
				   strValue=strValue.replace(APP_ROOT_PLACE_HOLDER, appRootLocation);
				   if(IDMS_DEPLOY_ENV.equalsIgnoreCase("DEV")){
				   //configuration.setProperty((String)type, strValue.replaceAll("/", "\\\\"));
				    strValue=strValue.replaceAll("/", "\\\\");
				   }
				   configuration.setProperty((String)type, strValue);
			   }
			  });
			configuration.setProperty("app.properties.file",configuration.getProperty("app.properties.file").replace("${idms.env}", IDMS_DEPLOY_ENV));
			LOGGER.info("\nAfter conversion:");
			configuration.forEach((key, value) -> LOGGER.info(key + " : " + value));
			LOGGER.info("Initial value:" + configuration.getProperty("caller.fid"));
			LOGGER.info("userserviceimpl:###" + userService);
			LOGGER.info("PROPERTY_FILE:" + PROPERTY_FILE);
			Thread.sleep(10000);
			((UserServiceImpl) userService).setCALLER_FID("Hellooo!!!");
			LOGGER.info("final value:" + ((UserServiceImpl) userService).getCALLER_FID());

		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public String getConfiguration(final String key) {
		return (String) getConfiguration().get(key);
	}

	public String getConfigurationWithDefaultValue(final String key, final String defaultValue) {
		return (String) getConfiguration().getProperty(key, defaultValue);
	}

}

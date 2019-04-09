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

@Service("propertyFileAutoRefresh")
public class PropertyFileAutoRefresh {

	private final static PropertyFileAutoRefresh INSTANCE = new PropertyFileAutoRefresh();
	

	/**
	 * Logger instance.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(PropertyFileAutoRefresh.class);


	@Value("${app.properties.file}")
	private String PROPERTY_FILE;

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
			//System.out.println("Refreshed value:" + configuration.getProperty("IDMS_BFO_profile"));
			// System.out.println("PROPERTY_FILE:"+PROPERTY_FILE);
			// Update all properties file entries
			((UserServiceImpl) userService).setCALLER_FID(configuration.getProperty("caller.fid"));
			((UserServiceImpl) userService).setLOGIN_ERROR(configuration.getProperty("caller.fid"));
			((UserServiceImpl) userService).setEMAIL_TEMPLATE_DIR(configuration.getProperty("email.template.dir"));
			((UserServiceImpl) userService).setAuthCsvPath(configuration.getProperty("authCsvPath"));
			((UserServiceImpl) userService).setRegistrationCsvPath(configuration.getProperty("registrationCsvPath"));
			((UserServiceImpl) userService).setAdminUserName(configuration.getProperty("adminUserName"));
			((UserServiceImpl) userService).setAdminPassword(configuration.getProperty("adminPassword"));
			((UserServiceImpl) userService).setIfwClientId(configuration.getProperty("ifwClientId"));
			((UserServiceImpl) userService).setIfwClientSecret(configuration.getProperty("ifwClientSecret"));
			((UserServiceImpl) userService).setSalesForceClientId(configuration.getProperty("salesForceClientId"));
			((UserServiceImpl) userService)
					.setSalesForceClientSecret(configuration.getProperty("salesForceClientSecret"));
			((UserServiceImpl) userService).setSalesForceUserName(configuration.getProperty("salesForceUserName"));
			((UserServiceImpl) userService).setSalesForcePassword(configuration.getProperty("salesForcePassword"));
			((UserServiceImpl) userService).setHa_mode(configuration.getProperty("ha_mode"));
			((UserServiceImpl) userService).setFromUserName(configuration.getProperty("fromUserName"));
			((UserServiceImpl) userService).setGoDitalToken(configuration.getProperty("goDitalToken"));
			((UserServiceImpl) userService).setGoDigitalValue(configuration.getProperty("goDigitalValue"));
			((UserServiceImpl) userService).setUimsClientId(configuration.getProperty("uimsClientId"));
			((UserServiceImpl) userService).setUimsClientSecret(configuration.getProperty("uimsClientSecret"));
			((UserServiceImpl) userService).setRedirectUri(configuration.getProperty("redirect.uri"));
			((UserServiceImpl) userService).setPrefixStartUrl(configuration.getProperty("openAMService.url"));
			((UserServiceImpl) userService).setPrefixIdentityUrl(configuration.getProperty("identityService.url"));
			((UserServiceImpl) userService).setRegisterPRMUserIdp(configuration.getProperty("register.prmUser.idp"));
			((UserServiceImpl) userService)
					.setOtpvalidationtimeinminute(configuration.getProperty("otpvalidationtimeinminute"));
			((UserServiceImpl) userService).setDjUserName(configuration.getProperty("openDJUserName"));
			((UserServiceImpl) userService).setDjUserPwd(configuration.getProperty("openDJUserPassword"));
			((UserServiceImpl) userService).setSendOTPOverEmail(configuration.getProperty("enable.sendOtpOverEmail"));
			((UserServiceImpl) userService).setDisableTestMailDomain(configuration.getProperty("disableTestMailDomain"));

			goDigitalUserService.setFromUserName(configuration.getProperty("fromUserName"));
			goDigitalUserService.setSupportUser(configuration.getProperty("supportUser"));

			emailService.setFrom(configuration.getProperty("fromUserName"));
			emailService.setDjUserName(configuration.getProperty("openDJUserName"));
			emailService.setDjUserPwd(configuration.getProperty("openDJUserPassword"));
			emailService.setHotpEmailVerificationURL(configuration.getProperty("hotpEmailVerificationURL"));
			emailService.setIDMS_USER_REST_PASSWORD_EMAILTEMPLATE_CN(
					configuration.getProperty("user.reset.password.email.template.cn"));
			emailService.setIDMS_USER_REST_PASSWORD_EMAILTEMPLATE_EN(
					configuration.getProperty("user.reset.password.email.template.en"));
			emailService.setIDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_CN(
					configuration.getProperty("user.registration.withpwd.email.template.cn"));
			emailService.setIDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_EN(
					configuration.getProperty("user.registration.withpwd.email.template.en"));
			emailService
					.setIDMS_USER_UPDATE_EMAILTEMPLATE_CN(configuration.getProperty("user.update.email.template.cn"));
			emailService
					.setIDMS_USER_UPDATE_EMAILTEMPLATE_EN(configuration.getProperty("user.update.email.template.en"));
			emailService
					.setIDMS_USER_DEFAULT_EMAILTEMPLATE_CN(configuration.getProperty("user.default.email.template.cn"));
			emailService
					.setIDMS_USER_DEFAULT_EMAILTEMPLATE_EN(configuration.getProperty("user.default.email.template.en"));
			emailService.setIDMS_SEND_INVITATION_EMAILTEMPLATE_EN(
					configuration.getProperty("send.invitation.email.template.en"));
			emailService.setIDMS_SEND_INVITATION_EMAILTEMPLATE_CN(
					configuration.getProperty("send.invitation.email.template.cn"));
			emailService.setIDMS_USER_ADD_EMAILTEMPLATE_CN(configuration.getProperty("user.add.email.template.cn"));
			emailService.setIDMS_USER_ADD_EMAILTEMPLATE_EN(configuration.getProperty("user.add.email.template.en"));

			uimsCompManagSoapService.setCALLER_FID(configuration.getProperty("caller.fid"));

			uimsSetPasswordSoapService.setCALLER_FID(configuration.getProperty("caller.fid"));
			uimsSetPasswordSoapService.setFromUserName(configuration.getProperty("fromUserName"));
			uimsSetPasswordSoapService.setSupportUser(configuration.getProperty("supportUser"));

			uimsUserManagSoapService.setCALLER_FID(configuration.getProperty("caller.fid"));
			uimsUserManagSoapService.setFromUserName(configuration.getProperty("fromUserName"));
			uimsUserManagSoapService.setSupportUser(configuration.getProperty("supportUser"));
			uimsUserManagSoapService.setGoDigitalValue(configuration.getProperty("goDigitalValue"));
			uimsUserManagSoapService.setGoDitalToken(configuration.getProperty("goDitalToken"));

			saleforceService.setSalesForceClientId(configuration.getProperty("salesForceClientId"));
			saleforceService.setSalesForceClientSecret(configuration.getProperty("salesForceClientSecret"));
			saleforceService.setSalesForcePassword(configuration.getProperty("salesForcePassword"));
			saleforceService.setSalesForceUserName(configuration.getProperty("salesForceUserName"));

			saleforceSynService.setSalesForceClientId(configuration.getProperty("salesForceClientId"));
			saleforceSynService.setSalesForceClientSecret(configuration.getProperty("salesForceClientSecret"));
			saleforceSynService.setSalesForcePassword(configuration.getProperty("salesForcePassword"));
			saleforceSynService.setSalesForceUserName(configuration.getProperty("salesForceUserName"));
			saleforceSynService.setSftokentimeinminute(configuration.getProperty("sftokentimeinminute"));

			createUserService.setCALLER_FID(configuration.getProperty("caller.fid"));

			commonService.setSalesForceClientId(configuration.getProperty("salesForceClientId"));
			commonService.setSalesForceClientSecret(configuration.getProperty("salesForceClientSecret"));
			commonService.setSalesForcePassword(configuration.getProperty("salesForcePassword"));
			commonService.setSalesForceUserName(configuration.getProperty("salesForceUserName"));
			commonService.setAdminUserName(configuration.getProperty("adminUserName"));
			commonService.setAdminPassword(configuration.getProperty("adminPassword"));
			commonService.setAuthCsvPath(configuration.getProperty("authCsvPath"));
			commonService.setRegistrationCsvPath(configuration.getProperty("registrationCsvPath"));
			commonService.setDirectApiSecretToken(configuration.getProperty("directApiSecretToken"));
			commonService.setEMAIL_TEMPLATE_DIR(configuration.getProperty("email.template.dir"));
			commonService.setFromUserName(configuration.getProperty("fromUserName"));

			commonService.setGoDigitalValue(configuration.getProperty("goDigitalValue"));
			commonService.setGoDitalToken(configuration.getProperty("goDitalToken"));
			commonService.setHa_mode(configuration.getProperty("ha_mode"));
			commonService.setIfwClientId(configuration.getProperty("ifwClientId"));

			commonService.setIfwClientSecret(configuration.getProperty("ifwClientSecret"));
			commonService.setOpenDJUserName(configuration.getProperty("openDJUserName"));
			commonService.setOpenDJUserPassword(configuration.getProperty("openDJUserPassword"));
			commonService.setRedirectUri(configuration.getProperty("redirect.uri"));

			commonService.setUimsClientId(configuration.getProperty("uimsClientId"));
			commonService.setUimsClientSecret(configuration.getProperty("uimsClientSecret"));
			commonService.setPrefixStartUrl(configuration.getProperty("openAMService.url"));

			directUIMSUserManagerSoapService.setCALLER_FID(configuration.getProperty("caller.fid"));
			directUIMSUserManagerSoapService.setFromUserName(configuration.getProperty("fromUserName"));
			directUIMSUserManagerSoapService.setGoDigitalValue(configuration.getProperty("goDigitalValue"));
			directUIMSUserManagerSoapService.setGoDitalToken(configuration.getProperty("goDitalToken"));
			directUIMSUserManagerSoapService.setSupportUser(configuration.getProperty("supportUser"));

			samlAssertionTokenService.setSamlAssertionKeyPassword(
					configuration.getProperty("keystore.samlAssertionSigning.keystore.privateKey.password"));
			samlAssertionTokenService.setSamlAssertionKeystorePassword(
					configuration.getProperty("keystore.samlAssertionSigning.keystore.password"));
			samlAssertionTokenService
					.setSamlAssertionSigningAlgo(configuration.getProperty("crypto.algo.samlAssertionSigning"));
			samlAssertionTokenService.setSamlAssertionSigningCert(
					configuration.getProperty("keystore.samlAssertionSigning.keystore.certAlias"));
			samlAssertionTokenService
					.setSamlAssertionSigningKeystore(configuration.getProperty("keystore.samlAssertionSigning.path"));
			
			pickListValidator.setIDMS_DEPLOY_ENV(configuration.getProperty("idms.env"));
			pickListValidator.setIDMS_FIELDSPICKLIST_PROPERTIES_PATH(configuration.getProperty("fields.picklist.props.path"));
			
			multiPickListValidator.setIDMS_DEPLOY_ENV(configuration.getProperty("idms.env"));
			multiPickListValidator.setIDMS_FIELDSMULTI_PICKLIST_PROPERTIES_PATH(configuration.getProperty("fields.multi.picklist.props.path"));
			
			mandatoryValidator.setIDMS_DEPLOY_ENV(configuration.getProperty("idms.env"));
			mandatoryValidator.setIDMS_FIELDSMANDATORY_PROPERTIES_PATH(configuration.getProperty("fields.mandatory.props.path"));
			
			legthValidator.setIDMS_DEPLOY_ENV(configuration.getProperty("idms.env"));
			legthValidator.setIDMS_FIELDSLENGTH_PROPERTIES_PATH(configuration.getProperty("fields.length.props.path"));
			
			fieldsMappingValidator.setIDMS_DEPLOY_ENV(configuration.getProperty("idms.env"));
			fieldsMappingValidator.setIDMS_FIELDSMAPPING_PROPERTIES_PATH(configuration.getProperty("fields.mapping.props.path"));
			
			LOGGER.info("initilize(final String file) end");
		} catch (IOException e) {
			LOGGER.error("Error in property file  initilizing"+e.getMessage(), e);
		}
	}

	public void initilize() throws Exception {
		InputStream in = null;
		try {
			LOGGER.info("PropertyFileAutoRefresh::initilize() called");
			in = new FileInputStream(new File(PROPERTY_FILE));
			configuration.load(in);
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

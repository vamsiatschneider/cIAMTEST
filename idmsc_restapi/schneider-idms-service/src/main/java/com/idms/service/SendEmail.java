package com.idms.service;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import javax.inject.Inject;
import javax.mail.Message.RecipientType;
import javax.mail.MessagingException;
import javax.mail.internet.AddressException;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;
import javax.ws.rs.core.Response;

import org.apache.commons.lang3.RandomStringUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.cxf.helpers.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.icu.util.StringTokenizer;
import com.idms.dynamic.mail.template.factory.DynamicEmailTemplateFactory;
import com.idms.dynamic.mail.template.factory.DynamicTemplatePHSubstitutorFactory;
import com.idms.dynamic.mail.template.factory.impl.DynamicEmailTemplateFactoryImpl;
import com.idms.dynamic.mail.template.factory.impl.DynamicTemplatePHSubstitutorFactoryImpl;
import com.idms.dynamic.mail.template.placeholder.creator.LinkTemplatePlaceholderCreator;
import com.idms.dynamic.mail.template.placeholder.creator.OTPTemplatePlaceholderCreator;
import com.idms.dynamic.mail.template.placeholder.creator.PRMInternalRegTemplatePHCreator;
import com.idms.dynamic.mail.template.placeholder.creator.TemplatePlaceholderCreator;
import com.idms.dynamic.mail.template.placeholder.substitutor.TemplatePlaceholderSubstitutor;
import com.idms.dynamic.mail.template.util.DynamicEmailTemplateInput;
import com.idms.dynamic.mail.template.util.OpenDJAttributes;
import com.idms.mail.template.factory.EmailTemplateAbstractFactory;
import com.idms.mail.template.factory.impl.EmailTemplateAbstractFactoryImpl;
import com.idms.mail.template.util.EmailTemplateColor;
import com.idms.mail.template.util.EmailTemplateInput;
import com.idms.mail.template.util.Locale;
import com.idms.mail.template.util.OperationType;
import com.idms.mail.template.util.PRMTemplateType;
import com.idms.model.RegistrationAttributes;
import com.idms.product.client.NewSmsService;
import com.idms.product.client.OpenAMService;
import com.idms.product.client.OpenDjService;
import com.idms.product.client.SmsService;
import com.idms.service.util.ChinaIdmsUtil;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Option;
import com.se.idms.cache.utils.EmailConstants;
import com.se.idms.util.JsonConstants;
import com.se.idms.util.UserConstants;
import com.sun.mail.smtp.SMTPSendFailedException;

@Service("emailService")
@EnableAsync
public class SendEmail {
	
	@Inject
	private OpenAMService productService;
	
	@Inject
	private SmsService smsService;
	
	@Inject
	private NewSmsService newSmsService;
	
	@Inject
	protected OpenDjService openDJService;
	
	@Value("${fromUserName}")
	private String from;

	@Value("${hotpEmailVerificationURL}")
	private String hotpEmailVerificationURL;
	
	@Value("${openDJUserName}")
	private String djUserName;
	
	@Value("${openDJUserPassword}")
	private String djUserPwd;
	
	//CODE-RE-STRUCTURING
	@Value("${user.reset.password.email.template.cn}")
	private String IDMS_USER_REST_PASSWORD_EMAILTEMPLATE_CN;
	
	@Value("${user.reset.password.email.template.en}")
	private String IDMS_USER_REST_PASSWORD_EMAILTEMPLATE_EN;
	
	@Value("${user.reset.password.email.template.blue.cn}")
	private String IDMS_USER_REST_PASSWORD_BLUE_EMAILTEMPLATE_CN;
	
	@Value("${user.reset.password.email.template.blue.en}")
	private String IDMS_USER_REST_PASSWORD_BLUE_EMAILTEMPLATE_EN;
	
	@Value("${user.registration.withpwd.email.template.cn}")
	private String IDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_CN;
	
	@Value("${user.registration.withpwd.email.template.en}")
	private String IDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_EN;
	
	@Value("${user.registration.withpwd.email.template.blue.cn}")
	private String IDMS_USER_REGESTRATION_WITHPWD_BLUE_EMAILTEMPLATE_CN;
	
	@Value("${user.registration.withpwd.email.template.blue.en}")
	private String IDMS_USER_REGESTRATION_WITHPWD_BLUE_EMAILTEMPLATE_EN;

	@Value("${user.update.email.template.cn}")
	private String IDMS_USER_UPDATE_EMAILTEMPLATE_CN;
	
	@Value("${user.update.email.template.en}")
	private String IDMS_USER_UPDATE_EMAILTEMPLATE_EN;
	
	@Value("${user.update.email.template.blue.cn}")
	private String IDMS_USER_UPDATE_BLUE_EMAILTEMPLATE_CN;
	
	@Value("${user.update.email.template.blue.en}")
	private String IDMS_USER_UPDATE_BLUE_EMAILTEMPLATE_EN;
	
	@Value("${user.default.email.template.cn}")
	private String IDMS_USER_DEFAULT_EMAILTEMPLATE_CN;
	
	@Value("${user.default.email.template.en}")
	private String IDMS_USER_DEFAULT_EMAILTEMPLATE_EN;
	
	@Value("${user.default.email.template.blue.cn}")
	private String IDMS_USER_DEFAULT_BLUE_EMAILTEMPLATE_CN;
	
	@Value("${user.default.email.template.blue.en}")
	private String IDMS_USER_DEFAULT_BLUE_EMAILTEMPLATE_EN;
	
	@Value("${send.invitation.email.template.en}")
	private String IDMS_SEND_INVITATION_EMAILTEMPLATE_EN;
	
	@Value("${send.invitation.email.template.cn}")
	private String IDMS_SEND_INVITATION_EMAILTEMPLATE_CN;
	
	@Value("${send.invitation.email.template.blue.en}")
	private String IDMS_SEND_INVITATION_BLUE_EMAILTEMPLATE_EN;
	
	@Value("${send.invitation.email.template.blue.cn}")
	private String IDMS_SEND_INVITATION_BLUE_EMAILTEMPLATE_CN;
	
	//OTP Templates
	@Value("${user.reset.password.otp.email.template.cn}")
	private String IDMS_USER_RESET_PASSWORD_OTP_EMAILTEMPLATE_CN;
	
	@Value("${user.reset.password.otp.email.template.en}")
	private String IDMS_USER_RESET_PASSWORD_OTP_EMAILTEMPLATE_EN;
	
	@Value("${user.registration.withpwd.otp.email.template.cn}")
	private String IDMS_USER_REGISTRATION_WITHPWD_OTP_EMAILTEMPLATE_CN;
	
	@Value("${user.registration.withpwd.otp.email.template.en}")
	private String IDMS_USER_REGISTRATION_WITHPWD_OTP_EMAILTEMPLATE_EN;
	
	@Value("${user.update.otp.email.template.cn}")
	private String IDMS_USER_UPDATE_OTP_EMAILTEMPLATE_CN;
	
	@Value("${user.update.otp.email.template.en}")
	private String IDMS_USER_UPDATE_OTP_EMAILTEMPLATE_EN;
	
	@Value("${user.default.otp.email.template.cn}")
	private String IDMS_USER_DEFAULT_OTP_EMAILTEMPLATE_CN;
	
	@Value("${user.default.otp.email.template.en}")
	private String IDMS_USER_DEFAULT_OTP_EMAILTEMPLATE_EN;
	
	@Value("${user.add.otp.email.template.cn}")
	private String IDMS_USER_OTP_ADD_EMAILTEMPLATE_CN;
	
	@Value("${user.add.otp.email.template.en}")
	private String IDMS_USER_OTP_ADD_EMAILTEMPLATE_EN;
	
	//CODE-RE-STRUCTURING - 3-Feb-19 merge
	@Value("${user.add.email.template.cn}")
	private String IDMS_USER_ADD_EMAILTEMPLATE_CN;
	
	@Value("${user.add.email.template.en}")
	private String IDMS_USER_ADD_EMAILTEMPLATE_EN;
	
	@Value("${user.add.email.template.blue.cn}")
	private String IDMS_USER_ADD_BLUE_EMAILTEMPLATE_CN;
	
	@Value("${user.add.email.template.blue.en}")
	private String IDMS_USER_ADD_BLUE_EMAILTEMPLATE_EN;
	
	@Value("${idmsc.emailUserNameFormat}")
	private String defaultUserNameFormat;
	
	@Value("${prm.self.registration.email.template.cn}")
	private String PRM_SELF_USER_REGESTRATION_EMAILTEMPLATE_CN;
	
	@Value("${prm.self.registration.email.template.en}")
	private String PRM_SELF_USER_REGESTRATION_EMAILTEMPLATE_EN;

	@Value("${prm.internal.registration.email.template.cn}")
	private String PRM_INTERNAL_USER_REGESTRATION_EMAILTEMPLATE_CN;
	
	@Value("${prm.internal.registration.email.template.en}")
	private String PRM_INTERNAL_USER_REGESTRATION_EMAILTEMPLATE_EN;
	
	@Value("${prm.eclipse.registration.email.template.cn}")
	private String PRM_ECLIPSE_USER_REGESTRATION_EMAILTEMPLATE_CN;
	
	@Value("${prm.eclipse.registration.email.template.en}")
	private String PRM_ECLIPSE_USER_REGESTRATION_EMAILTEMPLATE_EN;
	
	// Dynamic Email Templates
	@Value("${user.registration.withpwd.otp.email.template}")
	private String IDMS_USER_REGISTRATION_WITHPWD_OTP_EMAILTEMPLATE;
	
	@Value("${user.registration.withpwd.email.template}")
	private String IDMS_USER_REGISTRATION_WITHPWD_EMAILTEMPLATE;
	
	@Value("${prm.internal.registration.email.template}")
	private String PRM_INTERNAL_USER_REGISTRATION_EMAILTEMPLATE;
	
	@Value("${user.reset.password.otp.email.template}")
	private String IDMS_USER_RESET_PASSWORD_OTP_EMAILTEMPLATE;
	
	@Value("${user.reset.password.email.template}")
	private String IDMS_USER_RESET_PASSWORD_EMAILTEMPLATE;
	
	@Value("${user.add.otp.email.template}")
	private String IDMS_USER_OTP_ADD_EMAILTEMPLATE;
	
	@Value("${user.add.email.template}")
	private String IDMS_USER_ADD_EMAILTEMPLATE;
	
	@Value("${user.update.otp.email.template}")
	private String IDMS_USER_UPDATE_OTP_EMAILTEMPLATE;
	
	@Value("${user.update.email.template}")
	private String IDMS_USER_UPDATE_EMAILTEMPLATE;
	
	@Value("${user.change.email.notification.template}")
	private String IDMS_USER_CHANGE_EMAILTEMPLATE;
	
	public String getDefaultUserNameFormat() {
		return defaultUserNameFormat;
	}

	public void setDefaultUserNameFormat(String defaultUserNameFormat) {
		this.defaultUserNameFormat = defaultUserNameFormat;
	}

	private UserServiceImpl userService;
	
	@Autowired
	public void setUserService(UserService userService) {
		this.userService = (UserServiceImpl) userService;
	}
	
	private static StringBuilder contentBuilder = null;
	private static String content = null;
	private static String encodedHOTPcode = null;
	
	String smtpHostName = null;
	String smtpHostPort = null;
	String smtpUserName = null;
	String smtpUserPassword = null;
	String smtpSSLEnabled = null;
	String hotpOperationType = null;
	String hotpLanguage = null;
	String hotpPasswordRequired = null;
	String uid = null;
	String lang = null;
	String appid = null;
	String url = null;
	String cn = null;
	String invID = null;
	
	boolean sslEnabled = true;
	
	StringBuilder mailBuilder = null;
	
	Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
	DocumentContext productDocCtxUser = null, productDJData = null;

	private static final Logger LOGGER = LoggerFactory.getLogger(SendEmail.class);
	

	@Inject// for sending simple mails
	private JavaMailSender  mailSender;
 
	@Async
	public void emailReadyToSendEmail(String to, String from, String subject, String msgBody) {
		LOGGER.info("Entered emailReadyToSendEmail() -> Start");
		LOGGER.info("Parameter to -> " + to+" ,from -> "+from+" ,subject -> "+subject);
		MimeMessage mailMessage = mailSender.createMimeMessage();
		InternetAddress fromAddress = null;
		InternetAddress toAddress = null;
		try {
			
			fromAddress = new InternetAddress(from);
			toAddress = new InternetAddress(to);

			mailMessage.setSubject(subject, "utf-8");
			mailMessage.setText(msgBody, "utf-8", "html");
			mailMessage.setFrom(fromAddress);
			mailMessage.setRecipient(RecipientType.TO, toAddress);
			
			LOGGER.info("Start: sending email to:"+to);
			mailSender.send(mailMessage); 
			LOGGER.info("End: sending email finished to:"+to);
		} 
		catch (SMTPSendFailedException e) {
			LOGGER.error("ECODE-MAIL-SMTP-ERROR : SMTP error while sending mail");
			LOGGER.error("SMTPSendFailedException while sending email to "+to+" :: -> " + e.getMessage(),e);
		}
		catch (MessagingException e) {
			LOGGER.error("MessagingException while sending email to "+to+" :: -> " + e.getMessage(),e);
		}
		catch (Exception e) {
			LOGGER.error("ECODE-MAIL-GEN-ERROR : Generic error while sending mail");
			LOGGER.error("Exception while sending email to "+to+" :: -> " + e.getMessage(),e);
		}
	}
	
	
	public void  sendOpenAmEmail(String token, String code, String hotpOperationType,String userId, String appid, String pathString){
		LOGGER.info("Entered sendOpenAmEmail() -> Start");
		LOGGER.info("Parameter hotpOperationType -> "+hotpOperationType+" ,userId -> "+userId);
		LOGGER.info("Parameter appid -> " + appid);
			
		String userData = "";
		String to = "" ;
		String subject = "";
		String lang= "";
		String firstName = "";
		String aLink="";
		String linkParam="";
		String emailUserNameFormat = null;
		String attribute="";
		String appNameParam="";
		String prmTemplate=null;
		boolean isOTPEnabled = false;
		boolean isDynamicEmailEnabled = false;
			try {
				encodedHOTPcode = code;
				LOGGER.info("Start: getUser() of openamService for userId="+userId);
				userData = productService.getUser(userService.getSSOToken(), userId);
				LOGGER.info("End: getUser() of openamService finished for userId="+userId);
				productDocCtxUser = JsonPath.using(conf).parse(userData);
				
				// read support link url by passing appid
				LOGGER.info("Start: getUser() of OpenDjService for appId="+appid);
				Response applicationDetails = openDJService.getUser(djUserName, djUserPwd, appid);
				LOGGER.info("End: getUser() of OpenDjService finished for appId="+appid);
				
				
				productDJData = JsonPath.using(conf).parse(IOUtils.toString((InputStream) applicationDetails.getEntity()));
				String bfoSupportUrl = productDJData.read(JsonConstants.BFO_SUPPORT_URL);
				LOGGER.info("bfoSupportUrl = "+bfoSupportUrl);
				
				String templateColor = productDJData.read("_IDMS_Application_CSS");
				LOGGER.info("templateColor: "+templateColor);
				// For email name configuration 
				if (null != applicationDetails && 200 == applicationDetails.getStatus()) {
					String userNameFormatOpenDJ = productDJData.read("_userNameFormat");
					LOGGER.info("userNameFormatOpenDJ:"+userNameFormatOpenDJ);
					LOGGER.info("defaultUserNameFormat:"+defaultUserNameFormat);
					if(null != userNameFormatOpenDJ && !userNameFormatOpenDJ.isEmpty()){
						emailUserNameFormat = userNameFormatOpenDJ;
					} else{
						emailUserNameFormat = defaultUserNameFormat;
					}
					//check if otp option is enabled for app
					String isOTPEnabledForApp = productDJData.read("_isOTPEnabled");
					if(null!=isOTPEnabledForApp && !isOTPEnabledForApp.equals("")) {
						isOTPEnabled = Boolean.valueOf(isOTPEnabledForApp);
						LOGGER.info("isOTPEnabled: "+ isOTPEnabled);
					}
					//check if dynamic email template is enabled for app
					String isDynamicEmailEnabledForApp = productDJData.read("_isDynamicEmailEnabled");
					if(null!=isDynamicEmailEnabledForApp && !isDynamicEmailEnabledForApp.equals("")) {
						isDynamicEmailEnabled = Boolean.valueOf(isDynamicEmailEnabledForApp);
						LOGGER.info("isDynamicEmailEnabled: "+ isDynamicEmailEnabled);
					}
				}
				if (null != applicationDetails && 200 != applicationDetails.getStatus()) {
					emailUserNameFormat = defaultUserNameFormat;
				}
			    if(hotpOperationType.equalsIgnoreCase(EmailConstants.UPDATEUSERRECORD_OPT_TYPE)){
					subject=appid;
					to = productDocCtxUser.read("$.newmail[0]");
				}else if(hotpOperationType.equalsIgnoreCase(EmailConstants.ADDEMAILUSERRECORD_OPT_TYPE)){
					subject=appid;
					to = productDocCtxUser.read("$.mail[0]");
				} else {
					subject=productDocCtxUser.read("$.registerationSource[0]");
					to = productDocCtxUser.read("$.mail[0]");
				}
				lang=productDocCtxUser.read("$.preferredlanguage[0]");

				if(emailUserNameFormat.equalsIgnoreCase(UserConstants.FIRST_NAME))
					firstName=productDocCtxUser.read("$.givenName[0]");
				else if(emailUserNameFormat.equalsIgnoreCase(UserConstants.LAST_NAME))
					firstName=productDocCtxUser.read("$.sn[0]");
				else if(emailUserNameFormat.equalsIgnoreCase(UserConstants.FULL_NAME))
					firstName=productDocCtxUser.read("$.cn[0]");
				else
					firstName=productDocCtxUser.read("$.givenName[0]");
				LOGGER.info("Email format Name:"+firstName);
				aLink = productDocCtxUser.read("$.alink[0]");
				LOGGER.info("sendOpenAmEmail** alink:"+aLink);
				//for PRM - mySchneiderPartnerPortal user
				if(subject.equalsIgnoreCase(UserConstants.PRM_APP_NAME)){
					attribute=productDocCtxUser.read("$.RegistrationAttributes__c[0]");
					firstName=productDocCtxUser.read("$.cn[0]");//In case of mySchneiderPartnerPortal
					if(attribute!=null && !attribute.isEmpty()){
						ObjectMapper mapper = new ObjectMapper();
						//Convert JSON array to List of RegistrationAttributes objects
						List<RegistrationAttributes> attributeList = Arrays.asList(mapper.readValue(attribute, RegistrationAttributes[].class));
						for (int i = 0; i < attributeList.size(); i++) {
							String KeyName = attributeList.get(i).getKeyName();
							String KeyValue = attributeList.get(i).getKeyValue();
							LOGGER.info("KeyName = " + KeyName + " and KeyValue =" + KeyValue);
							if (KeyName.equalsIgnoreCase("prmRegEmailType") && null != KeyValue && !KeyValue.isEmpty()) {
								LOGGER.info("inside prmRegEmailType  block");
								prmTemplate=KeyValue;
							}
							else if (KeyName.equalsIgnoreCase("prmAppName") && null != KeyValue && !KeyValue.isEmpty()) {
								LOGGER.info("inside prmAppName  block");
								appNameParam="&appName="+KeyValue;
							}
						}
						if(prmTemplate==null){
							prmTemplate=UserConstants.PRM_SELF_REG_EMAIL;
						}
					}
					else{
						//select default self registration template.Pass the template to emailContentTemplate method or have one global variable and assign it the value 
						//of prmTemplate
						prmTemplate=UserConstants.PRM_SELF_REG_EMAIL;
					}
				}
				//Setting aLink in case of User Registration
				if(hotpOperationType.equalsIgnoreCase(EmailConstants.USERREGISTRATION_OPT_TYPE)){
					linkParam=(aLink==null | aLink =="") ? "" : ("&alink="+aLink);
					linkParam=linkParam+appNameParam;//Appending appName parameter for mySchneiderPartnerPortal.
				}
				LOGGER.info("linkParam: "+linkParam);
				
				if(isOTPEnabled){
					token = code;
				}
				if(!(hotpOperationType.equalsIgnoreCase(EmailConstants.SETUSERPWD_OPT_TYPE)
						|| hotpOperationType.equalsIgnoreCase(EmailConstants.USERREGISTRATION_OPT_TYPE)
						|| hotpOperationType.equalsIgnoreCase(EmailConstants.UPDATEUSERRECORD_OPT_TYPE))){
					token = code;
				}

				String mailDomain = to.substring(to.indexOf("@") + 1);

				LOGGER.info("mailDomain in sendOpenAmEmail= " + mailDomain);
				if(mailDomain.contains(UserConstants.YOP_MAIL)){
					url = hotpEmailVerificationURL + "?userid=" + userId + "&amp;pin=" + token + "&amp;operationType="
							+ hotpOperationType + "&amp;app=" + appid + "&amp;uid=" + userId+linkParam;
					if (null != pathString && !pathString.isEmpty()){
						pathString = pathString.replaceAll("&", "&amp;");
						url = url + "&amp;" + pathString;
					}
				}
				else{
					url = hotpEmailVerificationURL + "?userid=" + userId + "&pin=" + token + "&operationType="
							+ hotpOperationType + "&app=" + appid + "&uid=" + userId+linkParam;
					if (null != pathString && !pathString.isEmpty())
						url = url + "&" + pathString;
				}
				
				LOGGER.info("sendOpenAmEmail : URL compiled to : " + url);
				contentBuilder = new StringBuilder();
				contentBuilder.setLength(0);
				LOGGER.info("Before emailContentTemplate call first name:"+firstName);
				// if section for chinese user
				if ((lang != null
					&& (lang.equalsIgnoreCase("zh") || lang.equalsIgnoreCase("zh_cn") || lang.equalsIgnoreCase("zh_tw")))
					|| (hotpLanguage != null && (hotpLanguage.equalsIgnoreCase("zh")
							|| hotpLanguage.equalsIgnoreCase("zh_cn") || hotpLanguage.equalsIgnoreCase("zh_tw")))) {
					LOGGER.info("sendOpenAmEmail :  Building Chinese email content..for.."+to);
					subject = emailContentTemplate(to, subject, EmailConstants.HOTP_LAN_CN,hotpOperationType,firstName,bfoSupportUrl,prmTemplate,templateColor, isOTPEnabled, isDynamicEmailEnabled);
				}
				// Else section for English user
				else {
					LOGGER.info("sendOpenAmEmail :  Building English email content..for.."+to);
					subject = emailContentTemplate(to, subject, EmailConstants.HOTP_LAN_EN,hotpOperationType,firstName,bfoSupportUrl,prmTemplate,templateColor, isOTPEnabled, isDynamicEmailEnabled);

				}

				String tos[] = new String[1];
				tos[0] = to;

				LOGGER.info("sendOpenAmEmail : to : " + to);
				LOGGER.info("sendOpenAmEmail : subject : " + subject);
				LOGGER.info("sendOpenAmEmail : content.isEmpty : " + content.isEmpty());
				LOGGER.info("sendOpenAmEmail : from : " + from);

				emailReadyToSendEmail(to, from, subject, content);

				if (LOGGER.isDebugEnabled()) {
					LOGGER.info("sendOpenAmEmail : " + "HOTP sent to : " + to + ".");
				}
			} catch (Exception e) {
				LOGGER.error("ECODE-MAIL-DESPATCH-OAM-FAILED : Error sending OpenAM Email");
				LOGGER.info("Exception in sendOpenAmEmail() => "+e.getMessage(),e);
			}
	}
	
	public String generateOtp(String userId) throws Exception {
		LOGGER.info("Entered generateOtp() -> Start");
		LOGGER.info("Parameter userId -> " + userId);

		String hexpin = "";
		String product_json_string = "";
		String pin = "";
		pin = generateRamdomPin();
		hexpin = ChinaIdmsUtil.generateHashValue(pin);
		
		LocalDateTime currentDatenTime = LocalDateTime.now();
		long currentDatenTimeInMillisecs = currentDatenTime.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
		
		hexpin = hexpin+":"+currentDatenTimeInMillisecs;
		// open Attribute AuthID and Timestamp
		product_json_string = "{" + "\"authId\": \"" + hexpin + "\"}";
		// Need add the timestamp
		// update hashkey in openAM.
		LOGGER.info("Start: updateUser() of openamservice to update hashkey for userId:"+userId);
		productService.updateUser(UserConstants.CHINA_IDMS_TOKEN+userService.getSSOToken(), userId,
				product_json_string);
		LOGGER.info("End: updateUser() of openamservice to update hashkey finished for userId:"+userId);
		return pin;
	}
	
	public String generateEmailToken(String userId) throws Exception {
		LOGGER.info("Entered generateEmailToken() -> Start");
		LOGGER.info("Parameter userId -> " + userId);

		String hexpin = "";
		String product_json_string = "";
		String pin = "";
		pin = generateRandomToken();
		hexpin = ChinaIdmsUtil.generateHashValue(pin);
		
		LocalDateTime currentDatenTime = LocalDateTime.now();
		long currentDatenTimeInMillisecs = currentDatenTime.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
		
		hexpin = hexpin+":"+currentDatenTimeInMillisecs;
		product_json_string = "{" + "\"authId\": \"" + hexpin + "\"}";
		LOGGER.info("Start: updateUser() of openamservice to update hashkey for userId:"+userId);
		productService.updateUser(UserConstants.CHINA_IDMS_TOKEN+userService.getSSOToken(), userId,
				product_json_string);
		LOGGER.info("End: updateUser() of openamservice to update hashkey finished for userId:"+userId);
		return pin;
	}
	
	public void storePRMOtp(String userId, String hashedPin) throws Exception {
		LOGGER.info("Entered storePRMOtp() -> Start");

		String product_json_string = "";
		LOGGER.info("Parameter userId -> " + userId);
		
		LocalDateTime currentDatenTime = LocalDateTime.now();
		long currentDatenTimeInMillisecs = currentDatenTime.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
		
		hashedPin = hashedPin+":"+currentDatenTimeInMillisecs;
		// open Attribute AuthID and Timestamp
		product_json_string = "{" + "\"authId\": \"" + hashedPin + "\"}";
		// update hashkey in openAM.
		//LOGGER.info("hashedPin is " + hashedPin);
		LOGGER.info("Start: updateUser() of openamservice to store PRM hashedPin");
		productService.updateUser(UserConstants.CHINA_IDMS_TOKEN+userService.getSSOToken(), userId,
				product_json_string);
		LOGGER.info("End: updateUser() of openamservice to store PRM hashedPin finished");

	}
	
	public boolean validatePin(String otp,String userId)throws Exception {
		LOGGER.info("Entered validatePin() -> Start");
		LOGGER.info("Parameter userId -> " + userId);
		boolean validatePin = false;
		String authIdTimeStmp = "";
		String[] authIdTime = null;
			// convert otp to hash
			String newHashedValue = ChinaIdmsUtil.generateHashValue(otp);
			LOGGER.info("hexa string is:" + newHashedValue);

			// get user details to get stored hashkey.
			LOGGER.info("Start: getUser() of openamservice to get stored hashkey");
			String userData = productService.getUser(userService.getSSOToken(), userId);
			LOGGER.info("End: getUser() of openamservice to get stored hashkey finished");
			Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
			DocumentContext productDocCtx = JsonPath.using(conf).parse(userData);

			authIdTimeStmp = productDocCtx.read("$.authId[0]");
			
			if(null == authIdTimeStmp){
				authIdTimeStmp = productDocCtx.read("$.AuthID[0]");
			}
			if ((null != authIdTimeStmp && !authIdTimeStmp.isEmpty()) && (!"[]".equals(authIdTimeStmp))) {
				authIdTime = authIdTimeStmp.split(":");

				String storedHashedValue = authIdTime[0];

				long localDTInMilli = Long.valueOf(authIdTime[1]).longValue();
				// figure out login identifier type
				String emailOrMobile = productDocCtx.read("$.mail[0]");
				String loginIdentifierType = UserConstants.EMAIL;
				if (null == emailOrMobile) {
					emailOrMobile = productDocCtx.read("$.mobile_reg[0]");
					loginIdentifierType = UserConstants.MOBILE;
				}
				LOGGER.info("loginIdentifierType: "+loginIdentifierType);
				// compare Stored hashkey and generated hash key
				if (newHashedValue.equals(storedHashedValue) && checkTimeStamp(localDTInMilli, loginIdentifierType)) {
					validatePin = true;
				}
			}else{
				LOGGER.error("Some problem in validatePin() for userId="+userId);
				LOGGER.error("ECODE-VALIDATE-PIN-PROC-ERROR : Error during validate PIN");
				throw new Exception("inValid Pin Exception!!!");
			}
		return validatePin;
	}

	private String emailContentTemplate(String to, String subject, String lang,String hotpOperationType,String firstName, String bfoSupportUrl,String prmTemplate, String templateColor, boolean isOTPEnabled, boolean isDynamicEmailEnabled)  {
		LOGGER.info("Entered emailContentTemplate() -> Start");
		LOGGER.info("Parameter to -> " + to+" ,subject -> "+subject);//Senthil consider this to handle PRM scenario
		LOGGER.info("Parameter lang -> " + lang+" ,hotpOperationType -> "+hotpOperationType);
		LOGGER.info("firstName emailContentTemplate -> " + firstName);
		LOGGER.info("templateColor -> " + templateColor);
		String filePath;
		boolean chineseLangCheck = ((lang != null && lang.equalsIgnoreCase(EmailConstants.HOTP_LAN_CN)) || (hotpLanguage != null && hotpLanguage.equalsIgnoreCase(EmailConstants.HOTP_LAN_CN)));
		int startIndex=0;
		int endIndex=0;

		boolean isSupportedOperationType = false;
		if(hotpOperationType != null && !(EmailConstants.SENDINVITATION_OPT_TYPE.equalsIgnoreCase(hotpOperationType))) {
			isSupportedOperationType = true;
		}
		//changes for dynamic email templates
		/*
		 * This check is required to ensure that dynamic email template implementation is called
		 * only for user registration operation type at this point in time. Once dynamic
		 * email templates for other flows are implemented, this check will be removed.
		 */
		if(isDynamicEmailEnabled && isSupportedOperationType) {
			return buildDynamicEmailTemplate(subject, hotpOperationType,firstName,
					bfoSupportUrl, prmTemplate, templateColor, isOTPEnabled, chineseLangCheck);
		}
		// Below Static Templates will be refactored once dynamic email template 
		// implementation is complete.
		if (isOTPEnabled) {
		// changes for otp based email templates
			LOGGER.info("isOTPEnabled: "+ isOTPEnabled + " templateColor: "+templateColor);
			filePath = refactoredCode(subject, hotpOperationType, prmTemplate, templateColor, isOTPEnabled,
					chineseLangCheck);
		} else {
			filePath = oldCode(subject, hotpOperationType, prmTemplate, templateColor, chineseLangCheck);
		}
		
		try(FileReader file = new FileReader(filePath); BufferedReader in = new BufferedReader(file)) {
			String str;
			
			while ((str = in.readLine()) != null) {
				contentBuilder.append(str);
			}
		} catch (IOException e) {
			LOGGER.error("Executing while emailContentTemplate :: -> " + e.getMessage(),e);
		}
		
		if(null != invID &&  EmailConstants.SENDINVITATION_OPT_TYPE.equalsIgnoreCase(hotpOperationType)){
			 startIndex = contentBuilder.indexOf("{!invtID}");
			 endIndex = startIndex + 9;
			contentBuilder.replace(startIndex, endIndex, invID);
			
			startIndex = contentBuilder.indexOf("{!firstname}");
			endIndex = startIndex + 12;
			contentBuilder.replace(startIndex, endIndex, firstName);
			
			startIndex = contentBuilder.indexOf("{!invtURL}");
			endIndex = startIndex+10;
		}else{

			if(hotpOperationType != null && EmailConstants.USERREGISTRATION_OPT_TYPE.equalsIgnoreCase(hotpOperationType)){

				if(0 < contentBuilder.indexOf("{!registrationSource}")){
					startIndex = contentBuilder.indexOf("{!registrationSource}");
					endIndex = startIndex + 21;
					contentBuilder.replace(startIndex, endIndex, subject);
				}
			}
			startIndex = contentBuilder.indexOf("{!firstname}");
			endIndex = startIndex + 12;
			contentBuilder.replace(startIndex, endIndex, firstName);
			startIndex = contentBuilder.indexOf("{!url}");
			endIndex = startIndex+6;
		}
		
		if (hotpOperationType != null && EmailConstants.SETUSERPWD_OPT_TYPE.equalsIgnoreCase(hotpOperationType)) {
			if (chineseLangCheck) {
				subject = subject + EmailConstants.HYPHEN + EmailConstants.SCHE_PWD_RESET_CN;
			} else {
				subject = subject + EmailConstants.HYPHEN + EmailConstants.SCHE_PWD_RESET_EN;
			}
		} else if (hotpOperationType != null && EmailConstants.USERREGISTRATION_OPT_TYPE.equalsIgnoreCase(hotpOperationType)) {
			if (chineseLangCheck) {
				subject = subject + EmailConstants.HYPHEN + EmailConstants.COMPLETE_REG_CN;
			} else {
				subject = subject + EmailConstants.HYPHEN + EmailConstants.COMPLETE_REG_EN;
			}
		} else if (hotpOperationType != null && EmailConstants.UPDATEUSERRECORD_OPT_TYPE.equalsIgnoreCase(hotpOperationType)) {
			if (chineseLangCheck) {
				subject = subject + EmailConstants.HYPHEN + EmailConstants.SCHE_LOGINID_CHANGE_NOTIFICATION_CN;
			} else {
				subject = subject + EmailConstants.HYPHEN + EmailConstants.SCHE_LOGINID_CHANGE_NOTIFICATION_EN;
			}
		} else if (hotpOperationType != null && EmailConstants.ADDEMAILUSERRECORD_OPT_TYPE.equalsIgnoreCase(hotpOperationType)) {
			if (chineseLangCheck) {
				subject = subject + EmailConstants.HYPHEN + EmailConstants.SCHE_ADD_EMAIL_NOTIFICATION_CN;
			} else {
				subject = subject + EmailConstants.HYPHEN + EmailConstants.SCHE_ADD_EMAIL_NOTIFICATION_EN;
			}
		} else if (hotpOperationType != null && EmailConstants.SENDINVITATION_OPT_TYPE.equalsIgnoreCase(hotpOperationType)) {
			if (chineseLangCheck) {
				subject = subject + EmailConstants.HYPHEN + EmailConstants.SCHE_SEND_INVITATION_NOTIFICATION_CN;
			} else {
				subject = subject + EmailConstants.HYPHEN + EmailConstants.SCHE_SEND_INVITATION_NOTIFICATION_EN;
			}
		} else {
			if (chineseLangCheck) {
				subject = subject + EmailConstants.HYPHEN + EmailConstants.OPENAM_OTP_CN;
			} else {
				subject = subject + EmailConstants.HYPHEN + EmailConstants.OPENAM_OTP_EN;
			}
		}
		LOGGER.info("subject="+subject);
		if (isOTPEnabled) {
			LOGGER.info("isOTPEnabled: "+ isOTPEnabled + " templateColor: "+templateColor);
			startIndex = contentBuilder.indexOf("{!otp}");
			endIndex = startIndex+6;
			contentBuilder.replace(startIndex, endIndex, encodedHOTPcode);
		}else {
			contentBuilder.replace(startIndex, endIndex, url);
		}
		
		if(null != bfoSupportUrl && !bfoSupportUrl.isEmpty()){
			LOGGER.info("now changing support email to support link");
			if(0 < contentBuilder.indexOf(EmailConstants.USERREGISTRATION_SUPPORT_LINK)){
				startIndex = contentBuilder.indexOf(EmailConstants.USERREGISTRATION_SUPPORT_LINK);
				endIndex = startIndex + EmailConstants.USERREGISTRATION_SUPPORT_LINK.length();
				contentBuilder.replace(startIndex, endIndex, bfoSupportUrl);
			}
		}

		 content = contentBuilder.toString();
		 return subject;
	}

	private String buildDynamicEmailTemplate(String appName, String hotpOperationType, String firstName,
			String bfoSupportUrl, String prmTemplate, String templateColor, boolean isOTPEnabled,
			boolean chineseLangCheck) {
		
		DynamicEmailTemplateInput input = new DynamicEmailTemplateInput();
		if (chineseLangCheck)
			input.setLocale(Locale.CN);
		else
			input.setLocale(Locale.EN);

		input.setOperationType(OperationType.getKey(hotpOperationType));
		input.setOTPEnabled(isOTPEnabled);
		input.setEtColor(EmailTemplateColor.getKey(templateColor));
		if (appName.equalsIgnoreCase(UserConstants.PRM_APP_NAME) && prmTemplate != null)
			input.setPRMApp(true);
		input.setPrmTemplateType(PRMTemplateType.getKey(prmTemplate));
		input.setConfiguration(this);
		input.setBfoSupportUrl(bfoSupportUrl);
		input.setSubject(appName);
		input.setAppName(appName);
		input.setFirstName(firstName);
		input.setOtp(encodedHOTPcode);
		input.setConfirmationURL(url);
		DynamicEmailTemplateFactory factory = new DynamicEmailTemplateFactoryImpl(input);
		String filePath = factory.getEmailTemplate().getEmailTemplatePath();
		LOGGER.info("Dynamic Email FilePath: " + filePath);
		
		try {
			Response applicationDetails = openDJService.getEmailTemplateDetails(djUserName, djUserPwd,
					input.getOperationType().getOpenDJType());
			LOGGER.info("End: getEmailTemplateDetails() of OpenDjService =" + applicationDetails.getStatus());

			DocumentContext emailTemplateDJData = JsonPath.using(conf).parse(IOUtils.toString((InputStream) applicationDetails.getEntity()));
			String djDataAsJSON = emailTemplateDJData.jsonString();
			ObjectMapper objectMapper = new ObjectMapper();
			OpenDJAttributes openDJAttributes = objectMapper.readValue(djDataAsJSON, OpenDJAttributes.class);
			
			String fileContentString = Files.readAllLines(Paths.get(filePath), Charset.forName("UTF-8")).stream().collect(Collectors.joining("\n"));
			String[] searchList = getDynamicEmailSearchList(input);

			// set the correct color code for email templates based on templateColor
			setColorCode(templateColor, openDJAttributes);

			String[] replacementList = getDynamicEmailReplacementList(input,openDJAttributes);
			LOGGER.info("SearchList length: "+ searchList.length);
			LOGGER.info("ReplacementList length: "+ replacementList.length);
			String updatedContent = StringUtils.replaceEach(fileContentString, searchList, replacementList);
//			System.out.println("dynamicEmailContent: \n "+ updatedContent);
			content = updatedContent;
		} catch (IOException e) {
			LOGGER.error("ECODE-OPENDJ-DATA-RETREIVAL-FAILED : Error fetching OpenDJ Data");
			LOGGER.info("Exception in getting OPENDJ data() => "+e.getMessage(),e);
		}
		return input.getSubject();
	}

	private void setColorCode(String templateColor, OpenDJAttributes openDJAttributes) {
		String[] tcCodeArray = openDJAttributes.get_bodyColorCode();
		for(String tcCode: tcCodeArray) {
			StringTokenizer tokens = new StringTokenizer(tcCode, "=");
			int noOfTokens = tokens.countTokens();
			boolean hasEvenTokens = noOfTokens%2 == 0;
			while(hasEvenTokens && tokens.hasMoreTokens()){
				 String color = tokens.nextToken();
				 if(StringUtils.isNotBlank(templateColor) && templateColor.equalsIgnoreCase(color)) {
					 openDJAttributes.set_bodyColorCode(new String[] {tokens.nextToken()});
					 break;
				 }else {
					 tokens.nextToken();
				 }
			 }
			if(openDJAttributes.get_bodyColorCode().length == 1) {
				LOGGER.info("Template Color code: " + openDJAttributes.get_bodyColorCode()[0]);
				break;
			}
		}
	}

	private String[] getDynamicEmailReplacementList(DynamicEmailTemplateInput input, OpenDJAttributes openDJAttributes) {
		
		DynamicTemplatePHSubstitutorFactory substitutorFactory = new DynamicTemplatePHSubstitutorFactoryImpl(input, openDJAttributes);
		TemplatePlaceholderSubstitutor substitutor = substitutorFactory.getTemplatePlaceholderSubstitutor();
		substitutor.buildDynamicEmailPlaceholderValues();
		
		List<String> placeholderValues =substitutor.getPlaceholderValues();
		String[] replacementList = placeholderValues.toArray(new String[placeholderValues.size()]);
		return replacementList;
	}

	private String[] getDynamicEmailSearchList(DynamicEmailTemplateInput input) {
		
		TemplatePlaceholderCreator placeHolder = new LinkTemplatePlaceholderCreator();
		boolean isEmailNotificationType = false;
		if(OperationType.CHANGE_EMAIL_NOTIFICATION.getType().equals(input.getOperationType().getType())) {
			isEmailNotificationType = true;
		}
		if(!isEmailNotificationType && input.isOTPEnabled()) {
			placeHolder = new OTPTemplatePlaceholderCreator();
		}
		if(!isEmailNotificationType && input.isPRMApp() && PRMTemplateType.PRM_INTERNAL_REGISTRATION.equals(input.getPrmTemplateType())) {
			placeHolder = new PRMInternalRegTemplatePHCreator();
		}
		placeHolder.buildDynamicEmailPlaceholders();
		List<String> placeholders = placeHolder.getPlaceholders();
		String[] searchList = placeholders.toArray(new String[placeholders.size()]);
		return searchList;
	}

	private String refactoredCode(String subject, String hotpOperationType, String prmTemplate, String templateColor,
			boolean isOTPEnabled, boolean chineseLangCheck) {
		String filePath;

		/* Check for combination of isOTPEnabled and IDMS_Application_CSS application 
		 * attributes. Currently isOTPEnabled flag is supported with Green proface which
		 * is the default color.
		 * If blue proface is enabled with isOTPEnabled flag for an app, this check will 
		 * ensure that user receives a default otp email. This will be refactored once 
		 * dynamic email template is implemented.
		*/
		if(isOTPEnabled && StringUtils.isNotBlank(templateColor)) {
			return getDefaultTemplateForProfaceAndOTPCombination(chineseLangCheck);
		}

		EmailTemplateInput input = new EmailTemplateInput();
		if (chineseLangCheck)
			input.setLocale(Locale.CN);
		else
			input.setLocale(Locale.EN);

		input.setOperationType(OperationType.getKey(hotpOperationType));
		input.setOTPEnabled(isOTPEnabled);
		input.setEtColor(EmailTemplateColor.getKey(templateColor));
		if (subject.equalsIgnoreCase(UserConstants.PRM_APP_NAME) && prmTemplate != null)
			input.setPRMApp(true);
		input.setPrmTemplateType(PRMTemplateType.getKey(prmTemplate));
		input.setConfiguration(this);
		EmailTemplateAbstractFactory factory = new EmailTemplateAbstractFactoryImpl(input);
		filePath = factory.getEmailTemplateFactory().getEmailTemplatePath();
		return filePath;
	}

	private String getDefaultTemplateForProfaceAndOTPCombination(boolean chineseLangCheck) {
		if (chineseLangCheck)
			return IDMS_USER_DEFAULT_OTP_EMAILTEMPLATE_CN;
		else
			return IDMS_USER_DEFAULT_OTP_EMAILTEMPLATE_EN;
	}

	private String oldCode(String subject, String hotpOperationType, String prmTemplate, String templateColor,
			boolean chineseLangCheck) {
		String filePath;
		if (hotpOperationType != null && hotpOperationType.equalsIgnoreCase(EmailConstants.SETUSERPWD_OPT_TYPE)) {
			LOGGER.info("Inside SetUserPwd OperationType  :  " + hotpOperationType);
			if (chineseLangCheck) {
				if(null != templateColor && !templateColor.isEmpty() && templateColor.equalsIgnoreCase("Blue")){
					filePath = getIDMS_USER_REST_PASSWORD_BLUE_EMAILTEMPLATE_CN();
				}else
				filePath = IDMS_USER_REST_PASSWORD_EMAILTEMPLATE_CN;
			} else {
				if(null != templateColor && !templateColor.isEmpty() && templateColor.equalsIgnoreCase("Blue")){
					filePath = getIDMS_USER_REST_PASSWORD_BLUE_EMAILTEMPLATE_EN();
				}else
				filePath = IDMS_USER_REST_PASSWORD_EMAILTEMPLATE_EN;
			}
			LOGGER.info("filePath is"+filePath);
		} else if (hotpOperationType != null && hotpOperationType.equalsIgnoreCase(EmailConstants.USERREGISTRATION_OPT_TYPE)) {
			LOGGER.info("Inside userRegistration OperationType Create " + hotpOperationType);//senthil have logic to set correct prm template
			if (chineseLangCheck) {
				if(subject.equalsIgnoreCase(UserConstants.PRM_APP_NAME) && prmTemplate!=null){
					switch(prmTemplate) 
			        { 
			            case UserConstants.PRM_SELF_REG_EMAIL: 
			            	filePath=getPRM_SELF_USER_REGESTRATION_EMAILTEMPLATE_CN();
			                break; 
			            case UserConstants.PRM_INTERNAL_REG_EMAIL: 
			            	filePath=getPRM_INTERNAL_USER_REGESTRATION_EMAILTEMPLATE_CN(); 
			                break; 
			            case UserConstants.PRM_ECLIPSE_REG_EMAIL: 
			            	filePath=getPRM_ECLIPSE_USER_REGESTRATION_EMAILTEMPLATE_CN(); 
			                break; 
			            default: 
			            	filePath=getPRM_SELF_USER_REGESTRATION_EMAILTEMPLATE_CN(); 
			        } 
				}
				else{					
					if(null != templateColor && !templateColor.isEmpty() && templateColor.equalsIgnoreCase("Blue")){
						filePath = getIDMS_USER_REGESTRATION_WITHPWD_BLUE_EMAILTEMPLATE_CN();
					}else
					filePath = IDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_CN;
				}
			} else {
				if(subject.equalsIgnoreCase(UserConstants.PRM_APP_NAME) && prmTemplate!=null){
					switch(prmTemplate) 
			        { 
			            case UserConstants.PRM_SELF_REG_EMAIL: 
			            	filePath=getPRM_SELF_USER_REGESTRATION_EMAILTEMPLATE_EN();
			                break; 
			            case UserConstants.PRM_INTERNAL_REG_EMAIL: 
			            	filePath=getPRM_INTERNAL_USER_REGESTRATION_EMAILTEMPLATE_EN(); 
			                break; 
			            case UserConstants.PRM_ECLIPSE_REG_EMAIL: 
			            	filePath=getPRM_ECLIPSE_USER_REGESTRATION_EMAILTEMPLATE_EN(); 
			                break; 
			            default: 
			            	filePath=getPRM_SELF_USER_REGESTRATION_EMAILTEMPLATE_EN(); 
			        } 
				}
				else{
					if(null != templateColor && !templateColor.isEmpty() && templateColor.equalsIgnoreCase("Blue")){
						filePath = getIDMS_USER_REGESTRATION_WITHPWD_BLUE_EMAILTEMPLATE_EN();
					}else
					filePath = IDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_EN;
				}
			}
			LOGGER.info("filePath is"+filePath);
		} else if (hotpOperationType != null && hotpOperationType.equalsIgnoreCase(EmailConstants.UPDATEUSERRECORD_OPT_TYPE)) {
			LOGGER.info("Inside OperationType UpdateUserRecord " + hotpOperationType);
			if (chineseLangCheck) {
				if(null != templateColor && !templateColor.isEmpty() && templateColor.equalsIgnoreCase("Blue")){
					filePath = getIDMS_USER_UPDATE_BLUE_EMAILTEMPLATE_CN();
				}else
				filePath = IDMS_USER_UPDATE_EMAILTEMPLATE_CN;
			} else {
				if(null != templateColor && !templateColor.isEmpty() && templateColor.equalsIgnoreCase("Blue")){
					filePath = getIDMS_USER_UPDATE_BLUE_EMAILTEMPLATE_EN();
				}else
				filePath = IDMS_USER_UPDATE_EMAILTEMPLATE_EN;
			}
			LOGGER.info("filePath is"+filePath);
		} else if (hotpOperationType != null && hotpOperationType.equalsIgnoreCase(EmailConstants.ADDEMAILUSERRECORD_OPT_TYPE)) {
			LOGGER.info("Inside OperationType AddEmailUserRecord " + hotpOperationType);
			if (chineseLangCheck) {
				if(null != templateColor && !templateColor.isEmpty() && templateColor.equalsIgnoreCase("Blue")){
					filePath = getIDMS_USER_ADD_BLUE_EMAILTEMPLATE_CN();
				}else
				filePath = IDMS_USER_ADD_EMAILTEMPLATE_CN;
			} else {
				if(null != templateColor && !templateColor.isEmpty() && templateColor.equalsIgnoreCase("Blue")){
					filePath = getIDMS_USER_ADD_BLUE_EMAILTEMPLATE_EN();
				}else
				filePath = IDMS_USER_ADD_EMAILTEMPLATE_EN;
			}
			LOGGER.info("filePath is"+filePath);
		}else if (hotpOperationType != null && hotpOperationType.equalsIgnoreCase(EmailConstants.SENDINVITATION_OPT_TYPE)) {
			LOGGER.info("Inside OperationType sendInvitation " + hotpOperationType);
			if (chineseLangCheck) {
				if(null != templateColor && !templateColor.isEmpty() && templateColor.equalsIgnoreCase("Blue")){
					filePath = getIDMS_SEND_INVITATION_BLUE_EMAILTEMPLATE_CN();
				}else
				filePath = IDMS_SEND_INVITATION_EMAILTEMPLATE_CN;
			} else {
				if(null != templateColor && !templateColor.isEmpty() && templateColor.equalsIgnoreCase("Blue")){
					filePath = getIDMS_SEND_INVITATION_BLUE_EMAILTEMPLATE_EN();
				}else
				filePath = IDMS_SEND_INVITATION_EMAILTEMPLATE_EN;
			}
			LOGGER.info("filePath is"+filePath);
		} else {
			LOGGER.info("Inside Common OperationType " + hotpOperationType);
			if (chineseLangCheck) {
				if(null != templateColor && !templateColor.isEmpty() && templateColor.equalsIgnoreCase("Blue")){
					filePath = getIDMS_USER_DEFAULT_BLUE_EMAILTEMPLATE_CN();
				}else
				filePath = IDMS_USER_DEFAULT_EMAILTEMPLATE_CN;
			} else {
				if(null != templateColor && !templateColor.isEmpty() && templateColor.equalsIgnoreCase("Blue")){
					filePath = getIDMS_USER_DEFAULT_BLUE_EMAILTEMPLATE_EN();
				}else
				filePath = IDMS_USER_DEFAULT_EMAILTEMPLATE_EN;
			}
			LOGGER.info("filePath is"+filePath);
		}
		return filePath;
	}
	
	
	/**
	 * This method will generate the random password based on langUtils with
	 * string characters
	 * 
	 */

	private String generateRamdomPin() {
		LOGGER.info("Entered generateRamdomPin() -> Start");
		String tmpPr = RandomStringUtils.random(6, UserConstants.RANDOM_PIN_CHARS);
		return tmpPr;
	}
	
	private String generateRandomToken() {
		LOGGER.info("Entered generateRandomToken() -> Start");
		String tmpPr = RandomStringUtils.random(20, UserConstants.RANDOM_CHARS);
		return tmpPr;
	}
	
	private boolean checkTimeStamp(long localDTInMilli, String loginIdentifierType) {
		LOGGER.info("Entered checkTimeStamp() -> Start");
		LOGGER.info("Parameter localDTInMilli() ->"+ localDTInMilli);
		
		boolean validateTimeStamp = false;
		LocalDateTime otpGeneratedDatenTime = Instant.ofEpochMilli(localDTInMilli).atZone(ZoneId.systemDefault())
				.toLocalDateTime();
		if(null != loginIdentifierType && UserConstants.MOBILE.equals(loginIdentifierType)) {
			// mobile otp flow
			otpGeneratedDatenTime = otpGeneratedDatenTime.plusMinutes(15);
		}else {
			// email otp flow
			otpGeneratedDatenTime = otpGeneratedDatenTime.plusDays(7);
		}
		long expirationDateInMillisecs = otpGeneratedDatenTime.atZone(ZoneId.systemDefault()).toInstant()
				.toEpochMilli();

		LocalDateTime currentDatenTime = LocalDateTime.now();
		long currentDatenTimeInMillisecs = currentDatenTime.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
		LOGGER.info("currentDatenTimeInMillisecs: "+currentDatenTimeInMillisecs);
		LOGGER.info("expirationDateInMillisecs: "+expirationDateInMillisecs);

		if(currentDatenTimeInMillisecs < expirationDateInMillisecs){
			LOGGER.info("checkTimeStamp(): OTP timestamp validation OK! and checkTimeStamp() ended");
			 validateTimeStamp = true;
		}
		LOGGER.info("validation status="+validateTimeStamp);
		return validateTimeStamp;
	}
	
	@SuppressWarnings("unused")
	public void sendInvitationEmail(String hotpOperationType,String redirectUrl, String email,String invitationId)throws Exception{
		LOGGER.info("Entered sendInvitationEmail() -> Start");
		LOGGER.info("Parameter hotpOperationType -> " + hotpOperationType+" ,redirectUrl -> "+redirectUrl+" ,email -> "+email);
		LOGGER.info("Parameter invitationId -> " + invitationId);
		String userData = "";
		String subject = "";
		String lang= "";
		
		invID = invitationId;
		
			if(null != invitationId && !invitationId.isEmpty()){
				String mailDomain = email.substring(email.indexOf("@") + 1);
				LOGGER.info("mailDomain in sendInvitationEmail= " + mailDomain);
				if(mailDomain.contains(UserConstants.YOP_MAIL)){
					url = redirectUrl + "&amp;InvitationId="+ invitationId + "&amp;email=" + email ;
				}
				else{
					url = redirectUrl + "&InvitationId="+ invitationId + "&email=" + email ;
				}
			}
		
			LOGGER.info("sendInvitationEmail URL compiled to : " + url);

			contentBuilder = new StringBuilder();
			contentBuilder.setLength(0);
			// if section for chinese user
			if ((lang != null && lang.equalsIgnoreCase("zh")) || (hotpLanguage != null && hotpLanguage.equalsIgnoreCase("zh"))) {
				LOGGER.info("sendInvitationEmail :  Building Chinese email content..");
				subject = emailContentTemplate(email, subject, EmailConstants.HOTP_LAN_CN,hotpOperationType,email,null,null,null, false, false);
			}
			// Else section for English user
			else {
				LOGGER.info("sendInvitationEmail :  Building English email content..");
				subject = emailContentTemplate(email, subject, EmailConstants.HOTP_LAN_EN,hotpOperationType,email,null,null,null, false, false);
				LOGGER.info("subject="+subject);
			}

			String tos[] = new String[1];

			tos[0] = email;
			
			LOGGER.info("sendInvitationEmail : to : " + email);
			LOGGER.info("sendInvitationEmail : subject : " + subject);
			LOGGER.info("sendInvitationEmail : content.isEmpty : " + content.isEmpty());
			LOGGER.info("sendInvitationEmail : from : " + from);

		    emailReadyToSendEmail(email, from, subject, content);

			if (LOGGER.isDebugEnabled()) {
				LOGGER.info("sendInvitationEmail : " + "HOTP sent to : " + email + ".");
			}
	}
	
	/**
	 * Sending SMS by using the below method
	 * 
	 * */
	
	@Async
	public void sendSMSMessage(String code, String hotpOperationType, String userId, String appid) {
		LOGGER.info("Entered sendSMSMessage() -> Start");
		LOGGER.info("Parameter hotpOperationType -> " + hotpOperationType+" ,userId -> "+userId+" ,appid -> "+appid+" ,code="+code);

		String userData = "";
		String to = "";
		String SMSAdmin = "cy-snddq";
		String SMSAdminPassword = "cy-snddq123";
		String template = "1139";
		String smsContent = "%E3%80%90%E6%96%BD%E8%80%90%E5%BE%B7%E7%94%B5%E6%B0%94%E3%80%91%E9%AA%8C%E8%AF%81%E7%A0%81%EF%BC%9A"
				+code+"%20%EF%BC%8C30%E5%88%86%E9%92%9F%E5%86%85%E6%9C%89%E6%95%88";
		try {

			LOGGER.info("Start: getUser() of openamservice for sending SMS of user:"+userId);
			userData = productService.getUser(userService.getSSOToken(), userId);
			LOGGER.info("End: getUser() of openamservice finished for sending SMS of user:"+userId);
			productDocCtxUser = JsonPath.using(conf).parse(userData);

			if (hotpOperationType.equalsIgnoreCase(EmailConstants.UPDATEUSERRECORD_OPT_TYPE)) {
				to = productDocCtxUser.read("$.newmobile[0]");
			} else {
				to = productDocCtxUser.read("$.mobile[0]");
			}

			LOGGER.info("Start: sendSMSCode() of smsservice to "+to);
			Response smsResponse = smsService.sendSMSCode(SMSAdmin, SMSAdminPassword, to, smsContent, template);
			LOGGER.info("End: sendSMSCode() finished, Response status :: -> " + smsResponse.getStatus());
			if (200 == smsResponse.getStatus()) {
				LOGGER.info("sendSMSCode sent Succssfully :: -> "
						+ IOUtils.toString((InputStream) smsResponse.getEntity()));
			} else {
				LOGGER.error("Error occured while sendSMSCode() to "+to);
				throw new Exception(IOUtils.toString((InputStream) smsResponse.getEntity()));
			}

		} catch (Exception e) {
			LOGGER.error("Exception in sendSMSMessage() while sending code to: "+to);
			LOGGER.error(e.getMessage(),e);
		}

	}
	/**
	 * 
	 * The below method we are implementing to test the mobile scenarios we will delete this method later
	 * 
	 * */
	
	public void sendOpenAmMobileEmail(String code, String hotpOperationType,String userId, String appid ){
		LOGGER.info("Entered sendOpenAmMobileEmail() -> Start");
		LOGGER.info("Parameter hotpOperationType -> " + hotpOperationType+" ,userId -> "+userId+" ,appid -> "+appid);
			
		String userData = "";
		String to = "" ;
		String subject = "";
		String lang= "";
		
			encodedHOTPcode = code;
			LOGGER.info("Start: getUser() of openamservice for sendOpenAmMobileEmail of userid:"+userId);
			try {
				userData = productService.getUser(userService.getSSOToken(), userId);
			} catch (IOException ioExp) {
				LOGGER.error("Unable to get SSO Token " + ioExp.getMessage(),ioExp);
			}
			LOGGER.info("End: getUser() of openamservice finished for sendOpenAmMobileEmail of userid:"+userId);
			productDocCtxUser = JsonPath.using(conf).parse(userData);			
			
			if(hotpOperationType.equalsIgnoreCase(EmailConstants.UPDATEUSERRECORD_OPT_TYPE)){
				subject=appid;
				to = productDocCtxUser.read("$.newmobile[0]");
			}else{
				subject=productDocCtxUser.read("$.registerationSource[0]");
				to = productDocCtxUser.read("$.mobile_reg[0]");
			}
			
			to = to.concat("@mailinator.com");
			String emailContent = "Your OpenAM One Time Password is : " + code;

			lang=productDocCtxUser.read("$.preferredlanguage[0]");
			
			url = hotpEmailVerificationURL + "?userid=" + userId + "&pin=qyw1fnewri" + "&operationType="
					+ hotpOperationType + "&app=" + appid + "&uid=" + userId;
			
			LOGGER.info("URL compiled to : " + url);
			
			contentBuilder = new StringBuilder();
			contentBuilder.setLength(0);

			// if section for chinese user
			if ((lang != null && lang.equalsIgnoreCase("zh")) || (hotpLanguage != null && hotpLanguage.equalsIgnoreCase("zh"))) {
				LOGGER.info("Building Chinese email content..");
				subject = emailContentTemplate(to, subject, EmailConstants.HOTP_LAN_CN,hotpOperationType,to,null,null,null, false, false);
			}
			// Else section for English user
			else {
				LOGGER.info("Building English email content..");
				subject = emailContentTemplate(to, subject, EmailConstants.HOTP_LAN_EN,hotpOperationType,to,null,null,null, false, false);

			}

			String tos[] = new String[1];

			tos[0] = to;

			LOGGER.info("sendOpenAmMobileEmail : to : " + to);
			LOGGER.info("sendOpenAmMobileEmail : subject : " + subject);
			LOGGER.info("sendOpenAmMobileEmail : content.isEmpty : " + content.isEmpty());
			LOGGER.info("sendOpenAmMobileEmail : from : " + from);

			emailReadyToSendEmail(to, from, subject, emailContent);
				
			if (LOGGER.isDebugEnabled()) {
				LOGGER.info("sendOpenAmMobileEmail : " + "HOTP sent to : " + to + ".");
			}
	}
	
	/**
	 * 
	 * @param code
	 * @param mobile
	 */
	public void sendMobileEmail(String code, String mobile ){
		LOGGER.info("Entered sendOpenAmMobileEmail() -> Start");
		LOGGER.info("Parameter mobile -> "+mobile);

		String to = mobile ;
		String subject = null;

		to = to.concat("@getnada.com");
		String emailContent = "Your One Time Password is : " + code +" , valid for 15 minutes only.";
		emailContent = emailContent+"<BR><BR><BR><BR> Current Timestamp: "+System.currentTimeMillis();
		subject = "Complete Registration - OTP";
		
		emailReadyToSendEmail(to, from, subject, emailContent);
	}
	
	/**
	 * Sending demo email
	 * @param email
	 * @throws MessagingException 
	 */
	public boolean sendDemoEmail(String email ) throws MessagingException{
		LOGGER.info("Entered sendDemoEmail() -> Start");
		LOGGER.info("Parameter email -> "+email);
		boolean status = false;

		String to = email ;
		String subject = null;

		String emailContent = "This is a test email. Please do not reply.";
		emailContent = emailContent+"<BR><BR><BR><BR> Current Timestamp: "+System.currentTimeMillis();
		subject = "Test Email Verification";
		
		MimeMessage mailMessage = mailSender.createMimeMessage();
		InternetAddress fromAddress = null;
		InternetAddress toAddress = null;
		try {
			fromAddress = new InternetAddress(from);
			toAddress = new InternetAddress(to);

			mailMessage.setSubject(subject, "utf-8");
			mailMessage.setText(emailContent, "utf-8", "html");
			mailMessage.setFrom(fromAddress);
			mailMessage.setRecipient(RecipientType.TO, toAddress);

			LOGGER.info("Start: sending email to:"+to);
			mailSender.send(mailMessage);
			LOGGER.info("End: sending email finished to:"+to);
			status = true;
		}
		catch (SMTPSendFailedException e) {
			LOGGER.error("SMTPSendFailedException while sending email to "+to+" :: -> " + e.getMessage(),e);
			throw e;
		}
		catch (AddressException e) {
			LOGGER.error("Exception while sending email to "+to+" :: -> " + e.getMessage(),e);
			throw e;
		}
		catch (MessagingException e) {
			LOGGER.error("MessagingException while sending email to "+to+" :: -> " + e.getMessage(),e);
			throw e;
		}
		return status;
	}
	
	/**
	 * Sending SMS 
	 * New SMS Gateway
	 * @throws UnsupportedEncodingException 
	 **/
	
	@Async
	public void sendSMSNewGateway(String code, String hotpOperationType, String userId, String appid) throws UnsupportedEncodingException {
		LOGGER.info("Entered sendSMSNewGateway() -> Start");
		LOGGER.info("Parameter hotpOperationType -> " + hotpOperationType+" ,userId -> "+userId+" ,appid -> "+appid);

		String userData = "";
		String to = "";
		String sn = "SDK-BBX-010-28365";
		String password = "EEc1-61E0-4";
		String SMSPassword = getMD5(sn+password);
		//LOGGER.info("SMSPassword="+SMSPassword);
		//String template = "1139";
		//String smsContent = "%E3%80%90%E6%96%BD%E8%80%90%E5%BE%B7%E7%94%B5%E6%B0%94%E3%80%91%E9%AA%8C%E8%AF%81%E7%A0%81%EF%BC%9A"
		//		+code+"%20%EF%BC%8C30%E5%88%86%E9%92%9F%E5%86%85%E6%9C%89%E6%95%88";
		String smsContent = "【施耐德电气】验证码为："+code+"（请妥善保存，切勿告知他人），在页面输入以完成验证。";
		smsContent   =   java.net.URLEncoder.encode(smsContent,"utf-8");  
		//String ext=" ", stime=" ", rrid=" ", msgfmt=" ";
		
		try {

			LOGGER.info("Start: getUser() of openamservice for sending SMS of user:"+userId);
			userData = productService.getUser(userService.getSSOToken(), userId);
			LOGGER.info("End: getUser() of openamservice finished for sending SMS of user:"+userId);
			productDocCtxUser = JsonPath.using(conf).parse(userData);

			if (hotpOperationType.equalsIgnoreCase(EmailConstants.UPDATEUSERRECORD_OPT_TYPE)) {
				to = productDocCtxUser.read("$.newmobile[0]");
			} else {
				to = productDocCtxUser.read("$.mobile_reg[0]");
			}

			LOGGER.info("Start: sendSMSCode() of smsservice to "+to);
			Response smsResponse = newSmsService.sendSMSCode(sn, SMSPassword, to, smsContent);
			LOGGER.info("smsResponse="+smsResponse.getEntity());
			
			LOGGER.info("End: sendSMSCode() finished, Response status :: -> " + smsResponse.getStatus());
			if (200 == smsResponse.getStatus()) {
				LOGGER.info("sendSMSCode sent Succssfully :: -> "
						+ IOUtils.toString((InputStream) smsResponse.getEntity()));
			} else {
				LOGGER.error("Error occured while sendSMSCode() to "+to);
				throw new Exception(IOUtils.toString((InputStream) smsResponse.getEntity()));
			}

		} catch (Exception e) {
			LOGGER.error("Exception in sendSMSNewGateway() while sending code to: "+to);
			LOGGER.error("ECODE-SMS-SEND-FAILED : Error sending SMS");
			LOGGER.error(e.getMessage(),e);
		}

	}
	
	/**
	 * 
	 * @param code
	 * @param mobile
	 * @throws UnsupportedEncodingException
	 */
	@Async
	public void sendSMS(String code, String mobile) throws UnsupportedEncodingException {
		LOGGER.info("Entered sendSMS() -> Start");
		LOGGER.info("Parameter mobile -> "+mobile);

		String sn = "SDK-BBX-010-28365";
		String password = "EEc1-61E0-4";
		String SMSPassword = getMD5(sn+password);
		String smsContent = "【施耐德电气】验证码为："+code+"（请妥善保存，切勿告知他人），在页面输入以完成验证。有效期为15分钟。";
		smsContent   =   java.net.URLEncoder.encode(smsContent,"utf-8");  
		
		try {
			LOGGER.info("Start: sendSMSCode() of smsservice to "+mobile);
			Response smsResponse = newSmsService.sendSMSCode(sn, SMSPassword, mobile, smsContent);
			LOGGER.info("smsResponse="+smsResponse.getEntity());
			
			LOGGER.info("End: sendSMSCode() finished, Response status :: -> " + smsResponse.getStatus());
			if (200 == smsResponse.getStatus()) {
				LOGGER.info("sendSMSCode sent Succssfully :: -> "
						+ IOUtils.toString((InputStream) smsResponse.getEntity()));
			} else {
				LOGGER.error("Error occured while sendSMSCode() to "+mobile);
				throw new Exception(IOUtils.toString((InputStream) smsResponse.getEntity()));
			}
		} catch (Exception e) {
			LOGGER.error("Exception in sendSMSNewGateway() while sending code to: "+mobile);
			LOGGER.error(e.getMessage(),e);
		}
	}
	
	public String getMD5(String sourceStr) throws UnsupportedEncodingException {
		String resultStr = "";
		try {
			byte[] temp = sourceStr.getBytes();
			MessageDigest md5 = MessageDigest.getInstance("MD5");
			md5.update(temp);
			// resultStr = new String(md5.digest());
			byte[] b = md5.digest();
			for (int i = 0; i < b.length; i++) {
				char[] digit = { '0', '1', '2', '3', '4', '5', '6', '7', '8',
						'9', 'A', 'B', 'C', 'D', 'E', 'F' };
				char[] ob = new char[2];
				ob[0] = digit[(b[i] >>> 4) & 0X0F];
				ob[1] = digit[b[i] & 0X0F];
				resultStr += new String(ob);
			}
			return resultStr;
		} catch (NoSuchAlgorithmException e) {
			return null;
		}
	}

	public void setFrom(String from) {
		this.from = from;
	}

	public void setHotpEmailVerificationURL(String hotpEmailVerificationURL) {
		this.hotpEmailVerificationURL = hotpEmailVerificationURL;
	}

	public void setDjUserName(String djUserName) {
		this.djUserName = djUserName;
	}

	public void setDjUserPwd(String djUserPwd) {
		this.djUserPwd = djUserPwd;
	}
	
	public void setIDMS_USER_REST_PASSWORD_EMAILTEMPLATE_CN(String iDMS_USER_REST_PASSWORD_EMAILTEMPLATE_CN) {
		IDMS_USER_REST_PASSWORD_EMAILTEMPLATE_CN = iDMS_USER_REST_PASSWORD_EMAILTEMPLATE_CN;
	}
	
	public void setIDMS_USER_REST_PASSWORD_EMAILTEMPLATE_EN(String iDMS_USER_REST_PASSWORD_EMAILTEMPLATE_EN) {
		IDMS_USER_REST_PASSWORD_EMAILTEMPLATE_EN = iDMS_USER_REST_PASSWORD_EMAILTEMPLATE_EN;
	}

	public void setIDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_CN(String iDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_CN) {
		IDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_CN = iDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_CN;
	}

	public void setIDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_EN(String iDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_EN) {
		IDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_EN = iDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_EN;
	}

	public void setIDMS_USER_UPDATE_EMAILTEMPLATE_CN(String iDMS_USER_UPDATE_EMAILTEMPLATE_CN) {
		IDMS_USER_UPDATE_EMAILTEMPLATE_CN = iDMS_USER_UPDATE_EMAILTEMPLATE_CN;
	}

	public void setIDMS_USER_UPDATE_EMAILTEMPLATE_EN(String iDMS_USER_UPDATE_EMAILTEMPLATE_EN) {
		IDMS_USER_UPDATE_EMAILTEMPLATE_EN = iDMS_USER_UPDATE_EMAILTEMPLATE_EN;
	}

	public void setIDMS_USER_DEFAULT_EMAILTEMPLATE_CN(String iDMS_USER_DEFAULT_EMAILTEMPLATE_CN) {
		IDMS_USER_DEFAULT_EMAILTEMPLATE_CN = iDMS_USER_DEFAULT_EMAILTEMPLATE_CN;
	}

	public void setIDMS_USER_DEFAULT_EMAILTEMPLATE_EN(String iDMS_USER_DEFAULT_EMAILTEMPLATE_EN) {
		IDMS_USER_DEFAULT_EMAILTEMPLATE_EN = iDMS_USER_DEFAULT_EMAILTEMPLATE_EN;
	}

	public void setIDMS_SEND_INVITATION_EMAILTEMPLATE_EN(String iDMS_SEND_INVITATION_EMAILTEMPLATE_EN) {
		IDMS_SEND_INVITATION_EMAILTEMPLATE_EN = iDMS_SEND_INVITATION_EMAILTEMPLATE_EN;
	}

	public void setIDMS_SEND_INVITATION_EMAILTEMPLATE_CN(String iDMS_SEND_INVITATION_EMAILTEMPLATE_CN) {
		IDMS_SEND_INVITATION_EMAILTEMPLATE_CN = iDMS_SEND_INVITATION_EMAILTEMPLATE_CN;
	}

	public void setIDMS_USER_ADD_EMAILTEMPLATE_CN(String iDMS_USER_ADD_EMAILTEMPLATE_CN) {
		IDMS_USER_ADD_EMAILTEMPLATE_CN = iDMS_USER_ADD_EMAILTEMPLATE_CN;
	}

	public void setIDMS_USER_ADD_EMAILTEMPLATE_EN(String iDMS_USER_ADD_EMAILTEMPLATE_EN) {
		IDMS_USER_ADD_EMAILTEMPLATE_EN = iDMS_USER_ADD_EMAILTEMPLATE_EN;
	}


	public String getFrom() {
		return from;
	}


	public String getDjUserName() {
		return djUserName;
	}


	public String getDjUserPwd() {
		return djUserPwd;
	}


	public String getIDMS_USER_REST_PASSWORD_EMAILTEMPLATE_CN() {
		return IDMS_USER_REST_PASSWORD_EMAILTEMPLATE_CN;
	}


	public String getIDMS_USER_REST_PASSWORD_EMAILTEMPLATE_EN() {
		return IDMS_USER_REST_PASSWORD_EMAILTEMPLATE_EN;
	}


	public String getIDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_CN() {
		return IDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_CN;
	}


	public String getIDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_EN() {
		return IDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_EN;
	}


	public String getIDMS_USER_UPDATE_EMAILTEMPLATE_CN() {
		return IDMS_USER_UPDATE_EMAILTEMPLATE_CN;
	}


	public String getIDMS_USER_UPDATE_EMAILTEMPLATE_EN() {
		return IDMS_USER_UPDATE_EMAILTEMPLATE_EN;
	}


	public String getIDMS_USER_DEFAULT_EMAILTEMPLATE_CN() {
		return IDMS_USER_DEFAULT_EMAILTEMPLATE_CN;
	}


	public String getIDMS_USER_DEFAULT_EMAILTEMPLATE_EN() {
		return IDMS_USER_DEFAULT_EMAILTEMPLATE_EN;
	}


	public String getIDMS_SEND_INVITATION_EMAILTEMPLATE_EN() {
		return IDMS_SEND_INVITATION_EMAILTEMPLATE_EN;
	}


	public String getIDMS_SEND_INVITATION_EMAILTEMPLATE_CN() {
		return IDMS_SEND_INVITATION_EMAILTEMPLATE_CN;
	}


	public String getIDMS_USER_ADD_EMAILTEMPLATE_CN() {
		return IDMS_USER_ADD_EMAILTEMPLATE_CN;
	}


	public String getIDMS_USER_ADD_EMAILTEMPLATE_EN() {
		return IDMS_USER_ADD_EMAILTEMPLATE_EN;
	}


	public String getHotpEmailVerificationURL() {
		return hotpEmailVerificationURL;
	}


	public String getPRM_SELF_USER_REGESTRATION_EMAILTEMPLATE_EN() {
		return PRM_SELF_USER_REGESTRATION_EMAILTEMPLATE_EN;
	}


	public void setPRM_SELF_USER_REGESTRATION_EMAILTEMPLATE_EN(String pRM_SELF_USER_REGESTRATION_EMAILTEMPLATE_EN) {
		PRM_SELF_USER_REGESTRATION_EMAILTEMPLATE_EN = pRM_SELF_USER_REGESTRATION_EMAILTEMPLATE_EN;
	}


	public String getPRM_INTERNAL_USER_REGESTRATION_EMAILTEMPLATE_CN() {
		return PRM_INTERNAL_USER_REGESTRATION_EMAILTEMPLATE_CN;
	}


	public void setPRM_INTERNAL_USER_REGESTRATION_EMAILTEMPLATE_CN(
			String pRM_INTERNAL_USER_REGESTRATION_EMAILTEMPLATE_CN) {
		PRM_INTERNAL_USER_REGESTRATION_EMAILTEMPLATE_CN = pRM_INTERNAL_USER_REGESTRATION_EMAILTEMPLATE_CN;
	}


	public String getPRM_INTERNAL_USER_REGESTRATION_EMAILTEMPLATE_EN() {
		return PRM_INTERNAL_USER_REGESTRATION_EMAILTEMPLATE_EN;
	}


	public void setPRM_INTERNAL_USER_REGESTRATION_EMAILTEMPLATE_EN(
			String pRM_INTERNAL_USER_REGESTRATION_EMAILTEMPLATE_EN) {
		PRM_INTERNAL_USER_REGESTRATION_EMAILTEMPLATE_EN = pRM_INTERNAL_USER_REGESTRATION_EMAILTEMPLATE_EN;
	}


	public String getPRM_ECLIPSE_USER_REGESTRATION_EMAILTEMPLATE_CN() {
		return PRM_ECLIPSE_USER_REGESTRATION_EMAILTEMPLATE_CN;
	}


	public void setPRM_ECLIPSE_USER_REGESTRATION_EMAILTEMPLATE_CN(
			String pRM_ECLIPSE_USER_REGESTRATION_EMAILTEMPLATE_CN) {
		PRM_ECLIPSE_USER_REGESTRATION_EMAILTEMPLATE_CN = pRM_ECLIPSE_USER_REGESTRATION_EMAILTEMPLATE_CN;
	}


	public String getPRM_ECLIPSE_USER_REGESTRATION_EMAILTEMPLATE_EN() {
		return PRM_ECLIPSE_USER_REGESTRATION_EMAILTEMPLATE_EN;
	}


	public void setPRM_ECLIPSE_USER_REGESTRATION_EMAILTEMPLATE_EN(
			String pRM_ECLIPSE_USER_REGESTRATION_EMAILTEMPLATE_EN) {
		PRM_ECLIPSE_USER_REGESTRATION_EMAILTEMPLATE_EN = pRM_ECLIPSE_USER_REGESTRATION_EMAILTEMPLATE_EN;
	}


	public String getPRM_SELF_USER_REGESTRATION_EMAILTEMPLATE_CN() {
		return PRM_SELF_USER_REGESTRATION_EMAILTEMPLATE_CN;
	}


	public void setPRM_SELF_USER_REGESTRATION_EMAILTEMPLATE_CN(String pRM_SELF_USER_REGESTRATION_EMAILTEMPLATE_CN) {
		PRM_SELF_USER_REGESTRATION_EMAILTEMPLATE_CN = pRM_SELF_USER_REGESTRATION_EMAILTEMPLATE_CN;
	}


	public String getIDMS_USER_DEFAULT_BLUE_EMAILTEMPLATE_CN() {
		return IDMS_USER_DEFAULT_BLUE_EMAILTEMPLATE_CN;
	}


	public void setIDMS_USER_DEFAULT_BLUE_EMAILTEMPLATE_CN(String iDMS_USER_DEFAULT_BLUE_EMAILTEMPLATE_CN) {
		IDMS_USER_DEFAULT_BLUE_EMAILTEMPLATE_CN = iDMS_USER_DEFAULT_BLUE_EMAILTEMPLATE_CN;
	}


	public String getIDMS_USER_DEFAULT_BLUE_EMAILTEMPLATE_EN() {
		return IDMS_USER_DEFAULT_BLUE_EMAILTEMPLATE_EN;
	}


	public void setIDMS_USER_DEFAULT_BLUE_EMAILTEMPLATE_EN(String iDMS_USER_DEFAULT_BLUE_EMAILTEMPLATE_EN) {
		IDMS_USER_DEFAULT_BLUE_EMAILTEMPLATE_EN = iDMS_USER_DEFAULT_BLUE_EMAILTEMPLATE_EN;
	}


	public String getIDMS_USER_REST_PASSWORD_BLUE_EMAILTEMPLATE_CN() {
		return IDMS_USER_REST_PASSWORD_BLUE_EMAILTEMPLATE_CN;
	}


	public void setIDMS_USER_REST_PASSWORD_BLUE_EMAILTEMPLATE_CN(
			String iDMS_USER_REST_PASSWORD_BLUE_EMAILTEMPLATE_CN) {
		IDMS_USER_REST_PASSWORD_BLUE_EMAILTEMPLATE_CN = iDMS_USER_REST_PASSWORD_BLUE_EMAILTEMPLATE_CN;
	}


	public String getIDMS_USER_REST_PASSWORD_BLUE_EMAILTEMPLATE_EN() {
		return IDMS_USER_REST_PASSWORD_BLUE_EMAILTEMPLATE_EN;
	}


	public void setIDMS_USER_REST_PASSWORD_BLUE_EMAILTEMPLATE_EN(
			String iDMS_USER_REST_PASSWORD_BLUE_EMAILTEMPLATE_EN) {
		IDMS_USER_REST_PASSWORD_BLUE_EMAILTEMPLATE_EN = iDMS_USER_REST_PASSWORD_BLUE_EMAILTEMPLATE_EN;
	}


	public String getIDMS_USER_REGESTRATION_WITHPWD_BLUE_EMAILTEMPLATE_CN() {
		return IDMS_USER_REGESTRATION_WITHPWD_BLUE_EMAILTEMPLATE_CN;
	}


	public void setIDMS_USER_REGESTRATION_WITHPWD_BLUE_EMAILTEMPLATE_CN(
			String iDMS_USER_REGESTRATION_WITHPWD_BLUE_EMAILTEMPLATE_CN) {
		IDMS_USER_REGESTRATION_WITHPWD_BLUE_EMAILTEMPLATE_CN = iDMS_USER_REGESTRATION_WITHPWD_BLUE_EMAILTEMPLATE_CN;
	}


	public String getIDMS_USER_REGESTRATION_WITHPWD_BLUE_EMAILTEMPLATE_EN() {
		return IDMS_USER_REGESTRATION_WITHPWD_BLUE_EMAILTEMPLATE_EN;
	}


	public void setIDMS_USER_REGESTRATION_WITHPWD_BLUE_EMAILTEMPLATE_EN(
			String iDMS_USER_REGESTRATION_WITHPWD_BLUE_EMAILTEMPLATE_EN) {
		IDMS_USER_REGESTRATION_WITHPWD_BLUE_EMAILTEMPLATE_EN = iDMS_USER_REGESTRATION_WITHPWD_BLUE_EMAILTEMPLATE_EN;
	}


	public String getIDMS_USER_UPDATE_BLUE_EMAILTEMPLATE_CN() {
		return IDMS_USER_UPDATE_BLUE_EMAILTEMPLATE_CN;
	}


	public void setIDMS_USER_UPDATE_BLUE_EMAILTEMPLATE_CN(String iDMS_USER_UPDATE_BLUE_EMAILTEMPLATE_CN) {
		IDMS_USER_UPDATE_BLUE_EMAILTEMPLATE_CN = iDMS_USER_UPDATE_BLUE_EMAILTEMPLATE_CN;
	}


	public String getIDMS_USER_UPDATE_BLUE_EMAILTEMPLATE_EN() {
		return IDMS_USER_UPDATE_BLUE_EMAILTEMPLATE_EN;
	}


	public void setIDMS_USER_UPDATE_BLUE_EMAILTEMPLATE_EN(String iDMS_USER_UPDATE_BLUE_EMAILTEMPLATE_EN) {
		IDMS_USER_UPDATE_BLUE_EMAILTEMPLATE_EN = iDMS_USER_UPDATE_BLUE_EMAILTEMPLATE_EN;
	}


	public String getIDMS_SEND_INVITATION_BLUE_EMAILTEMPLATE_EN() {
		return IDMS_SEND_INVITATION_BLUE_EMAILTEMPLATE_EN;
	}


	public void setIDMS_SEND_INVITATION_BLUE_EMAILTEMPLATE_EN(String iDMS_SEND_INVITATION_BLUE_EMAILTEMPLATE_EN) {
		IDMS_SEND_INVITATION_BLUE_EMAILTEMPLATE_EN = iDMS_SEND_INVITATION_BLUE_EMAILTEMPLATE_EN;
	}


	public String getIDMS_SEND_INVITATION_BLUE_EMAILTEMPLATE_CN() {
		return IDMS_SEND_INVITATION_BLUE_EMAILTEMPLATE_CN;
	}


	public void setIDMS_SEND_INVITATION_BLUE_EMAILTEMPLATE_CN(String iDMS_SEND_INVITATION_BLUE_EMAILTEMPLATE_CN) {
		IDMS_SEND_INVITATION_BLUE_EMAILTEMPLATE_CN = iDMS_SEND_INVITATION_BLUE_EMAILTEMPLATE_CN;
	}


	public String getIDMS_USER_ADD_BLUE_EMAILTEMPLATE_CN() {
		return IDMS_USER_ADD_BLUE_EMAILTEMPLATE_CN;
	}


	public void setIDMS_USER_ADD_BLUE_EMAILTEMPLATE_CN(String iDMS_USER_ADD_BLUE_EMAILTEMPLATE_CN) {
		IDMS_USER_ADD_BLUE_EMAILTEMPLATE_CN = iDMS_USER_ADD_BLUE_EMAILTEMPLATE_CN;
	}


	public String getIDMS_USER_ADD_BLUE_EMAILTEMPLATE_EN() {
		return IDMS_USER_ADD_BLUE_EMAILTEMPLATE_EN;
	}


	public void setIDMS_USER_ADD_BLUE_EMAILTEMPLATE_EN(String iDMS_USER_ADD_BLUE_EMAILTEMPLATE_EN) {
		IDMS_USER_ADD_BLUE_EMAILTEMPLATE_EN = iDMS_USER_ADD_BLUE_EMAILTEMPLATE_EN;
	}


	public String getIDMS_USER_RESET_PASSWORD_OTP_EMAILTEMPLATE_CN() {
		return IDMS_USER_RESET_PASSWORD_OTP_EMAILTEMPLATE_CN;
	}


	public void setIDMS_USER_RESET_PASSWORD_OTP_EMAILTEMPLATE_CN(
			String iDMS_USER_RESET_PASSWORD_OTP_EMAILTEMPLATE_CN) {
		IDMS_USER_RESET_PASSWORD_OTP_EMAILTEMPLATE_CN = iDMS_USER_RESET_PASSWORD_OTP_EMAILTEMPLATE_CN;
	}


	public String getIDMS_USER_RESET_PASSWORD_OTP_EMAILTEMPLATE_EN() {
		return IDMS_USER_RESET_PASSWORD_OTP_EMAILTEMPLATE_EN;
	}


	public void setIDMS_USER_RESET_PASSWORD_OTP_EMAILTEMPLATE_EN(
			String iDMS_USER_RESET_PASSWORD_OTP_EMAILTEMPLATE_EN) {
		IDMS_USER_RESET_PASSWORD_OTP_EMAILTEMPLATE_EN = iDMS_USER_RESET_PASSWORD_OTP_EMAILTEMPLATE_EN;
	}


	public String getIDMS_USER_REGISTRATION_WITHPWD_OTP_EMAILTEMPLATE_CN() {
		return IDMS_USER_REGISTRATION_WITHPWD_OTP_EMAILTEMPLATE_CN;
	}


	public void setIDMS_USER_REGISTRATION_WITHPWD_OTP_EMAILTEMPLATE_CN(
			String iDMS_USER_REGISTRATION_WITHPWD_OTP_EMAILTEMPLATE_CN) {
		IDMS_USER_REGISTRATION_WITHPWD_OTP_EMAILTEMPLATE_CN = iDMS_USER_REGISTRATION_WITHPWD_OTP_EMAILTEMPLATE_CN;
	}


	public String getIDMS_USER_REGISTRATION_WITHPWD_OTP_EMAILTEMPLATE_EN() {
		return IDMS_USER_REGISTRATION_WITHPWD_OTP_EMAILTEMPLATE_EN;
	}


	public void setIDMS_USER_REGISTRATION_WITHPWD_OTP_EMAILTEMPLATE_EN(
			String iDMS_USER_REGISTRATION_WITHPWD_OTP_EMAILTEMPLATE_EN) {
		IDMS_USER_REGISTRATION_WITHPWD_OTP_EMAILTEMPLATE_EN = iDMS_USER_REGISTRATION_WITHPWD_OTP_EMAILTEMPLATE_EN;
	}


	public String getIDMS_USER_UPDATE_OTP_EMAILTEMPLATE_CN() {
		return IDMS_USER_UPDATE_OTP_EMAILTEMPLATE_CN;
	}


	public void setIDMS_USER_UPDATE_OTP_EMAILTEMPLATE_CN(String iDMS_USER_UPDATE_OTP_EMAILTEMPLATE_CN) {
		IDMS_USER_UPDATE_OTP_EMAILTEMPLATE_CN = iDMS_USER_UPDATE_OTP_EMAILTEMPLATE_CN;
	}


	public String getIDMS_USER_UPDATE_OTP_EMAILTEMPLATE_EN() {
		return IDMS_USER_UPDATE_OTP_EMAILTEMPLATE_EN;
	}


	public void setIDMS_USER_UPDATE_OTP_EMAILTEMPLATE_EN(String iDMS_USER_UPDATE_OTP_EMAILTEMPLATE_EN) {
		IDMS_USER_UPDATE_OTP_EMAILTEMPLATE_EN = iDMS_USER_UPDATE_OTP_EMAILTEMPLATE_EN;
	}


	public String getIDMS_USER_DEFAULT_OTP_EMAILTEMPLATE_CN() {
		return IDMS_USER_DEFAULT_OTP_EMAILTEMPLATE_CN;
	}


	public void setIDMS_USER_DEFAULT_OTP_EMAILTEMPLATE_CN(String iDMS_USER_DEFAULT_OTP_EMAILTEMPLATE_CN) {
		IDMS_USER_DEFAULT_OTP_EMAILTEMPLATE_CN = iDMS_USER_DEFAULT_OTP_EMAILTEMPLATE_CN;
	}


	public String getIDMS_USER_DEFAULT_OTP_EMAILTEMPLATE_EN() {
		return IDMS_USER_DEFAULT_OTP_EMAILTEMPLATE_EN;
	}


	public void setIDMS_USER_DEFAULT_OTP_EMAILTEMPLATE_EN(String iDMS_USER_DEFAULT_OTP_EMAILTEMPLATE_EN) {
		IDMS_USER_DEFAULT_OTP_EMAILTEMPLATE_EN = iDMS_USER_DEFAULT_OTP_EMAILTEMPLATE_EN;
	}


	public String getIDMS_USER_OTP_ADD_EMAILTEMPLATE_CN() {
		return IDMS_USER_OTP_ADD_EMAILTEMPLATE_CN;
	}


	public void setIDMS_USER_OTP_ADD_EMAILTEMPLATE_CN(String iDMS_USER_OTP_ADD_EMAILTEMPLATE_CN) {
		IDMS_USER_OTP_ADD_EMAILTEMPLATE_CN = iDMS_USER_OTP_ADD_EMAILTEMPLATE_CN;
	}


	public String getIDMS_USER_OTP_ADD_EMAILTEMPLATE_EN() {
		return IDMS_USER_OTP_ADD_EMAILTEMPLATE_EN;
	}


	public void setIDMS_USER_OTP_ADD_EMAILTEMPLATE_EN(String iDMS_USER_OTP_ADD_EMAILTEMPLATE_EN) {
		IDMS_USER_OTP_ADD_EMAILTEMPLATE_EN = iDMS_USER_OTP_ADD_EMAILTEMPLATE_EN;
	}

	public String getIDMS_USER_REGISTRATION_WITHPWD_OTP_EMAILTEMPLATE() {
		return IDMS_USER_REGISTRATION_WITHPWD_OTP_EMAILTEMPLATE;
	}

	public void setIDMS_USER_REGISTRATION_WITHPWD_OTP_EMAILTEMPLATE(
			String iDMS_USER_REGISTRATION_WITHPWD_OTP_EMAILTEMPLATE) {
		IDMS_USER_REGISTRATION_WITHPWD_OTP_EMAILTEMPLATE = iDMS_USER_REGISTRATION_WITHPWD_OTP_EMAILTEMPLATE;
	}

	public String getIDMS_USER_REGISTRATION_WITHPWD_EMAILTEMPLATE() {
		return IDMS_USER_REGISTRATION_WITHPWD_EMAILTEMPLATE;
	}

	public void setIDMS_USER_REGISTRATION_WITHPWD_EMAILTEMPLATE(String iDMS_USER_REGISTRATION_WITHPWD_EMAILTEMPLATE) {
		IDMS_USER_REGISTRATION_WITHPWD_EMAILTEMPLATE = iDMS_USER_REGISTRATION_WITHPWD_EMAILTEMPLATE;
	}

	public String getPRM_INTERNAL_USER_REGISTRATION_EMAILTEMPLATE() {
		return PRM_INTERNAL_USER_REGISTRATION_EMAILTEMPLATE;
	}

	public void setPRM_INTERNAL_USER_REGISTRATION_EMAILTEMPLATE(String pRM_INTERNAL_USER_REGISTRATION_EMAILTEMPLATE) {
		PRM_INTERNAL_USER_REGISTRATION_EMAILTEMPLATE = pRM_INTERNAL_USER_REGISTRATION_EMAILTEMPLATE;
	}

	public String getIDMS_USER_RESET_PASSWORD_OTP_EMAILTEMPLATE() {
		return IDMS_USER_RESET_PASSWORD_OTP_EMAILTEMPLATE;
	}

	public void setIDMS_USER_RESET_PASSWORD_OTP_EMAILTEMPLATE(String iDMS_USER_RESET_PASSWORD_OTP_EMAILTEMPLATE) {
		IDMS_USER_RESET_PASSWORD_OTP_EMAILTEMPLATE = iDMS_USER_RESET_PASSWORD_OTP_EMAILTEMPLATE;
	}

	public String getIDMS_USER_RESET_PASSWORD_EMAILTEMPLATE() {
		return IDMS_USER_RESET_PASSWORD_EMAILTEMPLATE;
	}

	public void setIDMS_USER_RESET_PASSWORD_EMAILTEMPLATE(String iDMS_USER_RESET_PASSWORD_EMAILTEMPLATE) {
		IDMS_USER_RESET_PASSWORD_EMAILTEMPLATE = iDMS_USER_RESET_PASSWORD_EMAILTEMPLATE;
	}

	public String getIDMS_USER_OTP_ADD_EMAILTEMPLATE() {
		return IDMS_USER_OTP_ADD_EMAILTEMPLATE;
	}

	public void setIDMS_USER_OTP_ADD_EMAILTEMPLATE(String iDMS_USER_OTP_ADD_EMAILTEMPLATE) {
		IDMS_USER_OTP_ADD_EMAILTEMPLATE = iDMS_USER_OTP_ADD_EMAILTEMPLATE;
	}

	public String getIDMS_USER_ADD_EMAILTEMPLATE() {
		return IDMS_USER_ADD_EMAILTEMPLATE;
	}

	public void setIDMS_USER_ADD_EMAILTEMPLATE(String iDMS_USER_ADD_EMAILTEMPLATE) {
		IDMS_USER_ADD_EMAILTEMPLATE = iDMS_USER_ADD_EMAILTEMPLATE;
	}

	public String getIDMS_USER_UPDATE_OTP_EMAILTEMPLATE() {
		return IDMS_USER_UPDATE_OTP_EMAILTEMPLATE;
	}

	public void setIDMS_USER_UPDATE_OTP_EMAILTEMPLATE(String iDMS_USER_UPDATE_OTP_EMAILTEMPLATE) {
		IDMS_USER_UPDATE_OTP_EMAILTEMPLATE = iDMS_USER_UPDATE_OTP_EMAILTEMPLATE;
	}

	public String getIDMS_USER_UPDATE_EMAILTEMPLATE() {
		return IDMS_USER_UPDATE_EMAILTEMPLATE;
	}

	public void setIDMS_USER_UPDATE_EMAILTEMPLATE(String iDMS_USER_UPDATE_EMAILTEMPLATE) {
		IDMS_USER_UPDATE_EMAILTEMPLATE = iDMS_USER_UPDATE_EMAILTEMPLATE;
	}

	public String getIDMS_USER_CHANGE_EMAILTEMPLATE() {
		return IDMS_USER_CHANGE_EMAILTEMPLATE;
	}

	public void setIDMS_USER_CHANGE_EMAILTEMPLATE(String iDMS_USER_CHANGE_EMAILTEMPLATE) {
		IDMS_USER_CHANGE_EMAILTEMPLATE = iDMS_USER_CHANGE_EMAILTEMPLATE;
	}
}

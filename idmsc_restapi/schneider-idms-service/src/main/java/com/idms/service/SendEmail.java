package com.idms.service;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStream;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.ZoneId;

import javax.inject.Inject;
import javax.mail.Message.RecipientType;
import javax.mail.MessagingException;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;
import javax.ws.rs.core.Response;

import org.apache.commons.lang3.RandomStringUtils;
import org.apache.cxf.helpers.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.mail.javamail.JavaMailSender;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.stereotype.Service;

import com.idms.product.client.OpenAMService;
import com.idms.product.client.SmsService;
import com.idms.service.util.ChinaIdmsUtil;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Option;
import com.se.idms.cache.utils.EmailConstants;
import com.se.idms.util.UserConstants;
import com.sun.mail.smtp.SMTPSendFailedException;

@Service("emailService")
@EnableAsync
public class SendEmail {
	
	@Inject
	private OpenAMService productService;
	
	@Inject
	private SmsService smsService;
	
	@Value("${fromUserName}")
	private String from;

	@Value("${hotpEmailVerificationURL}")
	private String hotpEmailVerificationURL;
	
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
	DocumentContext productDocCtxUser = null;
	
	/**
	 * Logger instance.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(SendEmail.class);
	

	@Inject// for sending simple mails
	private JavaMailSender  mailSender;
 
	@Async
	public void emailReadyToSendEmail(String to, String from, String subject, String msgBody) {
		LOGGER.info("Entered emailReadyToSendEmail() -> Start");
		LOGGER.info("Parameter to -> " + to+" ,from -> "+from+" ,subject -> "+subject);
		//LOGGER.info("Parameter msgBody -> " + msgBody);
		
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
			/*
			 * MimeMessageHelper helper = new MimeMessageHelper(mailMessage,
			 * true); helper.setFrom(fromAddress); helper.setTo(toAddress);
			 */
			// helper.setText(msgBody,true);

			LOGGER.info("started sending email");
			mailSender.send(mailMessage); 
			LOGGER.info("sending email finsihed");
		} 
		catch (SMTPSendFailedException e) {
			LOGGER.error("SMTPSendFailedException while sending email :: -> " + e.getMessage());
			e.printStackTrace();
		}
		catch (MessagingException e) {
			LOGGER.error("MessagingException while sending email :: -> " + e.getMessage());
			e.printStackTrace();
		}
		catch (Exception e) {
			LOGGER.error("Exception while sending email :: -> " + e.getMessage());
			e.printStackTrace();
		}
	}
	
	
	@SuppressWarnings("unused")
	public void sendOpenAmEmail(String code, String hotpOperationType,String userId, String appid ){
		LOGGER.info("Entered sendOpenAmEmail() -> Start");
		LOGGER.info("Parameter hotpOperationType -> "+hotpOperationType+" ,userId -> "+userId);
		LOGGER.info("Parameter appid -> " + appid);
			
		String userData = "";
		String to = "" ;
		String subject = "";
		String lang= "";
		String firstName = "";
		

			encodedHOTPcode = code;
			// get sso token.. iPlanetDirectoryKey
			LOGGER.info("Going to call getUser() of openamService");
			userData = productService.getUser(userService.getSSOToken(), userId);
			LOGGER.info("getUser() call of openamService finished with data from Openam: " + userData);
			productDocCtxUser = JsonPath.using(conf).parse(userData);
			
			
			if(hotpOperationType.equalsIgnoreCase(EmailConstants.UPDATEUSERRECORD_OPT_TYPE)){
				/*userData = provisionalService.getUser(userService.getSSOToken(), userId);
				productDocCtxUser = JsonPath.using(conf).parse(userData);*/
				subject=appid;
				to = productDocCtxUser.read("$.newmail[0]");
			}else{
				subject=productDocCtxUser.read("$.registerationSource[0]");
				to = productDocCtxUser.read("$.mail[0]");
			}

			lang=productDocCtxUser.read("$.preferredlanguage[0]");
			firstName=productDocCtxUser.read("$.givenName[0]");
			
			/*				url = hotpEmailVerificationURL + "?userid=" + to + "&pin=" + encodedHOTPcode + "&operationType="
						+ hotpOperationType + "&lang=" + lang + "&app=" + appid + "&uid=" + uid;
				subject = appid;
			} else {
				url = hotpEmailVerificationURL + "?userid=" + to + "&pin=" + encodedHOTPcode + "&operationType="
						+ hotpOperationType + "&lang=" + hotpLanguage;
				subject = "OpenAM";
			}*/
			//userId ==con
			//appid = regsource or updateresource pass from emthod
			
			url = hotpEmailVerificationURL + "?userid=" + userId + "&pin=" + encodedHOTPcode + "&operationType="
					+ hotpOperationType + "&lang=" + lang + "&app=" + appid + "&uid=" + userId;
			
			//url = URLEncoder.encode( structurl, "UTF-8");  
			
					
			//subject = appid;

			// String url =
			// hotpEmailVerificationURL+"?userid="+to+"&pin="+code+"&operationType="+hotpOperationType+"&pwdReq="+hotpPasswordRequired+"&lang="+hotpLanguage;
			// String url =
			// hotpEmailVerificationURL+"?userid="+email+"&pin="+code+"&operationType="+hotpOperationType+"&lang="+hotpLanguage;
			// String url =
			// hotpEmailVerificationURL+"?userid="+to+"&pin="+code+"&operationType="+hotpOperationType+"&lang="+lang+"&appid="+appid+"&uid="+uid;
			LOGGER.info("sendOpenAmEmail : URL compiled to : " + url);
			// String msg = html1+url+html2;

			// debug.error("SchneiderSMSGatewayImpl.sendEmail() : Message 1
			// compiled to : "+msg);

			contentBuilder = new StringBuilder();
			LOGGER.info(
					"sendOpenAmEmail : Content Builder Length initial:" + contentBuilder.length());
			contentBuilder.setLength(0);
			LOGGER.info(
					"sendOpenAmEmail : Content Builder Length cleared:" + contentBuilder.length());

			// if section for chinese user
			if ((lang != null && lang.equals("zh")) || (hotpLanguage != null && hotpLanguage.equals("zh"))) {
				LOGGER.info("sendOpenAmEmail :  Building Chinese email content..");
				subject = emailContentTemplate(to, subject, EmailConstants.HOTP_LAN_CN,hotpOperationType,firstName);
			}
			// Else section for English user
			else {
				LOGGER.info("sendOpenAmEmail :  Building English email content..");
				subject = emailContentTemplate(to, subject, EmailConstants.HOTP_LAN_EN,hotpOperationType,firstName);

			}

			String tos[] = new String[1];

			tos[0] = to;
			//AMSendMail sendMail = new AMSendMail();

			LOGGER.info("sendOpenAmEmail : to : " + to);
			LOGGER.info("sendOpenAmEmail : subject : " + subject);
			LOGGER.info("sendOpenAmEmail : content.isEmpty : " + content.isEmpty());
			LOGGER.info("sendOpenAmEmail : from : " + from);

			//sendMail.postMail(tos, subject, content, from, "text/html", "UTF-8", smtpHostName, smtpHostPort,
			//smtpUserName, smtpUserPassword, sslEnabled);
				
				emailReadyToSendEmail(to, from, subject, content);
				/*
				 * sendMail.postMail(tos, subject, msg, from, "UTF-8",
				 * smtpHostName, smtpHostPort, smtpUserName, smtpUserPassword,
				 * sslEnabled);
				 */

			if (LOGGER.isDebugEnabled()) {
				LOGGER.info("sendOpenAmEmail : " + "HOTP sent to : " + to + ".");
			}
	}
	
	public String generateOtp(String userId) throws Exception {
		LOGGER.info("Entered generateOtp() -> Start");
		LOGGER.info("Parameter userId -> " + userId);

		// Proper Exception handling
		String hexpin = "";
		String product_json_string = "";
		String pin = "";
		pin = generateRamdomPin();
		hexpin = ChinaIdmsUtil.generateHashValue(pin);
		LOGGER.info("hexa string is " + hexpin);
		
		LocalDateTime currentDatenTime = LocalDateTime.now();
		long currentDatenTimeInMillisecs = currentDatenTime.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
		
		hexpin = hexpin+":"+currentDatenTimeInMillisecs;
		// open Attribute AuthID and Timestamp
		product_json_string = "{" + "\"authId\": \"" + hexpin + "\"}";
		// Need add the timestamp
		// update hashkey in openAM.
		LOGGER.info("Going to call updateUser() of openamservice to update hashkey");
		productService.updateUser(UserConstants.CHINA_IDMS_TOKEN+userService.getSSOToken(), userId,
				product_json_string);
		LOGGER.info("updateUser() of openamservice to update hashkey finsihed");
		return pin;
	}
	
	public void storePRMOtp(String userId, String hashedPin) throws Exception {
		LOGGER.info("Entered storePRMOtp() -> Start");

		// Proper Exception handling
		String product_json_string = "";
		LOGGER.info("hexa string is " + hashedPin);
		LOGGER.info("Parameter userId -> " + userId);
		
		LocalDateTime currentDatenTime = LocalDateTime.now();
		long currentDatenTimeInMillisecs = currentDatenTime.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
		
		hashedPin = hashedPin+":"+currentDatenTimeInMillisecs;
		// open Attribute AuthID and Timestamp
		product_json_string = "{" + "\"authId\": \"" + hashedPin + "\"}";
		// update hashkey in openAM.
		LOGGER.info("hashedPin is " + hashedPin);
		LOGGER.info("Going to call updateUser() of openamservice to update hashedPin");
		productService.updateUser(UserConstants.CHINA_IDMS_TOKEN+userService.getSSOToken(), userId,
				product_json_string);
		LOGGER.info("updateUser() of openamservice to update hashedPin finsihed");

	}
	
	public boolean validatePin(String otp,String userId)throws Exception {
		LOGGER.info("Entered validatePin() -> Start");
		LOGGER.info("Parameter userId -> " + userId);
		boolean validatePin = false;
		String authIdTimeStmp = "";
		String[] authIdTime = null;
		String product_json_string = "";
			// convert otp to hash
			String newHashedValue = ChinaIdmsUtil.generateHashValue(otp);
			LOGGER.info("hexa string is:" + newHashedValue);

			// iplane getsson
			// get user details to get stored hashkey.
			LOGGER.info("Going to call getUser() of openamservice to get stored hashkey");
			String userData = productService.getUser(userService.getSSOToken(), userId);
			LOGGER.info("getUser() of openamservice to get stored hashkey finsihed, userdata="+userData);
			Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
			DocumentContext productDocCtx = JsonPath.using(conf).parse(userData);

			authIdTimeStmp = productDocCtx.read("$.authId[0]");
			
			if(null == authIdTimeStmp){
				authIdTimeStmp = productDocCtx.read("$.AuthID[0]");
			}
			if ((null != authIdTimeStmp && !authIdTimeStmp.isEmpty()) && (!"[]".equals(authIdTimeStmp))) {
				authIdTime = authIdTimeStmp.split(":");

				String storedHashedValue = authIdTime[0];

				// pull time in milliseconds from openam
				// long localDTInMilli =
				// productDocCtx.read("$.timestamp").toString();
				long localDTInMilli = Long.valueOf(authIdTime[1]).longValue();

				// compare Stored hashkey and generated hash key
				if (newHashedValue.equals(storedHashedValue) && checkTimeStamp(localDTInMilli)) {
					validatePin = true;
					product_json_string = "{" + "\"authId\": \"" + "[]" + "\"}";
					// Need add the timestamp
					// update hashkey in openAM.
					LOGGER.info("Going to call updateUser() of openamservice to update hashkey for userid ="+userId);
					productService.updateUser(UserConstants.CHINA_IDMS_TOKEN+userService.getSSOToken(), userId,
							product_json_string);
					LOGGER.info("updateUser() of openamservice to update hashkey finsihed");
				}
			}else{
				LOGGER.error("Some problem in validatePin() for userId="+userId);
				throw new Exception("inValid Pin Exception!!!");
			}
		return validatePin;
	}

	private String emailContentTemplate(String to, String subject, String lang,String hotpOperationType,String firstName)  {
		LOGGER.info("Entered emailContentTemplate() -> Start");
		LOGGER.info("Parameter to -> " + to+" ,subject -> "+subject);
		LOGGER.info("Parameter lang -> " + lang+" ,hotpOperationType -> "+hotpOperationType);
		String filePath;
		boolean chineseLangCheck = (lang != null && lang.equals(EmailConstants.HOTP_LAN_CN)) || (hotpLanguage != null && hotpLanguage.equals(EmailConstants.HOTP_LAN_CN));
		int startIndex=0;
		int endIndex=0;
		
		try {
			// For Windows//BufferedReader in = new BufferedReader(new
			// FileReader("C:\\Users\\neha.soni\\Desktop\\Schnieder\\POC's\\HOTP\\Template\\User_registration_with_password.html"));
			// For Unix//
			BufferedReader in = null;
			FileReader file = null;

			String filePathone = new File("").getAbsolutePath();
			LOGGER.info("Current File Path : " + filePathone);

			if (hotpOperationType != null && hotpOperationType.equals(EmailConstants.SETUSERPWD_OPT_TYPE)) {
				LOGGER.info("emailContentTemplate : Inside SetUserPwd OperationType  :  " + hotpOperationType);
				// filePath="C:\\Users\\neha.soni\\Desktop\\Schnieder\\POC's\\HOTP\\Template\\User_Reset_password.html";
				if (chineseLangCheck) {
					filePath = EmailConstants.IDMS_USER_REST_PASSWORD_EMAILTEMPLATE_CN;
				} else {
					filePath = EmailConstants.IDMS_USER_REST_PASSWORD_EMAILTEMPLATE_EN;
				}
				file = new FileReader(filePath);
				LOGGER.info("filePath is"+filePath);
				in = new BufferedReader(file);
			} else if (hotpOperationType != null && hotpOperationType.equals(EmailConstants.USERREGISTRATION_OPT_TYPE)) {
				LOGGER.info("emailContentTemplate : Inside userRegistration OperationType Create " + hotpOperationType);
				// filePath="C:\\Users\\neha.soni\\Desktop\\Schnieder\\POC's\\HOTP\\Template\\User_registration_with_password.html";
				if (chineseLangCheck) {
					filePath = EmailConstants.IDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_CN;
				} else {
					filePath = EmailConstants.IDMS_USER_REGESTRATION_WITHPWD_EMAILTEMPLATE_EN;
				}
				file = new FileReader(filePath);
				LOGGER.info("filePath is"+filePath);
				in = new BufferedReader(file);
			} else if (hotpOperationType != null && hotpOperationType.equals(EmailConstants.UPDATEUSERRECORD_OPT_TYPE)) {
				LOGGER.info("emailContentTemplate : Inside OperationType UpdateUserRecord " + hotpOperationType);
				// filePath="C:\\Users\\neha.soni\\Desktop\\Schnieder\\POC's\\HOTP\\Template\\User_registration_with_password.html";
				if (chineseLangCheck) {
					filePath = EmailConstants.IDMS_USER_UPDATE_EMAILTEMPLATE_CN;
				} else {
					filePath = EmailConstants.IDMS_USER_UPDATE_EMAILTEMPLATE_EN;
				}
				file = new FileReader(filePath);
				LOGGER.info("filePath is"+filePath);
				in = new BufferedReader(file);
			}else if (hotpOperationType != null && hotpOperationType.equals(EmailConstants.SENDINVITATION_OPT_TYPE)) {
				LOGGER.info("emailContentTemplate : Inside OperationType sendInvitation " + hotpOperationType);
				// filePath="C:\\Users\\neha.soni\\Desktop\\Schnieder\\POC's\\HOTP\\Template\\User_registration_with_password.html";
				if (chineseLangCheck) {
					filePath = EmailConstants.IDMS_SEND_INVITATION_EMAILTEMPLATE_CN;
				} else {
					filePath = EmailConstants.IDMS_SEND_INVITATION_EMAILTEMPLATE_EN;
				}
				file = new FileReader(filePath);
				LOGGER.info("filePath is"+filePath);
				in = new BufferedReader(file);
			} else {
				LOGGER.info("emailContentTemplate : Inside Common OperationType " + hotpOperationType);
				// filePath="C:\\Users\\neha.soni\\Desktop\\Schnieder\\POC's\\HOTP\\Template\\User_registration_with_password.html";
				if (chineseLangCheck) {
					filePath = EmailConstants.IDMS_USER_DEFAULT_EMAILTEMPLATE_CN;
				} else {
					filePath = EmailConstants.IDMS_USER_DEFAULT_EMAILTEMPLATE_EN;
				}
				file = new FileReader(filePath);
				LOGGER.info("filePath is"+filePath);
				in = new BufferedReader(file);
			}
			String str;
			while ((str = in.readLine()) != null) {
				contentBuilder.append(str);
			}
			in.close();
			file.close();
		} catch (IOException e) {
			//throw new FileNotFoundException("Caught Exception!!!!!!!!!!!! " + e);
			e.printStackTrace();
			LOGGER.error("Executing while sending email :: -> " + e.getMessage());
		}
		LOGGER.info("SchneiderSMSGatewayImpl.sendEmail() : Content Builder Length after reading the file :"
				+ contentBuilder.length());
		// int start=13165;
		// int end=13175;

		
		
		if(null != invID &&  EmailConstants.SENDINVITATION_OPT_TYPE.equalsIgnoreCase(hotpOperationType)){
			 startIndex = contentBuilder.indexOf("{!invtID}");
			 endIndex = startIndex + 9;
			LOGGER.info("Starting and Ending Index of invtID in Email : Start : " + startIndex + " : End : +" + endIndex);
			contentBuilder.replace(startIndex, endIndex, invID);
			
			startIndex = contentBuilder.indexOf("{!firstname}");
			endIndex = startIndex + 12;
			LOGGER.info("Starting and Ending Index of firstname in Email : Start : " + startIndex + " : End : +" + endIndex);
			contentBuilder.replace(startIndex, endIndex, firstName);
			
			startIndex = contentBuilder.indexOf("{!invtURL}");
			endIndex = startIndex+10;
		}else{
			
			if(hotpOperationType != null && EmailConstants.USERREGISTRATION_OPT_TYPE.equals(hotpOperationType)){
				
				if(0 < contentBuilder.indexOf("{!registrationSource}")){
				startIndex = contentBuilder.indexOf("{!registrationSource}");
				 endIndex = startIndex + 21;
				 	LOGGER.info("Starting and Ending Index of registrationSource in Email : Start : " + startIndex + " : End : +" + endIndex);
				contentBuilder.replace(startIndex, endIndex, subject);
				}
			}
			 startIndex = contentBuilder.indexOf("{!firstname}");
			 endIndex = startIndex + 12;
			LOGGER.info("Starting and Ending Index of firstname in Email : Start : " + startIndex + " : End : +" + endIndex);
			contentBuilder.replace(startIndex, endIndex, firstName);
			 startIndex = contentBuilder.indexOf("{!url}");
			 endIndex = startIndex+6;
		}
		
		/*int sendInvistartURL = contentBuilder.indexOf("{!invtURL}");
		int sendInviendURL = sendInvistartURL+10;
		
		int startURL = contentBuilder.indexOf("{!url}");
		int endURL = startURL+6;*/
		
		
		if (hotpOperationType != null && EmailConstants.SETUSERPWD_OPT_TYPE.equalsIgnoreCase(hotpOperationType)) {
			LOGGER.info("emailContentTemplate : Inside OperationType " + hotpOperationType
					+ " .. setting template starte and end value accordingly! ");
			//endURL = startURL + 57;
			if (chineseLangCheck) {
				subject = subject + EmailConstants.HYPHEN + EmailConstants.SCHE_PWD_RESET_CN;
			} else {
				subject = subject + EmailConstants.HYPHEN + EmailConstants.SCHE_PWD_RESET_EN;
			}
		} else if (hotpOperationType != null && EmailConstants.USERREGISTRATION_OPT_TYPE.equalsIgnoreCase(hotpOperationType)) {
			LOGGER.info("emailContentTemplate : Inside userRegistration OperationType " + hotpOperationType
					+ " .. setting template starte and end value accordingly! ");
			//endURL = startURL + 6;
			if (chineseLangCheck) {
				subject = subject + EmailConstants.HYPHEN + EmailConstants.COMPLETE_REG_CN;
			} else {
				subject = subject + EmailConstants.HYPHEN + EmailConstants.COMPLETE_REG_EN;
			}
		} else if (hotpOperationType != null && EmailConstants.UPDATEUSERRECORD_OPT_TYPE.equalsIgnoreCase(hotpOperationType)) {
			LOGGER.info("emailContentTemplate : Inside UpdateUserRecord OperationType " + hotpOperationType
					+ " .. setting template starte and end value accordingly! ");
			//endURL = startURL + 70;
			if (chineseLangCheck) {
				subject = subject + EmailConstants.HYPHEN + EmailConstants.SCHE_LOGINID_CHANGE_NOTIFICATION_CN;
			} else {
				subject = subject + EmailConstants.HYPHEN + EmailConstants.SCHE_LOGINID_CHANGE_NOTIFICATION_EN;
			}
		}else if (hotpOperationType != null && EmailConstants.SENDINVITATION_OPT_TYPE.equalsIgnoreCase(hotpOperationType)) {
			LOGGER.info("emailContentTemplate : Inside sendInvitation OperationType " + hotpOperationType
					+ " .. setting template starte and end value accordingly! ");
			//endURL = startURL + 70;
			if (chineseLangCheck) {
				subject = subject + EmailConstants.HYPHEN + EmailConstants.SCHE_SEND_INVITATION_NOTIFICATION_CN;
			} else {
				subject = subject + EmailConstants.HYPHEN + EmailConstants.SCHE_SEND_INVITATION_NOTIFICATION_EN;
			}
		} else {
			LOGGER.info(
					"emailContentTemplate : OperationType doesn't match any of the above .. setting template starte and end value accordingly! ");
			//endURL = startURL + 70;
			if (chineseLangCheck) {
				subject = subject + EmailConstants.HYPHEN + EmailConstants.OPENAM_OTP_CN;
			} else {
				subject = subject + EmailConstants.HYPHEN + EmailConstants.OPENAM_OTP_EN;
			}
		}
		LOGGER.info("subject="+subject);
		LOGGER.info("Starting and Ending Index of URL in Email : Start : " + startIndex + " : End : +" + endIndex);
		contentBuilder.replace(startIndex, endIndex, url);
		
		/*if (hotpOperationType != null && hotpOperationType.equals(EmailConstants.SENDINVITATION_OPT_TYPE)) {
			LOGGER.info("Starting and Ending Index of URL in Email : Start : " + sendInvistartURL + " : End : +"
					+ sendInviendURL);
			contentBuilder.replace(sendInvistartURL, sendInviendURL, url);
		} else {
			LOGGER.info("Starting and Ending Index of URL in Email : Start : " + startURL + " : End : +" + endURL);
			contentBuilder.replace(startURL, endURL, url);
		}*/

		content = contentBuilder.toString();
		LOGGER.info("emailContentTemplate : Message 2 compiled to : " );
		return subject;
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
	
	private boolean checkTimeStamp(long localDTInMilli) {
		LOGGER.info("Entered checkTimeStamp() -> Start");
		LOGGER.info("Parameter localDTInMilli() ->"+ localDTInMilli);
		
		boolean validateTimeStamp = false;
		LocalDateTime otpGeneratedDatenTime = Instant.ofEpochMilli(localDTInMilli).atZone(ZoneId.systemDefault())
				.toLocalDateTime();
		otpGeneratedDatenTime = otpGeneratedDatenTime.plusDays(7);
				//plusMinutes(30);

		long datePlusSevendaysInMillisecs = otpGeneratedDatenTime.atZone(ZoneId.systemDefault()).toInstant()
				.toEpochMilli();

		LocalDateTime currentDatenTime = LocalDateTime.now();

		long currentDatenTimeInMillisecs = currentDatenTime.atZone(ZoneId.systemDefault()).toInstant().toEpochMilli();
		if(currentDatenTimeInMillisecs < datePlusSevendaysInMillisecs){
			LOGGER.info("checkTimeStamp(): OTP timestamp validation OK! and checkTimeStamp() ended");
			 validateTimeStamp = true;
		}
		LOGGER.info("checkTimeStamp(): OTP timestamp validation NOT OK! and checkTimeStamp() ended");
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
				
				url = redirectUrl + "&InvitationId="+ invitationId + "&email=" + email ;
			}
			
			LOGGER.info("sendInvitationEmail URL compiled to : " + url);

			contentBuilder = new StringBuilder();
			LOGGER.info(
					"sendInvitationEmail : Content Builder Length initial:" + contentBuilder.length());
			contentBuilder.setLength(0);
			LOGGER.info(
					"sendInvitationEmail : Content Builder Length cleared:" + contentBuilder.length());

			// if section for chinese user
			if ((lang != null && lang.equals("zh")) || (hotpLanguage != null && hotpLanguage.equals("zh"))) {
				LOGGER.info("sendInvitationEmail :  Building Chinese email content..");
				subject = emailContentTemplate(email, subject, EmailConstants.HOTP_LAN_CN,hotpOperationType,email);
			}
			// Else section for English user
			else {
				LOGGER.info("sendInvitationEmail :  Building English email content..");
				subject = emailContentTemplate(email, subject, EmailConstants.HOTP_LAN_EN,hotpOperationType,email);
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
		LOGGER.info("Parameter hotpOperationType -> " + hotpOperationType+" ,userId -> "+userId+" ,appid -> "+appid);

		String userData = "";
		String to = "";
		String SMSAdmin = "cy-snddq";
		String SMSAdminPassword = "cy-snddq123";
		String template = "1139";
		String smsContent = "%E3%80%90%E6%96%BD%E8%80%90%E5%BE%B7%E7%94%B5%E6%B0%94%E3%80%91%E9%AA%8C%E8%AF%81%E7%A0%81%EF%BC%9A"
				+code+"%20%EF%BC%8C30%E5%88%86%E9%92%9F%E5%86%85%E6%9C%89%E6%95%88";
		try {

			LOGGER.info("Going to call getUser() of openamservice for sending SMS");
			userData = productService.getUser(userService.getSSOToken(), userId);
			LOGGER.info("getUser() call of openamservice finished for sending SMS with data from Openam: " + userData);
			productDocCtxUser = JsonPath.using(conf).parse(userData);

			if (hotpOperationType.equalsIgnoreCase(EmailConstants.UPDATEUSERRECORD_OPT_TYPE)) {
				to = productDocCtxUser.read("$.newmobile[0]");
			} else {
				to = productDocCtxUser.read("$.mobile[0]");
			}

			//LOGGER.info("productService.getUser : Response -> ", userData);

			LOGGER.info("Going to call sendSMSCode() of smsservice to "+to);
			Response smsResponse = smsService.sendSMSCode(SMSAdmin, SMSAdminPassword, to, smsContent, template);
			LOGGER.info("sendSMSCode() finished, Response status :: -> " + smsResponse.getStatus());
			if (200 == smsResponse.getStatus()) {
				LOGGER.info("sendSMSCode sent Succssfully :: -> "
						+ IOUtils.toString((InputStream) smsResponse.getEntity()));
			} else {
				LOGGER.error("Error occured while sendSMSCode() to "+to);
				throw new Exception(IOUtils.toString((InputStream) smsResponse.getEntity()));
			}

		} catch (Exception e) {
			e.printStackTrace();
			LOGGER.error("Exception in sendSMSMessage() while sending HOTP code : ", e.getMessage());
			// throw new AuthLoginException("Failed to send OTP code to " + to,
			// e);
		}

	}
	/**
	 * 
	 * The below method we are implementing to test the mobile scenarios we will delete this method later
	 * 
	 * */
	
	@SuppressWarnings("unused")
	public void sendOpenAmMobileEmail(String code, String hotpOperationType,String userId, String appid ){
		LOGGER.info("Entered sendOpenAmMobileEmail() -> Start");
		LOGGER.info("Parameter hotpOperationType -> " + hotpOperationType+" ,userId -> "+userId+" ,appid -> "+appid);
			
		String userData = "";
		String to = "" ;
		String subject = "";
		String lang= "";
		
		//LOGGER.info("SchneiderSMSGatewayImpl.sendEmail() : Inside sendOpenAmEmail()");
			//LOGGER.info("SchneiderSMSGatewayImpl.sendEmail() : User's Email : " + to);

			encodedHOTPcode = code;
			// get sso token.. iPlanetDirectoryKey
			LOGGER.info("Going to call getUser() of openamservice for sendOpenAmMobileEmail");
			userData = productService.getUser(userService.getSSOToken(), userId);
			LOGGER.info("getUser() call of openamuser finished with data from Openam: " + userData);
			productDocCtxUser = JsonPath.using(conf).parse(userData);
			
			
			if(hotpOperationType.equalsIgnoreCase(EmailConstants.UPDATEUSERRECORD_OPT_TYPE)){
				subject=appid;
				to = productDocCtxUser.read("$.newmobile[0]");
			}else{
				subject=productDocCtxUser.read("$.registerationSource[0]");
				to = productDocCtxUser.read("$.mobile[0]");
			}
			
			to = to.concat("@mailinator.com");
			String emailContent = "Your OpenAM One Time Password is : " + code;
			//LOGGER.info("productService.getUser : Response -> ", userData);

			lang=productDocCtxUser.read("$.preferredlanguage[0]");
			
			url = hotpEmailVerificationURL + "?userid=" + userId + "&pin=" + encodedHOTPcode + "&operationType="
					+ hotpOperationType + "&lang=" + lang + "&app=" + appid + "&uid=" + userId;
			
			LOGGER.info("URL compiled to : " + url);
			
			contentBuilder = new StringBuilder();
			LOGGER.info(
					"Content Builder Length initial:" + contentBuilder.length());
			contentBuilder.setLength(0);
			LOGGER.info(
					"Content Builder Length cleared:" + contentBuilder.length());

			// if section for chinese user
			if ((lang != null && lang.equals("zh")) || (hotpLanguage != null && hotpLanguage.equals("zh"))) {
				LOGGER.info("Building Chinese email content..");
				subject = emailContentTemplate(to, subject, EmailConstants.HOTP_LAN_CN,hotpOperationType,to);
			}
			// Else section for English user
			else {
				LOGGER.info("Building English email content..");
				subject = emailContentTemplate(to, subject, EmailConstants.HOTP_LAN_EN,hotpOperationType,to);

			}

			String tos[] = new String[1];

			tos[0] = to;
			//AMSendMail sendMail = new AMSendMail();

			LOGGER.info("sendOpenAmMobileEmail : to : " + to);
			LOGGER.info("sendOpenAmMobileEmail : subject : " + subject);
			LOGGER.info("sendOpenAmMobileEmail : content.isEmpty : " + content.isEmpty());
			LOGGER.info("sendOpenAmMobileEmail : from : " + from);

			//sendMail.postMail(tos, subject, content, from, "text/html", "UTF-8", smtpHostName, smtpHostPort,
			//smtpUserName, smtpUserPassword, sslEnabled);
				
				emailReadyToSendEmail(to, from, subject, emailContent);
				/*
				 * sendMail.postMail(tos, subject, msg, from, "UTF-8",
				 * smtpHostName, smtpHostPort, smtpUserName, smtpUserPassword,
				 * sslEnabled);
				 */

			if (LOGGER.isDebugEnabled()) {
				LOGGER.info("sendOpenAmMobileEmail : " + "HOTP sent to : " + to + ".");
			}
	}
}

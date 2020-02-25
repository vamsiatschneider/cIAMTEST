package com.idms.dynamic.mail.template.util;

import com.idms.mail.template.util.EmailTemplateColor;
import com.idms.mail.template.util.EmailTemplateInput;
import com.idms.mail.template.util.Locale;
import com.idms.mail.template.util.OperationType;
import com.idms.mail.template.util.PRMTemplateType;
import com.idms.service.SendEmail;

public class DynamicEmailTemplateInput extends EmailTemplateInput {

	private String bfoSupportUrl;
	private String subject;
	private String appName;
	private String firstName;
	private String otp;
	private String confirmationURL;
	private Locale locale;
	private OperationType operationType;
	private boolean isPRMApp;
	private PRMTemplateType prmTemplateType;
	private EmailTemplateColor etColor;
	private boolean isOTPEnabled;
	private SendEmail configuration;

	public String getBfoSupportUrl() {
		return bfoSupportUrl;
	}

	public void setBfoSupportUrl(String bfoSupportUrl) {
		this.bfoSupportUrl = bfoSupportUrl;
	}

	public String getSubject() {
		return subject;
	}

	public void setSubject(String subject) {
		this.subject = subject;
	}

	public String getFirstName() {
		return firstName;
	}

	public void setFirstName(String firstName) {
		this.firstName = firstName;
	}

	public String getOtp() {
		return otp;
	}

	public void setOtp(String otp) {
		this.otp = otp;
	}

	public String getConfirmationURL() {
		return confirmationURL;
	}

	public void setConfirmationURL(String confirmationURL) {
		this.confirmationURL = confirmationURL;
	}

	public String getAppName() {
		return appName;
	}

	public void setAppName(String appName) {
		this.appName = appName;
	}

	public Locale getLocale() {
		return locale;
	}

	public void setLocale(Locale locale) {
		this.locale = locale;
	}

	public OperationType getOperationType() {
		return operationType;
	}

	public void setOperationType(OperationType operationType) {
		this.operationType = operationType;
	}

	public boolean isPRMApp() {
		return isPRMApp;
	}

	public void setPRMApp(boolean isPRMApp) {
		this.isPRMApp = isPRMApp;
	}

	public PRMTemplateType getPrmTemplateType() {
		return prmTemplateType;
	}

	public void setPrmTemplateType(PRMTemplateType prmTemplateType) {
		this.prmTemplateType = prmTemplateType;
	}

	public EmailTemplateColor getEtColor() {
		return etColor;
	}

	public void setEtColor(EmailTemplateColor etColor) {
		this.etColor = etColor;
	}

	public boolean isOTPEnabled() {
		return isOTPEnabled;
	}

	public void setOTPEnabled(boolean isOTPEnabled) {
		this.isOTPEnabled = isOTPEnabled;
	}

	public SendEmail getConfiguration() {
		return configuration;
	}

	public void setConfiguration(SendEmail configuration) {
		this.configuration = configuration;
	}
}

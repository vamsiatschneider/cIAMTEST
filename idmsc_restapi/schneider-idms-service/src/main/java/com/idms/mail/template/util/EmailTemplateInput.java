package com.idms.mail.template.util;

import com.idms.service.SendEmail;

public class EmailTemplateInput {

	private Locale locale;
	private OperationType operationType;
	private boolean isPRMApp;
	private PRMTemplateType prmTemplateType;
	private EmailTemplateColor etColor;
	private boolean isOTPEnabled;
	private SendEmail configuration;
	
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

	public boolean isPRMApp() {
		return isPRMApp;
	}

	public void setPRMApp(boolean isPRMApp) {
		this.isPRMApp = isPRMApp;
	}

	public SendEmail getConfiguration() {
		return configuration;
	}

	public void setConfiguration(SendEmail configuration) {
		this.configuration = configuration;
	}
}

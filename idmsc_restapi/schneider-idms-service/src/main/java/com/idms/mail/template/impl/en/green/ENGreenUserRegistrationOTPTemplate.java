package com.idms.mail.template.impl.en.green;

import com.idms.mail.template.util.EmailTemplate;
import com.idms.mail.template.util.EmailTemplateInput;

public class ENGreenUserRegistrationOTPTemplate extends ENGreenDefaultTemplate {

	// @Value("${user.registration.withpwd.otp.email.template.en}")
	private String IDMS_USER_REGISTRATION_WITHPWD_OTP_EMAILTEMPLATE_EN;

	public ENGreenUserRegistrationOTPTemplate(EmailTemplateInput input) {
		super(input);
	}

	@Override
	public EmailTemplate getTemplate() {
		EmailTemplate template = new EmailTemplate();
		template.setEmailTemplatePath(IDMS_USER_REGISTRATION_WITHPWD_OTP_EMAILTEMPLATE_EN);
		return template;
	}
}

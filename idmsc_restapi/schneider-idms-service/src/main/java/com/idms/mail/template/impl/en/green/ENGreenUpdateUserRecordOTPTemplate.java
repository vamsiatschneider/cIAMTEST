package com.idms.mail.template.impl.en.green;

import com.idms.mail.template.util.EmailTemplate;
import com.idms.mail.template.util.EmailTemplateInput;

public class ENGreenUpdateUserRecordOTPTemplate extends ENGreenDefaultTemplate {
	// @Value("${user.update.otp.email.template.en}")
	private String IDMS_USER_UPDATE_OTP_EMAILTEMPLATE_EN;

	public ENGreenUpdateUserRecordOTPTemplate(EmailTemplateInput input) {
		super(input);
	}

	@Override
	public EmailTemplate getTemplate() {
		EmailTemplate template = new EmailTemplate();
		template.setEmailTemplatePath(IDMS_USER_UPDATE_OTP_EMAILTEMPLATE_EN);
		return template;
	}
}

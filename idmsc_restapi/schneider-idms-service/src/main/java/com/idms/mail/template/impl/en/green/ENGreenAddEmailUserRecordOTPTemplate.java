package com.idms.mail.template.impl.en.green;

import com.idms.mail.template.util.EmailTemplate;
import com.idms.mail.template.util.EmailTemplateInput;

public class ENGreenAddEmailUserRecordOTPTemplate extends ENGreenDefaultTemplate {
	//	@Value("${user.add.otp.email.template.en}")
	private String IDMS_USER_ADD_OTP_EMAILTEMPLATE_EN;

	public ENGreenAddEmailUserRecordOTPTemplate(EmailTemplateInput input) {
		super(input);
	}
	
	@Override
	public EmailTemplate getTemplate() {
		EmailTemplate template = new EmailTemplate();
		template.setEmailTemplatePath(IDMS_USER_ADD_OTP_EMAILTEMPLATE_EN);
		return template;
	}
}

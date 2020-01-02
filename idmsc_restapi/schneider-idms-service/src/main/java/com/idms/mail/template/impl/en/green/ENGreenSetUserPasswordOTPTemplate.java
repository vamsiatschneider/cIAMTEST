package com.idms.mail.template.impl.en.green;

import com.idms.mail.template.util.EmailTemplate;
import com.idms.mail.template.util.EmailTemplateInput;

public class ENGreenSetUserPasswordOTPTemplate extends ENGreenDefaultTemplate{

//	@Value("${user.reset.password.otp.email.template.en}")
	private String IDMS_USER_RESET_PASSWORD_OTP_EMAILTEMPLATE_EN;
	
	public ENGreenSetUserPasswordOTPTemplate(EmailTemplateInput input) {
		super(input);
	}
	
	@Override
	public EmailTemplate getTemplate() {
		EmailTemplate template = new EmailTemplate();
		template.setEmailTemplatePath(IDMS_USER_RESET_PASSWORD_OTP_EMAILTEMPLATE_EN);
		return template;
	}
}

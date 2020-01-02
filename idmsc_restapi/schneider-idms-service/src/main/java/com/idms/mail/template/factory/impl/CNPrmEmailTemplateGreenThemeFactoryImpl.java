package com.idms.mail.template.factory.impl;

import com.idms.mail.template.factory.EmailTemplateFactory;
import com.idms.mail.template.impl.cn.green.prm.CNGreenEclipseRegistrationTemplate;
import com.idms.mail.template.impl.cn.green.prm.CNGreenInternalRegistrationTemplate;
import com.idms.mail.template.impl.cn.green.prm.CNGreenSelfRegistrationTemplate;
import com.idms.mail.template.util.EmailTemplate;
import com.idms.mail.template.util.EmailTemplateInput;

public class CNPrmEmailTemplateGreenThemeFactoryImpl implements EmailTemplateFactory {

	private EmailTemplateInput input;

	public CNPrmEmailTemplateGreenThemeFactoryImpl(EmailTemplateInput input) {
		this.input = input;
	}
	
	@Override
	public EmailTemplate getTemplate() {
		EmailTemplate template = new EmailTemplate();
		switch (input.getPrmTemplateType()) {
		case PRM_SELF_REGISTRATION:
			template = new CNGreenSelfRegistrationTemplate(input).getTemplate();
			break;
		case PRM_INTERNAL_REGISTRATION:
			template = new CNGreenInternalRegistrationTemplate(input).getTemplate();
			break;
		case PRM_ECLIPSE_REGISTRATION:
			template = new CNGreenEclipseRegistrationTemplate(input).getTemplate();
			break;
		default:
			template = new CNGreenSelfRegistrationTemplate(input).getTemplate();
			break;
		}
		return template;
	}

}

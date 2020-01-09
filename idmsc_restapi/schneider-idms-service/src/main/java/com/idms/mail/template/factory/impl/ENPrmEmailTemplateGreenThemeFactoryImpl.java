package com.idms.mail.template.factory.impl;

import com.idms.mail.template.factory.EmailTemplateFactory;
import com.idms.mail.template.impl.en.green.prm.ENGreenEclipseRegistrationTemplate;
import com.idms.mail.template.impl.en.green.prm.ENGreenInternalRegistrationTemplate;
import com.idms.mail.template.impl.en.green.prm.ENGreenSelfRegistrationTemplate;
import com.idms.mail.template.util.EmailTemplate;
import com.idms.mail.template.util.EmailTemplateInput;

public class ENPrmEmailTemplateGreenThemeFactoryImpl implements EmailTemplateFactory {

	private EmailTemplateInput input;

	public ENPrmEmailTemplateGreenThemeFactoryImpl(EmailTemplateInput input) {
		this.input = input;
	}
	@Override
	public EmailTemplate getTemplate() {
		EmailTemplate template = new EmailTemplate();
		switch (input.getPrmTemplateType()) {
		case PRM_SELF_REGISTRATION:
			template = new ENGreenSelfRegistrationTemplate(input).getTemplate();
			break;
		case PRM_INTERNAL_REGISTRATION:
			template = new ENGreenInternalRegistrationTemplate(input).getTemplate();
			break;
		case PRM_ECLIPSE_REGISTRATION:
			template = new ENGreenEclipseRegistrationTemplate(input).getTemplate();
			break;
		default:
			template = new ENGreenSelfRegistrationTemplate(input).getTemplate();
			break;
		}
		return template;
	}

}

package com.idms.mail.template.factory.impl;

import com.idms.mail.template.factory.EmailTemplateFactory;
import com.idms.mail.template.util.EmailTemplate;
import com.idms.mail.template.util.EmailTemplateInput;

public class ENPrmEmailTemplateBlueThemeFactoryImpl implements EmailTemplateFactory {

	private EmailTemplateInput input;

	public ENPrmEmailTemplateBlueThemeFactoryImpl(EmailTemplateInput input) {
		this.input = input;
	}
	@Override
	public EmailTemplate getTemplate() {
		switch (input.getPrmTemplateType()) {
		case PRM_SELF_REGISTRATION:
		case PRM_INTERNAL_REGISTRATION:
		case PRM_ECLIPSE_REGISTRATION:
		default:
			break;
		}
		return null;
	}

}

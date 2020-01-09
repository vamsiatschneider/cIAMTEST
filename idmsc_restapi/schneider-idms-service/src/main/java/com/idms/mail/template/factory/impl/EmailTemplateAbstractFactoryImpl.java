package com.idms.mail.template.factory.impl;

import com.idms.mail.template.factory.EmailTemplateAbstractFactory;
import com.idms.mail.template.factory.EmailTemplateFactory;
import com.idms.mail.template.util.EmailTemplate;
import com.idms.mail.template.util.EmailTemplateInput;

public class EmailTemplateAbstractFactoryImpl implements EmailTemplateAbstractFactory{

	private EmailTemplateInput input;
	
	public EmailTemplateAbstractFactoryImpl(EmailTemplateInput input) {
		this.input = input;
	}
	
	@Override
	public EmailTemplate getEmailTemplateFactory() {
		EmailTemplateFactory factory;
		switch(input.getLocale()) {
			case CN:
				factory = new CNEmailTemplateThemeFactoryImpl(input).getEmailTemplateFactory();
				break;
			default:
				factory = new ENEmailTemplateThemeFactoryImpl(input).getEmailTemplateFactory();
				break;
		}
		return factory.getTemplate();
	}
}

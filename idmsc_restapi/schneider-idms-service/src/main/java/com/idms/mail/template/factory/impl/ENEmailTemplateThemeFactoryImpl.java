package com.idms.mail.template.factory.impl;

import com.idms.mail.template.factory.EmailTemplateFactory;
import com.idms.mail.template.factory.EmailTemplateThemeFactory;
import com.idms.mail.template.util.EmailTemplateInput;

public class ENEmailTemplateThemeFactoryImpl implements EmailTemplateThemeFactory {

	private EmailTemplateInput input;

	public ENEmailTemplateThemeFactoryImpl(EmailTemplateInput input) {
		this.input = input;
	}
	
	@Override
	public EmailTemplateFactory getEmailTemplateFactory() {
		EmailTemplateFactory factory;
		switch(input.getEtColor()) {
			case BLUE:
				factory = new ENEmailTemplateBlueThemeFactoryImpl(input); 
				break;
			default:
				if(input.isOTPEnabled()) {
					factory = new ENOTPEmailTemplateGreenThemeFactoryImpl(input);
					break;
				}else {
					factory = new ENEmailTemplateGreenThemeFactoryImpl(input);
					break;
				}
		}
		return factory;
	}

}

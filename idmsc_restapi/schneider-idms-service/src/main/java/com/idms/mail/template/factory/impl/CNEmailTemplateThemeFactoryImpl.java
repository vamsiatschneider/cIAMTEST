package com.idms.mail.template.factory.impl;

import com.idms.mail.template.factory.EmailTemplateFactory;
import com.idms.mail.template.factory.EmailTemplateThemeFactory;
import com.idms.mail.template.util.EmailTemplateInput;

public class CNEmailTemplateThemeFactoryImpl implements EmailTemplateThemeFactory {

	private EmailTemplateInput input;

	public CNEmailTemplateThemeFactoryImpl(EmailTemplateInput input) {
		this.input = input;
	}

	@Override
	public EmailTemplateFactory getEmailTemplateFactory() {
		EmailTemplateFactory factory;
		switch(input.getEtColor()) {
			case BLUE:
				factory = new CNEmailTemplateBlueThemeFactoryImpl(input); 
				break;
			default:
				if(input.isOTPEnabled()) {
					factory = new CNOTPEmailTemplateGreenThemeFactoryImpl(input);
					break;
				}else {
					factory = new CNEmailTemplateGreenThemeFactoryImpl(input); 
					break;
				}
		}
		return factory;
	}
	
	

}

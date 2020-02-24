package com.idms.dynamic.mail.template.factory.impl;

import com.idms.dynamic.mail.template.factory.DynamicEmailTemplateFactory;
import com.idms.dynamic.mail.template.util.DynamicEmailTemplate;
import com.idms.dynamic.mail.template.util.DynamicEmailTemplateInput;

public class DynamicEmailTemplateFactoryImpl implements DynamicEmailTemplateFactory {

	private DynamicEmailTemplateInput input;

	public DynamicEmailTemplateFactoryImpl(DynamicEmailTemplateInput input) {
		this.input = input;
	}
	
	@Override
	public DynamicEmailTemplate getEmailTemplate() {
		DynamicEmailTemplate template;
		if(input.isOTPEnabled()) {
			template = new DynamicOTPEmailTemplateFactoryImpl(input).getEmailTemplate();
		}else {
			template = new DynamicLinkEmailTemplateFactoryImpl(input).getEmailTemplate();
		}
		return template;
	}
}

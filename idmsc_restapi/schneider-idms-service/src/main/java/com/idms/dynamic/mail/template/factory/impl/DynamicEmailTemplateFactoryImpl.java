package com.idms.dynamic.mail.template.factory.impl;

import com.idms.dynamic.mail.template.factory.DynamicEmailTemplateFactory;
import com.idms.dynamic.mail.template.impl.DynamicChangeEmailTemplate;
import com.idms.dynamic.mail.template.util.DynamicEmailTemplate;
import com.idms.dynamic.mail.template.util.DynamicEmailTemplateInput;
import com.idms.mail.template.util.OperationType;

public class DynamicEmailTemplateFactoryImpl implements DynamicEmailTemplateFactory {

	private DynamicEmailTemplateInput input;

	public DynamicEmailTemplateFactoryImpl(DynamicEmailTemplateInput input) {
		this.input = input;
	}
	
	@Override
	public DynamicEmailTemplate getEmailTemplate() {
		DynamicEmailTemplate template;
		if(OperationType.CHANGE_EMAIL_NOTIFICATION.getType().equals(input.getOperationType().getType())) {
			return new DynamicChangeEmailTemplate(input).getTemplate();
		}
		if(input.isOTPEnabled()) {
			template = new DynamicOTPEmailTemplateFactoryImpl(input).getEmailTemplate();
		}else {
			template = new DynamicLinkEmailTemplateFactoryImpl(input).getEmailTemplate();
		}
		return template;
	}
}

package com.idms.dynamic.mail.template.factory.impl;

import com.idms.dynamic.mail.template.factory.DynamicEmailTemplateFactory;
import com.idms.dynamic.mail.template.impl.DynamicAddEmailOTPTemplate;
import com.idms.dynamic.mail.template.impl.DynamicPRMInternalRegistrationTemplate;
import com.idms.dynamic.mail.template.impl.DynamicResetPasswordOTPTemplate;
import com.idms.dynamic.mail.template.impl.DynamicUserRegistrationOTPTemplate;
import com.idms.dynamic.mail.template.util.DynamicEmailTemplate;
import com.idms.dynamic.mail.template.util.DynamicEmailTemplateInput;
import com.idms.mail.template.util.PRMTemplateType;

public class DynamicOTPEmailTemplateFactoryImpl implements DynamicEmailTemplateFactory {

	private DynamicEmailTemplateInput input;

	public DynamicOTPEmailTemplateFactoryImpl(DynamicEmailTemplateInput input) {
		this.input = input;
	}

	@Override
	public DynamicEmailTemplate getEmailTemplate() {
		DynamicEmailTemplate emailTemplate;
		switch (input.getOperationType()) {
			case USER_REGISTRATION:
				if(input.isPRMApp() && PRMTemplateType.PRM_INTERNAL_REGISTRATION.equals(input.getPrmTemplateType())) {
					emailTemplate = new DynamicPRMInternalRegistrationTemplate(input).getTemplate();
				}else {
					emailTemplate = new DynamicUserRegistrationOTPTemplate(input).getTemplate();
				}
				break;
			case SET_USER_PASSWORD:
				emailTemplate = new DynamicResetPasswordOTPTemplate(input).getTemplate();
				break;
			case ADD_EMAIL_USER_RECORD:
				emailTemplate = new DynamicAddEmailOTPTemplate(input).getTemplate();
				break;
//			case UPDATE_USER_RECORD:
//				break;
//			case SEND_INVITATION:
//				break;
			default:
				// Change if a common default template is required for all the operations.
				emailTemplate = new DynamicUserRegistrationOTPTemplate(input).getTemplate();
				break;
		}
		return emailTemplate;
	}

}

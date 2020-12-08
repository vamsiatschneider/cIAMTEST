package com.idms.dynamic.mail.template.factory.impl;

import com.idms.dynamic.mail.template.factory.DynamicEmailTemplateFactory;
import com.idms.dynamic.mail.template.impl.Dynamic2FAUserTemplate;
import com.idms.dynamic.mail.template.impl.DynamicAddEmailLinkTemplate;
import com.idms.dynamic.mail.template.impl.DynamicPRMInternalRegistrationTemplate;
import com.idms.dynamic.mail.template.impl.DynamicResetPasswordLinkTemplate;
import com.idms.dynamic.mail.template.impl.DynamicUpdateUserLinkTemplate;
import com.idms.dynamic.mail.template.impl.DynamicUserRegistrationLinkTemplate;
import com.idms.dynamic.mail.template.util.DynamicEmailTemplate;
import com.idms.dynamic.mail.template.util.DynamicEmailTemplateInput;
import com.idms.mail.template.util.PRMTemplateType;

public class DynamicLinkEmailTemplateFactoryImpl implements DynamicEmailTemplateFactory {

	private DynamicEmailTemplateInput input;

	public DynamicLinkEmailTemplateFactoryImpl(DynamicEmailTemplateInput input) {
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
					emailTemplate = new DynamicUserRegistrationLinkTemplate(input).getTemplate();
				}
				break;
			case SET_USER_PASSWORD:
				emailTemplate = new DynamicResetPasswordLinkTemplate(input).getTemplate();
				break;
			case ADD_EMAIL_USER_RECORD:
				emailTemplate = new DynamicAddEmailLinkTemplate(input).getTemplate();
				break;
			case UPDATE_USER_RECORD:
				emailTemplate = new DynamicUpdateUserLinkTemplate(input).getTemplate();
				break;
			case TWO_FACTOR_AUTHENTICATION:
				emailTemplate = new Dynamic2FAUserTemplate(input).getTemplate();
				break;
//			case SEND_INVITATION:
//				break;
			default:
				// Change if a common default template is required for all the operations.
				emailTemplate = new DynamicUserRegistrationLinkTemplate(input).getTemplate();
				break;
		}
		return emailTemplate;
	}
}

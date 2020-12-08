package com.idms.mail.template.factory.impl;

import com.idms.mail.template.factory.EmailTemplateFactory;
import com.idms.mail.template.impl.cn.green.CNGreen2FAUserTemplate;
import com.idms.mail.template.impl.cn.green.CNGreenAddEmailUserRecordTemplate;
import com.idms.mail.template.impl.cn.green.CNGreenDefaultTemplate;
import com.idms.mail.template.impl.cn.green.CNGreenSendInvitationTemplate;
import com.idms.mail.template.impl.cn.green.CNGreenSetUserPasswordTemplate;
import com.idms.mail.template.impl.cn.green.CNGreenUpdateUserRecordTemplate;
import com.idms.mail.template.impl.cn.green.CNGreenUserRegistrationTemplate;
import com.idms.mail.template.util.EmailTemplate;
import com.idms.mail.template.util.EmailTemplateInput;

public class CNEmailTemplateGreenThemeFactoryImpl implements EmailTemplateFactory {

	private EmailTemplateInput input;

	public CNEmailTemplateGreenThemeFactoryImpl(EmailTemplateInput input) {
		this.input = input;
	}

	@Override
	public EmailTemplate getTemplate() {

		EmailTemplate emailTemplate;
		switch (input.getOperationType()) {
			case SET_USER_PASSWORD:
				emailTemplate = new CNGreenSetUserPasswordTemplate(input).getTemplate();
				break;
			case USER_REGISTRATION:
				if(input.isPRMApp()) {
					emailTemplate = new CNPrmEmailTemplateGreenThemeFactoryImpl(input).getTemplate();
				}else {
					emailTemplate = new CNGreenUserRegistrationTemplate(input).getTemplate();
				}
				break;
			case UPDATE_USER_RECORD:
				emailTemplate = new CNGreenUpdateUserRecordTemplate(input).getTemplate();
				break;
			case ADD_EMAIL_USER_RECORD:
				emailTemplate = new CNGreenAddEmailUserRecordTemplate(input).getTemplate();
				break;
			case TWO_FACTOR_AUTHENTICATION:
				emailTemplate = new CNGreen2FAUserTemplate(input).getTemplate();
				break;
			case SEND_INVITATION:
				emailTemplate = new CNGreenSendInvitationTemplate(input).getTemplate();
				break;
			default:
				emailTemplate = new CNGreenDefaultTemplate(input).getTemplate();
				break;
		}
		return emailTemplate;
	}
}

package com.idms.mail.template.factory.impl;

import com.idms.mail.template.factory.EmailTemplateFactory;
import com.idms.mail.template.impl.cn.blue.CNBlue2FAUserTemplate;
import com.idms.mail.template.impl.cn.blue.CNBlueAddEmailUserRecordTemplate;
import com.idms.mail.template.impl.cn.blue.CNBlueDefaultTemplate;
import com.idms.mail.template.impl.cn.blue.CNBlueSendInvitationTemplate;
import com.idms.mail.template.impl.cn.blue.CNBlueSetUserPasswordTemplate;
import com.idms.mail.template.impl.cn.blue.CNBlueUpdateUserRecordTemplate;
import com.idms.mail.template.impl.cn.blue.CNBlueUserRegistrationTemplate;
import com.idms.mail.template.util.EmailTemplate;
import com.idms.mail.template.util.EmailTemplateInput;

public class CNEmailTemplateBlueThemeFactoryImpl implements EmailTemplateFactory {

	private EmailTemplateInput input;

	public CNEmailTemplateBlueThemeFactoryImpl(EmailTemplateInput input) {
		this.input = input;
	}

	@Override
	public EmailTemplate getTemplate() {
		EmailTemplate emailTemplate;
		switch (input.getOperationType()) {
			case SET_USER_PASSWORD:
				emailTemplate = new CNBlueSetUserPasswordTemplate(input).getTemplate();
				break;
			case USER_REGISTRATION:
				if (input.isPRMApp()) {
					emailTemplate = new CNPrmEmailTemplateGreenThemeFactoryImpl(input).getTemplate();
				} else {
					emailTemplate = new CNBlueUserRegistrationTemplate(input).getTemplate();
				}
				break;
			case UPDATE_USER_RECORD:
				emailTemplate = new CNBlueUpdateUserRecordTemplate(input).getTemplate();
				break;
			case ADD_EMAIL_USER_RECORD:
				emailTemplate = new CNBlueAddEmailUserRecordTemplate(input).getTemplate();
				break;
			case TWO_FACTOR_AUTHENTICATION:
				emailTemplate = new CNBlue2FAUserTemplate(input).getTemplate();
				break;
			case SEND_INVITATION:
				emailTemplate = new CNBlueSendInvitationTemplate(input).getTemplate();
				break;
			default:
				emailTemplate = new CNBlueDefaultTemplate(input).getTemplate();
				break;
			}
		return emailTemplate;
	}

}

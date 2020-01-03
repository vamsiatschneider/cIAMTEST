package com.idms.mail.template.factory.impl;

import com.idms.mail.template.factory.EmailTemplateFactory;
import com.idms.mail.template.impl.en.blue.ENBlueAddEmailUserRecordTemplate;
import com.idms.mail.template.impl.en.blue.ENBlueDefaultTemplate;
import com.idms.mail.template.impl.en.blue.ENBlueSendInvitationTemplate;
import com.idms.mail.template.impl.en.blue.ENBlueSetUserPasswordTemplate;
import com.idms.mail.template.impl.en.blue.ENBlueUpdateUserRecordTemplate;
import com.idms.mail.template.impl.en.blue.ENBlueUserRegistrationTemplate;
import com.idms.mail.template.util.EmailTemplate;
import com.idms.mail.template.util.EmailTemplateInput;

public class ENEmailTemplateBlueThemeFactoryImpl implements EmailTemplateFactory {

	private EmailTemplateInput input;

	public ENEmailTemplateBlueThemeFactoryImpl(EmailTemplateInput input) {
		this.input = input;
	}

	@Override
	public EmailTemplate getTemplate() {
		EmailTemplate emailTemplate;
		switch (input.getOperationType()) {
			case SET_USER_PASSWORD:
				emailTemplate = new ENBlueSetUserPasswordTemplate(input).getTemplate();
				break;
			case USER_REGISTRATION:
				if (input.isPRMApp()) {
					emailTemplate = new ENPrmEmailTemplateGreenThemeFactoryImpl(input).getTemplate();
				} else {
					emailTemplate = new ENBlueUserRegistrationTemplate(input).getTemplate();
				}
				break;
			case UPDATE_USER_RECORD:
				emailTemplate = new ENBlueUpdateUserRecordTemplate(input).getTemplate();
				break;
			case ADD_EMAIL_USER_RECORD:
				emailTemplate = new ENBlueAddEmailUserRecordTemplate(input).getTemplate();
				break;
			case SEND_INVITATION:
				emailTemplate = new ENBlueSendInvitationTemplate(input).getTemplate();
				break;
			default:
				emailTemplate = new ENBlueDefaultTemplate(input).getTemplate();
				break;
			}
		return emailTemplate;
	}

}

package com.idms.mail.template.factory.impl;

import com.idms.mail.template.factory.EmailTemplateFactory;
import com.idms.mail.template.impl.en.green.ENGreen2FAUserTemplate;
import com.idms.mail.template.impl.en.green.ENGreenAddEmailUserRecordTemplate;
import com.idms.mail.template.impl.en.green.ENGreenDefaultTemplate;
import com.idms.mail.template.impl.en.green.ENGreenSendInvitationTemplate;
import com.idms.mail.template.impl.en.green.ENGreenSetUserPasswordTemplate;
import com.idms.mail.template.impl.en.green.ENGreenUpdateUserRecordTemplate;
import com.idms.mail.template.impl.en.green.ENGreenUserRegistrationTemplate;
import com.idms.mail.template.util.EmailTemplate;
import com.idms.mail.template.util.EmailTemplateInput;

public class ENEmailTemplateGreenThemeFactoryImpl implements EmailTemplateFactory {

	private EmailTemplateInput input;

	public ENEmailTemplateGreenThemeFactoryImpl(EmailTemplateInput input) {
		this.input = input;
	}

	@Override
	public EmailTemplate getTemplate() {
		EmailTemplate emailTemplate;
		switch (input.getOperationType()) {
			case SET_USER_PASSWORD:
				emailTemplate = new ENGreenSetUserPasswordTemplate(input).getTemplate();
				break;
			case USER_REGISTRATION:
				if(input.isPRMApp()) {
					emailTemplate = new ENPrmEmailTemplateGreenThemeFactoryImpl(input).getTemplate();
				}else {
					emailTemplate = new ENGreenUserRegistrationTemplate(input).getTemplate();
				}
				break;
			case UPDATE_USER_RECORD:
				emailTemplate = new ENGreenUpdateUserRecordTemplate(input).getTemplate();
				break;
			case ADD_EMAIL_USER_RECORD:
				emailTemplate = new ENGreenAddEmailUserRecordTemplate(input).getTemplate();
				break;
			case TWO_FACTOR_AUTHENTICATION:
				emailTemplate = new ENGreen2FAUserTemplate(input).getTemplate();
				break;
			case SEND_INVITATION:
				emailTemplate = new ENGreenSendInvitationTemplate(input).getTemplate();
				break;
			default:
				emailTemplate = new ENGreenDefaultTemplate(input).getTemplate();
				break;
		}
		return emailTemplate;
	}

}

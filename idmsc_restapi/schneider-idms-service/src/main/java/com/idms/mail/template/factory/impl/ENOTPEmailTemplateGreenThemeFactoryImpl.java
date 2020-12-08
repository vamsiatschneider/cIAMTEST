package com.idms.mail.template.factory.impl;

import com.idms.mail.template.factory.EmailTemplateFactory;
import com.idms.mail.template.impl.en.blue.ENBlue2FAUserTemplate;
import com.idms.mail.template.impl.en.green.ENGreen2FAUserTemplate;
import com.idms.mail.template.impl.en.green.ENGreenAddEmailUserRecordOTPTemplate;
import com.idms.mail.template.impl.en.green.ENGreenDefaultOTPTemplate;
import com.idms.mail.template.impl.en.green.ENGreenSendInvitationTemplate;
import com.idms.mail.template.impl.en.green.ENGreenSetUserPasswordOTPTemplate;
import com.idms.mail.template.impl.en.green.ENGreenUpdateUserRecordOTPTemplate;
import com.idms.mail.template.impl.en.green.ENGreenUserRegistrationOTPTemplate;
import com.idms.mail.template.util.EmailTemplate;
import com.idms.mail.template.util.EmailTemplateInput;

public class ENOTPEmailTemplateGreenThemeFactoryImpl implements EmailTemplateFactory {

	private EmailTemplateInput input;

	public ENOTPEmailTemplateGreenThemeFactoryImpl(EmailTemplateInput input) {
		this.input = input;
	}

	@Override
	public EmailTemplate getTemplate() {

		EmailTemplate emailTemplate;
		switch (input.getOperationType()) {
			case SET_USER_PASSWORD:
				emailTemplate = new ENGreenSetUserPasswordOTPTemplate(input).getTemplate();
				break;
			case USER_REGISTRATION:
				if(input.isPRMApp()) {
					emailTemplate = new ENPrmEmailTemplateGreenThemeFactoryImpl(input).getTemplate();
				}else {
					emailTemplate = new ENGreenUserRegistrationOTPTemplate(input).getTemplate();
				}
				break;
			case UPDATE_USER_RECORD:
				emailTemplate = new ENGreenUpdateUserRecordOTPTemplate(input).getTemplate();
				break;
			case ADD_EMAIL_USER_RECORD:
				emailTemplate = new ENGreenAddEmailUserRecordOTPTemplate(input).getTemplate();
				break;
			case TWO_FACTOR_AUTHENTICATION:
				emailTemplate = new ENGreen2FAUserTemplate(input).getTemplate();
				break;
			case SEND_INVITATION:
				emailTemplate = new ENGreenSendInvitationTemplate(input).getTemplate();
				break;
			default:
				emailTemplate = new ENGreenDefaultOTPTemplate(input).getTemplate();
				break;
		}
		return emailTemplate;
	
	}

}

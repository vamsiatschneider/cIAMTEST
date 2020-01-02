package com.idms.mail.template.factory.impl;

import com.idms.mail.template.factory.EmailTemplateFactory;
import com.idms.mail.template.impl.cn.green.CNGreenAddEmailUserRecordOTPTemplate;
import com.idms.mail.template.impl.cn.green.CNGreenDefaultTemplate;
import com.idms.mail.template.impl.cn.green.CNGreenSendInvitationTemplate;
import com.idms.mail.template.impl.cn.green.CNGreenSetUserPasswordOTPTemplate;
import com.idms.mail.template.impl.cn.green.CNGreenUpdateUserRecordOTPTemplate;
import com.idms.mail.template.impl.cn.green.CNGreenUserRegistrationOTPTemplate;
import com.idms.mail.template.util.EmailTemplate;
import com.idms.mail.template.util.EmailTemplateInput;

public class CNOTPEmailTemplateGreenThemeFactoryImpl implements EmailTemplateFactory {

	private EmailTemplateInput input;

	public CNOTPEmailTemplateGreenThemeFactoryImpl(EmailTemplateInput input) {
		this.input = input;
	}

	@Override
	public EmailTemplate getTemplate() {

		EmailTemplate emailTemplate;
		switch (input.getOperationType()) {
			case SET_USER_PASSWORD:
				emailTemplate = new CNGreenSetUserPasswordOTPTemplate(input).getTemplate();
				break;
			case USER_REGISTRATION:
				if(input.isPRMApp()) {
					emailTemplate = new CNPrmEmailTemplateGreenThemeFactoryImpl(input).getTemplate();
				}else {
					emailTemplate = new CNGreenUserRegistrationOTPTemplate(input).getTemplate();
				}
				break;
			case UPDATE_USER_RECORD:
				emailTemplate = new CNGreenUpdateUserRecordOTPTemplate(input).getTemplate();
				break;
			case ADD_EMAIL_USER_RECORD:
				emailTemplate = new CNGreenAddEmailUserRecordOTPTemplate(input).getTemplate();
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

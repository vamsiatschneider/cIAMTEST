package com.idms.dynamic.mail.template;

import org.apache.commons.lang3.StringUtils;

import com.idms.dynamic.mail.template.util.DynamicEmailTemplateInput;
import com.idms.mail.template.util.OperationType;
import com.se.idms.cache.utils.EmailConstants;

public class OTPBlueTemplatePlaceholderCNSubstitutor extends TemplatePlaceholderSubstitutor {

	public OTPBlueTemplatePlaceholderCNSubstitutor(DynamicEmailTemplateInput input, OpenDJAttributes openDJAttributes) {
		this.openDJAttributes = openDJAttributes;
		this.input = input;
	}

	@Override
	public void buildEmailBodyPlaceholderValues() {
		placeholderValues.add(openDJAttributes.get_bodyContent1CN());
		placeholderValues.add(input.getAppName());
		placeholderValues.add(openDJAttributes.get_bodyContent2CN());
		placeholderValues.add(openDJAttributes.get_bodyContentOtpMsg1CN());
		placeholderValues.add(input.getOtp());
		placeholderValues.add(openDJAttributes.get_bodyContentOtpMsg2CN());
		placeholderValues.add(openDJAttributes.get_bodySalutationCN());
		placeholderValues.add(input.getFirstName());
		placeholderValues.add(openDJAttributes.get_bodySignOffCN());
		if(input.getOperationType().getType().equals(OperationType.USER_REGISTRATION.getType())) {
			placeholderValues.add(openDJAttributes.get_bodyOTPNote1CN());
		} else {
			placeholderValues.add(openDJAttributes.get_bodyLinkNote1CN());
		}
		placeholderValues.add("15");
		placeholderValues.add(openDJAttributes.get_bodyOTPNote2CN());
		if(StringUtils.isNotBlank(input.getBfoSupportUrl())) {
			placeholderValues.add(input.getBfoSupportUrl());
		}else {
			placeholderValues.add(openDJAttributes.get_bodySupportMail());
		}
		placeholderValues.add(openDJAttributes.get_bodySupportText());
		placeholderValues.add(openDJAttributes.get_bodySignoff1CN());
		placeholderValues.add(openDJAttributes.get_bodySignoff2CN());
		// get subject from OpenDJ and update it per operationType
		String emailSubject = input.getSubject() + EmailConstants.HYPHEN + openDJAttributes.get_subjectCN();
		input.setSubject(emailSubject);	
	}

	@Override
	public void buildEmailProfacePlaceholderValues() {
		
		placeholderValues.add(openDJAttributes.get_bodyBlueColorCode());
	}

	@Override
	public void buildEmailFooterCRightTextPlaceholderValues() {
		placeholderValues.add(openDJAttributes.get_footerCopyrightTextCN());
		placeholderValues.add(openDJAttributes.get_footerDontReplyTextCN());
		
	}
	
}

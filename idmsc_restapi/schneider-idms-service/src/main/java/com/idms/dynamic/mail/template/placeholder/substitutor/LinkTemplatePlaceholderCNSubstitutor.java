package com.idms.dynamic.mail.template.placeholder.substitutor;

import org.apache.commons.lang3.StringUtils;

import com.idms.dynamic.mail.template.util.DynamicEmailTemplateInput;
import com.idms.dynamic.mail.template.util.OpenDJAttributes;
import com.idms.mail.template.util.OperationType;
import com.se.idms.cache.utils.EmailConstants;

public class LinkTemplatePlaceholderCNSubstitutor extends TemplatePlaceholderSubstitutor {

	public LinkTemplatePlaceholderCNSubstitutor(DynamicEmailTemplateInput input, OpenDJAttributes openDJAttributes) {
		this.openDJAttributes = openDJAttributes;
		this.input = input;
	}
	
	@Override
	public void buildEmailBodyPlaceholderValues() {
		placeholderValues.add(openDJAttributes.get_bodyContent1CN());
		placeholderValues.add(input.getAppName());
		placeholderValues.add(openDJAttributes.get_bodyContent2CN());
		placeholderValues.add(openDJAttributes.get_bodySalutationCN());
		placeholderValues.add(input.getFirstName());
		placeholderValues.add(openDJAttributes.get_bodySignOffCN());
		placeholderValues.add(openDJAttributes.get_bodyContentLinkMsgCN());
		placeholderValues.add(input.getConfirmationURL());
		placeholderValues.add(openDJAttributes.get_bodyContentConfirmBtnCN());
		placeholderValues.add(openDJAttributes.get_bodyLinkNote1CN());
		placeholderValues.add("7");
		placeholderValues.add(openDJAttributes.get_bodyLinkNote2CN());
		if(StringUtils.isNotBlank(input.getBfoSupportUrl())) {
			placeholderValues.add(input.getBfoSupportUrl());
		}else {
			placeholderValues.add(openDJAttributes.get_bodySupportMail());
		}
		placeholderValues.add(openDJAttributes.get_bodySupportText());
		placeholderValues.add(openDJAttributes.get_bodySignoff1CN());
		placeholderValues.add(openDJAttributes.get_bodySignoff2CN());
		// get subject from OpenDJ and update it per operationType
		String emailSubject = "";
		if (OperationType.CHANGE_EMAIL_NOTIFICATION.getType().equals(input.getOperationType().getType())) {
			emailSubject = openDJAttributes.get_subjectCN();
		} else {
			emailSubject = input.getSubject() + EmailConstants.HYPHEN + openDJAttributes.get_subjectCN();
		}
		input.setSubject(emailSubject);	
	}

	@Override
	protected void buildEmailFooterCRightTextPlaceholderValues() {
		placeholderValues.add(openDJAttributes.get_footerCopyrightTextCN());
		placeholderValues.add(openDJAttributes.get_footerDontReplyTextCN());
	}
	
}

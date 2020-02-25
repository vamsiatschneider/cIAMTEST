package com.idms.dynamic.mail.template;

import org.apache.commons.lang3.StringUtils;

import com.idms.dynamic.mail.template.util.DynamicEmailTemplateInput;
import com.se.idms.cache.utils.EmailConstants;

public class LinkBlueTemplatePlaceholderENSubstitutor extends TemplatePlaceholderSubstitutor {

	public LinkBlueTemplatePlaceholderENSubstitutor(DynamicEmailTemplateInput input,
			OpenDJAttributes openDJAttributes) {
		this.input = input;
		this.openDJAttributes = openDJAttributes;
	}

	@Override
	public void buildEmailBodyPlaceholderValues() {
		placeholderValues.add(openDJAttributes.get_bodyContent1EN());
		placeholderValues.add(input.getAppName());
		placeholderValues.add(openDJAttributes.get_bodyContent2EN());
		placeholderValues.add(openDJAttributes.get_bodySalutationEN());
		placeholderValues.add(input.getFirstName());
		placeholderValues.add(openDJAttributes.get_bodySignOffEN());
		placeholderValues.add(openDJAttributes.get_bodyContentLinkMsgEN());
		placeholderValues.add(input.getConfirmationURL());
		placeholderValues.add(openDJAttributes.get_bodyContentConfirmBtnEN());
		placeholderValues.add(openDJAttributes.get_bodyLinkNote1EN());
		placeholderValues.add("7");
		placeholderValues.add(openDJAttributes.get_bodyLinkNote2EN());
		if(StringUtils.isNotBlank(input.getBfoSupportUrl())) {
			placeholderValues.add(input.getBfoSupportUrl());
		}else {
			placeholderValues.add(openDJAttributes.get_bodySupportMail());
		}
		placeholderValues.add(openDJAttributes.get_bodySupportText());
		placeholderValues.add(openDJAttributes.get_bodySignoff1EN());
		placeholderValues.add(openDJAttributes.get_bodySignoff2EN());
		// get subject from OpenDJ and update it per operationType
		String emailSubject = input.getSubject() + EmailConstants.HYPHEN + openDJAttributes.get_subjectEN();
		input.setSubject(emailSubject);	
	}

	@Override
	public void buildEmailProfacePlaceholderValues() {
		
		placeholderValues.add(openDJAttributes.get_bodyBlueColorCode());
	}

	@Override
	protected void buildEmailFooterCRightTextPlaceholderValues() {
		placeholderValues.add(openDJAttributes.get_footerCopyrightTextEN());
		placeholderValues.add(openDJAttributes.get_footerDontReplyTextEN());
	}
	
}

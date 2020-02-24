package com.idms.dynamic.mail.template;

import org.apache.commons.lang3.StringUtils;

import com.idms.dynamic.mail.template.util.DynamicEmailTemplateInput;
import com.se.idms.cache.utils.EmailConstants;

public class LinkGreenPRMInterRegTPHolderENSubstitutor extends LinkGreenTemplatePlaceholderENSubstitutor {
	
	public LinkGreenPRMInterRegTPHolderENSubstitutor(DynamicEmailTemplateInput input,
			OpenDJAttributes openDJAttributes) {
		super(input, openDJAttributes);
	}

	@Override
	public void buildEmailBodyPlaceholderValues() {
		placeholderValues.add(openDJAttributes.get_bodyContentPRMSelfEN());
		placeholderValues.add(openDJAttributes.get_bodyContentPRMInternalEN());
		placeholderValues.add(openDJAttributes.get_bodySalutationEN());
		placeholderValues.add(input.getFirstName());
		placeholderValues.add(openDJAttributes.get_bodyContentPRMInfoEN());
		placeholderValues.add(openDJAttributes.get_bodyContentPRMInfoList1EN());
		placeholderValues.add(openDJAttributes.get_bodyContentPRMInfoList2EN());
		placeholderValues.add(openDJAttributes.get_bodyContentPRMInfoList3EN());
		placeholderValues.add(openDJAttributes.get_bodyContentPRMInfoList4EN());
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
}

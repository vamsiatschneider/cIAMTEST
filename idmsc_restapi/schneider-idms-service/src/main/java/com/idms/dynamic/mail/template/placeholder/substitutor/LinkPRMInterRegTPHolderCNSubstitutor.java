package com.idms.dynamic.mail.template.placeholder.substitutor;

import org.apache.commons.lang3.StringUtils;

import com.idms.dynamic.mail.template.util.DynamicEmailTemplateInput;
import com.idms.dynamic.mail.template.util.OpenDJAttributes;
import com.se.idms.cache.utils.EmailConstants;

public class LinkPRMInterRegTPHolderCNSubstitutor extends LinkTemplatePlaceholderCNSubstitutor {
	
	public LinkPRMInterRegTPHolderCNSubstitutor(DynamicEmailTemplateInput input,
			OpenDJAttributes openDJAttributes) {
		super(input, openDJAttributes);
	}

	@Override
	public void buildEmailBodyPlaceholderValues() {
		placeholderValues.add(openDJAttributes.get_bodyContentPRMSelfCN());
		placeholderValues.add(openDJAttributes.get_bodyContentPRMInternalCN());
		placeholderValues.add(openDJAttributes.get_bodySalutationCN());
		placeholderValues.add(input.getFirstName());
		placeholderValues.add(openDJAttributes.get_bodyContentPRMInfoCN());
		placeholderValues.add(openDJAttributes.get_bodyContentPRMInfoList1CN());
		placeholderValues.add(openDJAttributes.get_bodyContentPRMInfoList2CN());
		placeholderValues.add(openDJAttributes.get_bodyContentPRMInfoList3CN());
		placeholderValues.add(openDJAttributes.get_bodyContentPRMInfoList4CN());
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
		String emailSubject = input.getSubject() + EmailConstants.HYPHEN + openDJAttributes.get_subjectCN();
		input.setSubject(emailSubject);	
	}
}

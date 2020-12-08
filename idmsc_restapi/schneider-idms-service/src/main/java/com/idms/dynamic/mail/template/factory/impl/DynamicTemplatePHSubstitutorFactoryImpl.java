package com.idms.dynamic.mail.template.factory.impl;

import com.idms.dynamic.mail.template.factory.DynamicTemplatePHSubstitutorFactory;
import com.idms.dynamic.mail.template.placeholder.substitutor.LinkPRMEclipseRegTPHolderCNSubstitutor;
import com.idms.dynamic.mail.template.placeholder.substitutor.LinkPRMEclipseRegTPHolderENSubstitutor;
import com.idms.dynamic.mail.template.placeholder.substitutor.LinkPRMInterRegTPHolderCNSubstitutor;
import com.idms.dynamic.mail.template.placeholder.substitutor.LinkPRMInterRegTPHolderENSubstitutor;
import com.idms.dynamic.mail.template.placeholder.substitutor.LinkPRMSelfRegTPHolderCNSubstitutor;
import com.idms.dynamic.mail.template.placeholder.substitutor.LinkPRMSelfRegTPHolderENSubstitutor;
import com.idms.dynamic.mail.template.placeholder.substitutor.LinkTemplatePlaceholderCNSubstitutor;
import com.idms.dynamic.mail.template.placeholder.substitutor.LinkTemplatePlaceholderENSubstitutor;
import com.idms.dynamic.mail.template.placeholder.substitutor.OTPTemplatePlaceholderCNSubstitutor;
import com.idms.dynamic.mail.template.placeholder.substitutor.OTPTemplatePlaceholderENSubstitutor;
import com.idms.dynamic.mail.template.placeholder.substitutor.TemplatePlaceholderSubstitutor;
import com.idms.dynamic.mail.template.util.DynamicEmailTemplateInput;
import com.idms.dynamic.mail.template.util.OpenDJAttributes;
import com.idms.mail.template.util.OperationType;
import com.idms.mail.template.util.PRMTemplateType;

public class DynamicTemplatePHSubstitutorFactoryImpl implements DynamicTemplatePHSubstitutorFactory {

	private DynamicEmailTemplateInput input;
	private OpenDJAttributes openDJAttributes;

	public DynamicTemplatePHSubstitutorFactoryImpl(DynamicEmailTemplateInput input, OpenDJAttributes openDJAttributes) {
		this.input = input;
		this.openDJAttributes = openDJAttributes;
	}

	@Override
	public TemplatePlaceholderSubstitutor getTemplatePlaceholderSubstitutor() {

		TemplatePlaceholderSubstitutor substitutor;
		switch (input.getLocale()) {
			case CN:
				substitutor = getOTPOrLinkCNSubstitutor();
				break;
			default:
				substitutor = getOTPOrLinkENSubstitutor();
				break;
		}
		return substitutor;
	}

	private TemplatePlaceholderSubstitutor getOTPOrLinkENSubstitutor() {
		if(OperationType.CHANGE_EMAIL_NOTIFICATION.getType().equals(input.getOperationType().getType())) {
			return new LinkTemplatePlaceholderENSubstitutor(input, openDJAttributes);
		}
		if(OperationType.TWO_FACTOR_AUTHENTICATION.getType().equals(input.getOperationType().getType())) {
			return new OTPTemplatePlaceholderENSubstitutor(input, openDJAttributes);
		}
		TemplatePlaceholderSubstitutor substitutor;
		if (input.isOTPEnabled()) {
			substitutor = new OTPTemplatePlaceholderENSubstitutor(input, openDJAttributes);
		}else {
			if(input.isPRMApp() && PRMTemplateType.PRM_SELF_REGISTRATION.equals(input.getPrmTemplateType())) {
				substitutor = new LinkPRMSelfRegTPHolderENSubstitutor(input, openDJAttributes);
			}else if(input.isPRMApp() && PRMTemplateType.PRM_ECLIPSE_REGISTRATION.equals(input.getPrmTemplateType())) {
				substitutor = new LinkPRMEclipseRegTPHolderENSubstitutor(input, openDJAttributes);
			}else if(input.isPRMApp() && PRMTemplateType.PRM_INTERNAL_REGISTRATION.equals(input.getPrmTemplateType())) {
				substitutor = new LinkPRMInterRegTPHolderENSubstitutor(input, openDJAttributes);
			}else {
				substitutor = new LinkTemplatePlaceholderENSubstitutor(input, openDJAttributes);
			}
		}
		return substitutor;
	}

	private TemplatePlaceholderSubstitutor getOTPOrLinkCNSubstitutor() {
		if(OperationType.CHANGE_EMAIL_NOTIFICATION.getType().equals(input.getOperationType().getType())) {
			return new LinkTemplatePlaceholderCNSubstitutor(input, openDJAttributes);
		}
		if(OperationType.TWO_FACTOR_AUTHENTICATION.getType().equals(input.getOperationType().getType())) {
			return new OTPTemplatePlaceholderCNSubstitutor(input, openDJAttributes);
		}
		TemplatePlaceholderSubstitutor substitutor;
		if (input.isOTPEnabled()) {
			substitutor = new OTPTemplatePlaceholderCNSubstitutor(input, openDJAttributes);
		}else {
			if(input.isPRMApp() && PRMTemplateType.PRM_SELF_REGISTRATION.equals(input.getPrmTemplateType())) {
				substitutor = new LinkPRMSelfRegTPHolderCNSubstitutor(input, openDJAttributes);
			}else if(input.isPRMApp() && PRMTemplateType.PRM_ECLIPSE_REGISTRATION.equals(input.getPrmTemplateType())) {
				substitutor = new LinkPRMEclipseRegTPHolderCNSubstitutor(input, openDJAttributes);
			}else if(input.isPRMApp() && PRMTemplateType.PRM_INTERNAL_REGISTRATION.equals(input.getPrmTemplateType())) {
				substitutor = new LinkPRMInterRegTPHolderCNSubstitutor(input, openDJAttributes);
			}else {
				substitutor = new LinkTemplatePlaceholderCNSubstitutor(input, openDJAttributes);
			}
		}
		return substitutor;
	}

}

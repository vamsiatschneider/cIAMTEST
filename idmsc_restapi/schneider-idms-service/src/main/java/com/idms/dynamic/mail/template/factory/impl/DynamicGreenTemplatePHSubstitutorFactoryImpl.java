package com.idms.dynamic.mail.template.factory.impl;

import com.idms.dynamic.mail.template.LinkGreenPRMEclipseRegTPHolderCNSubstitutor;
import com.idms.dynamic.mail.template.LinkGreenPRMEclipseRegTPHolderENSubstitutor;
import com.idms.dynamic.mail.template.LinkGreenPRMInterRegTPHolderCNSubstitutor;
import com.idms.dynamic.mail.template.LinkGreenPRMInterRegTPHolderENSubstitutor;
import com.idms.dynamic.mail.template.LinkGreenPRMSelfRegTPHolderCNSubstitutor;
import com.idms.dynamic.mail.template.LinkGreenPRMSelfRegTPHolderENSubstitutor;
import com.idms.dynamic.mail.template.LinkGreenTemplatePlaceholderCNSubstitutor;
import com.idms.dynamic.mail.template.LinkGreenTemplatePlaceholderENSubstitutor;
import com.idms.dynamic.mail.template.OTPGreenTemplatePlaceholderCNSubstitutor;
import com.idms.dynamic.mail.template.OTPGreenTemplatePlaceholderENSubstitutor;
import com.idms.dynamic.mail.template.OpenDJAttributes;
import com.idms.dynamic.mail.template.TemplatePlaceholderSubstitutor;
import com.idms.dynamic.mail.template.factory.DynamicTemplatePHSubstitutorFactory;
import com.idms.dynamic.mail.template.util.DynamicEmailTemplateInput;
import com.idms.mail.template.util.PRMTemplateType;

public class DynamicGreenTemplatePHSubstitutorFactoryImpl implements DynamicTemplatePHSubstitutorFactory {

	private DynamicEmailTemplateInput input;
	private OpenDJAttributes openDJAttributes;

	public DynamicGreenTemplatePHSubstitutorFactoryImpl(DynamicEmailTemplateInput input, OpenDJAttributes openDJAttributes) {
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
		TemplatePlaceholderSubstitutor substitutor;
		if (input.isOTPEnabled()) {
			substitutor = new OTPGreenTemplatePlaceholderENSubstitutor(input, openDJAttributes);
		}else {
			if(input.isPRMApp() && PRMTemplateType.PRM_SELF_REGISTRATION.equals(input.getPrmTemplateType())) {
				substitutor = new LinkGreenPRMSelfRegTPHolderENSubstitutor(input, openDJAttributes);
			}else if(input.isPRMApp() && PRMTemplateType.PRM_ECLIPSE_REGISTRATION.equals(input.getPrmTemplateType())) {
				substitutor = new LinkGreenPRMEclipseRegTPHolderENSubstitutor(input, openDJAttributes);
			}else if(input.isPRMApp() && PRMTemplateType.PRM_INTERNAL_REGISTRATION.equals(input.getPrmTemplateType())) {
				substitutor = new LinkGreenPRMInterRegTPHolderENSubstitutor(input, openDJAttributes);
			}else {
				substitutor = new LinkGreenTemplatePlaceholderENSubstitutor(input, openDJAttributes);
			}
		}
		return substitutor;
	}

	private TemplatePlaceholderSubstitutor getOTPOrLinkCNSubstitutor() {
		TemplatePlaceholderSubstitutor substitutor;
		if (input.isOTPEnabled()) {
			substitutor = new OTPGreenTemplatePlaceholderCNSubstitutor(input, openDJAttributes);
		}else {
			if(input.isPRMApp() && PRMTemplateType.PRM_SELF_REGISTRATION.equals(input.getPrmTemplateType())) {
				substitutor = new LinkGreenPRMSelfRegTPHolderCNSubstitutor(input, openDJAttributes);
			}else if(input.isPRMApp() && PRMTemplateType.PRM_ECLIPSE_REGISTRATION.equals(input.getPrmTemplateType())) {
				substitutor = new LinkGreenPRMEclipseRegTPHolderCNSubstitutor(input, openDJAttributes);
			}else if(input.isPRMApp() && PRMTemplateType.PRM_INTERNAL_REGISTRATION.equals(input.getPrmTemplateType())) {
				substitutor = new LinkGreenPRMInterRegTPHolderCNSubstitutor(input, openDJAttributes);
			}else {
				substitutor = new LinkGreenTemplatePlaceholderCNSubstitutor(input, openDJAttributes);
			}
		}
		return substitutor;
	}

}

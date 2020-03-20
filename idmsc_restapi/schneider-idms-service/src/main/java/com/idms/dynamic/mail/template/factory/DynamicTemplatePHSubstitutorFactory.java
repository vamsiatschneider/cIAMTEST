package com.idms.dynamic.mail.template.factory;

import com.idms.dynamic.mail.template.placeholder.substitutor.TemplatePlaceholderSubstitutor;

public interface DynamicTemplatePHSubstitutorFactory {

	public TemplatePlaceholderSubstitutor getTemplatePlaceholderSubstitutor();
}

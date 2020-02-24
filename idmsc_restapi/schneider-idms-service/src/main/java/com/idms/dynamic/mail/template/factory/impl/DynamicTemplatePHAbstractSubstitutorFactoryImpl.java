package com.idms.dynamic.mail.template.factory.impl;

import com.idms.dynamic.mail.template.OpenDJAttributes;
import com.idms.dynamic.mail.template.factory.DynamicTemplatePHAbstractSubstitutorFactory;
import com.idms.dynamic.mail.template.factory.DynamicTemplatePHSubstitutorFactory;
import com.idms.dynamic.mail.template.util.DynamicEmailTemplateInput;

public class DynamicTemplatePHAbstractSubstitutorFactoryImpl implements DynamicTemplatePHAbstractSubstitutorFactory {

	private DynamicEmailTemplateInput input;
	private OpenDJAttributes openDJAttributes;
	
	public DynamicTemplatePHAbstractSubstitutorFactoryImpl(DynamicEmailTemplateInput input, OpenDJAttributes openDJAttributes) {
		this.input = input;
		this.openDJAttributes = openDJAttributes;
	}

	@Override
	public DynamicTemplatePHSubstitutorFactory getTemplatePlaceholderSubstitutorFactory() {
		DynamicTemplatePHSubstitutorFactory factory;
		switch(input.getEtColor()) {
			case BLUE:
				factory =  new DynamicBlueTemplatePHSubstitutorFactoryImpl(input, openDJAttributes);
				break;
			default:
				factory =  new DynamicGreenTemplatePHSubstitutorFactoryImpl(input, openDJAttributes); 
				break;
		}
		return factory;
	}
	

}

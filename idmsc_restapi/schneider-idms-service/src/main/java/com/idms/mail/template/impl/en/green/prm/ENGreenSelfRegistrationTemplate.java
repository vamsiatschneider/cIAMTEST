package com.idms.mail.template.impl.en.green.prm;

import com.idms.mail.template.impl.en.green.ENGreenDefaultTemplate;
import com.idms.mail.template.util.EmailTemplate;
import com.idms.mail.template.util.EmailTemplateInput;

public class ENGreenSelfRegistrationTemplate extends ENGreenDefaultTemplate {

	public ENGreenSelfRegistrationTemplate(EmailTemplateInput input) {
		super(input);
	}
	
	@Override
	public EmailTemplate getTemplate() {
		/* 
		 * TODO- Ideally the configuration class PropertyFileAutoRefresh should return the value of the 
		 * property key passed onto getConfiguration method but it returns null. 
		 * Need to fix how configuration is loaded. The below commented code should work post that.
		 */
//		PropertyFileAutoRefresh _instance = PropertyFileAutoRefresh.getInstance();
//		String filePath = _instance.getConfiguration("prm.self.registration.email.template.en");
//		EmailTemplate template = new EmailTemplate();
//		template.setEmailTemplatePath(filePath);
//		return template;
		
		String filePath = input.getConfiguration().getPRM_SELF_USER_REGESTRATION_EMAILTEMPLATE_EN();
		EmailTemplate template = new EmailTemplate();
		template.setEmailTemplatePath(filePath);
		return template;
	}
}

package com.idms.mail.template.impl.en.blue;

import com.idms.mail.template.util.EmailTemplate;
import com.idms.mail.template.util.EmailTemplateInput;

public class ENBlueUserRegistrationTemplate extends ENBlueDefaultTemplate {

	protected EmailTemplateInput input;
	
	public ENBlueUserRegistrationTemplate(EmailTemplateInput input) {
		super(input);
	}

	public EmailTemplate getTemplate() {
/* 
 * TODO- Ideally the configuration class PropertyFileAutoRefresh should return the value of the 
 * property key passed onto getConfiguration method but it returns null. 
 * Need to fix how configuration is loaded. The below commented code should work post that.
 */
//		PropertyFileAutoRefresh _instance = PropertyFileAutoRefresh.getInstance();
//		String filePath = _instance.getConfiguration("user.registration.withpwd.email.template.blue.cn");
//		EmailTemplate template = new EmailTemplate();
//		template.setEmailTemplatePath(filePath);
//		return template;
				
		String filePath = input.getConfiguration().getIDMS_USER_REGESTRATION_WITHPWD_BLUE_EMAILTEMPLATE_EN();
		EmailTemplate template = new EmailTemplate();
		template.setEmailTemplatePath(filePath);
		return template;
	}
}

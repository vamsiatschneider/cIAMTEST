package com.idms.mail.template.impl.en.blue;

import com.idms.mail.template.util.EmailTemplate;
import com.idms.mail.template.util.EmailTemplateInput;

public class ENBlue2FAUserTemplate extends ENBlueDefaultTemplate {

	public ENBlue2FAUserTemplate(EmailTemplateInput input) {
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
//		String filePath = _instance.getConfiguration("user.add.otp.email.template.cn");
//		EmailTemplate template = new EmailTemplate();
//		template.setEmailTemplatePath(filePath);
//		return template;
		
		String filePath = input.getConfiguration().getIDMS_USER_2FA_EMAILTEMPLATE();
		EmailTemplate template = new EmailTemplate();
		template.setEmailTemplatePath(filePath);
		return template;
	}
}

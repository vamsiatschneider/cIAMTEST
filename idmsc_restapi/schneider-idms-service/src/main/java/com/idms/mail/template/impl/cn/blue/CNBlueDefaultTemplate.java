package com.idms.mail.template.impl.cn.blue;

import com.idms.mail.template.util.EmailTemplate;
import com.idms.mail.template.util.EmailTemplateInput;

public class CNBlueDefaultTemplate {
	
	//@Value("${user.default.email.template.blue.cn}")
	/* Commented for PMD violation fix for unused variable rule*/
	//private String IDMS_USER_DEFAULT_BLUE_EMAILTEMPLATE_CN;
	
	protected EmailTemplateInput input;
	
	public CNBlueDefaultTemplate(EmailTemplateInput input) {
		this.input = input;
	}

	public EmailTemplate getTemplate() {
/* 
 * TODO- Ideally the configuration class PropertyFileAutoRefresh should return the value of the 
 * property key passed onto getConfiguration method but it returns null. 
 * Need to fix how configuration is loaded. The below commented code should work post that.
 */
//		PropertyFileAutoRefresh _instance = PropertyFileAutoRefresh.getInstance();
//		String filePath = _instance.getConfiguration("user.default.email.template.blue.cn");
//		EmailTemplate template = new EmailTemplate();
//		template.setEmailTemplatePath(filePath);
//		return template;
				
		String filePath = input.getConfiguration().getIDMS_USER_DEFAULT_BLUE_EMAILTEMPLATE_CN();
		EmailTemplate template = new EmailTemplate();
		template.setEmailTemplatePath(filePath);
		return template;
	}

}

package com.idms.dynamic.mail.template.impl;

import com.idms.dynamic.mail.template.util.DynamicEmailTemplate;
import com.idms.dynamic.mail.template.util.DynamicEmailTemplateInput;

public class DynamicChangeEmailTemplate {

	private DynamicEmailTemplateInput input;
	
	public DynamicChangeEmailTemplate(DynamicEmailTemplateInput input) {
		this.input = input;
	}
	
	public DynamicEmailTemplate getTemplate() {
/* 
 * TODO- Ideally the configuration class PropertyFileAutoRefresh should return the value of the 
 * property key passed onto getConfiguration method but it returns null. 
 * Need to fix how configuration is loaded. The below commented code should work post that.
 */
//		PropertyFileAutoRefresh _instance = PropertyFileAutoRefresh.getInstance();
//		String filePath = _instance.getConfiguration("user.change.email.notification.template");
//		EmailTemplate template = new EmailTemplate();
//		template.setEmailTemplatePath(filePath);
//		return template;
		
		String filePath = input.getConfiguration().getIDMS_USER_CHANGE_EMAILTEMPLATE();
		DynamicEmailTemplate template = new DynamicEmailTemplate();
		template.setEmailTemplatePath(filePath);
		return template;
	}


}

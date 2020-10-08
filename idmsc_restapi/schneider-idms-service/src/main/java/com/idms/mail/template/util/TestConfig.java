package com.idms.mail.template.util;

import com.se.idms.util.PropertyFileAutoRefresh;

public class TestConfig {

	public static void main(String[] args) {
		
		PropertyFileAutoRefresh _instance = PropertyFileAutoRefresh.getInstance();
		try {
			_instance.initilize("C:\\\\Work_Docs\\\\config\\\\app_root\\\\properties\\\\application.INTG.properties");
		} catch (Exception e) {
			e.printStackTrace();
		}
		String filePath = _instance.getConfiguration("user.registration.withpwd.email.template.cn");
		
	}

}

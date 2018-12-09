package com.idms.service.util;

import javax.annotation.PostConstruct;

import org.apache.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.core.Ordered;
import org.springframework.core.annotation.Order;
import org.springframework.stereotype.Component;

@Order(Ordered.LOWEST_PRECEDENCE)
@Component
public class BeanPrinter {

	@Autowired
    ApplicationContext applicationContext;
	
	private boolean printBeans = ((System.getProperty("sys.print.beans") != null ||
			System.getProperty("sys.print.beans").equals("true")));
	
	@PostConstruct
    public void printBeans() {
		StringBuilder outStr = new StringBuilder();
		String[] beanDefinitionNames = applicationContext.getBeanDefinitionNames();
		
		if(!printBeans)
			return;
		
		outStr.append("~~~~~~~~~~~~~~~~~~ Loaded Beans ~~~~~~~~~~~~~~~~~~\n");
		
		for(String beanDef : beanDefinitionNames) {
			if(beanDef.toLowerCase().contains("service")) {
				Object bean = applicationContext.getBean(beanDef);
				String beanClass = bean.getClass().getCanonicalName();
				outStr.append("\t" + beanDef + " => " + beanClass + "\n");
			}
		}
		
		Logger.getLogger(BeanPrinter.class.getCanonicalName()).info(outStr.toString());
    }
    
}

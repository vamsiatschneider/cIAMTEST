package com.idms.mail.template.util;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


public class TestEnum {
	private static final Logger LOGGER = LoggerFactory.getLogger(TestEnum.class);

	public static void main(String[] args) {

		for(OperationType ot: OperationType.values()) {
			LOGGER.info("Key: " + ot.name() +" Value: " +ot.getType());
		}
		LOGGER.info("------------");
		for(EmailTemplateColor ot: EmailTemplateColor.values()) {
			LOGGER.info("Key: " + ot.name() +" Value: " +ot.getColor());
		}
		LOGGER.info("------------");
		for(Locale ot: Locale.values()) {
			LOGGER.info("Key: " + ot.name() +" Value: " +ot.getLocale());
		}
		LOGGER.info("------------");
		for(PRMTemplateType ot: PRMTemplateType.values()) {
			LOGGER.info("Key: " + ot.name() +" Value: " +ot.getType());
		}
	}

}

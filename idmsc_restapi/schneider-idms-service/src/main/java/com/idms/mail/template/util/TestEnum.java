package com.idms.mail.template.util;

public class TestEnum {

	public static void main(String[] args) {

		for(OperationType ot: OperationType.values()) {
			System.out.println("Key: " + ot.name() +" Value: " +ot.getType());
		}
		System.out.println("------------");
		for(EmailTemplateColor ot: EmailTemplateColor.values()) {
			System.out.println("Key: " + ot.name() +" Value: " +ot.getColor());
		}
		System.out.println("------------");
		for(Locale ot: Locale.values()) {
			System.out.println("Key: " + ot.name() +" Value: " +ot.getLocale());
		}
		System.out.println("------------");
		for(PRMTemplateType ot: PRMTemplateType.values()) {
			System.out.println("Key: " + ot.name() +" Value: " +ot.getType());
		}
	}

}

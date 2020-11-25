package com.idms.mail.template.util;

import java.util.Arrays;

public enum EmailTemplateColor {
	BLUE("Blue"),
	GREEN("Green"),
	RED("Red"),
	INVALID("Invalid");
	
	private String color;
	EmailTemplateColor(String color) {
		this.color = color;
	}
	public String getColor() {
		return this.color;
	}
	public static EmailTemplateColor getKey(String color) {
		
		return Arrays.stream(EmailTemplateColor.values()).
				filter(e -> e.color.equalsIgnoreCase(color)).
				findFirst().orElse(EmailTemplateColor.INVALID);
	}
}

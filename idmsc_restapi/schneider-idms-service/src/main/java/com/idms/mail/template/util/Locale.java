package com.idms.mail.template.util;

import java.util.Arrays;

public enum Locale {

	CN("CN"), EN("EN"), INVALID("Invalid");
	
	private String locale;
	Locale(String locale) {
		this.locale = locale;
	}
	public String getLocale() {
		return locale;
	}
	public static Locale getKey(String locale) {
		return Arrays.stream(Locale.values()).
				filter(e -> e.locale.equalsIgnoreCase(locale)).
				findFirst().orElse(Locale.INVALID);
	}
}

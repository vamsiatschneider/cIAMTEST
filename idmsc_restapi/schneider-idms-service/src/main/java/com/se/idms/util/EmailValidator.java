package com.se.idms.util;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Class to validate email
 * 
 * @author Aravindh Kumar
 *
 */

public  class EmailValidator {

	private static EmailValidator emailValidator = null;

	//private static final String EMAIL_PATTERN = "^((([^<>()\\[\\];:\\s@]([^<>()\\[\\]\\.;:\\s@])*)|(\".+\"))+@(([0-9]{1,3}+\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3})|([a-zA-Z&#45;0-9]+\\.[a-zA-Z]{2,})))$";

	//private static final String EMAIL_REGEX = "^[\\w-\\+]+(\\.[\\w]+)*@[\\w-]+(\\.[\\w]+)*(\\.[a-z]{2,})$";
	
	private static final String EMAIL_PATTERN1 = "^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@((\\[[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\])|(([a-zA-Z\\-0-9]+\\.)+[a-zA-Z]{2,}))$";
	static {
		emailValidator = new EmailValidator();
	}
//Removing private for PMD
	 EmailValidator() {

	}

	public static EmailValidator getInstance() {
		return emailValidator;

	}

	/**
	 * Validate hex with regular expression
	 *
	 * @param hex
	 *            hex for validation
	 * @return true valid hex, false invalid hex
	 */
	public boolean validate(final String hex) {

		//Pattern pattern = Pattern.compile(EMAIL_PATTERN);
		Pattern pattern = Pattern.compile(EMAIL_PATTERN1,Pattern.CASE_INSENSITIVE);
		Matcher matcher = null;
		matcher = pattern.matcher(hex);
		return matcher.matches();
	}
	
	/*public static void main(String args[]) {
		EmailValidator ev = new EmailValidator();
		String email = "abc$/dd@a-a.com.com";
		boolean status = ev.validate(email);
		System.out.println(status);
	}*/
}

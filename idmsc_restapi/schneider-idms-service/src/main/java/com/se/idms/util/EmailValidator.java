package com.se.idms.util;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Class to validate email
 * 
 * @author Aravindh Kumar
 *
 */

public class EmailValidator {

	private static EmailValidator emailValidator = null;

	private static final String EMAIL_PATTERN = "^[_A-Za-z0-9-\\+]+(\\.[_A-Za-z0-9-]+)*@"
			+ "[A-Za-z0-9-]+(\\.[A-Za-z0-9]+)*(\\.[A-Za-z]{2,})$";

	static {
		emailValidator = new EmailValidator();
	}

	private EmailValidator() {

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

		Pattern pattern = Pattern.compile(EMAIL_PATTERN);
		Matcher matcher = null;
		matcher = pattern.matcher(hex);
		return matcher.matches();
	}
}

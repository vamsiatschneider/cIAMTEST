package com.se.idms.util;

import org.springframework.stereotype.Component;

/**
 * Class to validate phone
 * @author Aravindh Kumar
 *
 */
@Component("phoneValidator")
public class PhoneValidator {

	/**
	 * Validate phoneNo with regular expression
	 *
	 * @param phoneNo
	 *            phoneNo for validation
	 * @return true valid phoneNo, false invalid phoneNo
	 */
	public boolean validate(final String phoneNo) {

		// validate phone numbers of format "12345678900"
		if (phoneNo.matches("\\d{11}"))
			return true;
		// validating phone number with -, . or spaces
		else if (phoneNo.matches("\\d{3}[-\\.\\s]\\d{3}[-\\.\\s]\\d{4}"))
			return true;
		// validating phone number with extension length from 3 to 5
		else if (phoneNo.matches("\\d{3}-\\d{3}-\\d{4}\\s(x|(ext))\\d{3,5}"))
			return true;
		// validating phone number where area code is in braces ()
		else if (phoneNo.matches("\\(\\d{3}\\)-\\d{3}-\\d{4}"))
			return true;
		// return false if nothing matches the input
		else
			return false;

	}

}

/**
 * 
 */
package com.idms.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

/**
 * @author SESA508936
 *
 */
public class VerifyPinRequest {
	
	@JsonInclude(Include.NON_NULL)
	private String  mobileRegNumber;
	
	@JsonInclude(Include.NON_NULL)
	private String  pin;

	public String getMobileRegNumber() {
		return mobileRegNumber;
	}

	public void setMobileRegNumber(String mobileRegNumber) {
		this.mobileRegNumber = mobileRegNumber;
	}

	public String getPin() {
		return pin;
	}

	public void setPin(String pin) {
		this.pin = pin;
	}

}

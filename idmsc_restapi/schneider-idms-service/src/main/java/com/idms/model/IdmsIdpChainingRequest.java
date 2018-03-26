package com.idms.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * 
 * @author Subbarao Maniam(SESA468450)
 *
 */
public class IdmsIdpChainingRequest {

	@JsonProperty
	private String IDToken1;
	
	@JsonProperty
	private String IDToken2;
	
	@JsonProperty
	private String IDButton;
	
	@JsonProperty
	private String Goto;
	
	@JsonProperty
	private String gotoOnFail;
	
	@JsonProperty
	private String encoded;
	
	@JsonProperty
	private String errorMessage;
	
	@JsonProperty
	private String gx_charset;

	@JsonProperty("IDToken1")
	public String getIDToken1() {
		return IDToken1;
	}

	@JsonProperty("IDToken1")
	public void setIDToken1(String iDToken1) {
		IDToken1 = iDToken1;
	}

	@JsonProperty("IDToken2")
	public String getIDToken2() {
		return IDToken2;
	}

	@JsonProperty("IDToken2")
	public void setIDToken2(String iDToken2) {
		IDToken2 = iDToken2;
	}

	
	@JsonProperty("IDButton")
	public String getIDButton() {
		return IDButton;
	}

	@JsonProperty("IDButton")
	public void setIDButton(String iDButton) {
		IDButton = iDButton;
	}

	@JsonProperty("goto")
	public String getGoto() {
		return Goto;
	}

	@JsonProperty("goto")
	public void setGoto(String goto1) {
		Goto = goto1;
	}

	@JsonProperty("gotoOnFail")
	public String getGotoOnFail() {
		return gotoOnFail;
	}

	@JsonProperty("gotoOnFail")
	public void setGotoOnFail(String gotoOnFail) {
		this.gotoOnFail = gotoOnFail;
	}

	@JsonProperty("encoded")
	public String getEncoded() {
		return encoded;
	}

	@JsonProperty("encoded")
	public void setEncoded(String encoded) {
		this.encoded = encoded;
	}

	@JsonProperty("errorMessage")
	public String getErrorMessage() {
		return errorMessage;
	}

	@JsonProperty("errorMessage")
	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}

	@JsonProperty("gx_charset")
	public String getGx_charset() {
		return gx_charset;
	}

	@JsonProperty("gx_charset")
	public void setGx_charset(String gx_charset) {
		this.gx_charset = gx_charset;
	}
	
	
}

package com.se.idms.dto;

import com.fasterxml.jackson.annotation.JsonProperty;

public class UIMSResponse {
	
	@JsonProperty("hasErrors")
	private String hasErrors;
	
	@JsonProperty("results")
	private UIMSStatusInfo results;


	public String getHasErrors() {
		return hasErrors;
	}

	public void setHasErrors(String hasErrors) {
		this.hasErrors = hasErrors;
	}

	public UIMSStatusInfo getResults() {
		return results;
	}

	public void setResults(UIMSStatusInfo results) {
		this.results = results;
	}

	
}

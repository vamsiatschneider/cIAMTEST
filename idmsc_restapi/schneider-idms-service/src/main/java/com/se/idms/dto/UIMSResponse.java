package com.se.idms.dto;

import com.fasterxml.jackson.annotation.JsonProperty;

public class UIMSResponse {
	

	@JsonProperty("hasErrors")
	private String hasErrors;
	
	@JsonProperty("Status")
	private String status;
	
	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

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

	@Override
	public String toString() {
		return "UIMSResponse [hasErrors=" + hasErrors + ", status=" + status + ", results=" + results + "]";
	}
}

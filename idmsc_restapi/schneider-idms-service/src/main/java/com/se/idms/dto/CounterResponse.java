package com.se.idms.dto;

import org.springframework.stereotype.Component;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

@Component
public class CounterResponse {
	
	@JsonProperty("strcurrentMailCounter")
	@JsonInclude(Include.NON_NULL)
	private String strcurrentMailCounter;
	
	@JsonProperty("maxEmailLimit")
	@JsonInclude(Include.NON_NULL)
	private String maxEmailLimit;
	
	@JsonProperty("strcurrentMobCounter")
	@JsonInclude(Include.NON_NULL)
	private String strcurrentMobCounter;
	
	@JsonProperty("maxMobLimit")
	@JsonInclude(Include.NON_NULL)
	private String maxMobLimit;
	
	
	public String getStrcurrentMobCounter() {
		return strcurrentMobCounter;
	}

	public void setStrcurrentMobCounter(String strcurrentMobCounter) {
		this.strcurrentMobCounter = strcurrentMobCounter;
	}

	public String getMaxMobLimit() {
		return maxMobLimit;
	}

	public void setMaxMobLimit(String maxMobLimit) {
		this.maxMobLimit = maxMobLimit;
	}

	public String getStrcurrentMailCounter() {
		return strcurrentMailCounter;
	}

	public void setStrcurrentMailCounter(String strcurrentMailCounter) {
		this.strcurrentMailCounter = strcurrentMailCounter;
	}

	public String getMaxEmailLimit() {
		return maxEmailLimit;
	}

	public void setMaxEmailLimit(String maxEmailLimit) {
		this.maxEmailLimit = maxEmailLimit;
	}

	
	

}

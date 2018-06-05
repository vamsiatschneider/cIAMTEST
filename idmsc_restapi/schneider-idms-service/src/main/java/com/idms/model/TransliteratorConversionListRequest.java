package com.idms.model;

import java.util.List;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonProperty;

public class TransliteratorConversionListRequest {

	@JsonProperty
	private List<TransliteratorConversionRequest> transliteratorRequest;

	public List<TransliteratorConversionRequest> getTransliteratorRequest() {
		return transliteratorRequest;
	}

	public void setTransliteratorRequest(List<TransliteratorConversionRequest> transliteratorRequest) {
		this.transliteratorRequest = transliteratorRequest;
	}
	
	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}
}

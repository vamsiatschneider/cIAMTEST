package com.idms.model;

import java.util.List;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonProperty;

public class TransliteratorListRequest {

	@JsonProperty
	private List<TransliteratorRequest> transliteratorRequest;

	public List<TransliteratorRequest> getTransliteratorRequest() {
		return transliteratorRequest;
	}

	public void setTransliteratorRequest(List<TransliteratorRequest> transliteratorRequest) {
		this.transliteratorRequest = transliteratorRequest;
	}

	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}
}

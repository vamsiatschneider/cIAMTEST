package com.idms.model;

import java.util.List;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonProperty;

public class TransliteratorListResponse {

	@JsonProperty
	private List<TransliteratorResponse> transliteratorResponse;

	public List<TransliteratorResponse> getTransliteratorResponse() {
		return transliteratorResponse;
	}

	public void setTransliteratorResponse(List<TransliteratorResponse> transliteratorResponse) {
		this.transliteratorResponse = transliteratorResponse;
	}

	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}
}

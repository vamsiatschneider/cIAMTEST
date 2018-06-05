package com.idms.model;

import java.util.List;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonProperty;

public class TransliteratorConversionRequest {

	@JsonProperty("identifier")
	private String identifier;
	
	@JsonProperty("sourceLanguage")
	private String sourceLanguage;
	
	@JsonProperty("targetLanguage")
	private String targetLanguage;
	
	@JsonProperty("attributes")
	private List<TransliteratorAttributes> attributes;

	public String getIdentifier() {
		return identifier;
	}

	public void setIdentifier(String identifier) {
		this.identifier = identifier;
	}

	public String getSourceLanguage() {
		return sourceLanguage;
	}

	public void setSourceLanguage(String sourceLanguage) {
		this.sourceLanguage = sourceLanguage;
	}

	public String getTargetLanguage() {
		return targetLanguage;
	}

	public void setTargetLanguage(String targetLanguage) {
		this.targetLanguage = targetLanguage;
	}

	public List<TransliteratorAttributes> getAttributes() {
		return attributes;
	}

	public void setAttributes(List<TransliteratorAttributes> attributes) {
		this.attributes = attributes;
	}
	
	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}
	
}

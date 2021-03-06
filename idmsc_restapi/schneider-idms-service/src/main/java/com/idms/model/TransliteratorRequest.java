package com.idms.model;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonProperty;

//@JsonIgnoreProperties(ignoreUnknown=true)
public class TransliteratorRequest {

	@JsonProperty("source")
	private String source;
	
	@JsonProperty("sourceLanguage")
	private String sourceLanguage;
	
	@JsonProperty("targetLanguage")
	private String targetLanguage;

	public String getSource() {
		return source;
	}

	public void setSource(String source) {
		this.source = source;
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

	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}
}

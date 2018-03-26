package com.idms.model;

import java.util.List;

import org.apache.commons.lang3.builder.ToStringBuilder;

public class TokenCallBacks {

	private String  type;
	
	private List<TokenPrompt> input;
	
	private List<TokenPrompt> output;

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public List<TokenPrompt> getInput() {
		return input;
	}

	public void setInput(List<TokenPrompt> input) {
		this.input = input;
	}

	public List<TokenPrompt> getOutput() {
		return output;
	}

	public void setOutput(List<TokenPrompt> output) {
		this.output = output;
	}
	
	@Override
    public String toString() {
        return  ToStringBuilder.reflectionToString(this);
    }
	
	
}

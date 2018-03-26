package com.idms.product.model;

import org.apache.commons.lang3.builder.ToStringBuilder;

public class OpenAmUserRequest {

	private OpenAmUserInput input;

	public OpenAmUserInput getInput() {
		return input;
	}

	public void setInput(OpenAmUserInput input) {
		this.input = input;
	}

	@Override
    public String toString() {
        return  ToStringBuilder.reflectionToString(this);
    }
}

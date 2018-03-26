package com.idms.product.model;

import org.apache.commons.lang3.builder.ToStringBuilder;

public class UimsUserRequest {

	private UimsUser input;

	public UimsUser getInput() {
		return input;
	}

	public void setInput(UimsUser input) {
		this.input = input;
	}

	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}
}

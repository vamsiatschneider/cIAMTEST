package com.idms.product.model;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class OpenAMPasswordRecoveryInput {

	@JsonInclude(Include.NON_EMPTY)
	private OpenAmUserRecord UserRecord;

	public OpenAmUserRecord getUserRecord() {
		return UserRecord;
	}

	public void setUserRecord(OpenAmUserRecord userRecord) {
		UserRecord = userRecord;
	}
	
	@Override
	public String toString() {
		return ToStringBuilder.reflectionToString(this);
	}
}

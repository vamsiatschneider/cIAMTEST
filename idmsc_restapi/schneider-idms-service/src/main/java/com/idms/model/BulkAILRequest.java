package com.idms.model;

import com.fasterxml.jackson.annotation.JsonProperty;

public class BulkAILRequest {
	
	@JsonProperty
	private BulkAILRequestBody requestBody;

	public BulkAILRequestBody getRequestBody() {
		return requestBody;
	}

	public void setRequestBody(BulkAILRequestBody requestBody) {
		this.requestBody = requestBody;
	}

}

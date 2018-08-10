package com.schneider.idms.model;

import com.fasterxml.jackson.annotation.JsonProperty;

public class IdmsCreateUserResponse extends IdmsUserRequest{

	@JsonProperty
	private String userId;
	
	@JsonProperty
	private String idmsFederatedId;
	
	@JsonProperty
	private String trustStatus;
	
	@JsonProperty
	private String trustLevel;
}

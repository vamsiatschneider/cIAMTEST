package com.idms.model;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The AIL  Request.
 * @author Pujarani Panda
 *
 */
public class AILRequest {
	
	@JsonProperty
	private UserAILRecord UserAILRecord;

	public UserAILRecord getUserAILRecord() {
		return UserAILRecord;
	}

	public void setUserAILRecord(UserAILRecord userAILRecord) {
		UserAILRecord = userAILRecord;
	}

	

}

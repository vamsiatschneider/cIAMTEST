package com.idms.service.bulkail.util;

public enum AILOperationType {

	GRANT("Grant"), 
	REVOKE("Revoke");
	
	private String type;
	
	AILOperationType(String type) {
		this.type = type;
	}

	public String getType() {
		return type;
	}
}

package com.idms.service.bulkail.util;

public enum AILStatus {

	ACTIVE("Active"), 
	INACTIVE("Inactive");
	
	private String status;
	
	 AILStatus(String status) {
		this.status = status;
	}

	public String getStatus() {
		return status;
	}

}

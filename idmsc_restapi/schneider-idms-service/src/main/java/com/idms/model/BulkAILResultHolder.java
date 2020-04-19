package com.idms.model;

public class BulkAILResultHolder {

	private AILRecord ailRecord;
	
	private String status;
	
	private int statusCode;
	
	private String message;
	
	private boolean isDupRequest;
	
	private String grantRequest;
	
	private String revokeRequest;

	public String getGrantRequest() {
		return grantRequest;
	}

	public void setGrantRequest(String grantRequest) {
		this.grantRequest = grantRequest;
	}

	public String getRevokeRequest() {
		return revokeRequest;
	}

	public void setRevokeRequest(String revokeRequest) {
		this.revokeRequest = revokeRequest;
	}
	
	public boolean isDupRequest() {
		return isDupRequest;
	}

	public void setDupRequest(boolean isDupRequest) {
		this.isDupRequest = isDupRequest;
	}

	public AILRecord getAilRecord() {
		return ailRecord;
	}

	public void setAilRecord(AILRecord ailRecord) {
		this.ailRecord = ailRecord;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public int getStatusCode() {
		return statusCode;
	}

	public void setStatusCode(int statusCode) {
		this.statusCode = statusCode;
	}
	
}

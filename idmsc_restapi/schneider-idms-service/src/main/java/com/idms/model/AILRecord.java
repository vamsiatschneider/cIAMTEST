package com.idms.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

@JsonInclude(Include.NON_NULL)
public class AILRecord {

	@JsonProperty
	private String aclType;

	@JsonProperty
	@JsonInclude(Include.ALWAYS)
	private String acl;

	@JsonProperty
	private String operation;

	@JsonProperty
	private String status;

	@JsonProperty
	private int statusCode;

	@JsonProperty
	private String message;

	public String getAclType() {
		return aclType;
	}

	public void setAclType(String aclType) {
		this.aclType = aclType;
	}

	public String getAcl() {
		return acl;
	}

	public void setAcl(String acl) {
		this.acl = acl;
	}

	public String getOperation() {
		return operation;
	}

	public void setOperation(String operation) {
		this.operation = operation;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public int getStatusCode() {
		return statusCode;
	}

	public void setStatusCode(int statusCode) {
		this.statusCode = statusCode;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((acl == null) ? 0 : acl.hashCode());
		result = prime * result + ((aclType == null) ? 0 : aclType.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj) {
			return true;
		}
		if (obj == null) {
			return false;
		}
		if (getClass() != obj.getClass()) {
			return false;
		}
		AILRecord other = (AILRecord) obj;
		if (acl == null) {
			if (other.acl != null) {
				return false;
			}
		} else if (!acl.equals(other.acl)) {
			return false;
		}
		if (aclType == null) {
			if (other.aclType != null) {
				return false;
			}
		} else if (!aclType.equals(other.aclType)) {
			return false;
		}
		return true;
	}

}
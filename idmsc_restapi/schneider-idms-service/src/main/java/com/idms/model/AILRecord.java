package com.idms.model;

import com.fasterxml.jackson.annotation.JsonProperty;

public class AILRecord {

	@JsonProperty
	private String aclType;

	@JsonProperty
	private String acl;

	@JsonProperty
	private String operation;

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
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		AILRecord other = (AILRecord) obj;
		if (acl == null) {
			if (other.acl != null)
				return false;
		} else if (!acl.equals(other.acl))
			return false;
		if (aclType == null) {
			if (other.aclType != null)
				return false;
		} else if (!aclType.equals(other.aclType))
			return false;
		return true;
	}

}
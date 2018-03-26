package com.idms.product.model;

import java.util.List;

public class OpenUserReponse {

	private String username;

	private String realm;

	private List<String> uid;

	private List<String> universalid;

	private List<String> givenName;

	private List<String> inetUserStatus;

	private List<String> dn;

	private List<String> cn;

	private List<String> sn;

	private List<String> createTimestamp;
	
	private List<String> objectClass;

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public String getRealm() {
		return realm;
	}

	public void setRealm(String realm) {
		this.realm = realm;
	}

	public List<String> getUid() {
		return uid;
	}

	public void setUid(List<String> uid) {
		this.uid = uid;
	}

	public List<String> getUniversalid() {
		return universalid;
	}

	public void setUniversalid(List<String> universalid) {
		this.universalid = universalid;
	}

	public List<String> getGivenName() {
		return givenName;
	}

	public void setGivenName(List<String> givenName) {
		this.givenName = givenName;
	}

	public List<String> getInetUserStatus() {
		return inetUserStatus;
	}

	public void setInetUserStatus(List<String> inetUserStatus) {
		this.inetUserStatus = inetUserStatus;
	}

	public List<String> getDn() {
		return dn;
	}

	public void setDn(List<String> dn) {
		this.dn = dn;
	}

	public List<String> getCn() {
		return cn;
	}

	public void setCn(List<String> cn) {
		this.cn = cn;
	}

	public List<String> getSn() {
		return sn;
	}

	public void setSn(List<String> sn) {
		this.sn = sn;
	}

	public List<String> getCreateTimestamp() {
		return createTimestamp;
	}

	public void setCreateTimestamp(List<String> createTimestamp) {
		this.createTimestamp = createTimestamp;
	}
	
	public List<String> getObjectClass() {
		return objectClass;
	}

	public void setObjectClass(List<String> objectClass) {
		this.objectClass = objectClass;
	}

	@Override
    public String toString() {
        return  null;//ToStringBuilder.reflectionToString(this);
    }
}

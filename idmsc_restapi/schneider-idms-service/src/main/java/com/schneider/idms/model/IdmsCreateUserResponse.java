package com.schneider.idms.model;

import com.fasterxml.jackson.annotation.JsonProperty;

public class IdmsCreateUserResponse extends IdmsUserRequest {

	@JsonProperty
	private String userId;

	@JsonProperty
	private String idmsFederatedId;

	@JsonProperty
	private String trustStatus;

	@JsonProperty
	private String trustLevel;

	@JsonProperty
	private String rejectionReason;

	@JsonProperty
	private String rejectionComment;

	@JsonProperty
	private String delegatedIdp;

	@JsonProperty
	private String identityType;

	@JsonProperty
	private String isInternal;

	@JsonProperty
	private String ail;

	@JsonProperty
	private String ailApplications;

	@JsonProperty
	private String ailFeatures;

	@JsonProperty
	private String ailPrograms;

	@JsonProperty
	private String division;

	@JsonProperty
	private String title;

	@JsonProperty
	private String businessUnit;

	@JsonProperty
	private String userStatus;

	@JsonProperty
	private String socialProviders;

	@JsonProperty
	private String trustedAdmin;

	@JsonProperty
	private String contactGoldenID;

	@JsonProperty
	private String accountGoldenID;

	public String getUserId() {
		return userId;
	}

	public void setUserId(String userId) {
		this.userId = userId;
	}

	public String getIdmsFederatedId() {
		return idmsFederatedId;
	}

	public void setIdmsFederatedId(String idmsFederatedId) {
		this.idmsFederatedId = idmsFederatedId;
	}

	public String getTrustStatus() {
		return trustStatus;
	}

	public void setTrustStatus(String trustStatus) {
		this.trustStatus = trustStatus;
	}

	public String getTrustLevel() {
		return trustLevel;
	}

	public void setTrustLevel(String trustLevel) {
		this.trustLevel = trustLevel;
	}

	public String getRejectionReason() {
		return rejectionReason;
	}

	public void setRejectionReason(String rejectionReason) {
		this.rejectionReason = rejectionReason;
	}

	public String getRejectionComment() {
		return rejectionComment;
	}

	public void setRejectionComment(String rejectionComment) {
		this.rejectionComment = rejectionComment;
	}

	public String getDelegatedIdp() {
		return delegatedIdp;
	}

	public void setDelegatedIdp(String delegatedIdp) {
		this.delegatedIdp = delegatedIdp;
	}

	public String getIdentityType() {
		return identityType;
	}

	public void setIdentityType(String identityType) {
		this.identityType = identityType;
	}

	public String getIsInternal() {
		return isInternal;
	}

	public void setIsInternal(String isInternal) {
		this.isInternal = isInternal;
	}

	public String getAil() {
		return ail;
	}

	public void setAil(String ail) {
		this.ail = ail;
	}

	public String getAilApplications() {
		return ailApplications;
	}

	public void setAilApplications(String ailApplications) {
		this.ailApplications = ailApplications;
	}

	public String getAilFeatures() {
		return ailFeatures;
	}

	public void setAilFeatures(String ailFeatures) {
		this.ailFeatures = ailFeatures;
	}

	public String getAilPrograms() {
		return ailPrograms;
	}

	public void setAilPrograms(String ailPrograms) {
		this.ailPrograms = ailPrograms;
	}

	public String getDivision() {
		return division;
	}

	public void setDivision(String division) {
		this.division = division;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getBusinessUnit() {
		return businessUnit;
	}

	public void setBusinessUnit(String businessUnit) {
		this.businessUnit = businessUnit;
	}

	public String getUserStatus() {
		return userStatus;
	}

	public void setUserStatus(String userStatus) {
		this.userStatus = userStatus;
	}

	public String getSocialProviders() {
		return socialProviders;
	}

	public void setSocialProviders(String socialProviders) {
		this.socialProviders = socialProviders;
	}

	public String getTrustedAdmin() {
		return trustedAdmin;
	}

	public void setTrustedAdmin(String trustedAdmin) {
		this.trustedAdmin = trustedAdmin;
	}

	public String getContactGoldenID() {
		return contactGoldenID;
	}

	public void setContactGoldenID(String contactGoldenID) {
		this.contactGoldenID = contactGoldenID;
	}

	public String getAccountGoldenID() {
		return accountGoldenID;
	}

	public void setAccountGoldenID(String accountGoldenID) {
		this.accountGoldenID = accountGoldenID;
	}

}

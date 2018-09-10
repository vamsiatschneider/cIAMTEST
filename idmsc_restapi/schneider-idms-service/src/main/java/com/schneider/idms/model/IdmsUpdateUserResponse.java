package com.schneider.idms.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class IdmsUpdateUserResponse extends IdmsUpdateUserRequest {

	@JsonInclude(Include.NON_NULL)
	private String userId;

	@JsonInclude(Include.NON_NULL)
	private String accountId;

	@JsonInclude(Include.NON_NULL)
	private String federatedId;

	@JsonInclude(Include.NON_NULL)
	private String idmsFederatedId;

	@JsonInclude(Include.NON_NULL)
	private String userContext;

	@JsonInclude(Include.NON_NULL)
	private String registrationSource;

	@JsonInclude(Include.NON_NULL)
	private String trustStatus;

	@JsonInclude(Include.NON_NULL)
	private String trustLevel;

	@JsonInclude(Include.NON_NULL)
	private String rejectionReason;

	@JsonInclude(Include.NON_NULL)
	private String rejectionComment;

	@JsonInclude(Include.NON_NULL)
	private String delegatedIdp;

	@JsonInclude(Include.NON_NULL)
	private String identityType;

	@JsonInclude(Include.NON_NULL)
	private String isInternal;

	@JsonInclude(Include.NON_NULL)
	private String ail;

	@JsonInclude(Include.NON_NULL)
	private String ailApplications;

	@JsonInclude(Include.NON_NULL)
	private String ailFeatures;

	@JsonInclude(Include.NON_NULL)
	private String ailPrograms;

	@JsonInclude(Include.NON_NULL)
	private String division;

	@JsonInclude(Include.NON_NULL)
	private String title;

	@JsonInclude(Include.NON_NULL)
	private String businessUnit;

	@JsonInclude(Include.NON_NULL)
	private String userStatus;

	@JsonInclude(Include.NON_NULL)
	private String socialProviders;

	@JsonInclude(Include.NON_NULL)
	private String companyFederatedId;

	@JsonInclude(Include.NON_NULL)
	private String trustedAdmin;

	@JsonInclude(Include.NON_NULL)
	private String contactGoldenID;

	@JsonInclude(Include.NON_NULL)
	private String accountGoldenID;

	@JsonInclude(Include.NON_NULL)
	private String channel;

	@JsonInclude(Include.NON_NULL)
	private String subChannel;

	@JsonInclude(Include.NON_NULL)
	private String contactId;

	public String getUserId() {
		return userId;
	}

	public void setUserId(String userId) {
		this.userId = userId;
	}

	public String getAccountId() {
		return accountId;
	}

	public void setAccountId(String accountId) {
		this.accountId = accountId;
	}

	public String getFederatedId() {
		return federatedId;
	}

	public void setFederatedId(String federatedId) {
		this.federatedId = federatedId;
	}

	public String getIdmsFederatedId() {
		return idmsFederatedId;
	}

	public void setIdmsFederatedId(String idmsFederatedId) {
		this.idmsFederatedId = idmsFederatedId;
	}

	public String getUserContext() {
		return userContext;
	}

	public void setUserContext(String userContext) {
		this.userContext = userContext;
	}

	public String getRegistrationSource() {
		return registrationSource;
	}

	public void setRegistrationSource(String registrationSource) {
		this.registrationSource = registrationSource;
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

	public String getCompanyFederatedId() {
		return companyFederatedId;
	}

	public void setCompanyFederatedId(String companyFederatedId) {
		this.companyFederatedId = companyFederatedId;
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

	public String getChannel() {
		return channel;
	}

	public void setChannel(String channel) {
		this.channel = channel;
	}

	public String getSubChannel() {
		return subChannel;
	}

	public void setSubChannel(String subChannel) {
		this.subChannel = subChannel;
	}

	public String getContactId() {
		return contactId;
	}

	public void setContactId(String contactId) {
		this.contactId = contactId;
	}

}

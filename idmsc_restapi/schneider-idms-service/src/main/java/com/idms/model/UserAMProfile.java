package com.idms.model;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;

@JsonIgnoreProperties(ignoreUnknown = true)
public class UserAMProfile {

	private String username;
	private String[] mail;
	private String[] givenName;
	private String[] sn;
	private String[] cn;
	private String[] emailOptIn;
	private String[] c;
	private String[] appleid;
	private String[] companyName;
	private String[] companyState;
	private String[] industrySegment;
	private String[] companyStreet;
	private String[] companyCity;
	private String[] companyCountry;
	private String[] iam1;
	private String[] iam2;
	private String[] registerationSource;
	private String[] tncFlag;
	private String[] updateSource;
	private String[] companyPostalCode;
	private String[] employeeType;
    private String[] preferredlanguage;
    
	public String getUsername() {
		return username;
	}
	public void setUsername(String username) {
		this.username = username;
	}
	public String[] getMail() {
		return mail;
	}
	public void setMail(String[] mail) {
		this.mail = mail;
	}
	public String[] getGivenName() {
		return givenName;
	}
	public void setGivenName(String[] givenName) {
		this.givenName = givenName;
	}
	public String[] getSn() {
		return sn;
	}
	public void setSn(String[] sn) {
		this.sn = sn;
	}
	public String[] getCn() {
		return cn;
	}
	public void setCn(String[] cn) {
		this.cn = cn;
	}
	public String[] getEmailOptIn() {
		return emailOptIn;
	}
	public void setEmailOptIn(String[] emailOptIn) {
		this.emailOptIn = emailOptIn;
	}
	public String[] getC() {
		return c;
	}
	public void setC(String[] c) {
		this.c = c;
	}
	public String[] getCompanyName() {
		return companyName;
	}
	public void setCompanyName(String[] companyName) {
		this.companyName = companyName;
	}
	public String[] getCompanyState() {
		return companyState;
	}
	public void setCompanyState(String[] companyState) {
		this.companyState = companyState;
	}
	public String[] getIndustrySegment() {
		return industrySegment;
	}
	public void setIndustrySegment(String[] industrySegment) {
		this.industrySegment = industrySegment;
	}
	public String[] getCompanyStreet() {
		return companyStreet;
	}
	public void setCompanyStreet(String[] companyStreet) {
		this.companyStreet = companyStreet;
	}
	public String[] getCompanyCity() {
		return companyCity;
	}
	public void setCompanyCity(String[] companyCity) {
		this.companyCity = companyCity;
	}
	public String[] getCompanyCountry() {
		return companyCountry;
	}
	public void setCompanyCountry(String[] companyCountry) {
		this.companyCountry = companyCountry;
	}
	public String[] getIam1() {
		return iam1;
	}
	public void setIam1(String[] iam1) {
		this.iam1 = iam1;
	}
	public String[] getIam2() {
		return iam2;
	}
	public void setIam2(String[] iam2) {
		this.iam2 = iam2;
	}
	public String[] getRegisterationSource() {
		return registerationSource;
	}
	public void setRegisterationSource(String[] registerationSource) {
		this.registerationSource = registerationSource;
	}
	public String[] getTncFlag() {
		return tncFlag;
	}
	public void setTncFlag(String[] tncFlag) {
		this.tncFlag = tncFlag;
	}
	public String[] getUpdateSource() {
		return updateSource;
	}
	public void setUpdateSource(String[] updateSource) {
		this.updateSource = updateSource;
	}
	public String[] getCompanyPostalCode() {
		return companyPostalCode;
	}
	public void setCompanyPostalCode(String[] companyPostalCode) {
		this.companyPostalCode = companyPostalCode;
	}
	public String[] getEmployeeType() {
		return employeeType;
	}
	public void setEmployeeType(String[] employeeType) {
		this.employeeType = employeeType;
	}
	public String[] getPreferredlanguage() {
		return preferredlanguage;
	}
	public void setPreferredlanguage(String[] preferredlanguage) {
		this.preferredlanguage = preferredlanguage;
	}
	public String[] getAppleid() {
		return appleid;
	}
	public void setAppleid(String[] appleid) {
		this.appleid = appleid;
	}
}

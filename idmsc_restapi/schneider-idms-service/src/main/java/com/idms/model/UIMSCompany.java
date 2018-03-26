package com.idms.model;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

public class UIMSCompany {
	
	private static final long serialVersionUID = 1L;
	
	@JsonInclude(Include.NON_EMPTY)
	private String federatedID;
	
	@JsonInclude(Include.NON_EMPTY)
	private String organizationName;
	
	@JsonInclude(Include.NON_EMPTY)
	private String employeeSize;
	
	@JsonInclude(Include.NON_EMPTY)
	private String currencyCode;
	
	@JsonInclude(Include.NON_EMPTY)
	private String customerClass;
	
	@JsonInclude(Include.NON_EMPTY)
	private String localityName;
	
	@JsonInclude(Include.NON_EMPTY)
	private String marketSegment;
	
	@JsonInclude(Include.NON_EMPTY)
	private String postalCode;
	
	@JsonInclude(Include.NON_EMPTY)
	private String postOfficeBox;
	
	@JsonInclude(Include.NON_EMPTY)
	private String st;
	
	@JsonInclude(Include.NON_EMPTY)
	private String street;
	
	@JsonInclude(Include.NON_EMPTY)
	private String headQuarter;
	
	@JsonInclude(Include.NON_EMPTY)
	private String addInfoAddress;
	
	@JsonInclude(Include.NON_EMPTY)
	private String county;
	
	@JsonInclude(Include.NON_EMPTY)
	private String webSite;
	
	@JsonInclude(Include.NON_EMPTY)
	private String marketServed;
	
	//private String employeeSize;
	
	@JsonInclude(Include.NON_EMPTY)
	private String taxIdentificationNumber;
	
	public String getFederatedID() {
		return federatedID;
	}
	public void setFederatedID(String federatedID) {
		this.federatedID = federatedID;
	}
	public String getOrganizationName() {
		return organizationName;
	}
	public void setOrganizationName(String organizationName) {
		this.organizationName = organizationName;
	}
	public String getEmployeeSize() {
		return employeeSize;
	}
	public void setEmployeeSize(String employeeSize) {
		this.employeeSize = employeeSize;
	}
	public String getCurrencyCode() {
		return currencyCode;
	}
	public void setCurrencyCode(String currencyCode) {
		this.currencyCode = currencyCode;
	}
	public String getCustomerClass() {
		return customerClass;
	}
	public void setCustomerClass(String customerClass) {
		this.customerClass = customerClass;
	}
	public String getLocalityName() {
		return localityName;
	}
	public void setLocalityName(String localityName) {
		this.localityName = localityName;
	}
	public String getMarketSegment() {
		return marketSegment;
	}
	public void setMarketSegment(String marketSegment) {
		this.marketSegment = marketSegment;
	}
	public String getPostalCode() {
		return postalCode;
	}
	public void setPostalCode(String postalCode) {
		this.postalCode = postalCode;
	}
	public String getPostOfficeBox() {
		return postOfficeBox;
	}
	public void setPostOfficeBox(String postOfficeBox) {
		this.postOfficeBox = postOfficeBox;
	}
	public String getSt() {
		return st;
	}
	public void setSt(String st) {
		this.st = st;
	}
	public String getStreet() {
		return street;
	}
	public void setStreet(String street) {
		this.street = street;
	}
	public String getHeadQuarter() {
		return headQuarter;
	}
	public void setHeadQuarter(String headQuarter) {
		this.headQuarter = headQuarter;
	}
	public String getAddInfoAddress() {
		return addInfoAddress;
	}
	public void setAddInfoAddress(String addInfoAddress) {
		this.addInfoAddress = addInfoAddress;
	}
	public String getCounty() {
		return county;
	}
	public void setCounty(String county) {
		this.county = county;
	}
	public String getWebSite() {
		return webSite;
	}
	public void setWebSite(String webSite) {
		this.webSite = webSite;
	}
	public String getMarketServed() {
		return marketServed;
	}
	public void setMarketServed(String marketServed) {
		this.marketServed = marketServed;
	}
	public String getTaxIdentificationNumber() {
		return taxIdentificationNumber;
	}
	public void setTaxIdentificationNumber(String taxIdentificationNumber) {
		this.taxIdentificationNumber = taxIdentificationNumber;
	}

}

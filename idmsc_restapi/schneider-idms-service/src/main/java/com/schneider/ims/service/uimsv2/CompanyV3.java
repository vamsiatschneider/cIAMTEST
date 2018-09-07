
package com.schneider.ims.service.uimsv2;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for companyV3 complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="companyV3">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="accountLifecycleCode" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="accountOwner" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="accountType" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="addInfoAddress" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="addInfoAddressECS" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="annualSales" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="bdDomainExpertise" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="bfoId" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="businessType" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="cityECS" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="competitorFlag" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="coordLatitude" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="coordLongitude" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="countryCode" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="county" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="countyECS" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="currencyCode" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="customerClass" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="deletionFlag" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="employeeSize" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="erpName" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="exportControlCode" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="facsimileTelephoneNumber" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="federatedId" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="goldenId" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="headQuarter" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="homeDomainExpertise" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="invitationUID" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="itDomainExpertise" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="keyAccountTypeCode" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="languageCode" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="leadingBusiness" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="localityName" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="marketSegment" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="marketServed" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="nameECS" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="note" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="numberEmployees" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="operDomainExpertise" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="organizationName" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="partnerLocatorFlag" type="{http://www.w3.org/2001/XMLSchema}boolean" minOccurs="0"/>
 *         &lt;element name="pmDomainExpertise" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="postOfficeBox" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="postOfficeBoxCode" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="postalAddress" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="postalCity" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="postalCode" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="postalCountry" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="postalCounty" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="postalStateProvince" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="preferredDistributor1" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="preferredDistributor2" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="preferredDistributor3" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="publicVisibility" type="{http://www.w3.org/2001/XMLSchema}boolean" minOccurs="0"/>
 *         &lt;element name="publisherGoldenId" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="purchaserId" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="pwDomainExpertise" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="relationshipLeaderCode" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="saccIdParent" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="saccIdUltimate" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="sdhVersion" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="securityDomainExpertise" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="shortNameECS" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="sourceSystemId" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="state" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="street" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="streetECS" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="taxIdentificationNumber" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="taxOffice" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="telephoneNumber" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="timeZone" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="webSite" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "companyV3", propOrder = {
    "accountLifecycleCode",
    "accountOwner",
    "accountType",
    "addInfoAddress",
    "addInfoAddressECS",
    "annualSales",
    "bdDomainExpertise",
    "bfoId",
    "businessType",
    "cityECS",
    "competitorFlag",
    "coordLatitude",
    "coordLongitude",
    "countryCode",
    "county",
    "countyECS",
    "currencyCode",
    "customerClass",
    "deletionFlag",
    "employeeSize",
    "erpName",
    "exportControlCode",
    "facsimileTelephoneNumber",
    "federatedId",
    "goldenId",
    "headQuarter",
    "homeDomainExpertise",
    "invitationUID",
    "itDomainExpertise",
    "keyAccountTypeCode",
    "languageCode",
    "leadingBusiness",
    "localityName",
    "marketSegment",
    "marketServed",
    "nameECS",
    "note",
    "numberEmployees",
    "operDomainExpertise",
    "organizationName",
    "partnerLocatorFlag",
    "pmDomainExpertise",
    "postOfficeBox",
    "postOfficeBoxCode",
    "postalAddress",
    "postalCity",
    "postalCode",
    "postalCountry",
    "postalCounty",
    "postalStateProvince",
    "preferredDistributor1",
    "preferredDistributor2",
    "preferredDistributor3",
    "publicVisibility",
    "publisherGoldenId",
    "purchaserId",
    "pwDomainExpertise",
    "relationshipLeaderCode",
    "saccIdParent",
    "saccIdUltimate",
    "sdhVersion",
    "securityDomainExpertise",
    "shortNameECS",
    "sourceSystemId",
    "state",
    "street",
    "streetECS",
    "taxIdentificationNumber",
    "taxOffice",
    "telephoneNumber",
    "timeZone",
    "webSite"
})
public class CompanyV3 {

    protected String accountLifecycleCode;
    protected String accountOwner;
    protected String accountType;
    protected String addInfoAddress;
    protected String addInfoAddressECS;
    protected String annualSales;
    protected String bdDomainExpertise;
    protected String bfoId;
    protected String businessType;
    protected String cityECS;
    protected String competitorFlag;
    protected String coordLatitude;
    protected String coordLongitude;
    protected String countryCode;
    protected String county;
    protected String countyECS;
    protected String currencyCode;
    protected String customerClass;
    protected String deletionFlag;
    protected String employeeSize;
    protected String erpName;
    protected String exportControlCode;
    protected String facsimileTelephoneNumber;
    protected String federatedId;
    protected String goldenId;
    protected String headQuarter;
    protected String homeDomainExpertise;
    protected String invitationUID;
    protected String itDomainExpertise;
    protected String keyAccountTypeCode;
    protected String languageCode;
    protected String leadingBusiness;
    protected String localityName;
    protected String marketSegment;
    protected String marketServed;
    protected String nameECS;
    protected String note;
    protected String numberEmployees;
    protected String operDomainExpertise;
    protected String organizationName;
    protected Boolean partnerLocatorFlag;
    protected String pmDomainExpertise;
    protected String postOfficeBox;
    protected String postOfficeBoxCode;
    protected String postalAddress;
    protected String postalCity;
    protected String postalCode;
    protected String postalCountry;
    protected String postalCounty;
    protected String postalStateProvince;
    protected String preferredDistributor1;
    protected String preferredDistributor2;
    protected String preferredDistributor3;
    protected Boolean publicVisibility;
    protected String publisherGoldenId;
    protected String purchaserId;
    protected String pwDomainExpertise;
    protected String relationshipLeaderCode;
    protected String saccIdParent;
    protected String saccIdUltimate;
    protected String sdhVersion;
    protected String securityDomainExpertise;
    protected String shortNameECS;
    protected String sourceSystemId;
    protected String state;
    protected String street;
    protected String streetECS;
    protected String taxIdentificationNumber;
    protected String taxOffice;
    protected String telephoneNumber;
    protected String timeZone;
    protected String webSite;

    /**
     * Gets the value of the accountLifecycleCode property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getAccountLifecycleCode() {
        return accountLifecycleCode;
    }

    /**
     * Sets the value of the accountLifecycleCode property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setAccountLifecycleCode(String value) {
        this.accountLifecycleCode = value;
    }

    /**
     * Gets the value of the accountOwner property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getAccountOwner() {
        return accountOwner;
    }

    /**
     * Sets the value of the accountOwner property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setAccountOwner(String value) {
        this.accountOwner = value;
    }

    /**
     * Gets the value of the accountType property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getAccountType() {
        return accountType;
    }

    /**
     * Sets the value of the accountType property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setAccountType(String value) {
        this.accountType = value;
    }

    /**
     * Gets the value of the addInfoAddress property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getAddInfoAddress() {
        return addInfoAddress;
    }

    /**
     * Sets the value of the addInfoAddress property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setAddInfoAddress(String value) {
        this.addInfoAddress = value;
    }

    /**
     * Gets the value of the addInfoAddressECS property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getAddInfoAddressECS() {
        return addInfoAddressECS;
    }

    /**
     * Sets the value of the addInfoAddressECS property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setAddInfoAddressECS(String value) {
        this.addInfoAddressECS = value;
    }

    /**
     * Gets the value of the annualSales property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getAnnualSales() {
        return annualSales;
    }

    /**
     * Sets the value of the annualSales property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setAnnualSales(String value) {
        this.annualSales = value;
    }

    /**
     * Gets the value of the bdDomainExpertise property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getBdDomainExpertise() {
        return bdDomainExpertise;
    }

    /**
     * Sets the value of the bdDomainExpertise property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setBdDomainExpertise(String value) {
        this.bdDomainExpertise = value;
    }

    /**
     * Gets the value of the bfoId property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getBfoId() {
        return bfoId;
    }

    /**
     * Sets the value of the bfoId property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setBfoId(String value) {
        this.bfoId = value;
    }

    /**
     * Gets the value of the businessType property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getBusinessType() {
        return businessType;
    }

    /**
     * Sets the value of the businessType property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setBusinessType(String value) {
        this.businessType = value;
    }

    /**
     * Gets the value of the cityECS property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCityECS() {
        return cityECS;
    }

    /**
     * Sets the value of the cityECS property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCityECS(String value) {
        this.cityECS = value;
    }

    /**
     * Gets the value of the competitorFlag property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCompetitorFlag() {
        return competitorFlag;
    }

    /**
     * Sets the value of the competitorFlag property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCompetitorFlag(String value) {
        this.competitorFlag = value;
    }

    /**
     * Gets the value of the coordLatitude property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCoordLatitude() {
        return coordLatitude;
    }

    /**
     * Sets the value of the coordLatitude property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCoordLatitude(String value) {
        this.coordLatitude = value;
    }

    /**
     * Gets the value of the coordLongitude property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCoordLongitude() {
        return coordLongitude;
    }

    /**
     * Sets the value of the coordLongitude property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCoordLongitude(String value) {
        this.coordLongitude = value;
    }

    /**
     * Gets the value of the countryCode property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCountryCode() {
        return countryCode;
    }

    /**
     * Sets the value of the countryCode property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCountryCode(String value) {
        this.countryCode = value;
    }

    /**
     * Gets the value of the county property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCounty() {
        return county;
    }

    /**
     * Sets the value of the county property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCounty(String value) {
        this.county = value;
    }

    /**
     * Gets the value of the countyECS property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCountyECS() {
        return countyECS;
    }

    /**
     * Sets the value of the countyECS property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCountyECS(String value) {
        this.countyECS = value;
    }

    /**
     * Gets the value of the currencyCode property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCurrencyCode() {
        return currencyCode;
    }

    /**
     * Sets the value of the currencyCode property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCurrencyCode(String value) {
        this.currencyCode = value;
    }

    /**
     * Gets the value of the customerClass property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCustomerClass() {
        return customerClass;
    }

    /**
     * Sets the value of the customerClass property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCustomerClass(String value) {
        this.customerClass = value;
    }

    /**
     * Gets the value of the deletionFlag property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDeletionFlag() {
        return deletionFlag;
    }

    /**
     * Sets the value of the deletionFlag property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDeletionFlag(String value) {
        this.deletionFlag = value;
    }

    /**
     * Gets the value of the employeeSize property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getEmployeeSize() {
        return employeeSize;
    }

    /**
     * Sets the value of the employeeSize property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setEmployeeSize(String value) {
        this.employeeSize = value;
    }

    /**
     * Gets the value of the erpName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getErpName() {
        return erpName;
    }

    /**
     * Sets the value of the erpName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setErpName(String value) {
        this.erpName = value;
    }

    /**
     * Gets the value of the exportControlCode property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getExportControlCode() {
        return exportControlCode;
    }

    /**
     * Sets the value of the exportControlCode property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setExportControlCode(String value) {
        this.exportControlCode = value;
    }

    /**
     * Gets the value of the facsimileTelephoneNumber property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFacsimileTelephoneNumber() {
        return facsimileTelephoneNumber;
    }

    /**
     * Sets the value of the facsimileTelephoneNumber property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFacsimileTelephoneNumber(String value) {
        this.facsimileTelephoneNumber = value;
    }

    /**
     * Gets the value of the federatedId property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFederatedId() {
        return federatedId;
    }

    /**
     * Sets the value of the federatedId property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFederatedId(String value) {
        this.federatedId = value;
    }

    /**
     * Gets the value of the goldenId property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getGoldenId() {
        return goldenId;
    }

    /**
     * Sets the value of the goldenId property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setGoldenId(String value) {
        this.goldenId = value;
    }

    /**
     * Gets the value of the headQuarter property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getHeadQuarter() {
        return headQuarter;
    }

    /**
     * Sets the value of the headQuarter property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setHeadQuarter(String value) {
        this.headQuarter = value;
    }

    /**
     * Gets the value of the homeDomainExpertise property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getHomeDomainExpertise() {
        return homeDomainExpertise;
    }

    /**
     * Sets the value of the homeDomainExpertise property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setHomeDomainExpertise(String value) {
        this.homeDomainExpertise = value;
    }

    /**
     * Gets the value of the invitationUID property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getInvitationUID() {
        return invitationUID;
    }

    /**
     * Sets the value of the invitationUID property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setInvitationUID(String value) {
        this.invitationUID = value;
    }

    /**
     * Gets the value of the itDomainExpertise property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getItDomainExpertise() {
        return itDomainExpertise;
    }

    /**
     * Sets the value of the itDomainExpertise property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setItDomainExpertise(String value) {
        this.itDomainExpertise = value;
    }

    /**
     * Gets the value of the keyAccountTypeCode property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getKeyAccountTypeCode() {
        return keyAccountTypeCode;
    }

    /**
     * Sets the value of the keyAccountTypeCode property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setKeyAccountTypeCode(String value) {
        this.keyAccountTypeCode = value;
    }

    /**
     * Gets the value of the languageCode property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLanguageCode() {
        return languageCode;
    }

    /**
     * Sets the value of the languageCode property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLanguageCode(String value) {
        this.languageCode = value;
    }

    /**
     * Gets the value of the leadingBusiness property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLeadingBusiness() {
        return leadingBusiness;
    }

    /**
     * Sets the value of the leadingBusiness property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLeadingBusiness(String value) {
        this.leadingBusiness = value;
    }

    /**
     * Gets the value of the localityName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLocalityName() {
        return localityName;
    }

    /**
     * Sets the value of the localityName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLocalityName(String value) {
        this.localityName = value;
    }

    /**
     * Gets the value of the marketSegment property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getMarketSegment() {
        return marketSegment;
    }

    /**
     * Sets the value of the marketSegment property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setMarketSegment(String value) {
        this.marketSegment = value;
    }

    /**
     * Gets the value of the marketServed property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getMarketServed() {
        return marketServed;
    }

    /**
     * Sets the value of the marketServed property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setMarketServed(String value) {
        this.marketServed = value;
    }

    /**
     * Gets the value of the nameECS property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getNameECS() {
        return nameECS;
    }

    /**
     * Sets the value of the nameECS property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setNameECS(String value) {
        this.nameECS = value;
    }

    /**
     * Gets the value of the note property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getNote() {
        return note;
    }

    /**
     * Sets the value of the note property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setNote(String value) {
        this.note = value;
    }

    /**
     * Gets the value of the numberEmployees property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getNumberEmployees() {
        return numberEmployees;
    }

    /**
     * Sets the value of the numberEmployees property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setNumberEmployees(String value) {
        this.numberEmployees = value;
    }

    /**
     * Gets the value of the operDomainExpertise property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getOperDomainExpertise() {
        return operDomainExpertise;
    }

    /**
     * Sets the value of the operDomainExpertise property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setOperDomainExpertise(String value) {
        this.operDomainExpertise = value;
    }

    /**
     * Gets the value of the organizationName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getOrganizationName() {
        return organizationName;
    }

    /**
     * Sets the value of the organizationName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setOrganizationName(String value) {
        this.organizationName = value;
    }

    /**
     * Gets the value of the partnerLocatorFlag property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isPartnerLocatorFlag() {
        return partnerLocatorFlag;
    }

    /**
     * Sets the value of the partnerLocatorFlag property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setPartnerLocatorFlag(Boolean value) {
        this.partnerLocatorFlag = value;
    }

    /**
     * Gets the value of the pmDomainExpertise property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPmDomainExpertise() {
        return pmDomainExpertise;
    }

    /**
     * Sets the value of the pmDomainExpertise property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPmDomainExpertise(String value) {
        this.pmDomainExpertise = value;
    }

    /**
     * Gets the value of the postOfficeBox property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPostOfficeBox() {
        return postOfficeBox;
    }

    /**
     * Sets the value of the postOfficeBox property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPostOfficeBox(String value) {
        this.postOfficeBox = value;
    }

    /**
     * Gets the value of the postOfficeBoxCode property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPostOfficeBoxCode() {
        return postOfficeBoxCode;
    }

    /**
     * Sets the value of the postOfficeBoxCode property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPostOfficeBoxCode(String value) {
        this.postOfficeBoxCode = value;
    }

    /**
     * Gets the value of the postalAddress property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPostalAddress() {
        return postalAddress;
    }

    /**
     * Sets the value of the postalAddress property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPostalAddress(String value) {
        this.postalAddress = value;
    }

    /**
     * Gets the value of the postalCity property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPostalCity() {
        return postalCity;
    }

    /**
     * Sets the value of the postalCity property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPostalCity(String value) {
        this.postalCity = value;
    }

    /**
     * Gets the value of the postalCode property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPostalCode() {
        return postalCode;
    }

    /**
     * Sets the value of the postalCode property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPostalCode(String value) {
        this.postalCode = value;
    }

    /**
     * Gets the value of the postalCountry property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPostalCountry() {
        return postalCountry;
    }

    /**
     * Sets the value of the postalCountry property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPostalCountry(String value) {
        this.postalCountry = value;
    }

    /**
     * Gets the value of the postalCounty property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPostalCounty() {
        return postalCounty;
    }

    /**
     * Sets the value of the postalCounty property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPostalCounty(String value) {
        this.postalCounty = value;
    }

    /**
     * Gets the value of the postalStateProvince property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPostalStateProvince() {
        return postalStateProvince;
    }

    /**
     * Sets the value of the postalStateProvince property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPostalStateProvince(String value) {
        this.postalStateProvince = value;
    }

    /**
     * Gets the value of the preferredDistributor1 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPreferredDistributor1() {
        return preferredDistributor1;
    }

    /**
     * Sets the value of the preferredDistributor1 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPreferredDistributor1(String value) {
        this.preferredDistributor1 = value;
    }

    /**
     * Gets the value of the preferredDistributor2 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPreferredDistributor2() {
        return preferredDistributor2;
    }

    /**
     * Sets the value of the preferredDistributor2 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPreferredDistributor2(String value) {
        this.preferredDistributor2 = value;
    }

    /**
     * Gets the value of the preferredDistributor3 property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPreferredDistributor3() {
        return preferredDistributor3;
    }

    /**
     * Sets the value of the preferredDistributor3 property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPreferredDistributor3(String value) {
        this.preferredDistributor3 = value;
    }

    /**
     * Gets the value of the publicVisibility property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isPublicVisibility() {
        return publicVisibility;
    }

    /**
     * Sets the value of the publicVisibility property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setPublicVisibility(Boolean value) {
        this.publicVisibility = value;
    }

    /**
     * Gets the value of the publisherGoldenId property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPublisherGoldenId() {
        return publisherGoldenId;
    }

    /**
     * Sets the value of the publisherGoldenId property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPublisherGoldenId(String value) {
        this.publisherGoldenId = value;
    }

    /**
     * Gets the value of the purchaserId property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPurchaserId() {
        return purchaserId;
    }

    /**
     * Sets the value of the purchaserId property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPurchaserId(String value) {
        this.purchaserId = value;
    }

    /**
     * Gets the value of the pwDomainExpertise property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPwDomainExpertise() {
        return pwDomainExpertise;
    }

    /**
     * Sets the value of the pwDomainExpertise property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPwDomainExpertise(String value) {
        this.pwDomainExpertise = value;
    }

    /**
     * Gets the value of the relationshipLeaderCode property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getRelationshipLeaderCode() {
        return relationshipLeaderCode;
    }

    /**
     * Sets the value of the relationshipLeaderCode property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setRelationshipLeaderCode(String value) {
        this.relationshipLeaderCode = value;
    }

    /**
     * Gets the value of the saccIdParent property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getSaccIdParent() {
        return saccIdParent;
    }

    /**
     * Sets the value of the saccIdParent property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setSaccIdParent(String value) {
        this.saccIdParent = value;
    }

    /**
     * Gets the value of the saccIdUltimate property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getSaccIdUltimate() {
        return saccIdUltimate;
    }

    /**
     * Sets the value of the saccIdUltimate property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setSaccIdUltimate(String value) {
        this.saccIdUltimate = value;
    }

    /**
     * Gets the value of the sdhVersion property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getSdhVersion() {
        return sdhVersion;
    }

    /**
     * Sets the value of the sdhVersion property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setSdhVersion(String value) {
        this.sdhVersion = value;
    }

    /**
     * Gets the value of the securityDomainExpertise property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getSecurityDomainExpertise() {
        return securityDomainExpertise;
    }

    /**
     * Sets the value of the securityDomainExpertise property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setSecurityDomainExpertise(String value) {
        this.securityDomainExpertise = value;
    }

    /**
     * Gets the value of the shortNameECS property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getShortNameECS() {
        return shortNameECS;
    }

    /**
     * Sets the value of the shortNameECS property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setShortNameECS(String value) {
        this.shortNameECS = value;
    }

    /**
     * Gets the value of the sourceSystemId property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getSourceSystemId() {
        return sourceSystemId;
    }

    /**
     * Sets the value of the sourceSystemId property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setSourceSystemId(String value) {
        this.sourceSystemId = value;
    }

    /**
     * Gets the value of the state property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getState() {
        return state;
    }

    /**
     * Sets the value of the state property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setState(String value) {
        this.state = value;
    }

    /**
     * Gets the value of the street property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getStreet() {
        return street;
    }

    /**
     * Sets the value of the street property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setStreet(String value) {
        this.street = value;
    }

    /**
     * Gets the value of the streetECS property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getStreetECS() {
        return streetECS;
    }

    /**
     * Sets the value of the streetECS property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setStreetECS(String value) {
        this.streetECS = value;
    }

    /**
     * Gets the value of the taxIdentificationNumber property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getTaxIdentificationNumber() {
        return taxIdentificationNumber;
    }

    /**
     * Sets the value of the taxIdentificationNumber property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setTaxIdentificationNumber(String value) {
        this.taxIdentificationNumber = value;
    }

    /**
     * Gets the value of the taxOffice property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getTaxOffice() {
        return taxOffice;
    }

    /**
     * Sets the value of the taxOffice property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setTaxOffice(String value) {
        this.taxOffice = value;
    }

    /**
     * Gets the value of the telephoneNumber property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getTelephoneNumber() {
        return telephoneNumber;
    }

    /**
     * Sets the value of the telephoneNumber property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setTelephoneNumber(String value) {
        this.telephoneNumber = value;
    }

    /**
     * Gets the value of the timeZone property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getTimeZone() {
        return timeZone;
    }

    /**
     * Sets the value of the timeZone property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setTimeZone(String value) {
        this.timeZone = value;
    }

    /**
     * Gets the value of the webSite property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getWebSite() {
        return webSite;
    }

    /**
     * Sets the value of the webSite property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setWebSite(String value) {
        this.webSite = value;
    }

}

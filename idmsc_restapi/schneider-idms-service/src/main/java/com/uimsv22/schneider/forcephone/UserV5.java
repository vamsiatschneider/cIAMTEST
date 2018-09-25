
package com.uimsv22.schneider.forcephone;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for userV5 complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="userV5">
 *   &lt;complexContent>
 *     &lt;extension base="{http://uimsv22.service.ims.schneider.com/}identity">
 *       &lt;sequence>
 *         &lt;element name="countryCode" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="companyId" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="employeeNumber" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="fax" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="goldenId" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="jobDescription" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="jobFunction" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="jobTitle" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="cell" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="partnerAccountId" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="primaryContact" type="{http://www.w3.org/2001/XMLSchema}boolean" minOccurs="0"/>
 *         &lt;element name="sourceSystemId" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="phone" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="salutation" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="channel" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="subChannel" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="state" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="addInfoAddress" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="addInfoAddressECS" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="associatedBrand" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="bdComContents" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="contactPreference" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="county" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="countyECS" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="deletionFlag" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="domainOfExpertise" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="facsimileTelephoneExtension" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="facsimileTelephoneNumberPersonal" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="gender" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="givenNameECS" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="howHeard" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="howHeardOtherReason" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="inheritAccountAddress" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="itComContents" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="jobFunctionLabel" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="jobTitleLabel" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="localityName" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="localityNameECS" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="mailPersonal" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="middleName" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="middleNameECS" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="mobilePersonal" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="note" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="pmComContents" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="postalCode" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="postOfficeBox" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="postOfficeBoxCode" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="prefComCall" type="{http://www.w3.org/2001/XMLSchema}boolean" minOccurs="0"/>
 *         &lt;element name="prefComEmail" type="{http://www.w3.org/2001/XMLSchema}boolean" minOccurs="0"/>
 *         &lt;element name="prefComFax" type="{http://www.w3.org/2001/XMLSchema}boolean" minOccurs="0"/>
 *         &lt;element name="prefComPostMail" type="{http://www.w3.org/2001/XMLSchema}boolean" minOccurs="0"/>
 *         &lt;element name="prefComSurvey" type="{http://www.w3.org/2001/XMLSchema}boolean" minOccurs="0"/>
 *         &lt;element name="prefComText" type="{http://www.w3.org/2001/XMLSchema}boolean" minOccurs="0"/>
 *         &lt;element name="publisherGoldenId" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="pwComContents" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="scontIdReportsTo" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="sdhVersion" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="securityComContents" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="snECS" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="strategicThemes" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="street" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="streetECS" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="telephoneExtension" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="telephoneNumberPersonal" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="timeZone" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="isApproved" type="{http://www.w3.org/2001/XMLSchema}boolean" minOccurs="0"/>
 *         &lt;element name="completionFlag" type="{http://www.w3.org/2001/XMLSchema}boolean" minOccurs="0"/>
 *         &lt;element name="bfoId" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="isInternalUser" type="{http://www.w3.org/2001/XMLSchema}boolean" minOccurs="0"/>
 *         &lt;element name="isActive" type="{http://www.w3.org/2001/XMLSchema}boolean" minOccurs="0"/>
 *         &lt;element name="ids" type="{http://uimsv22.service.ims.schneider.com/}officialsIds" minOccurs="0"/>
 *         &lt;element name="preferredLocale" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="isTrustedAdmin" type="{http://www.w3.org/2001/XMLSchema}boolean" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/extension>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "userV5", propOrder = {
    "countryCode",
    "companyId",
    "employeeNumber",
    "fax",
    "goldenId",
    "jobDescription",
    "jobFunction",
    "jobTitle",
    "cell",
    "partnerAccountId",
    "primaryContact",
    "sourceSystemId",
    "phone",
    "salutation",
    "channel",
    "subChannel",
    "state",
    "addInfoAddress",
    "addInfoAddressECS",
    "associatedBrand",
    "bdComContents",
    "contactPreference",
    "county",
    "countyECS",
    "deletionFlag",
    "domainOfExpertise",
    "facsimileTelephoneExtension",
    "facsimileTelephoneNumberPersonal",
    "gender",
    "givenNameECS",
    "howHeard",
    "howHeardOtherReason",
    "inheritAccountAddress",
    "itComContents",
    "jobFunctionLabel",
    "jobTitleLabel",
    "localityName",
    "localityNameECS",
    "mailPersonal",
    "middleName",
    "middleNameECS",
    "mobilePersonal",
    "note",
    "pmComContents",
    "postalCode",
    "postOfficeBox",
    "postOfficeBoxCode",
    "prefComCall",
    "prefComEmail",
    "prefComFax",
    "prefComPostMail",
    "prefComSurvey",
    "prefComText",
    "publisherGoldenId",
    "pwComContents",
    "scontIdReportsTo",
    "sdhVersion",
    "securityComContents",
    "snECS",
    "strategicThemes",
    "street",
    "streetECS",
    "telephoneExtension",
    "telephoneNumberPersonal",
    "timeZone",
    "isApproved",
    "completionFlag",
    "bfoId",
    "isInternalUser",
    "isActive",
    "ids",
    "preferredLocale",
    "isTrustedAdmin"
})
@XmlSeeAlso({
    UserV6 .class
})
public class UserV5
    extends Identity
{

    protected String countryCode;
    protected String companyId;
    protected String employeeNumber;
    protected String fax;
    protected String goldenId;
    protected String jobDescription;
    protected String jobFunction;
    protected String jobTitle;
    protected String cell;
    protected String partnerAccountId;
    protected Boolean primaryContact;
    protected String sourceSystemId;
    protected String phone;
    protected String salutation;
    protected String channel;
    protected String subChannel;
    protected String state;
    protected String addInfoAddress;
    protected String addInfoAddressECS;
    protected String associatedBrand;
    protected String bdComContents;
    protected String contactPreference;
    protected String county;
    protected String countyECS;
    protected String deletionFlag;
    protected String domainOfExpertise;
    protected String facsimileTelephoneExtension;
    protected String facsimileTelephoneNumberPersonal;
    protected String gender;
    protected String givenNameECS;
    protected String howHeard;
    protected String howHeardOtherReason;
    protected String inheritAccountAddress;
    protected String itComContents;
    protected String jobFunctionLabel;
    protected String jobTitleLabel;
    protected String localityName;
    protected String localityNameECS;
    protected String mailPersonal;
    protected String middleName;
    protected String middleNameECS;
    protected String mobilePersonal;
    protected String note;
    protected String pmComContents;
    protected String postalCode;
    protected String postOfficeBox;
    protected String postOfficeBoxCode;
    protected Boolean prefComCall;
    protected Boolean prefComEmail;
    protected Boolean prefComFax;
    protected Boolean prefComPostMail;
    protected Boolean prefComSurvey;
    protected Boolean prefComText;
    protected String publisherGoldenId;
    protected String pwComContents;
    protected String scontIdReportsTo;
    protected String sdhVersion;
    protected String securityComContents;
    protected String snECS;
    protected String strategicThemes;
    protected String street;
    protected String streetECS;
    protected String telephoneExtension;
    protected String telephoneNumberPersonal;
    protected String timeZone;
    protected Boolean isApproved;
    protected Boolean completionFlag;
    protected String bfoId;
    protected Boolean isInternalUser;
    protected Boolean isActive;
    protected OfficialsIds ids;
    protected String preferredLocale;
    protected Boolean isTrustedAdmin;

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
     * Gets the value of the companyId property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCompanyId() {
        return companyId;
    }

    /**
     * Sets the value of the companyId property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCompanyId(String value) {
        this.companyId = value;
    }

    /**
     * Gets the value of the employeeNumber property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getEmployeeNumber() {
        return employeeNumber;
    }

    /**
     * Sets the value of the employeeNumber property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setEmployeeNumber(String value) {
        this.employeeNumber = value;
    }

    /**
     * Gets the value of the fax property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFax() {
        return fax;
    }

    /**
     * Sets the value of the fax property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFax(String value) {
        this.fax = value;
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
     * Gets the value of the jobDescription property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getJobDescription() {
        return jobDescription;
    }

    /**
     * Sets the value of the jobDescription property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setJobDescription(String value) {
        this.jobDescription = value;
    }

    /**
     * Gets the value of the jobFunction property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getJobFunction() {
        return jobFunction;
    }

    /**
     * Sets the value of the jobFunction property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setJobFunction(String value) {
        this.jobFunction = value;
    }

    /**
     * Gets the value of the jobTitle property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getJobTitle() {
        return jobTitle;
    }

    /**
     * Sets the value of the jobTitle property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setJobTitle(String value) {
        this.jobTitle = value;
    }

    /**
     * Gets the value of the cell property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCell() {
        return cell;
    }

    /**
     * Sets the value of the cell property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCell(String value) {
        this.cell = value;
    }

    /**
     * Gets the value of the partnerAccountId property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPartnerAccountId() {
        return partnerAccountId;
    }

    /**
     * Sets the value of the partnerAccountId property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPartnerAccountId(String value) {
        this.partnerAccountId = value;
    }

    /**
     * Gets the value of the primaryContact property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isPrimaryContact() {
        return primaryContact;
    }

    /**
     * Sets the value of the primaryContact property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setPrimaryContact(Boolean value) {
        this.primaryContact = value;
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
     * Gets the value of the phone property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPhone() {
        return phone;
    }

    /**
     * Sets the value of the phone property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPhone(String value) {
        this.phone = value;
    }

    /**
     * Gets the value of the salutation property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getSalutation() {
        return salutation;
    }

    /**
     * Sets the value of the salutation property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setSalutation(String value) {
        this.salutation = value;
    }

    /**
     * Gets the value of the channel property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getChannel() {
        return channel;
    }

    /**
     * Sets the value of the channel property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setChannel(String value) {
        this.channel = value;
    }

    /**
     * Gets the value of the subChannel property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getSubChannel() {
        return subChannel;
    }

    /**
     * Sets the value of the subChannel property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setSubChannel(String value) {
        this.subChannel = value;
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
     * Gets the value of the associatedBrand property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getAssociatedBrand() {
        return associatedBrand;
    }

    /**
     * Sets the value of the associatedBrand property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setAssociatedBrand(String value) {
        this.associatedBrand = value;
    }

    /**
     * Gets the value of the bdComContents property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getBdComContents() {
        return bdComContents;
    }

    /**
     * Sets the value of the bdComContents property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setBdComContents(String value) {
        this.bdComContents = value;
    }

    /**
     * Gets the value of the contactPreference property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getContactPreference() {
        return contactPreference;
    }

    /**
     * Sets the value of the contactPreference property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setContactPreference(String value) {
        this.contactPreference = value;
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
     * Gets the value of the domainOfExpertise property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getDomainOfExpertise() {
        return domainOfExpertise;
    }

    /**
     * Sets the value of the domainOfExpertise property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setDomainOfExpertise(String value) {
        this.domainOfExpertise = value;
    }

    /**
     * Gets the value of the facsimileTelephoneExtension property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFacsimileTelephoneExtension() {
        return facsimileTelephoneExtension;
    }

    /**
     * Sets the value of the facsimileTelephoneExtension property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFacsimileTelephoneExtension(String value) {
        this.facsimileTelephoneExtension = value;
    }

    /**
     * Gets the value of the facsimileTelephoneNumberPersonal property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getFacsimileTelephoneNumberPersonal() {
        return facsimileTelephoneNumberPersonal;
    }

    /**
     * Sets the value of the facsimileTelephoneNumberPersonal property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setFacsimileTelephoneNumberPersonal(String value) {
        this.facsimileTelephoneNumberPersonal = value;
    }

    /**
     * Gets the value of the gender property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getGender() {
        return gender;
    }

    /**
     * Sets the value of the gender property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setGender(String value) {
        this.gender = value;
    }

    /**
     * Gets the value of the givenNameECS property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getGivenNameECS() {
        return givenNameECS;
    }

    /**
     * Sets the value of the givenNameECS property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setGivenNameECS(String value) {
        this.givenNameECS = value;
    }

    /**
     * Gets the value of the howHeard property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getHowHeard() {
        return howHeard;
    }

    /**
     * Sets the value of the howHeard property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setHowHeard(String value) {
        this.howHeard = value;
    }

    /**
     * Gets the value of the howHeardOtherReason property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getHowHeardOtherReason() {
        return howHeardOtherReason;
    }

    /**
     * Sets the value of the howHeardOtherReason property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setHowHeardOtherReason(String value) {
        this.howHeardOtherReason = value;
    }

    /**
     * Gets the value of the inheritAccountAddress property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getInheritAccountAddress() {
        return inheritAccountAddress;
    }

    /**
     * Sets the value of the inheritAccountAddress property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setInheritAccountAddress(String value) {
        this.inheritAccountAddress = value;
    }

    /**
     * Gets the value of the itComContents property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getItComContents() {
        return itComContents;
    }

    /**
     * Sets the value of the itComContents property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setItComContents(String value) {
        this.itComContents = value;
    }

    /**
     * Gets the value of the jobFunctionLabel property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getJobFunctionLabel() {
        return jobFunctionLabel;
    }

    /**
     * Sets the value of the jobFunctionLabel property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setJobFunctionLabel(String value) {
        this.jobFunctionLabel = value;
    }

    /**
     * Gets the value of the jobTitleLabel property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getJobTitleLabel() {
        return jobTitleLabel;
    }

    /**
     * Sets the value of the jobTitleLabel property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setJobTitleLabel(String value) {
        this.jobTitleLabel = value;
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
     * Gets the value of the localityNameECS property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getLocalityNameECS() {
        return localityNameECS;
    }

    /**
     * Sets the value of the localityNameECS property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setLocalityNameECS(String value) {
        this.localityNameECS = value;
    }

    /**
     * Gets the value of the mailPersonal property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getMailPersonal() {
        return mailPersonal;
    }

    /**
     * Sets the value of the mailPersonal property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setMailPersonal(String value) {
        this.mailPersonal = value;
    }

    /**
     * Gets the value of the middleName property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getMiddleName() {
        return middleName;
    }

    /**
     * Sets the value of the middleName property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setMiddleName(String value) {
        this.middleName = value;
    }

    /**
     * Gets the value of the middleNameECS property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getMiddleNameECS() {
        return middleNameECS;
    }

    /**
     * Sets the value of the middleNameECS property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setMiddleNameECS(String value) {
        this.middleNameECS = value;
    }

    /**
     * Gets the value of the mobilePersonal property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getMobilePersonal() {
        return mobilePersonal;
    }

    /**
     * Sets the value of the mobilePersonal property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setMobilePersonal(String value) {
        this.mobilePersonal = value;
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
     * Gets the value of the pmComContents property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPmComContents() {
        return pmComContents;
    }

    /**
     * Sets the value of the pmComContents property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPmComContents(String value) {
        this.pmComContents = value;
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
     * Gets the value of the prefComCall property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isPrefComCall() {
        return prefComCall;
    }

    /**
     * Sets the value of the prefComCall property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setPrefComCall(Boolean value) {
        this.prefComCall = value;
    }

    /**
     * Gets the value of the prefComEmail property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isPrefComEmail() {
        return prefComEmail;
    }

    /**
     * Sets the value of the prefComEmail property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setPrefComEmail(Boolean value) {
        this.prefComEmail = value;
    }

    /**
     * Gets the value of the prefComFax property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isPrefComFax() {
        return prefComFax;
    }

    /**
     * Sets the value of the prefComFax property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setPrefComFax(Boolean value) {
        this.prefComFax = value;
    }

    /**
     * Gets the value of the prefComPostMail property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isPrefComPostMail() {
        return prefComPostMail;
    }

    /**
     * Sets the value of the prefComPostMail property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setPrefComPostMail(Boolean value) {
        this.prefComPostMail = value;
    }

    /**
     * Gets the value of the prefComSurvey property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isPrefComSurvey() {
        return prefComSurvey;
    }

    /**
     * Sets the value of the prefComSurvey property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setPrefComSurvey(Boolean value) {
        this.prefComSurvey = value;
    }

    /**
     * Gets the value of the prefComText property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isPrefComText() {
        return prefComText;
    }

    /**
     * Sets the value of the prefComText property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setPrefComText(Boolean value) {
        this.prefComText = value;
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
     * Gets the value of the pwComContents property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPwComContents() {
        return pwComContents;
    }

    /**
     * Sets the value of the pwComContents property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPwComContents(String value) {
        this.pwComContents = value;
    }

    /**
     * Gets the value of the scontIdReportsTo property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getScontIdReportsTo() {
        return scontIdReportsTo;
    }

    /**
     * Sets the value of the scontIdReportsTo property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setScontIdReportsTo(String value) {
        this.scontIdReportsTo = value;
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
     * Gets the value of the securityComContents property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getSecurityComContents() {
        return securityComContents;
    }

    /**
     * Sets the value of the securityComContents property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setSecurityComContents(String value) {
        this.securityComContents = value;
    }

    /**
     * Gets the value of the snECS property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getSnECS() {
        return snECS;
    }

    /**
     * Sets the value of the snECS property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setSnECS(String value) {
        this.snECS = value;
    }

    /**
     * Gets the value of the strategicThemes property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getStrategicThemes() {
        return strategicThemes;
    }

    /**
     * Sets the value of the strategicThemes property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setStrategicThemes(String value) {
        this.strategicThemes = value;
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
     * Gets the value of the telephoneExtension property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getTelephoneExtension() {
        return telephoneExtension;
    }

    /**
     * Sets the value of the telephoneExtension property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setTelephoneExtension(String value) {
        this.telephoneExtension = value;
    }

    /**
     * Gets the value of the telephoneNumberPersonal property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getTelephoneNumberPersonal() {
        return telephoneNumberPersonal;
    }

    /**
     * Sets the value of the telephoneNumberPersonal property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setTelephoneNumberPersonal(String value) {
        this.telephoneNumberPersonal = value;
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
     * Gets the value of the isApproved property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isIsApproved() {
        return isApproved;
    }

    /**
     * Sets the value of the isApproved property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setIsApproved(Boolean value) {
        this.isApproved = value;
    }

    /**
     * Gets the value of the completionFlag property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isCompletionFlag() {
        return completionFlag;
    }

    /**
     * Sets the value of the completionFlag property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setCompletionFlag(Boolean value) {
        this.completionFlag = value;
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
     * Gets the value of the isInternalUser property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isIsInternalUser() {
        return isInternalUser;
    }

    /**
     * Sets the value of the isInternalUser property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setIsInternalUser(Boolean value) {
        this.isInternalUser = value;
    }

    /**
     * Gets the value of the isActive property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isIsActive() {
        return isActive;
    }

    /**
     * Sets the value of the isActive property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setIsActive(Boolean value) {
        this.isActive = value;
    }

    /**
     * Gets the value of the ids property.
     * 
     * @return
     *     possible object is
     *     {@link OfficialsIds }
     *     
     */
    public OfficialsIds getIds() {
        return ids;
    }

    /**
     * Sets the value of the ids property.
     * 
     * @param value
     *     allowed object is
     *     {@link OfficialsIds }
     *     
     */
    public void setIds(OfficialsIds value) {
        this.ids = value;
    }

    /**
     * Gets the value of the preferredLocale property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getPreferredLocale() {
        return preferredLocale;
    }

    /**
     * Sets the value of the preferredLocale property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setPreferredLocale(String value) {
        this.preferredLocale = value;
    }

    /**
     * Gets the value of the isTrustedAdmin property.
     * 
     * @return
     *     possible object is
     *     {@link Boolean }
     *     
     */
    public Boolean isIsTrustedAdmin() {
        return isTrustedAdmin;
    }

    /**
     * Sets the value of the isTrustedAdmin property.
     * 
     * @param value
     *     allowed object is
     *     {@link Boolean }
     *     
     */
    public void setIsTrustedAdmin(Boolean value) {
        this.isTrustedAdmin = value;
    }

}

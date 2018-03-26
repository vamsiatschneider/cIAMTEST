
package com.uims.companymanager;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for updateCompany complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="updateCompany">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="callerFid" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="samlAssertion" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="federatedId" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="company" type="{http://uimsv2.service.ims.schneider.com/}companyV3" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "updateCompany", propOrder = {
    "callerFid",
    "samlAssertion",
    "federatedId",
    "company"
})
public class UpdateCompany {

    protected String callerFid;
    protected String samlAssertion;
    protected String federatedId;
    protected CompanyV3 company;

    /**
     * Gets the value of the callerFid property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getCallerFid() {
        return callerFid;
    }

    /**
     * Sets the value of the callerFid property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setCallerFid(String value) {
        this.callerFid = value;
    }

    /**
     * Gets the value of the samlAssertion property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getSamlAssertion() {
        return samlAssertion;
    }

    /**
     * Sets the value of the samlAssertion property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setSamlAssertion(String value) {
        this.samlAssertion = value;
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
     * Gets the value of the company property.
     * 
     * @return
     *     possible object is
     *     {@link CompanyV3 }
     *     
     */
    public CompanyV3 getCompany() {
        return company;
    }

    /**
     * Sets the value of the company property.
     * 
     * @param value
     *     allowed object is
     *     {@link CompanyV3 }
     *     
     */
    public void setCompany(CompanyV3 value) {
        this.company = value;
    }

}


package com.uims.companymanager;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for getCompany complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="getCompany">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="callerFid" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="samlAssertionOrToken" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="federatedId" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "getCompany", propOrder = {
    "callerFid",
    "samlAssertionOrToken",
    "federatedId"
})
public class GetCompany {

    protected String callerFid;
    protected String samlAssertionOrToken;
    protected String federatedId;

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
     * Gets the value of the samlAssertionOrToken property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getSamlAssertionOrToken() {
        return samlAssertionOrToken;
    }

    /**
     * Sets the value of the samlAssertionOrToken property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setSamlAssertionOrToken(String value) {
        this.samlAssertionOrToken = value;
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

}


package com.uims.companymanager;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for removePrimaryContact complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="removePrimaryContact">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="callerFid" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="samlAssertion" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "removePrimaryContact", propOrder = {
    "callerFid",
    "samlAssertion"
})
public class RemovePrimaryContact {

    protected String callerFid;
    protected String samlAssertion;

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

}


package com.se.uims.usermanager;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for requestPhoneIdChange complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="requestPhoneIdChange">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="callerFid" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="samlAssertion" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="application" type="{http://uimsv22.service.ims.schneider.com/}accessElement" minOccurs="0"/>
 *         &lt;element name="newPhoneId" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "requestPhoneIdChange", propOrder = {
    "callerFid",
    "samlAssertion",
    "application",
    "newPhoneId"
})
public class RequestPhoneIdChange {

    protected String callerFid;
    protected String samlAssertion;
    protected AccessElement application;
    protected String newPhoneId;

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
     * Gets the value of the application property.
     * 
     * @return
     *     possible object is
     *     {@link AccessElement }
     *     
     */
    public AccessElement getApplication() {
        return application;
    }

    /**
     * Sets the value of the application property.
     * 
     * @param value
     *     allowed object is
     *     {@link AccessElement }
     *     
     */
    public void setApplication(AccessElement value) {
        this.application = value;
    }

    /**
     * Gets the value of the newPhoneId property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getNewPhoneId() {
        return newPhoneId;
    }

    /**
     * Sets the value of the newPhoneId property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setNewPhoneId(String value) {
        this.newPhoneId = value;
    }

}

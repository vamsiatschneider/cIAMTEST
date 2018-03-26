
package com.uims.authenticatedUsermanager;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for createIdentityForceIdmsId complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="createIdentityForceIdmsId">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="callerFid" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="identity" type="{http://uimsv22.service.ims.schneider.com/}userV6" minOccurs="0"/>
 *         &lt;element name="forcedFederatedId" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "createIdentityForceIdmsId", propOrder = {
    "callerFid",
    "identity",
    "forcedFederatedId"
})
public class CreateIdentityForceIdmsId {

    protected String callerFid;
    protected UserV6 identity;
    protected String forcedFederatedId;

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
     * Gets the value of the identity property.
     * 
     * @return
     *     possible object is
     *     {@link UserV6 }
     *     
     */
    public UserV6 getIdentity() {
        return identity;
    }

    /**
     * Sets the value of the identity property.
     * 
     * @param value
     *     allowed object is
     *     {@link UserV6 }
     *     
     */
    public void setIdentity(UserV6 value) {
        this.identity = value;
    }

    /**
     * Gets the value of the forcedFederatedId property.
     * 
     * @return
     *     possible object is
     *     {@link String }
     *     
     */
    public String getForcedFederatedId() {
        return forcedFederatedId;
    }

    /**
     * Sets the value of the forcedFederatedId property.
     * 
     * @param value
     *     allowed object is
     *     {@link String }
     *     
     */
    public void setForcedFederatedId(String value) {
        this.forcedFederatedId = value;
    }

}


package com.uims.authenticatedUsermanager;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for reactivate complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="reactivate">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="callerFid" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="federatedId" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="application" type="{http://uimsv22.service.ims.schneider.com/}accessElement" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "reactivate", propOrder = {
    "callerFid",
    "federatedId",
    "application"
})
public class Reactivate {

    protected String callerFid;
    protected String federatedId;
    protected AccessElement application;

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

}


package com.uims.authenticatedUsermanager;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for createIdentityWithPhoneIdForceIdmsIdResponse complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="createIdentityWithPhoneIdForceIdmsIdResponse">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="return" type="{http://uimsv22.service.ims.schneider.com/}createdIdentityReport" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "createIdentityWithPhoneIdForceIdmsIdResponse", propOrder = {
    "_return"
})
public class CreateIdentityWithPhoneIdForceIdmsIdResponse {

    @XmlElement(name = "return")
    protected CreatedIdentityReport _return;

    /**
     * Gets the value of the return property.
     * 
     * @return
     *     possible object is
     *     {@link CreatedIdentityReport }
     *     
     */
    public CreatedIdentityReport getReturn() {
        return _return;
    }

    /**
     * Sets the value of the return property.
     * 
     * @param value
     *     allowed object is
     *     {@link CreatedIdentityReport }
     *     
     */
    public void setReturn(CreatedIdentityReport value) {
        this._return = value;
    }

}

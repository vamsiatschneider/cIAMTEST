
package com.uims.authenticatedUsermanager;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for searchUserByPhoneIdResponse complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="searchUserByPhoneIdResponse">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="return" type="{http://uimsv22.service.ims.schneider.com/}userFederatedIdAndType" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "searchUserByPhoneIdResponse", propOrder = {
    "_return"
})
public class SearchUserByPhoneIdResponse {

    @XmlElement(name = "return")
    protected UserFederatedIdAndType _return;

    /**
     * Gets the value of the return property.
     * 
     * @return
     *     possible object is
     *     {@link UserFederatedIdAndType }
     *     
     */
    public UserFederatedIdAndType getReturn() {
        return _return;
    }

    /**
     * Sets the value of the return property.
     * 
     * @param value
     *     allowed object is
     *     {@link UserFederatedIdAndType }
     *     
     */
    public void setReturn(UserFederatedIdAndType value) {
        this._return = value;
    }

}

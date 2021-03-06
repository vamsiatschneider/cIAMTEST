
package com.schneider.ims.service.uimsv2;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for searchPublicCompany complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="searchPublicCompany">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="callerFid" type="{http://www.w3.org/2001/XMLSchema}string" minOccurs="0"/>
 *         &lt;element name="template" type="{http://uimsv2.service.ims.schneider.com/}companyV3" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "searchPublicCompany", propOrder = {
    "callerFid",
    "template"
})
public class SearchPublicCompany {

    protected String callerFid;
    protected CompanyV3 template;

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
     * Gets the value of the template property.
     * 
     * @return
     *     possible object is
     *     {@link CompanyV3 }
     *     
     */
    public CompanyV3 getTemplate() {
        return template;
    }

    /**
     * Sets the value of the template property.
     * 
     * @param value
     *     allowed object is
     *     {@link CompanyV3 }
     *     
     */
    public void setTemplate(CompanyV3 value) {
        this.template = value;
    }

}

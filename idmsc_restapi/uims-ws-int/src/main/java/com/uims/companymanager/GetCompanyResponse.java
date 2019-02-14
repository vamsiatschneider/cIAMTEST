
package com.uims.companymanager;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for getCompanyResponse complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="getCompanyResponse">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
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
@XmlType(name = "getCompanyResponse", propOrder = {
    "company"
})
public class GetCompanyResponse {

    protected CompanyV3 company;

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

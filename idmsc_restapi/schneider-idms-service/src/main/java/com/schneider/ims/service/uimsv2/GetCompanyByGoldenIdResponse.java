
package com.schneider.ims.service.uimsv2;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for getCompanyByGoldenIdResponse complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="getCompanyByGoldenIdResponse">
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
@XmlType(name = "getCompanyByGoldenIdResponse", propOrder = {
    "company"
})
public class GetCompanyByGoldenIdResponse {

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

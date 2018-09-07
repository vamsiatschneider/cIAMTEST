
package com.schneider.ims.service.uimsv2;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for searchPublicCompanyResponse complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="searchPublicCompanyResponse">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="companies" type="{http://uimsv2.service.ims.schneider.com/}companyV3" maxOccurs="unbounded" minOccurs="0"/>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "searchPublicCompanyResponse", propOrder = {
    "companies"
})
public class SearchPublicCompanyResponse {

    protected List<CompanyV3> companies;

    /**
     * Gets the value of the companies property.
     * 
     * <p>
     * This accessor method returns a reference to the live list,
     * not a snapshot. Therefore any modification you make to the
     * returned list will be present inside the JAXB object.
     * This is why there is not a <CODE>set</CODE> method for the companies property.
     * 
     * <p>
     * For example, to add a new item, do as follows:
     * <pre>
     *    getCompanies().add(newItem);
     * </pre>
     * 
     * 
     * <p>
     * Objects of the following type(s) are allowed in the list
     * {@link CompanyV3 }
     * 
     * 
     */
    public List<CompanyV3> getCompanies() {
        if (companies == null) {
            companies = new ArrayList<CompanyV3>();
        }
        return this.companies;
    }

}

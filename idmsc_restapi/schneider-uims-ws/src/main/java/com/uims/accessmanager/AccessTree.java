
package com.uims.accessmanager;

import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for accessTree complex type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * 
 * <pre>
 * &lt;complexType name="accessTree">
 *   &lt;complexContent>
 *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *       &lt;sequence>
 *         &lt;element name="accessList" minOccurs="0">
 *           &lt;complexType>
 *             &lt;complexContent>
 *               &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
 *                 &lt;sequence>
 *                   &lt;element name="access" type="{http://uimsv2.service.ims.schneider.com/}accessElement" maxOccurs="unbounded" minOccurs="0"/>
 *                 &lt;/sequence>
 *               &lt;/restriction>
 *             &lt;/complexContent>
 *           &lt;/complexType>
 *         &lt;/element>
 *       &lt;/sequence>
 *     &lt;/restriction>
 *   &lt;/complexContent>
 * &lt;/complexType>
 * </pre>
 * 
 * 
 */
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "accessTree", propOrder = {
    "accessList"
})
public class AccessTree {

    protected AccessTree.AccessList accessList;

    /**
     * Gets the value of the accessList property.
     * 
     * @return
     *     possible object is
     *     {@link AccessTree.AccessList }
     *     
     */
    public AccessTree.AccessList getAccessList() {
        return accessList;
    }

    /**
     * Sets the value of the accessList property.
     * 
     * @param value
     *     allowed object is
     *     {@link AccessTree.AccessList }
     *     
     */
    public void setAccessList(AccessTree.AccessList value) {
        this.accessList = value;
    }


    /**
     * <p>Java class for anonymous complex type.
     * 
     * <p>The following schema fragment specifies the expected content contained within this class.
     * 
     * <pre>
     * &lt;complexType>
     *   &lt;complexContent>
     *     &lt;restriction base="{http://www.w3.org/2001/XMLSchema}anyType">
     *       &lt;sequence>
     *         &lt;element name="access" type="{http://uimsv2.service.ims.schneider.com/}accessElement" maxOccurs="unbounded" minOccurs="0"/>
     *       &lt;/sequence>
     *     &lt;/restriction>
     *   &lt;/complexContent>
     * &lt;/complexType>
     * </pre>
     * 
     * 
     */
    @XmlAccessorType(XmlAccessType.FIELD)
    @XmlType(name = "", propOrder = {
        "access"
    })
    public static class AccessList {

        protected List<AccessElement> access;

        /**
         * Gets the value of the access property.
         * 
         * <p>
         * This accessor method returns a reference to the live list,
         * not a snapshot. Therefore any modification you make to the
         * returned list will be present inside the JAXB object.
         * This is why there is not a <CODE>set</CODE> method for the access property.
         * 
         * <p>
         * For example, to add a new item, do as follows:
         * <pre>
         *    getAccess().add(newItem);
         * </pre>
         * 
         * 
         * <p>
         * Objects of the following type(s) are allowed in the list
         * {@link AccessElement }
         * 
         * 
         */
        public List<AccessElement> getAccess() {
            if (access == null) {
                access = new ArrayList<AccessElement>();
            }
            return this.access;
        }

    }

}

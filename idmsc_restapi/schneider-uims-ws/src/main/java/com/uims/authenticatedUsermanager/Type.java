
package com.uims.authenticatedUsermanager;

import javax.xml.bind.annotation.XmlEnum;
import javax.xml.bind.annotation.XmlType;


/**
 * <p>Java class for type.
 * 
 * <p>The following schema fragment specifies the expected content contained within this class.
 * <p>
 * <pre>
 * &lt;simpleType name="type">
 *   &lt;restriction base="{http://www.w3.org/2001/XMLSchema}string">
 *     &lt;enumeration value="APPLICATION"/>
 *     &lt;enumeration value="FEATURE"/>
 *     &lt;enumeration value="PROGRAM"/>
 *     &lt;enumeration value="PROGRAM_LEVEL"/>
 *   &lt;/restriction>
 * &lt;/simpleType>
 * </pre>
 * 
 */
@XmlType(name = "type")
@XmlEnum
public enum Type {

    APPLICATION,
    FEATURE,
    PROGRAM,
    PROGRAM_LEVEL;

    public String value() {
        return name();
    }

    public static Type fromValue(String v) {
        return valueOf(v);
    }

}


package com.uims.companymanager;

import javax.xml.ws.WebFault;


/**
 * This class was generated by the JAX-WS RI.
 * JAX-WS RI 2.2.9-b130926.1035
 * Generated source version: 2.2
 * 
 */
@WebFault(name = "SecuredImsException", targetNamespace = "http://uimsv2.service.ims.schneider.com/")
public class SecuredImsException_Exception
    extends Exception
{

    /**
     * Java type that goes as soapenv:Fault detail element.
     * 
     */
    private SecuredImsException faultInfo;

    /**
     * 
     * @param faultInfo
     * @param message
     */
    public SecuredImsException_Exception(String message, SecuredImsException faultInfo) {
        super(message);
        this.faultInfo = faultInfo;
    }

    /**
     * 
     * @param faultInfo
     * @param cause
     * @param message
     */
    public SecuredImsException_Exception(String message, SecuredImsException faultInfo, Throwable cause) {
        super(message, cause);
        this.faultInfo = faultInfo;
    }

    /**
     * 
     * @return
     *     returns fault bean: com.uims.companymanager.SecuredImsException
     */
    public SecuredImsException getFaultInfo() {
        return faultInfo;
    }

}

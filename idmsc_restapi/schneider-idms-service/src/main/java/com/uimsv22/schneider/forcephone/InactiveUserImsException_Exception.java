
package com.uimsv22.schneider.forcephone;

import javax.xml.ws.WebFault;


/**
 * This class was generated by the JAX-WS RI.
 * JAX-WS RI 2.2.9-b130926.1035
 * Generated source version: 2.2
 * 
 */
@WebFault(name = "InactiveUserImsException", targetNamespace = "http://uimsv22.service.ims.schneider.com/")
public class InactiveUserImsException_Exception
    extends Exception
{

    /**
     * Java type that goes as soapenv:Fault detail element.
     * 
     */
    private InactiveUserImsException faultInfo;

    /**
     * 
     * @param faultInfo
     * @param message
     */
    public InactiveUserImsException_Exception(String message, InactiveUserImsException faultInfo) {
        super(message);
        this.faultInfo = faultInfo;
    }

    /**
     * 
     * @param faultInfo
     * @param cause
     * @param message
     */
    public InactiveUserImsException_Exception(String message, InactiveUserImsException faultInfo, Throwable cause) {
        super(message, cause);
        this.faultInfo = faultInfo;
    }

    /**
     * 
     * @return
     *     returns fault bean: com.uimsv22.schneider.forcephone.InactiveUserImsException
     */
    public InactiveUserImsException getFaultInfo() {
        return faultInfo;
    }

}
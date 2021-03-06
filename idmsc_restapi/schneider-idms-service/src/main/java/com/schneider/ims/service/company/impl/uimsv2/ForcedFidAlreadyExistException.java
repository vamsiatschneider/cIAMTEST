
package com.schneider.ims.service.company.impl.uimsv2;

import javax.xml.ws.WebFault;


/**
 * This class was generated by the JAX-WS RI.
 * JAX-WS RI 2.2.9-b130926.1035
 * Generated source version: 2.2
 * 
 */
@WebFault(name = "ForcedFidAlreadyExistException", targetNamespace = "http://uimsv2.service.ims.schneider.com/")
public class ForcedFidAlreadyExistException
    extends Exception
{

    /**
     * Java type that goes as soapenv:Fault detail element.
     * 
     */
    private com.schneider.ims.service.uimsv2.ForcedFidAlreadyExistException faultInfo;

    /**
     * 
     * @param faultInfo
     * @param message
     */
    public ForcedFidAlreadyExistException(String message, com.schneider.ims.service.uimsv2.ForcedFidAlreadyExistException faultInfo) {
        super(message);
        this.faultInfo = faultInfo;
    }

    /**
     * 
     * @param faultInfo
     * @param cause
     * @param message
     */
    public ForcedFidAlreadyExistException(String message, com.schneider.ims.service.uimsv2.ForcedFidAlreadyExistException faultInfo, Throwable cause) {
        super(message, cause);
        this.faultInfo = faultInfo;
    }

    /**
     * 
     * @return
     *     returns fault bean: com.schneider.ims.service.uimsv2.ForcedFidAlreadyExistException
     */
    public com.schneider.ims.service.uimsv2.ForcedFidAlreadyExistException getFaultInfo() {
        return faultInfo;
    }

}

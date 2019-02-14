
package com.uimsv22.schneider.forcephone;

import java.net.MalformedURLException;
import java.net.URL;
import javax.xml.namespace.QName;
import javax.xml.ws.Service;
import javax.xml.ws.WebEndpoint;
import javax.xml.ws.WebServiceClient;
import javax.xml.ws.WebServiceException;
import javax.xml.ws.WebServiceFeature;


/**
 * This class was generated by the JAX-WS RI.
 * JAX-WS RI 2.2.9-b130926.1035
 * Generated source version: 2.2
 * 
 */
@WebServiceClient(name = "UIMSV22_UserAuthenticatedManagement", targetNamespace = "http://uimsv22.impl.service.ims.schneider.com/", wsdlLocation = "https://ims-sqe.btsec.dev.schneider-electric.com/IMS-UserManager/UIMSV22/2WAuthenticated-UserManager?wsdl")
public class UIMSV22UserAuthenticatedManagement
    extends Service
{

    private final static URL UIMSV22USERAUTHENTICATEDMANAGEMENT_WSDL_LOCATION;
    private final static WebServiceException UIMSV22USERAUTHENTICATEDMANAGEMENT_EXCEPTION;
    private final static QName UIMSV22USERAUTHENTICATEDMANAGEMENT_QNAME = new QName("http://uimsv22.impl.service.ims.schneider.com/", "UIMSV22_UserAuthenticatedManagement");

    static {
        URL url = null;
        WebServiceException e = null;
        try {
            url = new URL("https://ims-sqe.btsec.dev.schneider-electric.com/IMS-UserManager/UIMSV22/2WAuthenticated-UserManager?wsdl");
        } catch (MalformedURLException ex) {
            e = new WebServiceException(ex);
        }
        UIMSV22USERAUTHENTICATEDMANAGEMENT_WSDL_LOCATION = url;
        UIMSV22USERAUTHENTICATEDMANAGEMENT_EXCEPTION = e;
    }

    public UIMSV22UserAuthenticatedManagement() {
        super(__getWsdlLocation(), UIMSV22USERAUTHENTICATEDMANAGEMENT_QNAME);
    }

    public UIMSV22UserAuthenticatedManagement(WebServiceFeature... features) {
        super(__getWsdlLocation(), UIMSV22USERAUTHENTICATEDMANAGEMENT_QNAME, features);
    }

    public UIMSV22UserAuthenticatedManagement(URL wsdlLocation) {
        super(wsdlLocation, UIMSV22USERAUTHENTICATEDMANAGEMENT_QNAME);
    }

    public UIMSV22UserAuthenticatedManagement(URL wsdlLocation, WebServiceFeature... features) {
        super(wsdlLocation, UIMSV22USERAUTHENTICATEDMANAGEMENT_QNAME, features);
    }

    public UIMSV22UserAuthenticatedManagement(URL wsdlLocation, QName serviceName) {
        super(wsdlLocation, serviceName);
    }

    public UIMSV22UserAuthenticatedManagement(URL wsdlLocation, QName serviceName, WebServiceFeature... features) {
        super(wsdlLocation, serviceName, features);
    }

    /**
     * 
     * @return
     *     returns AuthenticatedUserManagerUIMSV22
     */
    @WebEndpoint(name = "AuthenticatedUserManager_UIMSV22_ImplPort")
    public AuthenticatedUserManagerUIMSV22 getAuthenticatedUserManagerUIMSV22ImplPort() {
        return super.getPort(new QName("http://uimsv22.impl.service.ims.schneider.com/", "AuthenticatedUserManager_UIMSV22_ImplPort"), AuthenticatedUserManagerUIMSV22.class);
    }

    /**
     * 
     * @param features
     *     A list of {@link javax.xml.ws.WebServiceFeature} to configure on the proxy.  Supported features not in the <code>features</code> parameter will have their default values.
     * @return
     *     returns AuthenticatedUserManagerUIMSV22
     */
    @WebEndpoint(name = "AuthenticatedUserManager_UIMSV22_ImplPort")
    public AuthenticatedUserManagerUIMSV22 getAuthenticatedUserManagerUIMSV22ImplPort(WebServiceFeature... features) {
        return super.getPort(new QName("http://uimsv22.impl.service.ims.schneider.com/", "AuthenticatedUserManager_UIMSV22_ImplPort"), AuthenticatedUserManagerUIMSV22.class, features);
    }

    private static URL __getWsdlLocation() {
        if (UIMSV22USERAUTHENTICATEDMANAGEMENT_EXCEPTION!= null) {
            throw UIMSV22USERAUTHENTICATEDMANAGEMENT_EXCEPTION;
        }
        return UIMSV22USERAUTHENTICATEDMANAGEMENT_WSDL_LOCATION;
    }

}

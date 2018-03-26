
package com.uims.companymanager;

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
@WebServiceClient(name = "CompanyManagementV2", targetNamespace = "http://uimsv2.impl.company.service.ims.schneider.com/", wsdlLocation = "https://ims-int.btsec.dev.schneider-electric.com/IMS-CompanyManager/UIMSV2/CompanyManagement?wsdl")
public class CompanyManagementV2
    extends Service
{

    private final static URL COMPANYMANAGEMENTV2_WSDL_LOCATION;
    private final static WebServiceException COMPANYMANAGEMENTV2_EXCEPTION;
    private final static QName COMPANYMANAGEMENTV2_QNAME = new QName("http://uimsv2.impl.company.service.ims.schneider.com/", "CompanyManagementV2");

    static {
        URL url = null;
        WebServiceException e = null;
        try {
            url = new URL("https://ims-int.btsec.dev.schneider-electric.com/IMS-CompanyManager/UIMSV2/CompanyManagement?wsdl");
        } catch (MalformedURLException ex) {
            e = new WebServiceException(ex);
        }
        COMPANYMANAGEMENTV2_WSDL_LOCATION = url;
        COMPANYMANAGEMENTV2_EXCEPTION = e;
    }

    public CompanyManagementV2() {
        super(__getWsdlLocation(), COMPANYMANAGEMENTV2_QNAME);
    }

    public CompanyManagementV2(WebServiceFeature... features) {
        super(__getWsdlLocation(), COMPANYMANAGEMENTV2_QNAME, features);
    }

    public CompanyManagementV2(URL wsdlLocation) {
        super(wsdlLocation, COMPANYMANAGEMENTV2_QNAME);
    }

    public CompanyManagementV2(URL wsdlLocation, WebServiceFeature... features) {
        super(wsdlLocation, COMPANYMANAGEMENTV2_QNAME, features);
    }

    public CompanyManagementV2(URL wsdlLocation, QName serviceName) {
        super(wsdlLocation, serviceName);
    }

    public CompanyManagementV2(URL wsdlLocation, QName serviceName, WebServiceFeature... features) {
        super(wsdlLocation, serviceName, features);
    }

    /**
     * 
     * @return
     *     returns CompanyManagerUIMSV2
     */
    @WebEndpoint(name = "CompanyManager_UIMSV2_ImplPort")
    public CompanyManagerUIMSV2 getCompanyManagerUIMSV2ImplPort() {
        return super.getPort(new QName("http://uimsv2.impl.company.service.ims.schneider.com/", "CompanyManager_UIMSV2_ImplPort"), CompanyManagerUIMSV2.class);
    }

    /**
     * 
     * @param features
     *     A list of {@link javax.xml.ws.WebServiceFeature} to configure on the proxy.  Supported features not in the <code>features</code> parameter will have their default values.
     * @return
     *     returns CompanyManagerUIMSV2
     */
    @WebEndpoint(name = "CompanyManager_UIMSV2_ImplPort")
    public CompanyManagerUIMSV2 getCompanyManagerUIMSV2ImplPort(WebServiceFeature... features) {
        return super.getPort(new QName("http://uimsv2.impl.company.service.ims.schneider.com/", "CompanyManager_UIMSV2_ImplPort"), CompanyManagerUIMSV2.class, features);
    }

    private static URL __getWsdlLocation() {
        if (COMPANYMANAGEMENTV2_EXCEPTION!= null) {
            throw COMPANYMANAGEMENTV2_EXCEPTION;
        }
        return COMPANYMANAGEMENTV2_WSDL_LOCATION;
    }

}

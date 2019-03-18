
package com.uims.accessmanager;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.WebService;
import javax.xml.bind.annotation.XmlSeeAlso;
import javax.xml.ws.RequestWrapper;
import javax.xml.ws.ResponseWrapper;


/**
 * This class was generated by the JAX-WS RI.
 * JAX-WS RI 2.2.9-b130926.1035
 * Generated source version: 2.2
 * 
 */
@WebService(name = "UserAccessManager_UIMSV2", targetNamespace = "http://uimsv2.service.ims.schneider.com/")
@XmlSeeAlso({
    ObjectFactory.class
})
public interface UserAccessManagerUIMSV2 {


    /**
     * 
     * @param access
     * @param federatedId
     * @param callerFid
     * @return
     *     returns boolean
     * @throws IMSBadApplicationException_Exception
     * @throws LdapTemplateNotReadyException_Exception
     * @throws InvalidImsServiceMethodArgumentException_Exception
     * @throws UnexpectedRuntimeImsException_Exception
     * @throws IMSServiceSecurityCallNotAllowedException_Exception
     * @throws RequestedEntryNotExistsException_Exception
     * @throws SecuredImsException_Exception
     * @throws UnexpectedLdapResponseException_Exception
     */
    @WebMethod
    @WebResult(targetNamespace = "")
    @RequestWrapper(localName = "grantAccessControlToUser", targetNamespace = "http://uimsv2.service.ims.schneider.com/", className = "com.uims.accessmanager.GrantAccessControlToUser")
    @ResponseWrapper(localName = "grantAccessControlToUserResponse", targetNamespace = "http://uimsv2.service.ims.schneider.com/", className = "com.uims.accessmanager.GrantAccessControlToUserResponse")
    public boolean grantAccessControlToUser(
        @WebParam(name = "callerFid", targetNamespace = "")
        String callerFid,
        @WebParam(name = "federatedId", targetNamespace = "")
        String federatedId,
        @WebParam(name = "access", targetNamespace = "")
        AccessElement access)
        throws IMSBadApplicationException_Exception, IMSServiceSecurityCallNotAllowedException_Exception, InvalidImsServiceMethodArgumentException_Exception, LdapTemplateNotReadyException_Exception, RequestedEntryNotExistsException_Exception, SecuredImsException_Exception, UnexpectedLdapResponseException_Exception, UnexpectedRuntimeImsException_Exception
    ;

    /**
     * 
     * @param access
     * @param federatedId
     * @param callerFid
     * @return
     *     returns boolean
     * @throws IMSBadApplicationException_Exception
     * @throws LdapTemplateNotReadyException_Exception
     * @throws InvalidImsServiceMethodArgumentException_Exception
     * @throws UnexpectedRuntimeImsException_Exception
     * @throws IMSServiceSecurityCallNotAllowedException_Exception
     * @throws RequestedEntryNotExistsException_Exception
     * @throws SecuredImsException_Exception
     * @throws UnexpectedLdapResponseException_Exception
     */
    @WebMethod
    @WebResult(targetNamespace = "")
    @RequestWrapper(localName = "revokeAccessControlToUser", targetNamespace = "http://uimsv2.service.ims.schneider.com/", className = "com.uims.accessmanager.RevokeAccessControlToUser")
    @ResponseWrapper(localName = "revokeAccessControlToUserResponse", targetNamespace = "http://uimsv2.service.ims.schneider.com/", className = "com.uims.accessmanager.RevokeAccessControlToUserResponse")
    public boolean revokeAccessControlToUser(
        @WebParam(name = "callerFid", targetNamespace = "")
        String callerFid,
        @WebParam(name = "federatedId", targetNamespace = "")
        String federatedId,
        @WebParam(name = "access", targetNamespace = "")
        AccessElement access)
        throws IMSBadApplicationException_Exception, IMSServiceSecurityCallNotAllowedException_Exception, InvalidImsServiceMethodArgumentException_Exception, LdapTemplateNotReadyException_Exception, RequestedEntryNotExistsException_Exception, SecuredImsException_Exception, UnexpectedLdapResponseException_Exception, UnexpectedRuntimeImsException_Exception
    ;

    /**
     * 
     * @param federatedId
     * @param callerFid
     * @return
     *     returns com.uims.accessmanager.AccessTree
     * @throws InactiveUserImsException_Exception
     * @throws LdapTemplateNotReadyException_Exception
     * @throws InvalidImsServiceMethodArgumentException_Exception
     * @throws UnexpectedRuntimeImsException_Exception
     * @throws IMSServiceSecurityCallNotAllowedException_Exception
     * @throws RequestedEntryNotExistsException_Exception
     * @throws SecuredImsException_Exception
     * @throws UnexpectedLdapResponseException_Exception
     */
    @WebMethod
    @WebResult(targetNamespace = "")
    @RequestWrapper(localName = "getAccessControlByUser", targetNamespace = "http://uimsv2.service.ims.schneider.com/", className = "com.uims.accessmanager.GetAccessControlByUser")
    @ResponseWrapper(localName = "getAccessControlByUserResponse", targetNamespace = "http://uimsv2.service.ims.schneider.com/", className = "com.uims.accessmanager.GetAccessControlByUserResponse")
    public AccessTree getAccessControlByUser(
        @WebParam(name = "callerFid", targetNamespace = "")
        String callerFid,
        @WebParam(name = "federatedId", targetNamespace = "")
        String federatedId)
        throws IMSServiceSecurityCallNotAllowedException_Exception, InactiveUserImsException_Exception, InvalidImsServiceMethodArgumentException_Exception, LdapTemplateNotReadyException_Exception, RequestedEntryNotExistsException_Exception, SecuredImsException_Exception, UnexpectedLdapResponseException_Exception, UnexpectedRuntimeImsException_Exception
    ;

}
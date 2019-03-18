
package com.uims.companymanager;

import java.util.List;
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
@WebService(name = "CompanyManager_UIMSV2", targetNamespace = "http://uimsv2.service.ims.schneider.com/")
@XmlSeeAlso({
    ObjectFactory.class
})
public interface CompanyManagerUIMSV2 {


    /**
     * 
     * @param samlAssertionOrToken
     * @param goldenId
     * @param callerFid
     * @return
     *     returns com.uims.companymanager.CompanyV3
     * @throws SecuredImsException_Exception
     * @throws UnexpectedLdapResponseException_Exception
     * @throws IMSServiceSecurityCallNotAllowedException_Exception
     * @throws RequestedEntryNotExistsException_Exception
     * @throws LdapTemplateNotReadyException_Exception
     * @throws InvalidImsServiceMethodArgumentException_Exception
     * @throws UnexpectedRuntimeImsException_Exception
     */
    @WebMethod
    @WebResult(name = "company", targetNamespace = "")
    @RequestWrapper(localName = "getCompanyByGoldenId", targetNamespace = "http://uimsv2.service.ims.schneider.com/", className = "com.uims.companymanager.GetCompanyByGoldenId")
    @ResponseWrapper(localName = "getCompanyByGoldenIdResponse", targetNamespace = "http://uimsv2.service.ims.schneider.com/", className = "com.uims.companymanager.GetCompanyByGoldenIdResponse")
    public CompanyV3 getCompanyByGoldenId(
        @WebParam(name = "callerFid", targetNamespace = "")
        String callerFid,
        @WebParam(name = "samlAssertionOrToken", targetNamespace = "")
        String samlAssertionOrToken,
        @WebParam(name = "goldenId", targetNamespace = "")
        String goldenId)
        throws IMSServiceSecurityCallNotAllowedException_Exception, InvalidImsServiceMethodArgumentException_Exception, LdapTemplateNotReadyException_Exception, RequestedEntryNotExistsException_Exception, SecuredImsException_Exception, UnexpectedLdapResponseException_Exception, UnexpectedRuntimeImsException_Exception
    ;

    /**
     * 
     * @param federatedId
     * @param samlAssertionOrToken
     * @param callerFid
     * @return
     *     returns com.uims.companymanager.CompanyV3
     * @throws SecuredImsException_Exception
     * @throws IMSServiceSecurityCallNotAllowedException_Exception
     * @throws UnexpectedLdapResponseException_Exception
     * @throws RequestedEntryNotExistsException_Exception
     * @throws LdapTemplateNotReadyException_Exception
     * @throws UnexpectedRuntimeImsException_Exception
     * @throws InvalidImsServiceMethodArgumentException_Exception
     */
    @WebMethod
    @WebResult(name = "company", targetNamespace = "")
    @RequestWrapper(localName = "getCompany", targetNamespace = "http://uimsv2.service.ims.schneider.com/", className = "com.uims.companymanager.GetCompany")
    @ResponseWrapper(localName = "getCompanyResponse", targetNamespace = "http://uimsv2.service.ims.schneider.com/", className = "com.uims.companymanager.GetCompanyResponse")
    public CompanyV3 getCompany(
        @WebParam(name = "callerFid", targetNamespace = "")
        String callerFid,
        @WebParam(name = "samlAssertionOrToken", targetNamespace = "")
        String samlAssertionOrToken,
        @WebParam(name = "federatedId", targetNamespace = "")
        String federatedId)
        throws IMSServiceSecurityCallNotAllowedException_Exception, InvalidImsServiceMethodArgumentException_Exception, LdapTemplateNotReadyException_Exception, RequestedEntryNotExistsException_Exception, SecuredImsException_Exception, UnexpectedLdapResponseException_Exception, UnexpectedRuntimeImsException_Exception
    ;

    /**
     * 
     * @param authentificationToken
     * @param federatedId
     * @param callerFid
     * @return
     *     returns boolean
     * @throws SecuredImsException_Exception
     * @throws UnexpectedLdapResponseException_Exception
     * @throws IMSServiceSecurityCallNotAllowedException_Exception
     * @throws RequestedEntryNotExistsException_Exception
     * @throws LdapTemplateNotReadyException_Exception
     * @throws InvalidImsServiceMethodArgumentException_Exception
     * @throws UnexpectedRuntimeImsException_Exception
     */
    @WebMethod
    @WebResult(name = "success", targetNamespace = "")
    @RequestWrapper(localName = "rejectCompanyMerge", targetNamespace = "http://uimsv2.service.ims.schneider.com/", className = "com.uims.companymanager.RejectCompanyMerge")
    @ResponseWrapper(localName = "rejectCompanyMergeResponse", targetNamespace = "http://uimsv2.service.ims.schneider.com/", className = "com.uims.companymanager.RejectCompanyMergeResponse")
    public boolean rejectCompanyMerge(
        @WebParam(name = "callerFid", targetNamespace = "")
        String callerFid,
        @WebParam(name = "authentificationToken", targetNamespace = "")
        String authentificationToken,
        @WebParam(name = "federatedId", targetNamespace = "")
        String federatedId)
        throws IMSServiceSecurityCallNotAllowedException_Exception, InvalidImsServiceMethodArgumentException_Exception, LdapTemplateNotReadyException_Exception, RequestedEntryNotExistsException_Exception, SecuredImsException_Exception, UnexpectedLdapResponseException_Exception, UnexpectedRuntimeImsException_Exception
    ;

    /**
     * 
     * @param authentificationToken
     * @param federatedId
     * @param callerFid
     * @return
     *     returns boolean
     * @throws SecuredImsException_Exception
     * @throws UnexpectedLdapResponseException_Exception
     * @throws IMSServiceSecurityCallNotAllowedException_Exception
     * @throws RequestedEntryNotExistsException_Exception
     * @throws LdapTemplateNotReadyException_Exception
     * @throws InvalidImsServiceMethodArgumentException_Exception
     * @throws UnexpectedRuntimeImsException_Exception
     */
    @WebMethod
    @WebResult(name = "success", targetNamespace = "")
    @RequestWrapper(localName = "acceptCompanyMerge", targetNamespace = "http://uimsv2.service.ims.schneider.com/", className = "com.uims.companymanager.AcceptCompanyMerge")
    @ResponseWrapper(localName = "acceptCompanyMergeResponse", targetNamespace = "http://uimsv2.service.ims.schneider.com/", className = "com.uims.companymanager.AcceptCompanyMergeResponse")
    public boolean acceptCompanyMerge(
        @WebParam(name = "callerFid", targetNamespace = "")
        String callerFid,
        @WebParam(name = "authentificationToken", targetNamespace = "")
        String authentificationToken,
        @WebParam(name = "federatedId", targetNamespace = "")
        String federatedId)
        throws IMSServiceSecurityCallNotAllowedException_Exception, InvalidImsServiceMethodArgumentException_Exception, LdapTemplateNotReadyException_Exception, RequestedEntryNotExistsException_Exception, SecuredImsException_Exception, UnexpectedLdapResponseException_Exception, UnexpectedRuntimeImsException_Exception
    ;

    /**
     * 
     * @param samlAssertion
     * @param company
     * @param callerFid
     * @return
     *     returns java.lang.String
     * @throws SecuredImsException_Exception
     * @throws UnexpectedLdapResponseException_Exception
     * @throws IMSServiceSecurityCallNotAllowedException_Exception
     * @throws RequestedEntryNotExistsException_Exception
     * @throws LdapTemplateNotReadyException_Exception
     * @throws UnexpectedRuntimeImsException_Exception
     * @throws InvalidImsServiceMethodArgumentException_Exception
     * @throws RequestedInternalUserException_Exception
     */
    @WebMethod
    @WebResult(name = "federatedId", targetNamespace = "")
    @RequestWrapper(localName = "createCompany", targetNamespace = "http://uimsv2.service.ims.schneider.com/", className = "com.uims.companymanager.CreateCompany")
    @ResponseWrapper(localName = "createCompanyResponse", targetNamespace = "http://uimsv2.service.ims.schneider.com/", className = "com.uims.companymanager.CreateCompanyResponse")
    public String createCompany(
        @WebParam(name = "callerFid", targetNamespace = "")
        String callerFid,
        @WebParam(name = "samlAssertion", targetNamespace = "")
        String samlAssertion,
        @WebParam(name = "company", targetNamespace = "")
        CompanyV3 company)
        throws IMSServiceSecurityCallNotAllowedException_Exception, InvalidImsServiceMethodArgumentException_Exception, LdapTemplateNotReadyException_Exception, RequestedEntryNotExistsException_Exception, RequestedInternalUserException_Exception, SecuredImsException_Exception, UnexpectedLdapResponseException_Exception, UnexpectedRuntimeImsException_Exception
    ;

    /**
     * 
     * @param invitationUid
     * @param samlAssertionOrToken
     * @param callerFid
     * @return
     *     returns com.uims.companymanager.CompanyV3
     * @throws SecuredImsException_Exception
     * @throws UnexpectedLdapResponseException_Exception
     * @throws IMSServiceSecurityCallNotAllowedException_Exception
     * @throws RequestedEntryNotExistsException_Exception
     * @throws LdapTemplateNotReadyException_Exception
     * @throws UnexpectedRuntimeImsException_Exception
     * @throws InvalidImsServiceMethodArgumentException_Exception
     */
    @WebMethod
    @WebResult(name = "company", targetNamespace = "")
    @RequestWrapper(localName = "getCompanyByInvitationUid", targetNamespace = "http://uimsv2.service.ims.schneider.com/", className = "com.uims.companymanager.GetCompanyByInvitationUid")
    @ResponseWrapper(localName = "getCompanyByInvitationUidResponse", targetNamespace = "http://uimsv2.service.ims.schneider.com/", className = "com.uims.companymanager.GetCompanyByInvitationUidResponse")
    public CompanyV3 getCompanyByInvitationUid(
        @WebParam(name = "callerFid", targetNamespace = "")
        String callerFid,
        @WebParam(name = "samlAssertionOrToken", targetNamespace = "")
        String samlAssertionOrToken,
        @WebParam(name = "invitationUid", targetNamespace = "")
        String invitationUid)
        throws IMSServiceSecurityCallNotAllowedException_Exception, InvalidImsServiceMethodArgumentException_Exception, LdapTemplateNotReadyException_Exception, RequestedEntryNotExistsException_Exception, SecuredImsException_Exception, UnexpectedLdapResponseException_Exception, UnexpectedRuntimeImsException_Exception
    ;

    /**
     * 
     * @param template
     * @param samlAssertion
     * @param callerFid
     * @return
     *     returns java.util.List<com.uims.companymanager.CompanyV3>
     * @throws SecuredImsException_Exception
     * @throws IMSServiceSecurityCallNotAllowedException_Exception
     * @throws LdapTemplateNotReadyException_Exception
     * @throws UnexpectedRuntimeImsException_Exception
     * @throws InvalidImsServiceMethodArgumentException_Exception
     */
    @WebMethod
    @WebResult(name = "companies", targetNamespace = "")
    @RequestWrapper(localName = "searchCompany", targetNamespace = "http://uimsv2.service.ims.schneider.com/", className = "com.uims.companymanager.SearchCompany")
    @ResponseWrapper(localName = "searchCompanyResponse", targetNamespace = "http://uimsv2.service.ims.schneider.com/", className = "com.uims.companymanager.SearchCompanyResponse")
    public List<CompanyV3> searchCompany(
        @WebParam(name = "callerFid", targetNamespace = "")
        String callerFid,
        @WebParam(name = "samlAssertion", targetNamespace = "")
        String samlAssertion,
        @WebParam(name = "template", targetNamespace = "")
        CompanyV3 template)
        throws IMSServiceSecurityCallNotAllowedException_Exception, InvalidImsServiceMethodArgumentException_Exception, LdapTemplateNotReadyException_Exception, SecuredImsException_Exception, UnexpectedRuntimeImsException_Exception
    ;

    /**
     * 
     * @param samlAssertion
     * @param federatedId
     * @param company
     * @param callerFid
     * @return
     *     returns boolean
     * @throws SecuredImsException_Exception
     * @throws IMSServiceSecurityCallNotAllowedException_Exception
     * @throws UnexpectedLdapResponseException_Exception
     * @throws RequestedEntryNotExistsException_Exception
     * @throws LdapTemplateNotReadyException_Exception
     * @throws InvalidImsServiceMethodArgumentException_Exception
     * @throws UnexpectedRuntimeImsException_Exception
     */
    @WebMethod
    @WebResult(name = "success", targetNamespace = "")
    @RequestWrapper(localName = "updateCompany", targetNamespace = "http://uimsv2.service.ims.schneider.com/", className = "com.uims.companymanager.UpdateCompany")
    @ResponseWrapper(localName = "updateCompanyResponse", targetNamespace = "http://uimsv2.service.ims.schneider.com/", className = "com.uims.companymanager.UpdateCompanyResponse")
    public boolean updateCompany(
        @WebParam(name = "callerFid", targetNamespace = "")
        String callerFid,
        @WebParam(name = "samlAssertion", targetNamespace = "")
        String samlAssertion,
        @WebParam(name = "federatedId", targetNamespace = "")
        String federatedId,
        @WebParam(name = "company", targetNamespace = "")
        CompanyV3 company)
        throws IMSServiceSecurityCallNotAllowedException_Exception, InvalidImsServiceMethodArgumentException_Exception, LdapTemplateNotReadyException_Exception, RequestedEntryNotExistsException_Exception, SecuredImsException_Exception, UnexpectedLdapResponseException_Exception, UnexpectedRuntimeImsException_Exception
    ;

    /**
     * 
     * @param samlAssertion
     * @param callerFid
     * @return
     *     returns boolean
     * @throws SecuredImsException_Exception
     * @throws IMSServiceSecurityCallNotAllowedException_Exception
     * @throws UnexpectedLdapResponseException_Exception
     * @throws RequestedEntryNotExistsException_Exception
     * @throws LdapTemplateNotReadyException_Exception
     * @throws UnexpectedRuntimeImsException_Exception
     * @throws InvalidImsServiceMethodArgumentException_Exception
     * @throws RequestedInternalUserException_Exception
     */
    @WebMethod
    @WebResult(name = "success", targetNamespace = "")
    @RequestWrapper(localName = "removePrimaryContact", targetNamespace = "http://uimsv2.service.ims.schneider.com/", className = "com.uims.companymanager.RemovePrimaryContact")
    @ResponseWrapper(localName = "removePrimaryContactResponse", targetNamespace = "http://uimsv2.service.ims.schneider.com/", className = "com.uims.companymanager.RemovePrimaryContactResponse")
    public boolean removePrimaryContact(
        @WebParam(name = "callerFid", targetNamespace = "")
        String callerFid,
        @WebParam(name = "samlAssertion", targetNamespace = "")
        String samlAssertion)
        throws IMSServiceSecurityCallNotAllowedException_Exception, InvalidImsServiceMethodArgumentException_Exception, LdapTemplateNotReadyException_Exception, RequestedEntryNotExistsException_Exception, RequestedInternalUserException_Exception, SecuredImsException_Exception, UnexpectedLdapResponseException_Exception, UnexpectedRuntimeImsException_Exception
    ;

}
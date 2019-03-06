
package com.uims.accessmanager;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.uims.accessmanager package. 
 * <p>An ObjectFactory allows you to programatically 
 * construct new instances of the Java representation 
 * for XML content. The Java representation of XML 
 * content can consist of schema derived interfaces 
 * and classes representing the binding of schema 
 * type definitions, element declarations and model 
 * groups.  Factory methods for each of these are 
 * provided in this class.
 * 
 */
@XmlRegistry
public class ObjectFactory {

    private final static QName _IMSServiceSecurityCallNotAllowedException_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "IMSServiceSecurityCallNotAllowedException");
    private final static QName _IMSBadApplicationException_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "IMSBadApplicationException");
    private final static QName _SecuredImsException_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "SecuredImsException");
    private final static QName _LdapTemplateNotReadyException_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "LdapTemplateNotReadyException");
    private final static QName _GrantAccessControlToUserResponse_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "grantAccessControlToUserResponse");
    private final static QName _InactiveUserImsException_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "InactiveUserImsException");
    private final static QName _UnexpectedLdapResponseException_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "UnexpectedLdapResponseException");
    private final static QName _RequestedEntryNotExistsException_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "RequestedEntryNotExistsException");
    private final static QName _GrantAccessControlToUser_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "grantAccessControlToUser");
    private final static QName _UnexpectedRuntimeImsException_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "UnexpectedRuntimeImsException");
    private final static QName _Access_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "access");
    private final static QName _GetAccessControlByUser_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "getAccessControlByUser");
    private final static QName _RevokeAccessControlToUserResponse_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "revokeAccessControlToUserResponse");
    private final static QName _RevokeAccessControlToUser_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "revokeAccessControlToUser");
    private final static QName _InvalidImsServiceMethodArgumentException_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "InvalidImsServiceMethodArgumentException");
    private final static QName _GetAccessControlByUserResponse_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "getAccessControlByUserResponse");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.uims.accessmanager
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link AccessElement }
     * 
     */
    public AccessElement createAccessElement() {
        return new AccessElement();
    }

    /**
     * Create an instance of {@link AccessTree }
     * 
     */
    public AccessTree createAccessTree() {
        return new AccessTree();
    }

    /**
     * Create an instance of {@link GetAccessControlByUser }
     * 
     */
    public GetAccessControlByUser createGetAccessControlByUser() {
        return new GetAccessControlByUser();
    }

    /**
     * Create an instance of {@link UnexpectedRuntimeImsException }
     * 
     */
    public UnexpectedRuntimeImsException createUnexpectedRuntimeImsException() {
        return new UnexpectedRuntimeImsException();
    }

    /**
     * Create an instance of {@link UnexpectedLdapResponseException }
     * 
     */
    public UnexpectedLdapResponseException createUnexpectedLdapResponseException() {
        return new UnexpectedLdapResponseException();
    }

    /**
     * Create an instance of {@link RequestedEntryNotExistsException }
     * 
     */
    public RequestedEntryNotExistsException createRequestedEntryNotExistsException() {
        return new RequestedEntryNotExistsException();
    }

    /**
     * Create an instance of {@link GrantAccessControlToUser }
     * 
     */
    public GrantAccessControlToUser createGrantAccessControlToUser() {
        return new GrantAccessControlToUser();
    }

    /**
     * Create an instance of {@link GetAccessControlByUserResponse }
     * 
     */
    public GetAccessControlByUserResponse createGetAccessControlByUserResponse() {
        return new GetAccessControlByUserResponse();
    }

    /**
     * Create an instance of {@link RevokeAccessControlToUser }
     * 
     */
    public RevokeAccessControlToUser createRevokeAccessControlToUser() {
        return new RevokeAccessControlToUser();
    }

    /**
     * Create an instance of {@link InvalidImsServiceMethodArgumentException }
     * 
     */
    public InvalidImsServiceMethodArgumentException createInvalidImsServiceMethodArgumentException() {
        return new InvalidImsServiceMethodArgumentException();
    }

    /**
     * Create an instance of {@link RevokeAccessControlToUserResponse }
     * 
     */
    public RevokeAccessControlToUserResponse createRevokeAccessControlToUserResponse() {
        return new RevokeAccessControlToUserResponse();
    }

    /**
     * Create an instance of {@link IMSServiceSecurityCallNotAllowedException }
     * 
     */
    public IMSServiceSecurityCallNotAllowedException createIMSServiceSecurityCallNotAllowedException() {
        return new IMSServiceSecurityCallNotAllowedException();
    }

    /**
     * Create an instance of {@link IMSBadApplicationException }
     * 
     */
    public IMSBadApplicationException createIMSBadApplicationException() {
        return new IMSBadApplicationException();
    }

    /**
     * Create an instance of {@link GrantAccessControlToUserResponse }
     * 
     */
    public GrantAccessControlToUserResponse createGrantAccessControlToUserResponse() {
        return new GrantAccessControlToUserResponse();
    }

    /**
     * Create an instance of {@link InactiveUserImsException }
     * 
     */
    public InactiveUserImsException createInactiveUserImsException() {
        return new InactiveUserImsException();
    }

    /**
     * Create an instance of {@link LdapTemplateNotReadyException }
     * 
     */
    public LdapTemplateNotReadyException createLdapTemplateNotReadyException() {
        return new LdapTemplateNotReadyException();
    }

    /**
     * Create an instance of {@link SecuredImsException }
     * 
     */
    public SecuredImsException createSecuredImsException() {
        return new SecuredImsException();
    }

    /**
     * Create an instance of {@link AccessElement.Childs }
     * 
     */
    public AccessElement.Childs createAccessElementChilds() {
        return new AccessElement.Childs();
    }

    /**
     * Create an instance of {@link AccessTree.AccessList }
     * 
     */
    public AccessTree.AccessList createAccessTreeAccessList() {
        return new AccessTree.AccessList();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link IMSServiceSecurityCallNotAllowedException }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "IMSServiceSecurityCallNotAllowedException")
    public JAXBElement<IMSServiceSecurityCallNotAllowedException> createIMSServiceSecurityCallNotAllowedException(IMSServiceSecurityCallNotAllowedException value) {
        return new JAXBElement<IMSServiceSecurityCallNotAllowedException>(_IMSServiceSecurityCallNotAllowedException_QNAME, IMSServiceSecurityCallNotAllowedException.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link IMSBadApplicationException }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "IMSBadApplicationException")
    public JAXBElement<IMSBadApplicationException> createIMSBadApplicationException(IMSBadApplicationException value) {
        return new JAXBElement<IMSBadApplicationException>(_IMSBadApplicationException_QNAME, IMSBadApplicationException.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link SecuredImsException }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "SecuredImsException")
    public JAXBElement<SecuredImsException> createSecuredImsException(SecuredImsException value) {
        return new JAXBElement<SecuredImsException>(_SecuredImsException_QNAME, SecuredImsException.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link LdapTemplateNotReadyException }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "LdapTemplateNotReadyException")
    public JAXBElement<LdapTemplateNotReadyException> createLdapTemplateNotReadyException(LdapTemplateNotReadyException value) {
        return new JAXBElement<LdapTemplateNotReadyException>(_LdapTemplateNotReadyException_QNAME, LdapTemplateNotReadyException.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link GrantAccessControlToUserResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "grantAccessControlToUserResponse")
    public JAXBElement<GrantAccessControlToUserResponse> createGrantAccessControlToUserResponse(GrantAccessControlToUserResponse value) {
        return new JAXBElement<GrantAccessControlToUserResponse>(_GrantAccessControlToUserResponse_QNAME, GrantAccessControlToUserResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link InactiveUserImsException }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "InactiveUserImsException")
    public JAXBElement<InactiveUserImsException> createInactiveUserImsException(InactiveUserImsException value) {
        return new JAXBElement<InactiveUserImsException>(_InactiveUserImsException_QNAME, InactiveUserImsException.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link UnexpectedLdapResponseException }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "UnexpectedLdapResponseException")
    public JAXBElement<UnexpectedLdapResponseException> createUnexpectedLdapResponseException(UnexpectedLdapResponseException value) {
        return new JAXBElement<UnexpectedLdapResponseException>(_UnexpectedLdapResponseException_QNAME, UnexpectedLdapResponseException.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RequestedEntryNotExistsException }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "RequestedEntryNotExistsException")
    public JAXBElement<RequestedEntryNotExistsException> createRequestedEntryNotExistsException(RequestedEntryNotExistsException value) {
        return new JAXBElement<RequestedEntryNotExistsException>(_RequestedEntryNotExistsException_QNAME, RequestedEntryNotExistsException.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link GrantAccessControlToUser }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "grantAccessControlToUser")
    public JAXBElement<GrantAccessControlToUser> createGrantAccessControlToUser(GrantAccessControlToUser value) {
        return new JAXBElement<GrantAccessControlToUser>(_GrantAccessControlToUser_QNAME, GrantAccessControlToUser.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link UnexpectedRuntimeImsException }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "UnexpectedRuntimeImsException")
    public JAXBElement<UnexpectedRuntimeImsException> createUnexpectedRuntimeImsException(UnexpectedRuntimeImsException value) {
        return new JAXBElement<UnexpectedRuntimeImsException>(_UnexpectedRuntimeImsException_QNAME, UnexpectedRuntimeImsException.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link Object }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "access")
    public JAXBElement<Object> createAccess(Object value) {
        return new JAXBElement<Object>(_Access_QNAME, Object.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link GetAccessControlByUser }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "getAccessControlByUser")
    public JAXBElement<GetAccessControlByUser> createGetAccessControlByUser(GetAccessControlByUser value) {
        return new JAXBElement<GetAccessControlByUser>(_GetAccessControlByUser_QNAME, GetAccessControlByUser.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RevokeAccessControlToUserResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "revokeAccessControlToUserResponse")
    public JAXBElement<RevokeAccessControlToUserResponse> createRevokeAccessControlToUserResponse(RevokeAccessControlToUserResponse value) {
        return new JAXBElement<RevokeAccessControlToUserResponse>(_RevokeAccessControlToUserResponse_QNAME, RevokeAccessControlToUserResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RevokeAccessControlToUser }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "revokeAccessControlToUser")
    public JAXBElement<RevokeAccessControlToUser> createRevokeAccessControlToUser(RevokeAccessControlToUser value) {
        return new JAXBElement<RevokeAccessControlToUser>(_RevokeAccessControlToUser_QNAME, RevokeAccessControlToUser.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link InvalidImsServiceMethodArgumentException }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "InvalidImsServiceMethodArgumentException")
    public JAXBElement<InvalidImsServiceMethodArgumentException> createInvalidImsServiceMethodArgumentException(InvalidImsServiceMethodArgumentException value) {
        return new JAXBElement<InvalidImsServiceMethodArgumentException>(_InvalidImsServiceMethodArgumentException_QNAME, InvalidImsServiceMethodArgumentException.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link GetAccessControlByUserResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "getAccessControlByUserResponse")
    public JAXBElement<GetAccessControlByUserResponse> createGetAccessControlByUserResponse(GetAccessControlByUserResponse value) {
        return new JAXBElement<GetAccessControlByUserResponse>(_GetAccessControlByUserResponse_QNAME, GetAccessControlByUserResponse.class, null, value);
    }

}

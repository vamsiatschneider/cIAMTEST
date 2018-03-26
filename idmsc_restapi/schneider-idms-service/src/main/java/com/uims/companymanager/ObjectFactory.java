
package com.uims.companymanager;

import javax.xml.bind.JAXBElement;
import javax.xml.bind.annotation.XmlElementDecl;
import javax.xml.bind.annotation.XmlRegistry;
import javax.xml.namespace.QName;


/**
 * This object contains factory methods for each 
 * Java content interface and Java element interface 
 * generated in the com.uims.companymanager package. 
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

    private final static QName _GetCompanyByGoldenId_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "getCompanyByGoldenId");
    private final static QName _SearchCompanyResponse_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "searchCompanyResponse");
    private final static QName _RequestedInternalUserException_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "RequestedInternalUserException");
    private final static QName _SecuredImsException_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "SecuredImsException");
    private final static QName _GetCompanyByInvitationUid_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "getCompanyByInvitationUid");
    private final static QName _UpdateCompanyResponse_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "updateCompanyResponse");
    private final static QName _GetCompanyByGoldenIdResponse_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "getCompanyByGoldenIdResponse");
    private final static QName _LdapTemplateNotReadyException_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "LdapTemplateNotReadyException");
    private final static QName _AcceptCompanyMergeResponse_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "acceptCompanyMergeResponse");
    private final static QName _UnexpectedRuntimeImsException_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "UnexpectedRuntimeImsException");
    private final static QName _RejectCompanyMergeResponse_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "rejectCompanyMergeResponse");
    private final static QName _InvalidImsServiceMethodArgumentException_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "InvalidImsServiceMethodArgumentException");
    private final static QName _UpdateCompany_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "updateCompany");
    private final static QName _RemovePrimaryContact_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "removePrimaryContact");
    private final static QName _RemovePrimaryContactResponse_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "removePrimaryContactResponse");
    private final static QName _IMSServiceSecurityCallNotAllowedException_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "IMSServiceSecurityCallNotAllowedException");
    private final static QName _CreateCompanyResponse_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "createCompanyResponse");
    private final static QName _GetCompany_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "getCompany");
    private final static QName _AcceptCompanyMerge_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "acceptCompanyMerge");
    private final static QName _RequestedEntryNotExistsException_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "RequestedEntryNotExistsException");
    private final static QName _UnexpectedLdapResponseException_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "UnexpectedLdapResponseException");
    private final static QName _GetCompanyByInvitationUidResponse_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "getCompanyByInvitationUidResponse");
    private final static QName _GetCompanyResponse_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "getCompanyResponse");
    private final static QName _RejectCompanyMerge_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "rejectCompanyMerge");
    private final static QName _CreateCompany_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "createCompany");
    private final static QName _SearchCompany_QNAME = new QName("http://uimsv2.service.ims.schneider.com/", "searchCompany");

    /**
     * Create a new ObjectFactory that can be used to create new instances of schema derived classes for package: com.uims.companymanager
     * 
     */
    public ObjectFactory() {
    }

    /**
     * Create an instance of {@link UnexpectedRuntimeImsException }
     * 
     */
    public UnexpectedRuntimeImsException createUnexpectedRuntimeImsException() {
        return new UnexpectedRuntimeImsException();
    }

    /**
     * Create an instance of {@link AcceptCompanyMergeResponse }
     * 
     */
    public AcceptCompanyMergeResponse createAcceptCompanyMergeResponse() {
        return new AcceptCompanyMergeResponse();
    }

    /**
     * Create an instance of {@link UpdateCompany }
     * 
     */
    public UpdateCompany createUpdateCompany() {
        return new UpdateCompany();
    }

    /**
     * Create an instance of {@link RemovePrimaryContact }
     * 
     */
    public RemovePrimaryContact createRemovePrimaryContact() {
        return new RemovePrimaryContact();
    }

    /**
     * Create an instance of {@link RemovePrimaryContactResponse }
     * 
     */
    public RemovePrimaryContactResponse createRemovePrimaryContactResponse() {
        return new RemovePrimaryContactResponse();
    }

    /**
     * Create an instance of {@link RejectCompanyMergeResponse }
     * 
     */
    public RejectCompanyMergeResponse createRejectCompanyMergeResponse() {
        return new RejectCompanyMergeResponse();
    }

    /**
     * Create an instance of {@link InvalidImsServiceMethodArgumentException }
     * 
     */
    public InvalidImsServiceMethodArgumentException createInvalidImsServiceMethodArgumentException() {
        return new InvalidImsServiceMethodArgumentException();
    }

    /**
     * Create an instance of {@link RequestedInternalUserException }
     * 
     */
    public RequestedInternalUserException createRequestedInternalUserException() {
        return new RequestedInternalUserException();
    }

    /**
     * Create an instance of {@link GetCompanyByGoldenId }
     * 
     */
    public GetCompanyByGoldenId createGetCompanyByGoldenId() {
        return new GetCompanyByGoldenId();
    }

    /**
     * Create an instance of {@link SearchCompanyResponse }
     * 
     */
    public SearchCompanyResponse createSearchCompanyResponse() {
        return new SearchCompanyResponse();
    }

    /**
     * Create an instance of {@link LdapTemplateNotReadyException }
     * 
     */
    public LdapTemplateNotReadyException createLdapTemplateNotReadyException() {
        return new LdapTemplateNotReadyException();
    }

    /**
     * Create an instance of {@link GetCompanyByInvitationUid }
     * 
     */
    public GetCompanyByInvitationUid createGetCompanyByInvitationUid() {
        return new GetCompanyByInvitationUid();
    }

    /**
     * Create an instance of {@link UpdateCompanyResponse }
     * 
     */
    public UpdateCompanyResponse createUpdateCompanyResponse() {
        return new UpdateCompanyResponse();
    }

    /**
     * Create an instance of {@link GetCompanyByGoldenIdResponse }
     * 
     */
    public GetCompanyByGoldenIdResponse createGetCompanyByGoldenIdResponse() {
        return new GetCompanyByGoldenIdResponse();
    }

    /**
     * Create an instance of {@link SecuredImsException }
     * 
     */
    public SecuredImsException createSecuredImsException() {
        return new SecuredImsException();
    }

    /**
     * Create an instance of {@link GetCompanyByInvitationUidResponse }
     * 
     */
    public GetCompanyByInvitationUidResponse createGetCompanyByInvitationUidResponse() {
        return new GetCompanyByInvitationUidResponse();
    }

    /**
     * Create an instance of {@link GetCompanyResponse }
     * 
     */
    public GetCompanyResponse createGetCompanyResponse() {
        return new GetCompanyResponse();
    }

    /**
     * Create an instance of {@link RejectCompanyMerge }
     * 
     */
    public RejectCompanyMerge createRejectCompanyMerge() {
        return new RejectCompanyMerge();
    }

    /**
     * Create an instance of {@link RequestedEntryNotExistsException }
     * 
     */
    public RequestedEntryNotExistsException createRequestedEntryNotExistsException() {
        return new RequestedEntryNotExistsException();
    }

    /**
     * Create an instance of {@link UnexpectedLdapResponseException }
     * 
     */
    public UnexpectedLdapResponseException createUnexpectedLdapResponseException() {
        return new UnexpectedLdapResponseException();
    }

    /**
     * Create an instance of {@link SearchCompany }
     * 
     */
    public SearchCompany createSearchCompany() {
        return new SearchCompany();
    }

    /**
     * Create an instance of {@link CreateCompany }
     * 
     */
    public CreateCompany createCreateCompany() {
        return new CreateCompany();
    }

    /**
     * Create an instance of {@link GetCompany }
     * 
     */
    public GetCompany createGetCompany() {
        return new GetCompany();
    }

    /**
     * Create an instance of {@link CreateCompanyResponse }
     * 
     */
    public CreateCompanyResponse createCreateCompanyResponse() {
        return new CreateCompanyResponse();
    }

    /**
     * Create an instance of {@link IMSServiceSecurityCallNotAllowedException }
     * 
     */
    public IMSServiceSecurityCallNotAllowedException createIMSServiceSecurityCallNotAllowedException() {
        return new IMSServiceSecurityCallNotAllowedException();
    }

    /**
     * Create an instance of {@link AcceptCompanyMerge }
     * 
     */
    public AcceptCompanyMerge createAcceptCompanyMerge() {
        return new AcceptCompanyMerge();
    }

    /**
     * Create an instance of {@link CompanyV3 }
     * 
     */
    public CompanyV3 createCompanyV3() {
        return new CompanyV3();
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link GetCompanyByGoldenId }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "getCompanyByGoldenId")
    public JAXBElement<GetCompanyByGoldenId> createGetCompanyByGoldenId(GetCompanyByGoldenId value) {
        return new JAXBElement<GetCompanyByGoldenId>(_GetCompanyByGoldenId_QNAME, GetCompanyByGoldenId.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link SearchCompanyResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "searchCompanyResponse")
    public JAXBElement<SearchCompanyResponse> createSearchCompanyResponse(SearchCompanyResponse value) {
        return new JAXBElement<SearchCompanyResponse>(_SearchCompanyResponse_QNAME, SearchCompanyResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RequestedInternalUserException }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "RequestedInternalUserException")
    public JAXBElement<RequestedInternalUserException> createRequestedInternalUserException(RequestedInternalUserException value) {
        return new JAXBElement<RequestedInternalUserException>(_RequestedInternalUserException_QNAME, RequestedInternalUserException.class, null, value);
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
     * Create an instance of {@link JAXBElement }{@code <}{@link GetCompanyByInvitationUid }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "getCompanyByInvitationUid")
    public JAXBElement<GetCompanyByInvitationUid> createGetCompanyByInvitationUid(GetCompanyByInvitationUid value) {
        return new JAXBElement<GetCompanyByInvitationUid>(_GetCompanyByInvitationUid_QNAME, GetCompanyByInvitationUid.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link UpdateCompanyResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "updateCompanyResponse")
    public JAXBElement<UpdateCompanyResponse> createUpdateCompanyResponse(UpdateCompanyResponse value) {
        return new JAXBElement<UpdateCompanyResponse>(_UpdateCompanyResponse_QNAME, UpdateCompanyResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link GetCompanyByGoldenIdResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "getCompanyByGoldenIdResponse")
    public JAXBElement<GetCompanyByGoldenIdResponse> createGetCompanyByGoldenIdResponse(GetCompanyByGoldenIdResponse value) {
        return new JAXBElement<GetCompanyByGoldenIdResponse>(_GetCompanyByGoldenIdResponse_QNAME, GetCompanyByGoldenIdResponse.class, null, value);
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
     * Create an instance of {@link JAXBElement }{@code <}{@link AcceptCompanyMergeResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "acceptCompanyMergeResponse")
    public JAXBElement<AcceptCompanyMergeResponse> createAcceptCompanyMergeResponse(AcceptCompanyMergeResponse value) {
        return new JAXBElement<AcceptCompanyMergeResponse>(_AcceptCompanyMergeResponse_QNAME, AcceptCompanyMergeResponse.class, null, value);
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
     * Create an instance of {@link JAXBElement }{@code <}{@link RejectCompanyMergeResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "rejectCompanyMergeResponse")
    public JAXBElement<RejectCompanyMergeResponse> createRejectCompanyMergeResponse(RejectCompanyMergeResponse value) {
        return new JAXBElement<RejectCompanyMergeResponse>(_RejectCompanyMergeResponse_QNAME, RejectCompanyMergeResponse.class, null, value);
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
     * Create an instance of {@link JAXBElement }{@code <}{@link UpdateCompany }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "updateCompany")
    public JAXBElement<UpdateCompany> createUpdateCompany(UpdateCompany value) {
        return new JAXBElement<UpdateCompany>(_UpdateCompany_QNAME, UpdateCompany.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RemovePrimaryContact }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "removePrimaryContact")
    public JAXBElement<RemovePrimaryContact> createRemovePrimaryContact(RemovePrimaryContact value) {
        return new JAXBElement<RemovePrimaryContact>(_RemovePrimaryContact_QNAME, RemovePrimaryContact.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RemovePrimaryContactResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "removePrimaryContactResponse")
    public JAXBElement<RemovePrimaryContactResponse> createRemovePrimaryContactResponse(RemovePrimaryContactResponse value) {
        return new JAXBElement<RemovePrimaryContactResponse>(_RemovePrimaryContactResponse_QNAME, RemovePrimaryContactResponse.class, null, value);
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
     * Create an instance of {@link JAXBElement }{@code <}{@link CreateCompanyResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "createCompanyResponse")
    public JAXBElement<CreateCompanyResponse> createCreateCompanyResponse(CreateCompanyResponse value) {
        return new JAXBElement<CreateCompanyResponse>(_CreateCompanyResponse_QNAME, CreateCompanyResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link GetCompany }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "getCompany")
    public JAXBElement<GetCompany> createGetCompany(GetCompany value) {
        return new JAXBElement<GetCompany>(_GetCompany_QNAME, GetCompany.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link AcceptCompanyMerge }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "acceptCompanyMerge")
    public JAXBElement<AcceptCompanyMerge> createAcceptCompanyMerge(AcceptCompanyMerge value) {
        return new JAXBElement<AcceptCompanyMerge>(_AcceptCompanyMerge_QNAME, AcceptCompanyMerge.class, null, value);
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
     * Create an instance of {@link JAXBElement }{@code <}{@link UnexpectedLdapResponseException }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "UnexpectedLdapResponseException")
    public JAXBElement<UnexpectedLdapResponseException> createUnexpectedLdapResponseException(UnexpectedLdapResponseException value) {
        return new JAXBElement<UnexpectedLdapResponseException>(_UnexpectedLdapResponseException_QNAME, UnexpectedLdapResponseException.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link GetCompanyByInvitationUidResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "getCompanyByInvitationUidResponse")
    public JAXBElement<GetCompanyByInvitationUidResponse> createGetCompanyByInvitationUidResponse(GetCompanyByInvitationUidResponse value) {
        return new JAXBElement<GetCompanyByInvitationUidResponse>(_GetCompanyByInvitationUidResponse_QNAME, GetCompanyByInvitationUidResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link GetCompanyResponse }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "getCompanyResponse")
    public JAXBElement<GetCompanyResponse> createGetCompanyResponse(GetCompanyResponse value) {
        return new JAXBElement<GetCompanyResponse>(_GetCompanyResponse_QNAME, GetCompanyResponse.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link RejectCompanyMerge }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "rejectCompanyMerge")
    public JAXBElement<RejectCompanyMerge> createRejectCompanyMerge(RejectCompanyMerge value) {
        return new JAXBElement<RejectCompanyMerge>(_RejectCompanyMerge_QNAME, RejectCompanyMerge.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link CreateCompany }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "createCompany")
    public JAXBElement<CreateCompany> createCreateCompany(CreateCompany value) {
        return new JAXBElement<CreateCompany>(_CreateCompany_QNAME, CreateCompany.class, null, value);
    }

    /**
     * Create an instance of {@link JAXBElement }{@code <}{@link SearchCompany }{@code >}}
     * 
     */
    @XmlElementDecl(namespace = "http://uimsv2.service.ims.schneider.com/", name = "searchCompany")
    public JAXBElement<SearchCompany> createSearchCompany(SearchCompany value) {
        return new JAXBElement<SearchCompany>(_SearchCompany_QNAME, SearchCompany.class, null, value);
    }

}

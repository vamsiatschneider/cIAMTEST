
package com.uims.authenticatedUsermanager;

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
@WebService(name = "AuthenticatedUserManager_UIMSV22", targetNamespace = "http://uimsv22.service.ims.schneider.com/")
@XmlSeeAlso({
    ObjectFactory.class
})
public interface AuthenticatedUserManagerUIMSV22 {


    /**
     * 
     * @param application
     * @param identity
     * @param callerFid
     * @return
     *     returns com.uims.authenticatedUsermanager.CreatedIdentityReport
     * @throws SecuredImsException_Exception
     * @throws InvalidImsServiceMethodArgumentException_Exception
     * @throws LdapTemplateNotReadyException_Exception
     * @throws ImsMailerException_Exception
     * @throws RequestedInternalUserException_Exception
     * @throws IMSServiceSecurityCallNotAllowedException_Exception
     * @throws UnexpectedRuntimeImsException_Exception
     * @throws RequestedEntryNotExistsException_Exception
     * @throws UnexpectedLdapResponseException_Exception
     */
    @WebMethod
    @WebResult(targetNamespace = "")
    @RequestWrapper(localName = "createIdentityWithPhoneId", targetNamespace = "http://uimsv22.service.ims.schneider.com/", className = "com.uims.authenticatedUsermanager.CreateIdentityWithPhoneId")
    @ResponseWrapper(localName = "createIdentityWithPhoneIdResponse", targetNamespace = "http://uimsv22.service.ims.schneider.com/", className = "com.uims.authenticatedUsermanager.CreateIdentityWithPhoneIdResponse")
    public CreatedIdentityReport createIdentityWithPhoneId(
        @WebParam(name = "callerFid", targetNamespace = "")
        String callerFid,
        @WebParam(name = "identity", targetNamespace = "")
        UserV6 identity,
        @WebParam(name = "application", targetNamespace = "")
        AccessElement application)
        throws IMSServiceSecurityCallNotAllowedException_Exception, ImsMailerException_Exception, InvalidImsServiceMethodArgumentException_Exception, LdapTemplateNotReadyException_Exception, RequestedEntryNotExistsException_Exception, RequestedInternalUserException_Exception, SecuredImsException_Exception, UnexpectedLdapResponseException_Exception, UnexpectedRuntimeImsException_Exception
    ;

    /**
     * 
     * @param password
     * @param identity
     * @param forcedFederatedId
     * @param callerFid
     * @return
     *     returns com.uims.authenticatedUsermanager.CreatedIdentityReport
     * @throws SecuredImsException_Exception
     * @throws InvalidImsServiceMethodArgumentException_Exception
     * @throws ImsMailerException_Exception
     * @throws LdapTemplateNotReadyException_Exception
     * @throws RequestedInternalUserException_Exception
     * @throws UnexpectedRuntimeImsException_Exception
     * @throws IMSServiceSecurityCallNotAllowedException_Exception
     * @throws ForcedFidAlreadyExistException_Exception
     * @throws RequestedEntryNotExistsException_Exception
     * @throws UnexpectedLdapResponseException_Exception
     */
    @WebMethod
    @WebResult(targetNamespace = "")
    @RequestWrapper(localName = "createIdentityWithMobileWithPasswordForceIdmsId", targetNamespace = "http://uimsv22.service.ims.schneider.com/", className = "com.uims.authenticatedUsermanager.CreateIdentityWithMobileWithPasswordForceIdmsId")
    @ResponseWrapper(localName = "createIdentityWithMobileWithPasswordForceIdmsIdResponse", targetNamespace = "http://uimsv22.service.ims.schneider.com/", className = "com.uims.authenticatedUsermanager.CreateIdentityWithMobileWithPasswordForceIdmsIdResponse")
    public CreatedIdentityReport createIdentityWithMobileWithPasswordForceIdmsId(
        @WebParam(name = "callerFid", targetNamespace = "")
        String callerFid,
        @WebParam(name = "identity", targetNamespace = "")
        UserV6 identity,
        @WebParam(name = "password", targetNamespace = "")
        String password,
        @WebParam(name = "forcedFederatedId", targetNamespace = "")
        String forcedFederatedId)
        throws ForcedFidAlreadyExistException_Exception, IMSServiceSecurityCallNotAllowedException_Exception, ImsMailerException_Exception, InvalidImsServiceMethodArgumentException_Exception, LdapTemplateNotReadyException_Exception, RequestedEntryNotExistsException_Exception, RequestedInternalUserException_Exception, SecuredImsException_Exception, UnexpectedLdapResponseException_Exception, UnexpectedRuntimeImsException_Exception
    ;

    /**
     * 
     * @param application
     * @param identity
     * @param callerFid
     * @return
     *     returns com.uims.authenticatedUsermanager.CreatedIdentityReport
     * @throws SecuredImsException_Exception
     * @throws InvalidImsServiceMethodArgumentException_Exception
     * @throws ImsMailerException_Exception
     * @throws LdapTemplateNotReadyException_Exception
     * @throws RequestedInternalUserException_Exception
     * @throws UnexpectedRuntimeImsException_Exception
     * @throws IMSServiceSecurityCallNotAllowedException_Exception
     * @throws RequestedEntryNotExistsException_Exception
     * @throws UnexpectedLdapResponseException_Exception
     */
    @WebMethod
    @WebResult(targetNamespace = "")
    @RequestWrapper(localName = "createIdentity", targetNamespace = "http://uimsv22.service.ims.schneider.com/", className = "com.uims.authenticatedUsermanager.CreateIdentity")
    @ResponseWrapper(localName = "createIdentityResponse", targetNamespace = "http://uimsv22.service.ims.schneider.com/", className = "com.uims.authenticatedUsermanager.CreateIdentityResponse")
    public CreatedIdentityReport createIdentity(
        @WebParam(name = "callerFid", targetNamespace = "")
        String callerFid,
        @WebParam(name = "identity", targetNamespace = "")
        UserV6 identity,
        @WebParam(name = "application", targetNamespace = "")
        AccessElement application)
        throws IMSServiceSecurityCallNotAllowedException_Exception, ImsMailerException_Exception, InvalidImsServiceMethodArgumentException_Exception, LdapTemplateNotReadyException_Exception, RequestedEntryNotExistsException_Exception, RequestedInternalUserException_Exception, SecuredImsException_Exception, UnexpectedLdapResponseException_Exception, UnexpectedRuntimeImsException_Exception
    ;

    /**
     * 
     * @param password
     * @param identity
     * @param forcedFederatedId
     * @param callerFid
     * @return
     *     returns com.uims.authenticatedUsermanager.CreatedIdentityReport
     * @throws SecuredImsException_Exception
     * @throws InvalidImsServiceMethodArgumentException_Exception
     * @throws ImsMailerException_Exception
     * @throws LdapTemplateNotReadyException_Exception
     * @throws RequestedInternalUserException_Exception
     * @throws ForcedFidAlreadyExistException_Exception
     * @throws UnexpectedRuntimeImsException_Exception
     * @throws IMSServiceSecurityCallNotAllowedException_Exception
     * @throws RequestedEntryNotExistsException_Exception
     * @throws UnexpectedLdapResponseException_Exception
     */
    @WebMethod
    @WebResult(targetNamespace = "")
    @RequestWrapper(localName = "createIdentityWithPasswordForceIdmsId", targetNamespace = "http://uimsv22.service.ims.schneider.com/", className = "com.uims.authenticatedUsermanager.CreateIdentityWithPasswordForceIdmsId")
    @ResponseWrapper(localName = "createIdentityWithPasswordForceIdmsIdResponse", targetNamespace = "http://uimsv22.service.ims.schneider.com/", className = "com.uims.authenticatedUsermanager.CreateIdentityWithPasswordForceIdmsIdResponse")
    public CreatedIdentityReport createIdentityWithPasswordForceIdmsId(
        @WebParam(name = "callerFid", targetNamespace = "")
        String callerFid,
        @WebParam(name = "identity", targetNamespace = "")
        UserV6 identity,
        @WebParam(name = "password", targetNamespace = "")
        String password,
        @WebParam(name = "forcedFederatedId", targetNamespace = "")
        String forcedFederatedId)
        throws ForcedFidAlreadyExistException_Exception, IMSServiceSecurityCallNotAllowedException_Exception, ImsMailerException_Exception, InvalidImsServiceMethodArgumentException_Exception, LdapTemplateNotReadyException_Exception, RequestedEntryNotExistsException_Exception, RequestedInternalUserException_Exception, SecuredImsException_Exception, UnexpectedLdapResponseException_Exception, UnexpectedRuntimeImsException_Exception
    ;

    /**
     * 
     * @param identity
     * @param forcedFederatedId
     * @param callerFid
     * @return
     *     returns com.uims.authenticatedUsermanager.CreatedIdentityReport
     * @throws SecuredImsException_Exception
     * @throws InvalidImsServiceMethodArgumentException_Exception
     * @throws ImsMailerException_Exception
     * @throws LdapTemplateNotReadyException_Exception
     * @throws RequestedInternalUserException_Exception
     * @throws UnexpectedRuntimeImsException_Exception
     * @throws IMSServiceSecurityCallNotAllowedException_Exception
     * @throws ForcedFidAlreadyExistException_Exception
     * @throws RequestedEntryNotExistsException_Exception
     * @throws UnexpectedLdapResponseException_Exception
     */
    @WebMethod
    @WebResult(targetNamespace = "")
    @RequestWrapper(localName = "createIdentityWithPhoneIdForceIdmsId", targetNamespace = "http://uimsv22.service.ims.schneider.com/", className = "com.uims.authenticatedUsermanager.CreateIdentityWithPhoneIdForceIdmsId")
    @ResponseWrapper(localName = "createIdentityWithPhoneIdForceIdmsIdResponse", targetNamespace = "http://uimsv22.service.ims.schneider.com/", className = "com.uims.authenticatedUsermanager.CreateIdentityWithPhoneIdForceIdmsIdResponse")
    public CreatedIdentityReport createIdentityWithPhoneIdForceIdmsId(
        @WebParam(name = "callerFid", targetNamespace = "")
        String callerFid,
        @WebParam(name = "identity", targetNamespace = "")
        UserV6 identity,
        @WebParam(name = "forcedFederatedId", targetNamespace = "")
        String forcedFederatedId)
        throws ForcedFidAlreadyExistException_Exception, IMSServiceSecurityCallNotAllowedException_Exception, ImsMailerException_Exception, InvalidImsServiceMethodArgumentException_Exception, LdapTemplateNotReadyException_Exception, RequestedEntryNotExistsException_Exception, RequestedInternalUserException_Exception, SecuredImsException_Exception, UnexpectedLdapResponseException_Exception, UnexpectedRuntimeImsException_Exception
    ;

    /**
     * 
     * @param phoneId
     * @param callerFid
     * @return
     *     returns com.uims.authenticatedUsermanager.UserFederatedIdAndType
     * @throws SecuredImsException_Exception
     * @throws InvalidImsServiceMethodArgumentException_Exception
     * @throws InactiveUserImsException_Exception
     * @throws LdapTemplateNotReadyException_Exception
     * @throws RequestedInternalUserException_Exception
     * @throws IMSServiceSecurityCallNotAllowedException_Exception
     * @throws UnexpectedRuntimeImsException_Exception
     * @throws RequestedEntryNotExistsException_Exception
     * @throws UnexpectedLdapResponseException_Exception
     */
    @WebMethod
    @WebResult(targetNamespace = "")
    @RequestWrapper(localName = "searchUserByPhoneId", targetNamespace = "http://uimsv22.service.ims.schneider.com/", className = "com.uims.authenticatedUsermanager.SearchUserByPhoneId")
    @ResponseWrapper(localName = "searchUserByPhoneIdResponse", targetNamespace = "http://uimsv22.service.ims.schneider.com/", className = "com.uims.authenticatedUsermanager.SearchUserByPhoneIdResponse")
    public UserFederatedIdAndType searchUserByPhoneId(
        @WebParam(name = "callerFid", targetNamespace = "")
        String callerFid,
        @WebParam(name = "phoneId", targetNamespace = "")
        String phoneId)
        throws IMSServiceSecurityCallNotAllowedException_Exception, InactiveUserImsException_Exception, InvalidImsServiceMethodArgumentException_Exception, LdapTemplateNotReadyException_Exception, RequestedEntryNotExistsException_Exception, RequestedInternalUserException_Exception, SecuredImsException_Exception, UnexpectedLdapResponseException_Exception, UnexpectedRuntimeImsException_Exception
    ;

    /**
     * 
     * @param email
     * @param callerFid
     * @return
     *     returns com.uims.authenticatedUsermanager.UserFederatedIdAndType
     * @throws SecuredImsException_Exception
     * @throws InvalidImsServiceMethodArgumentException_Exception
     * @throws InactiveUserImsException_Exception
     * @throws LdapTemplateNotReadyException_Exception
     * @throws RequestedInternalUserException_Exception
     * @throws UnexpectedRuntimeImsException_Exception
     * @throws IMSServiceSecurityCallNotAllowedException_Exception
     * @throws RequestedEntryNotExistsException_Exception
     * @throws UnexpectedLdapResponseException_Exception
     */
    @WebMethod
    @WebResult(targetNamespace = "")
    @RequestWrapper(localName = "searchUser", targetNamespace = "http://uimsv22.service.ims.schneider.com/", className = "com.uims.authenticatedUsermanager.SearchUser")
    @ResponseWrapper(localName = "searchUserResponse", targetNamespace = "http://uimsv22.service.ims.schneider.com/", className = "com.uims.authenticatedUsermanager.SearchUserResponse")
    public UserFederatedIdAndType searchUser(
        @WebParam(name = "callerFid", targetNamespace = "")
        String callerFid,
        @WebParam(name = "email", targetNamespace = "")
        String email)
        throws IMSServiceSecurityCallNotAllowedException_Exception, InactiveUserImsException_Exception, InvalidImsServiceMethodArgumentException_Exception, LdapTemplateNotReadyException_Exception, RequestedEntryNotExistsException_Exception, RequestedInternalUserException_Exception, SecuredImsException_Exception, UnexpectedLdapResponseException_Exception, UnexpectedRuntimeImsException_Exception
    ;

    /**
     * 
     * @param password
     * @param application
     * @param identity
     * @param callerFid
     * @return
     *     returns com.uims.authenticatedUsermanager.CreatedIdentityReport
     * @throws SecuredImsException_Exception
     * @throws InvalidImsServiceMethodArgumentException_Exception
     * @throws LdapTemplateNotReadyException_Exception
     * @throws ImsMailerException_Exception
     * @throws RequestedInternalUserException_Exception
     * @throws UnexpectedRuntimeImsException_Exception
     * @throws IMSServiceSecurityCallNotAllowedException_Exception
     * @throws RequestedEntryNotExistsException_Exception
     * @throws UnexpectedLdapResponseException_Exception
     */
    @WebMethod
    @WebResult(targetNamespace = "")
    @RequestWrapper(localName = "createIdentityWithPassword", targetNamespace = "http://uimsv22.service.ims.schneider.com/", className = "com.uims.authenticatedUsermanager.CreateIdentityWithPassword")
    @ResponseWrapper(localName = "createIdentityWithPasswordResponse", targetNamespace = "http://uimsv22.service.ims.schneider.com/", className = "com.uims.authenticatedUsermanager.CreateIdentityWithPasswordResponse")
    public CreatedIdentityReport createIdentityWithPassword(
        @WebParam(name = "callerFid", targetNamespace = "")
        String callerFid,
        @WebParam(name = "identity", targetNamespace = "")
        UserV6 identity,
        @WebParam(name = "application", targetNamespace = "")
        AccessElement application,
        @WebParam(name = "password", targetNamespace = "")
        String password)
        throws IMSServiceSecurityCallNotAllowedException_Exception, ImsMailerException_Exception, InvalidImsServiceMethodArgumentException_Exception, LdapTemplateNotReadyException_Exception, RequestedEntryNotExistsException_Exception, RequestedInternalUserException_Exception, SecuredImsException_Exception, UnexpectedLdapResponseException_Exception, UnexpectedRuntimeImsException_Exception
    ;

    /**
     * 
     * @param federatedId
     * @param user
     * @param callerFid
     * @return
     *     returns boolean
     * @throws SecuredImsException_Exception
     * @throws InvalidImsServiceMethodArgumentException_Exception
     * @throws InactiveUserImsException_Exception
     * @throws LdapTemplateNotReadyException_Exception
     * @throws UnexpectedRuntimeImsException_Exception
     * @throws IMSServiceSecurityCallNotAllowedException_Exception
     * @throws RequestedEntryNotExistsException_Exception
     * @throws UnexpectedLdapResponseException_Exception
     */
    @WebMethod
    @WebResult(targetNamespace = "")
    @RequestWrapper(localName = "updateUser", targetNamespace = "http://uimsv22.service.ims.schneider.com/", className = "com.uims.authenticatedUsermanager.UpdateUser")
    @ResponseWrapper(localName = "updateUserResponse", targetNamespace = "http://uimsv22.service.ims.schneider.com/", className = "com.uims.authenticatedUsermanager.UpdateUserResponse")
    public boolean updateUser(
        @WebParam(name = "callerFid", targetNamespace = "")
        String callerFid,
        @WebParam(name = "federatedId", targetNamespace = "")
        String federatedId,
        @WebParam(name = "user", targetNamespace = "")
        UserV6 user)
        throws IMSServiceSecurityCallNotAllowedException_Exception, InactiveUserImsException_Exception, InvalidImsServiceMethodArgumentException_Exception, LdapTemplateNotReadyException_Exception, RequestedEntryNotExistsException_Exception, SecuredImsException_Exception, UnexpectedLdapResponseException_Exception, UnexpectedRuntimeImsException_Exception
    ;

    /**
     * 
     * @param application
     * @param federatedId
     * @param callerFid
     * @return
     *     returns java.lang.String
     * @throws SecuredImsException_Exception
     * @throws InvalidImsServiceMethodArgumentException_Exception
     * @throws InvalidImsPropertiesFileException_Exception
     * @throws InactiveUserImsException_Exception
     * @throws LdapTemplateNotReadyException_Exception
     * @throws ImsMailerException_Exception
     * @throws UnexpectedRuntimeImsException_Exception
     * @throws IMSServiceSecurityCallNotAllowedException_Exception
     * @throws RequestedEntryNotExistsException_Exception
     * @throws UnexpectedLdapResponseException_Exception
     */
    @WebMethod
    @WebResult(targetNamespace = "")
    @RequestWrapper(localName = "resetPasswordWithSms", targetNamespace = "http://uimsv22.service.ims.schneider.com/", className = "com.uims.authenticatedUsermanager.ResetPasswordWithSms")
    @ResponseWrapper(localName = "resetPasswordWithSmsResponse", targetNamespace = "http://uimsv22.service.ims.schneider.com/", className = "com.uims.authenticatedUsermanager.ResetPasswordWithSmsResponse")
    public String resetPasswordWithSms(
        @WebParam(name = "callerFid", targetNamespace = "")
        String callerFid,
        @WebParam(name = "federatedId", targetNamespace = "")
        String federatedId,
        @WebParam(name = "application", targetNamespace = "")
        AccessElement application)
        throws IMSServiceSecurityCallNotAllowedException_Exception, ImsMailerException_Exception, InactiveUserImsException_Exception, InvalidImsPropertiesFileException_Exception, InvalidImsServiceMethodArgumentException_Exception, LdapTemplateNotReadyException_Exception, RequestedEntryNotExistsException_Exception, SecuredImsException_Exception, UnexpectedLdapResponseException_Exception, UnexpectedRuntimeImsException_Exception
    ;

    /**
     * 
     * @param identity
     * @param forcedFederatedId
     * @param callerFid
     * @return
     *     returns com.uims.authenticatedUsermanager.CreatedIdentityReport
     * @throws SecuredImsException_Exception
     * @throws InvalidImsServiceMethodArgumentException_Exception
     * @throws LdapTemplateNotReadyException_Exception
     * @throws ImsMailerException_Exception
     * @throws RequestedInternalUserException_Exception
     * @throws ForcedFidAlreadyExistException_Exception
     * @throws IMSServiceSecurityCallNotAllowedException_Exception
     * @throws UnexpectedRuntimeImsException_Exception
     * @throws RequestedEntryNotExistsException_Exception
     * @throws UnexpectedLdapResponseException_Exception
     */
    @WebMethod
    @WebResult(targetNamespace = "")
    @RequestWrapper(localName = "createIdentityForceIdmsId", targetNamespace = "http://uimsv22.service.ims.schneider.com/", className = "com.uims.authenticatedUsermanager.CreateIdentityForceIdmsId")
    @ResponseWrapper(localName = "createIdentityForceIdmsIdResponse", targetNamespace = "http://uimsv22.service.ims.schneider.com/", className = "com.uims.authenticatedUsermanager.CreateIdentityForceIdmsIdResponse")
    public CreatedIdentityReport createIdentityForceIdmsId(
        @WebParam(name = "callerFid", targetNamespace = "")
        String callerFid,
        @WebParam(name = "identity", targetNamespace = "")
        UserV6 identity,
        @WebParam(name = "forcedFederatedId", targetNamespace = "")
        String forcedFederatedId)
        throws ForcedFidAlreadyExistException_Exception, IMSServiceSecurityCallNotAllowedException_Exception, ImsMailerException_Exception, InvalidImsServiceMethodArgumentException_Exception, LdapTemplateNotReadyException_Exception, RequestedEntryNotExistsException_Exception, RequestedInternalUserException_Exception, SecuredImsException_Exception, UnexpectedLdapResponseException_Exception, UnexpectedRuntimeImsException_Exception
    ;

    /**
     * 
     * @param password
     * @param application
     * @param identity
     * @param callerFid
     * @return
     *     returns com.uims.authenticatedUsermanager.CreatedIdentityReport
     * @throws SecuredImsException_Exception
     * @throws InvalidImsServiceMethodArgumentException_Exception
     * @throws ImsMailerException_Exception
     * @throws LdapTemplateNotReadyException_Exception
     * @throws RequestedInternalUserException_Exception
     * @throws UnexpectedRuntimeImsException_Exception
     * @throws IMSServiceSecurityCallNotAllowedException_Exception
     * @throws RequestedEntryNotExistsException_Exception
     * @throws UnexpectedLdapResponseException_Exception
     */
    @WebMethod
    @WebResult(targetNamespace = "")
    @RequestWrapper(localName = "createIdentityWithMobileWithPassword", targetNamespace = "http://uimsv22.service.ims.schneider.com/", className = "com.uims.authenticatedUsermanager.CreateIdentityWithMobileWithPassword")
    @ResponseWrapper(localName = "createIdentityWithMobileWithPasswordResponse", targetNamespace = "http://uimsv22.service.ims.schneider.com/", className = "com.uims.authenticatedUsermanager.CreateIdentityWithMobileWithPasswordResponse")
    public CreatedIdentityReport createIdentityWithMobileWithPassword(
        @WebParam(name = "callerFid", targetNamespace = "")
        String callerFid,
        @WebParam(name = "identity", targetNamespace = "")
        UserV6 identity,
        @WebParam(name = "application", targetNamespace = "")
        AccessElement application,
        @WebParam(name = "password", targetNamespace = "")
        String password)
        throws IMSServiceSecurityCallNotAllowedException_Exception, ImsMailerException_Exception, InvalidImsServiceMethodArgumentException_Exception, LdapTemplateNotReadyException_Exception, RequestedEntryNotExistsException_Exception, RequestedInternalUserException_Exception, SecuredImsException_Exception, UnexpectedLdapResponseException_Exception, UnexpectedRuntimeImsException_Exception
    ;

    /**
     * 
     * @param application
     * @param federatedId
     * @param callerFid
     * @return
     *     returns java.lang.String
     * @throws SecuredImsException_Exception
     * @throws InvalidImsServiceMethodArgumentException_Exception
     * @throws InvalidImsPropertiesFileException_Exception
     * @throws InactiveUserImsException_Exception
     * @throws ImsMailerException_Exception
     * @throws LdapTemplateNotReadyException_Exception
     * @throws UnexpectedRuntimeImsException_Exception
     * @throws IMSServiceSecurityCallNotAllowedException_Exception
     * @throws RequestedEntryNotExistsException_Exception
     * @throws UnexpectedLdapResponseException_Exception
     */
    @WebMethod
    @WebResult(targetNamespace = "")
    @RequestWrapper(localName = "resetPassword", targetNamespace = "http://uimsv22.service.ims.schneider.com/", className = "com.uims.authenticatedUsermanager.ResetPassword")
    @ResponseWrapper(localName = "resetPasswordResponse", targetNamespace = "http://uimsv22.service.ims.schneider.com/", className = "com.uims.authenticatedUsermanager.ResetPasswordResponse")
    public String resetPassword(
        @WebParam(name = "callerFid", targetNamespace = "")
        String callerFid,
        @WebParam(name = "federatedId", targetNamespace = "")
        String federatedId,
        @WebParam(name = "application", targetNamespace = "")
        AccessElement application)
        throws IMSServiceSecurityCallNotAllowedException_Exception, ImsMailerException_Exception, InactiveUserImsException_Exception, InvalidImsPropertiesFileException_Exception, InvalidImsServiceMethodArgumentException_Exception, LdapTemplateNotReadyException_Exception, RequestedEntryNotExistsException_Exception, SecuredImsException_Exception, UnexpectedLdapResponseException_Exception, UnexpectedRuntimeImsException_Exception
    ;

    /**
     * 
     * @param application
     * @param federatedId
     * @param callerFid
     * @return
     *     returns java.lang.String
     * @throws SecuredImsException_Exception
     * @throws InvalidImsServiceMethodArgumentException_Exception
     * @throws LdapTemplateNotReadyException_Exception
     * @throws ImsMailerException_Exception
     * @throws UnexpectedRuntimeImsException_Exception
     * @throws IMSServiceSecurityCallNotAllowedException_Exception
     * @throws RequestedEntryNotExistsException_Exception
     * @throws UnexpectedLdapResponseException_Exception
     */
    @WebMethod
    @WebResult(targetNamespace = "")
    @RequestWrapper(localName = "reactivate", targetNamespace = "http://uimsv22.service.ims.schneider.com/", className = "com.uims.authenticatedUsermanager.Reactivate")
    @ResponseWrapper(localName = "reactivateResponse", targetNamespace = "http://uimsv22.service.ims.schneider.com/", className = "com.uims.authenticatedUsermanager.ReactivateResponse")
    public String reactivate(
        @WebParam(name = "callerFid", targetNamespace = "")
        String callerFid,
        @WebParam(name = "federatedId", targetNamespace = "")
        String federatedId,
        @WebParam(name = "application", targetNamespace = "")
        AccessElement application)
        throws IMSServiceSecurityCallNotAllowedException_Exception, ImsMailerException_Exception, InvalidImsServiceMethodArgumentException_Exception, LdapTemplateNotReadyException_Exception, RequestedEntryNotExistsException_Exception, SecuredImsException_Exception, UnexpectedLdapResponseException_Exception, UnexpectedRuntimeImsException_Exception
    ;

    /**
     * 
     * @param federatedId
     * @param callerFid
     * @return
     *     returns com.uims.authenticatedUsermanager.UserV6
     * @throws SecuredImsException_Exception
     * @throws InvalidImsServiceMethodArgumentException_Exception
     * @throws LdapTemplateNotReadyException_Exception
     * @throws UnexpectedRuntimeImsException_Exception
     * @throws IMSServiceSecurityCallNotAllowedException_Exception
     * @throws RequestedEntryNotExistsException_Exception
     * @throws UnexpectedLdapResponseException_Exception
     */
    @WebMethod
    @WebResult(targetNamespace = "")
    @RequestWrapper(localName = "getUser", targetNamespace = "http://uimsv22.service.ims.schneider.com/", className = "com.uims.authenticatedUsermanager.GetUser")
    @ResponseWrapper(localName = "getUserResponse", targetNamespace = "http://uimsv22.service.ims.schneider.com/", className = "com.uims.authenticatedUsermanager.GetUserResponse")
    public UserV6 getUser(
        @WebParam(name = "callerFid", targetNamespace = "")
        String callerFid,
        @WebParam(name = "federatedId", targetNamespace = "")
        String federatedId)
        throws IMSServiceSecurityCallNotAllowedException_Exception, InvalidImsServiceMethodArgumentException_Exception, LdapTemplateNotReadyException_Exception, RequestedEntryNotExistsException_Exception, SecuredImsException_Exception, UnexpectedLdapResponseException_Exception, UnexpectedRuntimeImsException_Exception
    ;

}
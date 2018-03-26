package com.idms.service;

import com.idms.model.AILRequest;
import com.idms.model.ActivateUser;
import com.idms.model.ActivateUserRequest;
import com.idms.model.ConfirmPinRequest;
import com.idms.model.CreateUserRequest;
import com.idms.model.IFWUser;
import com.idms.model.ResendEmailChangeRequest;
import com.idms.model.ResendPinRequest;
import com.idms.model.ResendRegEmailRequest;
import com.idms.model.SendInvitationRequest;
import com.idms.model.UpdatePasswordRequest;
import com.idms.model.UpdateUserRequest;
import com.idms.model.UserAILRecord;
import com.idms.product.model.OpenAmUser;
import com.idms.product.model.OpenAmUserInput;
import com.idms.product.model.OpenAmUserRequest;
import com.se.idms.dto.SetPasswordRequest;
import com.uims.authenticatedUsermanager.UserV6;
import com.uims.companymanager.CompanyV3;

/**
 * Mock data creation for dto objects in the Service layer.
 * Note: It uses the core layer {@link DomainMockData} which is available across layers.
 */
public class DtoMockData {

	/**
     * Private default constructor to prevent external instantiation.
     */
    private DtoMockData() {
    }
    
    /**
     * Builds instance of {@link CreateUserRequest} with default details.
     *
     * @return the constructed {@link CreateUserRequest} instance
     */
    public static CreateUserRequest buildUserRegistrationRequset() {
    	CreateUserRequest request = new CreateUserRequest();
    	IFWUser userRecord = DomainMockData.buildUser();   	
    	request.setUserRecord(userRecord);
        return request;
    }
    
    /**
     * Builds instance of {@link IFWUser} with default details.
     *
     * @return the constructed {@link IFWUser} instance
     */
    public static IFWUser buildIFWUser() {
    	IFWUser userRecord = DomainMockData.buildUser();   	
        return userRecord;
    }
    
    /**
     * Builds instance of {@link CreateUserRequest} with default details.
     *
     * @return the constructed {@link CreateUserRequest} instance
     */
    public static OpenAmUserRequest buildUserRegistrationOpenAmRequset() {
    	OpenAmUserRequest request = new OpenAmUserRequest();
    	OpenAmUserInput input = new OpenAmUserInput();
    	OpenAmUser user = DomainMockData.buildOpenAmUser(); 
    	input.setUser(user);
    	request.setInput(input);
        return request;
    }
    
    /**
     * Builds instance of {@link CreateUserRequest} with default details.
     *
     * @return the constructed {@link CreateUserRequest} instance
     */
    public static ConfirmPinRequest buildUserPinConfirmationRequset() {
    	ConfirmPinRequest request = new ConfirmPinRequest();
    	request.setId(DomainMockData.ID);
    	request.setIDMS_Federated_ID__c(DomainMockData.FEDERATION_ID);
    	request.setOperation(DomainMockData.USER_REGISTRATION_OPERATION);
    	request.setIDMS_Profile_update_source(DomainMockData.PROFILE_UPDATE_SOURCE);
    	request.setPinCode(DomainMockData.REGISTRATION_PIN);
    	
    	
        return request;
    }
    
    /**
     * Builds instance of {@link CreateUserRequest} with default details.
     *
     * @return the constructed {@link CreateUserRequest} instance
     */
    public static CreateUserRequest buildUpdateUserProvisionalRequset() {
    	CreateUserRequest provisionalRequest = new CreateUserRequest();
		IFWUser userRecord = new IFWUser();
		userRecord.setId(DomainMockData.ID);
		userRecord.setEmail(DomainMockData.EMAIL);
		provisionalRequest.setUserRecord(userRecord);
        return provisionalRequest;
    }
    
    /**
     * Builds instance of {@link CreateUserRequest} with default details.
     *
     * @return the constructed {@link CreateUserRequest} instance
     */
    public static OpenAmUserRequest buildUpdateUserProvisionalOpenAmRequset() {
    	OpenAmUserRequest request = new OpenAmUserRequest();
    	OpenAmUserInput input = new OpenAmUserInput();
    	OpenAmUser user = new OpenAmUser();
    	user.setMail(DomainMockData.EMAIL);
    	input.setUser(user);
    	request.setInput(input);
        return request;
    }

    /**
     * Builds instance of {@link UpdateUserRequest} with default details.
     *
     * @return the constructed {@link UpdateUserRequest} instance
     */
    public static UpdateUserRequest buildUpdateUserRequset() {
    	UpdateUserRequest request = new UpdateUserRequest();
    	IFWUser userRecord = DomainMockData.buildUser();   	
    	request.setUserRecord(userRecord);
        return request;
    }
    
    /**
     * Builds instance of {@link ConfirmPinRequest} with default details.
     *
     * @return the constructed {@link ConfirmPinRequest} instance
     */
    public static ResendPinRequest buildResendRequPinReuest() {
    	ResendPinRequest request = new ResendPinRequest();
    	request.setIdmsUserId(DomainMockData.ID);   	
        return request;
    }

    /**
     * Builds instance of {@link ConfirmPinRequest} with default details.
     *
     * @return the constructed {@link ConfirmPinRequest} instance
     */
    public static ActivateUserRequest buildActivateUserRequest() {
    	ActivateUserRequest request = new ActivateUserRequest();
    	ActivateUser userRecord = new ActivateUser();
    	userRecord.setId(DomainMockData.ID);
    	userRecord.setIDMS_Federated_ID__c(DomainMockData.IDMS_Federated_ID__c);
    	userRecord.setIDMS_Registration_Source__c(DomainMockData.IDMS_Registration_Source__c);
    	
    	request.setUserRecord(userRecord);   	
        return request;
    }
    
    /**
     * Builds instance of {@link UserV6} with default details.
     *
     * @return the constructed {@link UserV6} instance
     */
    public static UserV6 buildUserV6Reuest() {
    	UserV6 request = new UserV6();
    	request.setAddInfoAddress("abc");   	
        return request;
    }
    
    /**
     * Builds instance of {@link UserV6} with default details.
     *
     * @return the constructed {@link UserV6} instance
     */
    public static CompanyV3 buildCompanyV3Reuest() {
    	CompanyV3 request = new CompanyV3();
    	request.setAddInfoAddress("abc");   	
        return request;
    }
    
    /**
     * Builds instance of {@link ResendEmailChangeRequest} with default details.
     *
     * @return the constructed {@link ResendEmailChangeRequest} instance
     */
    public static ResendEmailChangeRequest buildResendEmailChangeRequest() {
    	ResendEmailChangeRequest request = new ResendEmailChangeRequest();
    	request.setFirstName(DomainMockData.FIRST_NAME);
    	request.setLastName(DomainMockData.LAST_NAME);
    	request.setNewEmail(DomainMockData.EMAIL);
    	request.setOldEmail(DomainMockData.EMAIL);
    	
        return request;
    }
    
    /**
     * Builds instance of {@link ResendEmailChangeRequest} with default details.
     *
     * @return the constructed {@link ResendEmailChangeRequest} instance
     */
    public static ResendRegEmailRequest buildRegEmailChangeRequest() {
    	ResendRegEmailRequest request = new ResendRegEmailRequest();
    	request.setFirstName(DomainMockData.FIRST_NAME);
    	request.setLastName(DomainMockData.LAST_NAME);
    	request.setEmail(DomainMockData.EMAIL);
    	
        return request;
    }
    
    /**
     * Builds instance of {@link ResendEmailChangeRequest} with default details.
     *
     * @return the constructed {@link ResendEmailChangeRequest} instance
     */
    public static AILRequest buildUserUpdateAILRequest() {
    	AILRequest request = new AILRequest();
    	
    	UserAILRecord userAILRecord = new UserAILRecord();
    	request.setUserAILRecord(userAILRecord);
    	
    	return request;
    }
    /**
     * Builds instance of {@link SendInvitationRequest} with default details.
     *
     * @return the constructed {@link SendInvitationRequest} instance
     */
    public static SendInvitationRequest buildSendInvitationRequset() {
    	SendInvitationRequest request = new SendInvitationRequest();
    	
    	request.setEmail("test@mailinator.com");
    	request.setInvitationId("eee7354ydhd882821");
    	request.setRedirectUrl("https://myse.schneider-electric.com/InvitationProcess");
        return request;
    }
    
    /**
     * Builds instance of {@link SetPasswordRequest} with default details.
     *
     * @return the constructed {@link SetPasswordRequest} instance
     */
    public static SetPasswordRequest buildSetPasswordRequset() {
    	SetPasswordRequest request = new SetPasswordRequest();
    	
    	request.setFederationIdentifier(DomainMockData.FEDERATION_ID);
    	request.setId(DomainMockData.ID);
    	request.setIDMS_Federated_ID__c(DomainMockData.IDMS_Federated_ID__c);
    	request.setIDMS_Profile_update_source(DomainMockData.PROFILE_UPDATE_SOURCE);
    	request.setNewPwd(DomainMockData.PASSWORD);
    	request.setToken(DomainMockData.REGISTRATION_PIN);
    	
    	return request;
}    
    /**
     * Builds instance of {@link UpdatePasswordRequest} with default details.
     *
     * @return the constructed {@link UpdatePasswordRequest} instance
     */
    public static UpdatePasswordRequest buildUpdatePasswordRequest() {
    	UpdatePasswordRequest request = new UpdatePasswordRequest();
    	
    	request.setIDMS_Profile_update_source(DomainMockData.IDMS_Profile_update_source__c);
    	request.setExistingPwd("Password123");
    	request.setNewPwd("Password1234");
        return request;
    }
}

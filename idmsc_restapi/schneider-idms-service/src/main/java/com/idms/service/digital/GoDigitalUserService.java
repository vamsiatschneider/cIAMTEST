package com.idms.service.digital;

public interface GoDigitalUserService {

	void goDigitalUserRegistration(String userRequest);

	void setFromUserName(String property);

	void setSupportUser(String property);
	
    String getFromUserName();

	String getSupportUser();
}
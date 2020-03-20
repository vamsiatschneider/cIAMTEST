package com.idms.mail.template.util;

import java.util.Arrays;

public enum OperationType {

	SET_USER_PASSWORD("SetUserPwd", "PasswordReset"), 
	USER_REGISTRATION("userRegistration", "UserRegistration"), 
	UPDATE_USER_RECORD("UpdateUserRecord", "UserUpdate"),
	ADD_EMAIL_USER_RECORD("AddEmailUserRecord", "AddEmail"),
	CHANGE_EMAIL("ChangeEmail", "ChangeEmail"), 
	SEND_INVITATION("sendInvitation", "SendInvitation"),
	CHANGE_EMAIL_NOTIFICATION("changeEmailNotification", "ChangeEmail"),
	INVALID("Invalid", "Invalid");

	private String type;
	private String openDJType;

	private OperationType(String operationType, String openDJType) {
		this.type = operationType;
		this.openDJType = openDJType;
	}

	public String getType() {
		return this.type;
	}
	
	public String getOpenDJType() {
		return this.openDJType;
	}

	public static OperationType getKey(String type) {
		return Arrays.stream(OperationType.values()).
				filter(e -> e.type.equalsIgnoreCase(type)).
				findFirst().orElse(OperationType.INVALID);
	}

}

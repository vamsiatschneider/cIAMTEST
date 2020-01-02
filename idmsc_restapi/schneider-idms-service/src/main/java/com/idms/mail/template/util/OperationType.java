package com.idms.mail.template.util;

import java.util.Arrays;

public enum OperationType {

	SET_USER_PASSWORD("SetUserPwd"), 
	USER_REGISTRATION("userRegistration"), 
	UPDATE_USER_RECORD("UpdateUserRecord"),
	ADD_EMAIL_USER_RECORD("AddEmailUserRecord"), 
	SEND_INVITATION("sendInvitation"), 
	INVALID("Invalid");

	private String type;

	private OperationType(String operationType) {
		this.type = operationType;
	}

	public String getType() {
		return this.type;
	}

	public static OperationType getKey(String type) {
		return Arrays.stream(OperationType.values()).
				filter(e -> e.type.equalsIgnoreCase(type)).
				findFirst().orElse(OperationType.INVALID);
	}
}

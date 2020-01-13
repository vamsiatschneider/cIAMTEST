package com.idms.mail.template.util;

import java.util.Arrays;

public enum PRMTemplateType {

	PRM_SELF_REGISTRATION("Self Registration"),
	PRM_INTERNAL_REGISTRATION("Internal Registration"),
	PRM_ECLIPSE_REGISTRATION("Eclipse Registration"),
	INVALID("Invalid");
	
	private String type;
	private PRMTemplateType(String operationType) {
		this.type = operationType;
	}
	public String getType() {
		return this.type;
	}
	public static PRMTemplateType getKey(String tType) {
		return Arrays.stream(PRMTemplateType.values()).
				filter(e -> e.type.equalsIgnoreCase(tType)).
				findFirst().orElse(PRMTemplateType.INVALID);
	}
}

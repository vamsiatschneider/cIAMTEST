package com.idms.model;

import org.apache.commons.lang3.builder.ToStringBuilder;

import com.fasterxml.jackson.annotation.JsonProperty;

/**
 * The UpdatePasswordRequest class.
 * 
 * @author Pujarani Panda
 *
 */

public class UpdatePasswordRequest extends BaseEntity {

	@JsonProperty("IDMS_Profile_update_source")
	private String IDMS_Profile_update_source;

	@JsonProperty("ExistingPwd")
	private String ExistingPwd;

	@JsonProperty("NewPwd")
	private String NewPwd;

	@JsonProperty("UIFlag")
	private String UIFlag;

	public String getIDMS_Profile_update_source() {
		return IDMS_Profile_update_source;
	}

	public void setIDMS_Profile_update_source(String iDMS_Profile_update_source) {
		IDMS_Profile_update_source = iDMS_Profile_update_source;
	}

	public String getExistingPwd() {
		return ExistingPwd;
	}

	public void setExistingPwd(String existingPwd) {
		ExistingPwd = existingPwd;
	}

	public String getNewPwd() {
		return NewPwd;
	}

	public void setNewPwd(String newPwd) {
		NewPwd = newPwd;
	}

	public String getUIFlag() {
		return UIFlag;
	}

	public void setUIFlag(String uIFlag) {
		UIFlag = uIFlag;
	}

}

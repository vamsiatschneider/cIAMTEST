package com.idms.model;

import com.fasterxml.jackson.annotation.JsonProperty;

public class UIMSCompanyRequest {

	@JsonProperty
	private UIMSCompany uimscompany;

	public UIMSCompany getUimscompany() {
		return uimscompany;
	}

	public void setUimscompany(UIMSCompany uimscompany) {
		this.uimscompany = uimscompany;
	}

	@Override
	public String toString() {
		return "UIMSCompanyRequest [uimscompany=" + uimscompany + "]";
	}
	
	
}

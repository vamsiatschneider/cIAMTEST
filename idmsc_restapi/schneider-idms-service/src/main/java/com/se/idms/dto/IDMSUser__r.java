package com.se.idms.dto;

import javax.xml.bind.annotation.XmlRootElement;

import org.springframework.stereotype.Component;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.idms.product.model.Attributes;
/**
 * The IDMSUser__r class.
 * 
 * @author Pujarani Panda
 *
 */
@Component
@XmlRootElement
public class IDMSUser__r {

	@JsonInclude
	@JsonProperty("attributes")
	private Attributes attributes;

	@JsonProperty("Id")
	private String id;
	
	
	@JsonProperty("IDMS_Federated_ID__c")
	private  String IDMS_Federated_ID__c;

	public Attributes getAttributes() {
		return attributes;
	}

	public void setAttributes(Attributes attributes) {
		this.attributes = attributes;
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getIDMS_Federated_ID__c() {
		return IDMS_Federated_ID__c;
	}

	public void setIDMS_Federated_ID__c(String iDMS_Federated_ID__c) {
		IDMS_Federated_ID__c = iDMS_Federated_ID__c;
	}
}

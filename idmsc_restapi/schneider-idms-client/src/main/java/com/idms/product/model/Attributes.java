package com.idms.product.model;

import javax.xml.bind.annotation.XmlRootElement;

@XmlRootElement
public class Attributes {

	private String type;
	
	private String url;
	
	public Attributes(){
		this.type = "User";
		this.url = "";
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getUrl() {
		return url;
	}

	public void setUrl(String url) {
		this.url = url;
	}
	
}

package com.se.idms.dto;

import java.util.Objects;

import org.apache.commons.lang3.builder.ToStringBuilder;
import org.springframework.stereotype.Component;

import com.fasterxml.jackson.annotation.JsonProperty;

@Component
public class UserServiceResponseMobCounter {

	@JsonProperty("Status") 
	private String status;
    
	@JsonProperty("Message")
    private String message;
	
	@JsonProperty("Id")
	private String Id;
	
	@JsonProperty("maxMobLimit")
	private String maxMobLimit;
	
	@JsonProperty("strcurrentMobCounter")
	private String strcurrentMobCounter;
	
	public String getMaxMobLimit() {
		return maxMobLimit;
	}

	public void setMaxMobLimit(String maxMobLimit) {
		this.maxMobLimit = maxMobLimit;
	}

	public String getStrcurrentMobCounter() {
		return strcurrentMobCounter;
	}

	public void setStrcurrentMobCounter(String strcurrentMobCounter) {
		this.strcurrentMobCounter = strcurrentMobCounter;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}
	
	@JsonProperty("Id")
	public String getId() {
		return Id;
	}

	public void setId(String id) {
		Id = id;
	}

	@Override
	public boolean equals(Object obj) {
	        if (this == obj) {
	            return true;
	        }
	        if (obj == null || getClass() != obj.getClass()) {
	            return false;
	        }
	        UserServiceResponseMobCounter that = (UserServiceResponseMobCounter) obj;
	        return Objects.equals(this.status, that.status)
	                && Objects.equals(this.message, that.message);
	    }

	 @Override
	 public int hashCode() {
	        return Objects.hash(status, message);
	    }
	    
	 @Override
	 public String toString() {
	        return ToStringBuilder.reflectionToString(this);
	    }
}

package com.se.idms.dto;

import java.util.Objects;

import org.apache.commons.lang3.builder.ToStringBuilder;
import org.springframework.stereotype.Component;

import com.fasterxml.jackson.annotation.JsonProperty;

@Component
public class UserServiceResponseMailCounter {
	
	@JsonProperty("Status") 
	private String status;
    
	@JsonProperty("Message")
    private String message;
	
	@JsonProperty("Id")
	private String Id;
	
	@JsonProperty("maxEmailLimit")
	private String maxEmailLimit;
	
	@JsonProperty("strcurrentMailCounter")
	private String strcurrentMailCounter;
	
	public String getMaxEmailLimit() {
		return maxEmailLimit;
	}

	public void setMaxEmailLimit(String maxEmailLimit) {
		this.maxEmailLimit = maxEmailLimit;
	}

	public String getStrcurrentMailCounter() {
		return strcurrentMailCounter;
	}

	public void setStrcurrentMailCounter(String strcurrentMailCounter) {
		this.strcurrentMailCounter = strcurrentMailCounter;
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
	        UserServiceResponseMailCounter that = (UserServiceResponseMailCounter) obj;
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

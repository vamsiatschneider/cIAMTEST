package com.schneider.idms.common;

public class ErrorCodeConstants {

	public static final String BAD_REQUEST = "BAD_REQUEST";
	
	public static final String BADREQUEST_MESSAGE = "||description of the error from backend||";
	
	public static final String UNAUTHORIZED_REQUEST = "INVALID_SESSION_ID";
	
	public static final String UNAUTHORIZED_MESSAGE = "Session expired or invalid";
	
	public static final String NOTFOUND_REQUEST = "USER NOT FOUND ";
	
	public static final String NOTFOUND_MESSAGE = "||user not found from backend||";
	
	public static final String CONFLICT_REQUEST = "CREATE_FAILED ";
	
	public static final String CONFLICT_MESSAGE = "||description of the error from backend ||";
	
	public static final String SERVER_ERROR_REQUEST = "INTERNAL_SERVER_ERROR ";
	
	public static final String SERVER_ERROR_MESSAGE = "||description of the error from backend||";
	
}

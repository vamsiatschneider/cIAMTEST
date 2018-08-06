/**
 * 
 */
package com.schneider.idms.common;

import org.json.simple.JSONArray;
import org.json.simple.JSONObject;

/**
 * @author SESA508936
 *
 */

@SuppressWarnings("unchecked")
public class JsonBody {
	
	public static JSONArray EmptyJsonArray(String userId){
		JSONObject jsonObject = new JSONObject();
		jsonObject.put("errorCode", "NOT_FOUND");
		jsonObject.put("message", "Provided external ID field does not exist or is  not accessible "+userId);

		JSONArray jsonArray = new JSONArray();
		jsonArray.add(jsonObject);
		return jsonArray;
	}
	
	public static JSONArray InvalidSessionJsonArray(){
		JSONObject jsonObject = new JSONObject();
		jsonObject.put("errorCode", "INVALID_SESSION_ID");
		jsonObject.put("message", "Session expired or invalid");

		JSONArray jsonArray = new JSONArray();
		jsonArray.add(jsonObject);
		return jsonArray;
	}
	
	public static JSONArray UnauthorizedJsonArray(){
		JSONObject jsonObject = new JSONObject();
		jsonObject.put("errorCode", "Unauthorized");
		jsonObject.put("message", "Provided external ID field does not exist or is  not accessible ");

		JSONArray jsonArray = new JSONArray();
		jsonArray.add(jsonObject);
		return jsonArray;
	}

}

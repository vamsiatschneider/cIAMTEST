package com.idms.service.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import javax.ws.rs.core.Response;

import org.apache.commons.codec.binary.Hex;
import org.apache.commons.lang3.RandomStringUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.http.HttpResponse;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.DefaultHttpClient;
import org.springframework.beans.factory.annotation.Value;

import com.se.idms.util.UserConstants;

public class ChinaIdmsUtil {

	public static String generateHashValue(String generatedPin) {
		MessageDigest md;
		String hexString = null;
		try {
			md = MessageDigest.getInstance("SHA-256");
			md.update(generatedPin.getBytes());

			byte byteData[] = md.digest();

			hexString = new String(Hex.encodeHex(byteData));
			
		} catch (NoSuchAlgorithmException e) {
			e.printStackTrace();
		}

		return hexString;
	}
	
	public static Response executeHttpClient(String uri, String realm, String userName, String password)
			throws ClientProtocolException, IOException {

		HttpClient client = new DefaultHttpClient();
		HttpPost request = new HttpPost(uri + "/accessmanager/json/authenticate?realm=" + realm);
		request.setHeader("X-OpenAM-Username", userName);
		request.setHeader("X-OpenAM-Password", password);
		HttpResponse response = client.execute(request);
		BufferedReader rd = new BufferedReader(new InputStreamReader(response.getEntity().getContent()));

		StringBuffer result = new StringBuffer();
		String line = "";
		while ((line = rd.readLine()) != null) {
			result.append(line);
		}
		if (401 == response.getStatusLine().getStatusCode()) {
			return Response.status(Response.Status.UNAUTHORIZED).entity(result.toString()).build();
		}
		
		return Response.status(response.getStatusLine().getStatusCode()).entity(result.toString()).build();
	}
	
	/**
	 * returns the UID with 36 random characters.
	 * @return
	 */
	public static String generateFedId(){		 
		
        String fedId = "cn00";
        fedId += RandomStringUtils.random(4, UserConstants.RANDOM_CHARS);; 
        fedId += '-' + RandomStringUtils.random(4, UserConstants.RANDOM_CHARS);;
        fedId += '-' + RandomStringUtils.random(4, UserConstants.RANDOM_CHARS);;
        fedId += '-' + RandomStringUtils.random(4, UserConstants.RANDOM_CHARS);;
        fedId += '-' + RandomStringUtils.random(12, UserConstants.RANDOM_CHARS);;
        return fedId;
    }
	
	/**
	 * Print user request without password
	 * @param rawData
	 * @return
	 */
	public static String printData(String rawData){	
		String newrawdata = rawData;
        if (rawData.toLowerCase().contains("Password".toLowerCase())){
        	int i = rawData.indexOf(",\"Password");
        	int y = rawData.indexOf(",", i+1);
        	String chars = rawData.substring(i, y);
        	newrawdata = rawData.replace(chars, "");
        	
        }else{
        	//System.out.println("false");
        }
        return newrawdata;
    }
	
	/**
	 * Print user request without PIN
	 * @param rawData
	 * @return
	 */
	public static String printInfo(String rawData){
		String newrawdata = rawData;
        if (rawData.toLowerCase().contains("PinCode".toLowerCase())){
        	int i = rawData.indexOf(",\"PinCode");
        	int y = rawData.indexOf(",", i+1);
        	String chars = rawData.substring(i, y);
        	newrawdata = rawData.replace(chars, "");
        	
        }else{
        	//System.out.println("false");
        }
        return newrawdata;
    }
	
	/**
	 * Get cookies values from response
	 * @param response
	 * @return
	 * @throws Exception
	 */
	public static String getCookie(Response response, String haMode){
		String amlbcookieArray[] = null;
		String amlbcookie = "";

		if (UserConstants.TRUE.equalsIgnoreCase(haMode)) {
			amlbcookieArray = response.getHeaderString("Set-Cookie").split(";");
			if (null != amlbcookieArray && amlbcookieArray.length > 0) {
				for (String cookie : amlbcookieArray) {
					if (cookie.contains("AWSELB") && !cookie.contains("path")) {
						amlbcookie = cookie.substring(cookie.indexOf("AWSELB"), cookie.length());
						// amlbcookieValue =
						// cookie.substring(cookie.lastIndexOf("=") + 1,
						// cookie.length());
						break;
					}
				}
				for (String cookie : amlbcookieArray) {
					if (cookie.contains("amlbcookie") && !cookie.contains("path")) {
						amlbcookie = amlbcookie.concat(";")
								.concat(cookie.substring(cookie.indexOf("amlbcookie"), cookie.length()));
						// amlbcookieValue =
						// cookie.substring(cookie.lastIndexOf("=") + 1,
						// cookie.length());
						break;
					}
				}
			}
			if(amlbcookie.startsWith(";")){
				String amlbcookie1 = amlbcookie.substring(1);
				amlbcookie=amlbcookie1;				
			}			
		} else {
			amlbcookie = "amlbcookie=01";
		}

		return amlbcookie; 
	}

	public static boolean mobileValidator(String mobile){
		boolean checkMobile = true;		
		mobile = mobileTransformation(mobile);

		if ((!StringUtils.isNumeric(mobile)) || (mobile.length() != 11)) {
			checkMobile = false;
		}
		return checkMobile;
	}
	
	/**
	 * 
	 * @param str
	 * @return
	 */
	public static String mobileTransformation(String str) 
	{ 
		String mobileNumber = str.trim();
		str = mobileNumber.replaceAll("[\\(\\)\\-\\+\\s]", "");
		int i = 0; 
		while (str.charAt(i) == '0') 
			i++; 

		str = str.substring(i);
		
		if(str.startsWith(UserConstants.MOBILE_CHINA_CODE)) {
			str = str.substring(UserConstants.MOBILE_CHINA_CODE.length());
		}

		return str;  // return in String 
	} 
	
	/*public static void main(String[] args) {
		//String longvalue = generateHashValue("tY4MomqIwjg34932ZhTx651K38WJcZ");
		//System.out.println(longvalue);
		boolean mobileStr = mobileValidator("5987559777");
		System.out.println("mobileStr = "+mobileStr);
	}*/
	
}

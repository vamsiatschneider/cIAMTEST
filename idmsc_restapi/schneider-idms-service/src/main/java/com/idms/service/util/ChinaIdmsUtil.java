package com.idms.service.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.Files;
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
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.DefaultHttpClient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.util.ResourceUtils;

import com.schneider.idms.common.ErrorCodeConstants;
import com.se.idms.dto.ErrorResponse;
import com.se.idms.util.UserConstants;

public class ChinaIdmsUtil {
	
	private static final Logger LOGGER = LoggerFactory.getLogger(ChinaIdmsUtil.class);

	public static String generateHashValue(String generatedPin) {
		MessageDigest md;
		String hexString = null;
		try {
			md = MessageDigest.getInstance("SHA-256");
			md.update(generatedPin.getBytes());

			byte byteData[] = md.digest();

			hexString = new String(Hex.encodeHex(byteData));
			
		} catch (NoSuchAlgorithmException e) {
			LOGGER.error("An error occured."+e.getMessage());
			LOGGER.error("Exception >"+e);
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
        }
        if (newrawdata.toLowerCase().contains("pin")) {
			int i = newrawdata.indexOf(",\"pin");
			int y = newrawdata.lastIndexOf("\"");
			String chars = newrawdata.substring(i, y+1);
			newrawdata = newrawdata.replace(chars, "");
		}
        return newrawdata;
    }
	
	/**
	 * Print openam request without Password
	 * @param rawData
	 * @return
	 */
	public static String printOpenAMInfo(String rawData) {
		String newrawdata = rawData;
		if (rawData.toLowerCase().contains("userPassword".toLowerCase())) {
			int i = rawData.indexOf(",\"userPassword");
			int y = rawData.indexOf(",", i + 1);
			String chars = rawData.substring(i, y);
			newrawdata = rawData.replace(chars, "");
		}
		if (newrawdata.toLowerCase().contains("tmp_password".toLowerCase())) {
			int i = newrawdata.indexOf(",\"tmp_password");
			int y = newrawdata.indexOf(",", i + 1);
			String chars = newrawdata.substring(i, y);
			newrawdata = newrawdata.replace(chars, "");
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

	public static boolean mobileValidator(String mobile) {
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
	public static String mobileTransformation(String str) {
		String mobileNumber = str.trim();
		//str = mobileNumber.replaceAll("[\\(\\)\\-\\+\\.\\s]", "");
		str = mobileNumber.replaceAll("[^0-9]", "");
		int i = 0;
		while (str.charAt(i) == '0')
			i++;

		str = str.substring(i);

		if (str.startsWith(UserConstants.MOBILE_CHINA_CODE)) {
			str = str.substring(UserConstants.MOBILE_CHINA_CODE.length());
		}

		return str; // return in String
	}
	
	public static boolean passwordCheck(String strPass, String strEmail, String strMobile) {
		boolean checkEmailInPassword = false;
		boolean checkMobileInPassword = false;
		boolean checkPassword = false;

		if (null != strEmail && !strEmail.isEmpty()) {
			strEmail = strEmail.trim();
			for (int i = 0; i <= strEmail.length() - 4; i++) {
				String subStrEmail = strEmail.substring(i, i + 4);
				if (strPass.contains(subStrEmail)) {
					checkEmailInPassword = true;
					break;
				}
			}
		}

		if (null != strMobile && !strMobile.isEmpty()) {
			strMobile = strMobile.trim();
			if (strPass.contains(strMobile)) {
				checkMobileInPassword = true;
			}
		}

		checkPassword = checkEmailInPassword || checkMobileInPassword;
		return checkPassword;
	}
	
	public static String maskText(String strToken) {
		int strLength = 0, startPoint = 5, maskLength = 0;
		StringBuilder sbMaskString = new StringBuilder(maskLength);

		if (null != strToken && !strToken.isEmpty()) {
			if(strToken.trim().toLowerCase().contains("bearer")){
				startPoint = 12;
			}
			strLength = strToken.trim().length();
			maskLength = strLength - startPoint;

			for (int i = 0; i < maskLength; i++) {
				sbMaskString.append('X');
			}
			return strToken.trim().substring(0, startPoint) + sbMaskString.toString();
		}
		return null;
	}
	
	/**
	 * 
	 * @param uri
	 * @param realm
	 * @param deviceJsonData
	 * @return
	 * @throws ClientProtocolException
	 * @throws IOException
	 */
	public static Response executeHttpDeviceClient(String uri, String realm, String authId, String deviceOrOTPData, String fileName)
			throws ClientProtocolException, IOException {
		String jsonString = formJsonRequest(authId,deviceOrOTPData,fileName);
		
		if(null != jsonString && !jsonString.isEmpty()){
			HttpClient client = new DefaultHttpClient();
			HttpPost request = new HttpPost(uri + "/accessmanager/json/authenticate?realm=" + realm);
			StringEntity entity = new StringEntity(jsonString);
			request.setEntity(entity);
			request.setHeader("Accept", "application/json");
			request.setHeader("Content-type", "application/json");

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
		ErrorResponse errorResponse = new ErrorResponse();
		errorResponse.setStatus(ErrorCodeConstants.ERROR);
		errorResponse.setMessage("Device/OTP Info JSON string is unavailable. Please contact support team.");
		
		return Response.status(Response.Status.NO_CONTENT).entity(errorResponse).build();		
	}
	
	/**
	 * Used to form JSON request for stage 2 & 3 login
	 * Device data and OTP info
	 */
	private static String formJsonRequest(String part1, String part2, String fileName) {
		int startIndex = 0, endIndex = 0;
		StringBuilder contentBuilder = null;
		String content = null, formattedDeviceData = null;
		try {
			LOGGER.info("file name = "+fileName);
			File file = ResourceUtils.getFile("classpath:" + fileName);
			LOGGER.info("File Found : " + file.exists());

			// Read File Content
			if(file.exists()){
				contentBuilder = new StringBuilder(new String(Files.readAllBytes(file.toPath())));
				
				startIndex = contentBuilder.indexOf("<AuthenticateIdentification>");
				if(startIndex != -1 && startIndex > 0){
					endIndex = startIndex + "<AuthenticateIdentification>".length();
					contentBuilder.replace(startIndex, endIndex, part1);
				}
				startIndex = contentBuilder.indexOf("<SystemDeviceInfo>");
				if(startIndex != -1 && startIndex > 0){
					endIndex = startIndex + "<SystemDeviceInfo>".length();
					formattedDeviceData = formatEscapeCharacter(part2);
					contentBuilder.replace(startIndex, endIndex, formattedDeviceData);
				}
				startIndex = contentBuilder.indexOf("<OTPIdentification>");
				if(startIndex != -1 && startIndex > 0){
					endIndex = startIndex + "<OTPIdentification>".length();
					contentBuilder.replace(startIndex, endIndex, part2);
				}
				content = contentBuilder.toString();
			}
		} catch (IOException e) {
			// throw new FileNotFoundException("Caught Exception!!!!!!! " + e);
			LOGGER.error("Executing while formDeviceJson :: -> " + e.getMessage(), e);
		}
		return content;
	}
	
	
	public static String formatEscapeCharacter(String deviceData){
		String temp = deviceData.substring(1, deviceData.length()-1);
		temp = temp.replaceAll("\"", "\\\\\"");
		temp = "{"+temp+"}";
		return temp;
	}
	
	public static String removeEscapeCharacter(String deviceData){
		String temp = deviceData;
		if(deviceData.contains("\\")){
			temp = deviceData.replaceAll("\\\\", "");
		}
		return temp;
	}
	
	/*public static String printFileContent(){
		String content = null;
		StringBuilder contentBuilder = null;
		int startIndex;
		try {
			
			String fileName = "DeviceDataInformation.txt";
			
			File file = ResourceUtils.getFile("classpath:"+fileName);
			//File is found
			System.out.println("File Foundrrrr : " + file.exists());
			 
			//Read File Content
			if(file.exists()){
				contentBuilder = new StringBuilder(new String(Files.readAllBytes(file.toPath())));
			}
			startIndex = contentBuilder.indexOf("<AuthenticateIdentification>");
			System.out.println("startIndex = "+startIndex);
			startIndex = contentBuilder.indexOf("<OTPIdentification>");
			System.out.println(contentBuilder);
			System.out.println("startIndex2 = "+startIndex);
		} catch (IOException e) {
			e.printStackTrace();
		}
        return content;
	}*/
	
	
	
	/*public static void main(String[] args) {
		String abc = "{\\\"screen\\\":";
		System.out.println("abc= "+abc);
		String xyz = removeEscapeCharacter(abc);
		System.out.println("xyz= "+xyz);
		String str = "88ee604-735c-4b95-a0d8-41439bfbbd14";
		try {
			String modString  = maskText(str);
			System.out.println("modString = "+modString);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}*/
	
	/*public static void main(String[] args) {
		//String longvalue = generateHashValue("tY4MomqIwjg34932ZhTx651K38WJcZ");
		String mobileStr = mobileTransformation("+086-(508) 75*&^%$59.777");
		//String abc = "\"Hello \"world\" estr\"";
		//formatEscapeCharacter(abc);
		//printFileContent();
		System.out.println(mobileStr);
	}*/
	
}

package com.idms.service.util;

import static com.se.idms.util.UserConstants.AUTH_VERSION_HEADER;
import static com.se.idms.util.UserConstants.FR6_5Version;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.file.Files;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;

import javax.naming.InvalidNameException;
import javax.naming.SizeLimitExceededException;
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

import com.idms.product.model.OpenAmUser;
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
	
	public static Response executeHttpClient(String frVersion, String uri, String realm, String userName, String password)
			throws ClientProtocolException, IOException {

		HttpClient client = new DefaultHttpClient();
		HttpPost request = new HttpPost(uri + "/accessmanager/json/authenticate?realm=" + realm);
		//HttpPost request = new HttpPost(uri + "/accessmanager/json/authenticate?realm=" + realm+"&authIndexType=service&authIndexValue=NewMFA");
		request.setHeader("X-OpenAM-Username", userName);
		request.setHeader("X-OpenAM-Password", password);
		LOGGER.info("Execute http client for version : " + frVersion);
		if(FR6_5Version.equalsIgnoreCase(frVersion)) {
			request.setHeader("Accept-API-Version", AUTH_VERSION_HEADER);
		}
		HttpResponse response = client.execute(request);
		BufferedReader rd = new BufferedReader(new InputStreamReader(response.getEntity().getContent()));

		StringBuffer result = new StringBuffer();
		String line = "";
		try {
		while ((line = rd.readLine()) != null) {
			result.append(line);
		}
		if (401 == response.getStatusLine().getStatusCode()) {
			return Response.status(Response.Status.UNAUTHORIZED).entity(result.toString()).build();
		}
		}
		finally {
			rd.close();
			
		}
		return Response.status(response.getStatusLine().getStatusCode()).entity(result.toString()).build();
	}
	
	/**
	 * returns the UID with 36 random characters.
	 * @return
	 */
	public static String generateFedId(){		 
		
        String fedId = "cn00";
        fedId += RandomStringUtils.random(4, UserConstants.RANDOM_CHARS);
        fedId += '-' + RandomStringUtils.random(4, UserConstants.RANDOM_CHARS);
        fedId += '-' + RandomStringUtils.random(4, UserConstants.RANDOM_CHARS);
        fedId += '-' + RandomStringUtils.random(4, UserConstants.RANDOM_CHARS);
        fedId += '-' + RandomStringUtils.random(12, UserConstants.RANDOM_CHARS);
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
        if (rawData.toLowerCase().contains(",\"PinCode".toLowerCase())){
        	int i = rawData.indexOf(",\"PinCode");
        	int y = rawData.indexOf(",", i+1);
        	String chars = rawData.substring(i, y);
        	newrawdata = rawData.replace(chars, "");
        }
        if (newrawdata.toLowerCase().contains(",\"pin")) {
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
		str = mobileNumber.replaceAll("[^0-9]", "");
		int i = 0;
		while (str.charAt(i) == '0') {
			i++;
		}
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
	public static Response executeHttpDeviceClient(String frVersion, String uri, String realm, String authId, String deviceOrOTPData, String fileName)
			throws ClientProtocolException, IOException {
		String jsonString = formJsonRequest(authId,deviceOrOTPData,fileName);
		
		if(null != jsonString && !jsonString.isEmpty()){
			HttpClient client = new DefaultHttpClient();
			//HttpPost request = new HttpPost(uri + "/accessmanager/json/authenticate?realm=" + realm);
			HttpPost request = new HttpPost(uri + "/accessmanager/json/authenticate?realm=" + realm + "&authIndexType=service&authIndexValue=NewMFA");
			StringEntity entity = new StringEntity(jsonString);
			request.setEntity(entity);
			request.setHeader("Accept", "application/json");
			request.setHeader("Content-type", "application/json");
			LOGGER.info("Execute http device client for version : " + frVersion);
			if(FR6_5Version.equalsIgnoreCase(frVersion)) {
				request.setHeader("Accept-API-Version", AUTH_VERSION_HEADER);
			}
			HttpResponse response = client.execute(request);
			BufferedReader rd = new BufferedReader(new InputStreamReader(response.getEntity().getContent()));
			
			StringBuffer result = new StringBuffer();
			String line = "";
			try {
			while ((line = rd.readLine()) != null) {
				result.append(line);
			}
			if (401 == response.getStatusLine().getStatusCode()) {
				return Response.status(Response.Status.UNAUTHORIZED).entity(result.toString()).build();
			}}
			finally {
			rd.close();
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
	
	public static String getInvalidCount(String data){
		int startIndex = 0, endIndex = 0;
		String countValue = null;
		startIndex = data.indexOf("<InvalidCount>");
		endIndex = data.indexOf("</InvalidCount>");
		
		if(startIndex != -1 && startIndex > 0){
			startIndex = startIndex + "<InvalidCount>".length();
			countValue = data.substring(startIndex, endIndex);
		}
		return countValue;
	}
	
	public static String getPathString(String data) {
		String[] pathStrings = data.split("&");
		String finalPathString = "";
		for (String pathKeyVal : pathStrings) {
			String[] keyValue = pathKeyVal.split("=");
			if (keyValue.length > 1 && !keyValue[1].isEmpty() && !keyValue[1].equalsIgnoreCase("null")) {
				finalPathString = finalPathString + pathKeyVal + "&";
			}
		}
		if (null != finalPathString && !finalPathString.isEmpty()) {
			finalPathString = finalPathString.substring(0, finalPathString.lastIndexOf("&"));
		}
		return finalPathString;
	}
	
	public static List<String> getDataFromFile(String filename) throws Exception {
		BufferedReader reader;
		List<String> lines = new ArrayList<>();
		String line = null;
		reader = new BufferedReader(new FileReader(filename));
			
			try {
			while ((line = reader.readLine()) != null) {
				lines.add(line);
			}

		} catch (IOException e) {
			e.printStackTrace();
		}
		finally {
			reader.close();
		}
		checkFileData(lines);
		return lines;
	}

	public static void checkFileData(List<String> lines) throws Exception {
		String[] splitedFirstRecord = lines.get(0).split(",");
		if(!"federationID".equals(splitedFirstRecord[0])){
			throw new InvalidNameException("First column name should be federationID");
		}
		for (String value : splitedFirstRecord) {
			if (null == value || value.isEmpty()) {
				throw new NullPointerException("Header column is Null in CSV file");
			}
		}
		LOGGER.info("Header length = " + splitedFirstRecord.length);

		for (int i = 1; i < lines.size(); i++) {
			if (splitedFirstRecord.length < lines.get(i).split(",").length) {
				throw new SizeLimitExceededException("Row number " + (i + 1) + " has more column.");
			}
		}
	}
	
	public static List<OpenAmUser> buildUserObject(List<String> dataFile) {
		String headerNames = dataFile.get(0);
		String[] headers = headerNames.split(",");
		List<OpenAmUser> queryList = new ArrayList<OpenAmUser>();

		for(int i=1;i<dataFile.size();i++){
			String rowContent = dataFile.get(i);
			String[] rowContentInfo = rowContent.split(",");
			//String singleQuery = createQuery(headers,rowContentInfo);
			OpenAmUser oam = buildObject(headers,rowContentInfo);
			queryList.add(oam);
		}
		return queryList;
	}
	
	public static OpenAmUser buildObject(String[] header, String[] data) {
		OpenAmUser openAmUser = new OpenAmUser();
		/*
		 * String singleQuery = "{\"userRecord\":{\""; for(int
		 * i=0;i<header.length;i++){ singleQuery =
		 * singleQuery+header[i]+"\":\""+data[i]+"\",\""; } LOGGER.info(
		 * "Query = "+singleQuery); singleQuery =
		 * singleQuery.substring(0,singleQuery.length()-2).concat("}}");
		 * LOGGER.info("again query = "+singleQuery); return singleQuery;
		 */

		for (int i = 0; i < header.length; i++) {
			if (header[i].equals("mail")) {
				if (data.length < i+1) {
					openAmUser.setMail(null);
				} else {
					openAmUser.setMail((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("mobile")) {
				if (data.length < i+1) {
					openAmUser.setMobile(null);
				} else {
					openAmUser.setMobile((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("givenName")) {
				if (data.length < i+1) {
					openAmUser.setGivenName(null);
				} else {
					openAmUser.setGivenName((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("sn")) {
				if (data.length < i+1) {
					openAmUser.setSn(null);
				} else {
					openAmUser.setSn((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("c")) {
				if (data.length < i+1) {
					openAmUser.setC(null);
				} else {
					openAmUser.setC((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("federationID")) {
				if (data.length < i+1) {
					openAmUser.setFederationID(null);
				} else {
					openAmUser.setFederationID((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("preferredlanguage")) {
				if (data.length < i+1) {
					openAmUser.setPreferredlanguage(null);
				} else {
					openAmUser.setPreferredlanguage((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("additionalInfo")) {
				if (data.length < i+1) {
					openAmUser.setAdditionalInfo(null);
				} else {
					openAmUser.setAdditionalInfo((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("jobDescription")) {
				if (data.length < i+1) {
					openAmUser.setJobDescription(null);
				} else {
					openAmUser.setJobDescription((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("jobFunction")) {
				if (data.length < i+1) {
					openAmUser.setJobFunction(null);
				} else {
					openAmUser.setJobFunction((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("title")) {
				if (data.length < i+1) {
					openAmUser.setTitle(null);
				} else {
					openAmUser.setTitle((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("l")) {
				if (data.length < i+1) {
					openAmUser.setL(null);
				} else {
					openAmUser.setL((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("middleName")) {
				if (data.length < i+1) {
					openAmUser.setMiddleName(null);
				} else {
					openAmUser.setMiddleName((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("postOfficeBox")) {
				if (data.length < i+1) {
					openAmUser.setPostOfficeBox(null);
				} else {
					openAmUser.setPostOfficeBox((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("initials")) {
				if (data.length < i+1) {
					openAmUser.setInitials(null);
				} else {
					openAmUser.setInitials((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("street")) {
				if (data.length < i+1) {
					openAmUser.setStreet(null);
				} else {
					openAmUser.setStreet((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("postalCode")) {
				if (data.length < i+1) {
					openAmUser.setPostalCode(null);
				} else {
					openAmUser.setPostalCode((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("st")) {
				if (data.length < i+1) {
					openAmUser.setSt(null);
				} else {
					openAmUser.setSt((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("telephoneNumber")) {
				if (data.length < i+1) {
					openAmUser.setTelephoneNumber(null);
				} else {
					openAmUser.setTelephoneNumber((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("fax")) {
				if (data.length < i+1) {
					openAmUser.setFax(null);
				} else {
					openAmUser.setFax((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("county")) {
				if (data.length < i+1) {
					openAmUser.setCounty(null);
				} else {
					openAmUser.setCounty((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("channel")) {
				if (data.length < i+1) {
					openAmUser.setChannel(null);
				} else {
					openAmUser.setChannel((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("subchannel")) {
				if (data.length < i+1) {
					openAmUser.setSubchannel(null);
				} else {
					openAmUser.setSubchannel((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("primaryContact")) {
				if (data.length < i+1) {
					openAmUser.setPrimaryContact(null);
				} else {
					openAmUser.setPrimaryContact((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("contactId")) {
				if (data.length < i+1) {
					openAmUser.setContactId(null);
				} else {
					openAmUser.setContactId((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("companyName")) {
				if (data.length < i+1) {
					openAmUser.setCompanyName(null);
				} else {
					openAmUser.setCompanyName((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("companyCountry")) {
				if (data.length < i+1) {
					openAmUser.setCompanyCountry(null);
				} else {
					openAmUser.setCompanyCountry((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("currency")) {
				if (data.length < i+1) {
					openAmUser.setCurrency(null);
				} else {
					openAmUser.setCurrency((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("iam1")) {
				if (data.length < i+1) {
					openAmUser.setIam1(null);
				} else {
					openAmUser.setIam1((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("iam2")) {
				if (data.length < i+1) {
					openAmUser.setIam2(null);
				} else {
					openAmUser.setIam2((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("companyCity")) {
				if (data.length < i+1) {
					openAmUser.setCompanyCity(null);
				} else {
					openAmUser.setCompanyCity((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("industrySegment")) {
				if (data.length < i+1) {
					openAmUser.setIndustrySegment(null);
				} else {
					openAmUser.setIndustrySegment((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("companyPostalCode")) {
				if (data.length < i+1) {
					openAmUser.setCompanyPostalCode(null);
				} else {
					openAmUser.setCompanyPostalCode((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("companyPostOfficeBox")) {
				if (data.length < i+1) {
					openAmUser.setCompanyPostOfficeBox(null);
				} else {
					openAmUser.setCompanyPostOfficeBox((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("companyState")) {
				if (data.length < i+1) {
					openAmUser.setCompanyState(null);
				} else {
					openAmUser.setCompanyState((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("companyStreet")) {
				if (data.length < i+1) {
					openAmUser.setCompanyStreet(null);
				} else {
					openAmUser.setCompanyStreet((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("headquarters")) {
				if (data.length < i+1) {
					openAmUser.setHeadquarters(null);
				} else {
					openAmUser.setHeadquarters((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("companyAdditionalInfo")) {
				if (data.length < i+1) {
					openAmUser.setCompanyAdditionalInfo(null);
				} else {
					openAmUser.setCompanyAdditionalInfo((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("companyCounty")) {
				if (data.length < i+1) {
					openAmUser.setCompanyCounty(null);
				} else {
					openAmUser.setCompanyCounty((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("companyWebSite")) {
				if (data.length < i+1) {
					openAmUser.setCompanyWebSite(null);
				} else {
					openAmUser.setCompanyWebSite((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("industries")) {
				if (data.length < i+1) {
					openAmUser.setIndustries(null);
				} else {
					openAmUser.setIndustries((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("employeeSize")) {
				if (data.length < i+1) {
					openAmUser.setEmployeeSize(null);
				} else {
					openAmUser.setEmployeeSize((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("taxID")) {
				if (data.length < i+1) {
					openAmUser.setTaxID(null);
				} else {
					openAmUser.setTaxID((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("annualRevenue")) {
				if (data.length < i+1) {
					openAmUser.setAnnualRevenue(null);
				} else {
					openAmUser.setAnnualRevenue((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("IDMSWorkPhone__c")) {
				if (data.length < i+1) {
					openAmUser.setIDMSWorkPhone__c(null);
				} else {
					openAmUser.setIDMSWorkPhone__c((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else if (header[i].equals("companyFederatedID")) {
				if (data.length < i+1) {
					openAmUser.setCompanyFederatedID(null);
				} else {
					openAmUser.setCompanyFederatedID((null != data[i] && !data[i].isEmpty()) ? data[i] : null);
				}
			} else {
				throw new IllegalArgumentException(header[i] + " is not a valid field in file");
			}
		}
		return openAmUser;
	}
		

		
}

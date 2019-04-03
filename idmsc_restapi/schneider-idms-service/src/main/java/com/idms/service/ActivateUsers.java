package com.idms.service;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSession;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Option;

public class ActivateUsers {

	/**
	 * Logger instance.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(ActivateUsers.class);

	public static void main(String[] args) {

		 String hostName = "https://identity-stg.schneider-electric.com";
		 String csvFileName =		 "C:\\JsonRequestURLs\\GoDigitalCert\\UserData.csv";
		//String hostName = args[0];
		//String csvFileName = args[1];
		try {
			ActivateUsers user = new ActivateUsers();

			if ((null != hostName && !hostName.isEmpty()) && (null != csvFileName && !csvFileName.isEmpty())) {
				user.readDataFromFile(hostName, csvFileName);
			} else {
				LOGGER.info("HostName and filePath should not be empty");
			}

		} catch (Exception e) {



		}

	}

	public void readDataFromFile(String hostname, String csvFile) throws NoSuchAlgorithmException {
		BufferedReader br = null;
		// csvFile = "C:\\JsonRequestURLs\\GoDigitalCert\\UserData.csv";
		String user = "";
		String jsonString = "";
		String inputRequesr = "";
		DocumentContext productDocCtx = null;
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		StringBuffer dataNotFount = new StringBuffer();

		try {

			br = new BufferedReader(new FileReader(csvFile));
			while ((user = br.readLine()) != null) {

				LOGGER.info("User Is : " + user);
				try {
					jsonString = executeRequestApi(hostname + "/services/apexrest/GetIDMSUser/" + user, "GET", null);
				} catch (Exception exp) {
					// TODO Auto-generated catch block
					LOGGER.error("An error occured."+exp.getMessage());		
					LOGGER.error("Exception >"+exp);}

				if (!"{}".equals(jsonString.toString())) {
					inputRequesr = buildUserRequest(productDocCtx, conf, jsonString);

					try {
						jsonString = executeRequestApi(hostname + "/services/apexrest/ActivateUser", "PUT", inputRequesr);
					} catch (Exception e) {
						// TODO Auto-generated catch block
						LOGGER.error("An error occured."+e.getMessage());
						LOGGER.error("Exception >"+e);
					}
				} else {
					dataNotFount.append(user);
				}
			}

			if(!dataNotFount.toString().isEmpty()){
				LOGGER.info("The Following users not activated : " + dataNotFount);
			}else{
				LOGGER.info("Users Successfully Activated :: ");
			}

		} catch (FileNotFoundException e) {
			LOGGER.error("An error occured."+e.getMessage());
			LOGGER.error("Exception >"+e);
		} catch (IOException e) {
			LOGGER.error("An error occured."+e.getMessage());
			LOGGER.error("Exception >"+e);
		} finally {
			if (br != null) {
				try {
					br.close();
				} catch (IOException e) {
					LOGGER.error("An error occured."+e.getMessage());
					LOGGER.error("Exception >"+e);
				}
			}
		}

	}

	public String buildUserRequest(DocumentContext productDocCtx, Configuration conf, String jsonString) {

		productDocCtx = JsonPath.using(conf).parse(jsonString);
		String regSource = productDocCtx.read("$.regSource");

		String fedId = null != productDocCtx.read("$.fedId") ? getValue(productDocCtx.read("$.fedId").toString())
				: null;

		String userId = null != productDocCtx.read("$.userId") ? getValue(productDocCtx.read("$.userId").toString())
				: null;
		String input = "{\"UserRecord\":{\"Id\": \"" + userId + "\",\"IDMS_Federated_ID__c\": \"" + fedId
				+ "\",\"IDMS_Registration_Source__c\": \"" + regSource + "\"}}";

		return input;
	}

	public String executeRequestApi(String url, String requestType, String inputData) throws Exception {
		String output = "";
		String jsonString = "";
		try {
			
			 // Create a trust manager that does not validate certificate chains
	        TrustManager[] trustAllCerts = new TrustManager[] {new X509TrustManager() {
	                public java.security.cert.X509Certificate[] getAcceptedIssuers() {
	                    return null;
	                }
	                public void checkClientTrusted(X509Certificate[] certs, String authType) {
	                }
	                public void checkServerTrusted(X509Certificate[] certs, String authType) {
	                }
	            }
	        };
	 
	        // Install the all-trusting trust manager
	        SSLContext sc = SSLContext.getInstance("SSL");
	        sc.init(null, trustAllCerts, new java.security.SecureRandom());
	        HttpsURLConnection.setDefaultSSLSocketFactory(sc.getSocketFactory());
	 
	        // Create all-trusting host name verifier
	        HostnameVerifier allHostsValid = new HostnameVerifier() {
	            public boolean verify(String hostname, SSLSession session) {
	                return true;
	            }
	        };
	 
	        // Install the all-trusting host verifier
	        HttpsURLConnection.setDefaultHostnameVerifier(allHostsValid);				

			URL urlLink = new URL(url);
			HttpURLConnection conn = (HttpURLConnection) urlLink.openConnection();
			conn.setRequestMethod(requestType);
			conn.setDoInput(true);
			conn.setDoOutput(true);
			conn.setUseCaches(false);
			conn.setRequestProperty("Accept", "application/json");
			conn.setRequestProperty("Content-Type", "application/json");
			conn.connect();

			if (null != inputData) {
				OutputStream os = conn.getOutputStream();
				os.write(inputData.getBytes());
				os.flush();

			}

			if (conn.getResponseCode() != 200) {
				throw new RuntimeException("Failed : HTTP error code : " + conn.getResponseCode());
			}

			BufferedReader br = new BufferedReader(new InputStreamReader((conn.getInputStream())));

			LOGGER.info("Output from Server .... \n");
			while ((output = br.readLine()) != null) {
				LOGGER.info(output);
				jsonString = output;
			}

			conn.disconnect();
			return jsonString;

		} catch (MalformedURLException e) {
			LOGGER.error("An error occured."+e.getMessage());
			LOGGER.error("Exception >"+e);
		} catch (IOException e) {
			LOGGER.error("An error occured."+e.getMessage());
			LOGGER.error("Exception >"+e);
		}
		return jsonString;
	}

	public static String getValue(String key) {
		if (null != key) {
			if (!key.contains("[")) {
				return key;
			}
			if (key.contains("[\"[]")) {
				return "[]";
			}
			if (key.contains("[\"[(") || key.contains("[\"[nul,(") || key.contains("[\"[null,")) {
				return key.substring(key.indexOf("[\"[") + 3, key.lastIndexOf("]\""));
			}

			if (key.contains("[\"[")) {
				return key.substring(key.indexOf("[\"[") + 3, key.lastIndexOf("]\""));
			}
			int beginIndex = key.indexOf('[') + 1;
			int endIndex = key.indexOf(']');
			String preValue = key.substring(beginIndex, endIndex);
			return preValue.substring(preValue.indexOf('\"') + 1, preValue.lastIndexOf('\"'));
		}
		return "";
	}
}

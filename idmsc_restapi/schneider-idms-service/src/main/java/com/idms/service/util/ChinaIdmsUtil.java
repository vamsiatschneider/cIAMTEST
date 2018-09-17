package com.idms.service.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import javax.ws.rs.core.Response;

import org.apache.commons.codec.binary.Hex;
import org.apache.commons.lang3.RandomStringUtils;
import org.apache.http.HttpResponse;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.DefaultHttpClient;

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
	
	/*public static void main(String[] args) {
		String longvalue = generateHashValue("tY4MomqIwjg34932ZhTx651K38WJcZ");
		System.out.println(longvalue);
	}*/
	
}

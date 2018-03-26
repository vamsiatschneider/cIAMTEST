package com.idms.service.util;

import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

import org.apache.commons.codec.binary.Hex;
import org.springframework.stereotype.Component;

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
}

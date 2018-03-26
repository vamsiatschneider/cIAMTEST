package com.se.idms.util;

import java.io.FileInputStream;
import java.security.Key;
import java.security.KeyPair;
import java.security.KeyStore;
import java.security.PrivateKey;
import java.security.PublicKey;
import java.security.Signature;
import java.security.cert.Certificate;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;

import org.springframework.util.ResourceUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.idms.model.TransliteratorRequest;

import sun.misc.BASE64Encoder;

/**
 * The Saml assertion token generator class.
 * 
 * @author Aravindh Kumar
 *
 */
public class SamlAssertionTokenGenerator {

	// This method encrypts the signed certificate.
	public static String getSamlAssertionToken(String fedId, String vnew) throws Exception {

		Date date = new Date();
		SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");
		String strDate = formatter.format(date);
		// the data is of the form :
		// c5e601cf-864d-4a85-8343-ef2c8a259690;2017-06-29 10:06:05;3
		String data = fedId + ";" + strDate + ";" + vnew;
		String certName = "bfo_idms_uims_sync_sit";
		String algorithm = "SHA1WithRSA";
		String encrypted = signNverify(algorithm, data, certName);

		return data + ";" + encrypted;
	}

	public static KeyPair getKeyPairFromKeyStore(String alias) throws Exception {

		FileInputStream is = new FileInputStream(ResourceUtils.getFile("classpath:sit_keystore.jks"));

		KeyStore keystore = KeyStore.getInstance(KeyStore.getDefaultType());
		keystore.load(is, "IDMS_SIT_2017".toCharArray());

		Key key = keystore.getKey(alias, "IDMS_SIT_2017".toCharArray());
		if (key instanceof PrivateKey) {
			// Get certificate of public key
			Certificate cert = keystore.getCertificate(alias);

			// Get public key
			PublicKey publicKey = cert.getPublicKey();

			// Return a key pair
			return new KeyPair(publicKey, (PrivateKey) key);
		}
		return null;
	}

	@SuppressWarnings("restriction")
	public static String signNverify(String algorithm, String data, String certName) throws Exception {
		KeyPair keyPair = getKeyPairFromKeyStore(certName);

		byte[] dataInBytes = data.getBytes("UTF-8");

		Signature sig = Signature.getInstance("SHA1WithRSA");
		sig.initSign(keyPair.getPrivate());
		sig.update(dataInBytes);
		byte[] signatureBytes = sig.sign();

		sig.initVerify(keyPair.getPublic());
		sig.update(dataInBytes);

		sig.verify(signatureBytes);

		return new BASE64Encoder().encode(signatureBytes);

	}


}

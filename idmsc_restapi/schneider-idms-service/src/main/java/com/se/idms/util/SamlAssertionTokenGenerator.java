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
import java.util.Date;

import org.springframework.util.ResourceUtils;

import sun.misc.BASE64Encoder;

/**
 * SAML assertion token generator
 * 
 * @author Aravindh Kumar
 *
 */

public class SamlAssertionTokenGenerator {
	
	private final SimpleDateFormat dateFormatter = new SimpleDateFormat("yyyy-MM-dd hh:mm:ss");
	
	@Value("${keystore.samlAssertionSigning.path}")
	private String samlAssertionSigningKeystore;
	
	@Value("${keystore.samlAssertionSigning.keystore.password}")
	private String samlAssertionKeystorePassword;
	
	@Value("${keystore.samlAssertionSigning.keystore.privateKey.password}")
	private String samlAssertionKeyPassword;
	
	@Value("${keystore.samlAssertionSigning.keystore.certAlias}")
	private String samlAssertionSigningCert;
	
	@Value("${crypto.algo.samlAssertionSigning}")
	private String samlAssertionSigningAlgo;
	

	/**
	 * Generates the SAML Assertion token, using the private key loaded from the Keystore
	 * Data input format for signing:
	 * <FederationId>;<Date>;vnew
	 * 
	 * Example:
	 * c5e601cf-864d-4a85-8343-ef2c8a259690;2017-06-29 10:06:05;3
	 * 
	 * @param fedId
	 * @param vnew
	 * @return
	 * @throws Exception
	 */
	
	public static String getSamlAssertionToken(String fedId, String vnew) throws Exception {
		Date date = new Date();
		String strDate = dateFormatter.format(date);
		String data = fedId + ";" + strDate + ";" + vnew;
		String encrypted = signNverify(samlAssertionSigningAlgo, data, samlAssertionSigningCert);

		return data + ";" + encrypted;
	}

	/**
	 * Loads the keystore and returns the private key pair
	 * 
	 * @param alias
	 * @return
	 * @throws Exception
	 */
	
	private static KeyPair getKeyPairFromKeyStore(String alias) throws Exception {
		FileInputStream is = new FileInputStream(ResourceUtils.getFile(this.samlAssertionSigningKeystore));

		KeyStore keystore = KeyStore.getInstance(KeyStore.getDefaultType());
		keystore.load(is, samlAssertionKeystorePassword.toCharArray());

		Key key = keystore.getKey(alias, samlAssertionKeyPassword.toCharArray());
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
	private static String signNverify(String algorithm, String data, String certName) throws Exception {
		KeyPair keyPair = getKeyPairFromKeyStore(certName);

		byte[] dataInBytes = data.getBytes("UTF-8");

		Signature sig = Signature.getInstance(algorithm);
		sig.initSign(keyPair.getPrivate());
		sig.update(dataInBytes);
		byte[] signatureBytes = sig.sign();

		sig.initVerify(keyPair.getPublic());
		sig.update(dataInBytes);

		sig.verify(signatureBytes);

		return new BASE64Encoder().encode(signatureBytes);
	}

	public static void main(String args[]) throws Exception{
		System.out.println(getSamlAssertionToken("7abb7286-60c7-4f58-a856-8b55c4b45d9a","1"));
	}

}

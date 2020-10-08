package com.idms.service;

import static org.junit.Assert.assertEquals;

import java.net.URL;
import java.util.UUID;

import javax.xml.namespace.QName;
import javax.xml.ws.Service;

import org.junit.Test;

import com.se.uims.usermanager.UserManagerUIMSV22;
import com.uims.authenticatedUsermanager.AuthenticatedUserManagerUIMSV22;
import com.uims.authenticatedUsermanager.CreatedIdentityReport;
import com.uims.authenticatedUsermanager.UserV6;

/**
 * Test to validate soap UIMS services
 * 
 * @author Aravindh Kumar
 *
 */
public class SoapTest {

	private UIMSAuthenticatedUserManagerSoapService authenticatedUserManagerSoapService = null;

	public SoapTest() {
		authenticatedUserManagerSoapService = new UIMSAuthenticatedUserManagerSoapService();
	}

	@Test
	public void createUser() throws Exception {
		AuthenticatedUserManagerUIMSV22	 authenticatedUserManager = authenticatedUserManagerSoapService
				.getAuthenticatedUserManager();


		UserV6 user = new UserV6();
		user.setFederatedID("123457");
		String email = UUID.randomUUID().toString() + "@mailinator.com";
		user.setEmail(email);
		user.setFirstName("Arvind");
		user.setLastName("kumar");
		user.setLanguageCode("zh");
		user.setCountryCode("CN");

		CreatedIdentityReport createIdentity = authenticatedUserManager.createIdentity("IDMSAdmin", user, null);
		assertEquals(true, createIdentity.isHasBeenCreated());
	}

	public static void getUser() throws Exception {
		/*PMD Violation UnusedLocalVariable
		 * URL url = new URL(
		 * "https://ims-int.btsec.dev.schneider-electric.com/IMS-UserManager/UIMSV2/UserManager?wsdl"
		 * );
		 * 
		 * QName qname = new QName("http://uimsv2.impl.service.ims.schneider.com/",
		 * "UIMSV2_UserManagement");
		 * 
		 * Service service = Service.create(url, qname);
		 * 
		 * UserManagerUIMSV22 userManagerUIMSV22 =
		 * service.getPort(UserManagerUIMSV22.class);
		 * 
		 * 
		 * String callerFid = "IDMSAdmin"; String samlAssertionOrToken =
		 * "9aca36f2-45dd-468a-9a5a-e30afd05eb78;2017-07-20 11:35:23;1;hbDnQgh3oXPJzkJeer7DwSuYd/2aezQka4lgejQdR7Q0ubFtazxGV7FD/AqJOyJQKLWZyniKnGYyAEfMVwjfvZlxrOd476xq/8UBP4YinI0pFJZMbxL4J6p0h7zGjsHu2iDC+hJTNDtjFJAWRpDhnt7z8pADDgbwdkZSTLGCAXNaDX4aTAWomeX+f/DkH+Ud/juutV9W8gvFVT9tfkOkaVjnNMjHJvf7ycvSiV9I15gYnsrQzMWJKudgu93HbCF18zUgOBlL6o3vC9BWcwWIDQPfqO4/6tY5D6gZT1RnUftYunx0x9WBiYImIznMsAABi8tl64GeOBervZ2cnEDasw=="
		 * ;
		 */
	}
}

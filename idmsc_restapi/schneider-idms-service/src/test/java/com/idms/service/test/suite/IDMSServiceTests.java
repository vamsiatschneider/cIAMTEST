package com.idms.service.test.suite;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.idms.service.ActivateUserTest;
import com.idms.service.CheckUsersExistsTest;
import com.idms.service.GetUserTest;
import com.idms.service.IdmsIdpChainingTest;
import com.idms.service.ResendChangeEmailTest;
import com.idms.service.ResendPinTest;
import com.idms.service.ResendRegEmailTest;
import com.idms.service.SendInvitationTest;
import com.idms.service.SetPasswordTest;
import com.idms.service.UpdatePasswordTest;

@RunWith(Suite.class)
@SuiteClasses({
	ActivateUserTest.class,
	CheckUsersExistsTest.class,
	GetUserTest.class,
	IdmsIdpChainingTest.class,
	ResendChangeEmailTest.class,
	ResendPinTest.class,
	ResendRegEmailTest.class,
	SendInvitationTest.class,
	SetPasswordTest.class,
	UpdatePasswordTest.class
})
public class IDMSServiceTests {
	private static final Logger LOGGER = LoggerFactory.getLogger(IDMSServiceTests.class);

	public IDMSServiceTests() {
		LOGGER.info("Running Junit test Suite");
	}
}
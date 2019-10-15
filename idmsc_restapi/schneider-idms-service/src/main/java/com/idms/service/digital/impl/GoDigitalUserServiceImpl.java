package com.idms.service.digital.impl;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;

import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Lazy;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.stereotype.Service;

import com.github.rholder.retry.RetryException;
import com.github.rholder.retry.Retryer;
import com.github.rholder.retry.RetryerBuilder;
import com.github.rholder.retry.StopStrategies;
import com.google.common.base.Predicates;
import com.idms.product.client.GoDigitalServiceApi;
import com.idms.service.SendEmail;
import com.idms.service.digital.GoDigitalUserService;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Option;

@Service("goDigitalUserService")
@EnableAsync
public class GoDigitalUserServiceImpl implements GoDigitalUserService {

	/**
	 * Logger instance.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(GoDigitalUserServiceImpl.class);
	
	private static final Logger GoDigitalLog = LoggerFactory.getLogger("goDigitalLogger");

	/**
	 * Service to fetch information about {@link Product}s.
	 */

	@Inject
	private GoDigitalServiceApi goDigitalServiceApi;
	
	private SendEmail sendEmail;
	
	@Autowired
	@Lazy
	public void setSendEmail(SendEmail sendEmail) {
		this.sendEmail = sendEmail;
	}

	@Value("${fromUserName}")
	private String fromUserName;
	
	@Value("${supportUser}")
	private String supportUser;
	
	String serviceResponse = "";

	@Async
	@Override
	public void goDigitalUserRegistration(String userRegistrationInfoRequest) {

		boolean isGodFail = false;
		DocumentContext productDocCtx = null;
		String response = null;
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		LOGGER.info("GoDigitalUserServiceImpl :goDigitalUserRegistration ");
		
		GoDigitalLog.info("goDigitalUserRegistration :Request :  " +userRegistrationInfoRequest);
		try {

			Callable<Boolean> callableUser = new Callable<Boolean>() {
				public Boolean call() throws Exception {
					serviceResponse = goDigitalServiceApi.userRegistrationInfo(userRegistrationInfoRequest);
					
					GoDigitalLog.info("goDigitalUserRegistration :serviceResponse :  " +serviceResponse);
					LOGGER.info("goDigitalUserRegistration  Response is :: " + serviceResponse);
					return true;
				}
			};

			Retryer<Boolean> retryer = RetryerBuilder.<Boolean> newBuilder()
					.retryIfResult(Predicates.<Boolean> isNull())
					.retryIfExceptionOfType(Exception.class)
					.retryIfRuntimeException().withStopStrategy(StopStrategies.stopAfterAttempt(5)).build();
			try {
				retryer.call(callableUser);
			} catch (RetryException e) {
				isGodFail=true;
				LOGGER.error("Retry failed while calling the GoDigitalServiceApi create user::" + e.getMessage());
				LOGGER.error("Exception >"+e);
				GoDigitalLog.error("Retry failed while calling the GoDigitalServiceApi create user::" + e.getMessage());
				LOGGER.error("ECODE-GODIGIT-USER-REG-RETRY-ERR : Retry error");
			} catch (ExecutionException e) {
				isGodFail=true;
				LOGGER.error("Retry failed while calling the GoDigitalServiceApi create user::ExecutionException " + e.getMessage());
				LOGGER.error("Exception >"+e);
				GoDigitalLog.error("Retry failed while calling the GoDigitalServiceApi create user::ExecutionException " + e.getMessage());
				LOGGER.error("ECODE-GODIGIT-USER-REG-EXEC-ERR : Execution error");
			}
			
			if (!serviceResponse.isEmpty()) {
				productDocCtx = JsonPath.using(conf).parse(serviceResponse);
				response = productDocCtx.read("$.userRegistrationInfoResponse.success");
			}
			LOGGER.info("response is :: "+response);
			if((response == null ||"false".equalsIgnoreCase(response)) || (isGodFail) ){
				LOGGER.info(":::::::isGodFail value is::::::::"+isGodFail);
				GoDigitalLog.info("GoDigitalUser Create User Failed  ");
				productDocCtx = JsonPath.using(conf).parse(userRegistrationInfoRequest);
				String toAddress =  productDocCtx.read("$.userRegistrationInfoRequest.userDetails.EMAIL");
				LOGGER.info(""+productDocCtx.read("$.userRegistrationInfoRequest.authentication.EMAIL"));
				sendEmail.emailReadyToSendEmail(supportUser, fromUserName, "GoDigitalUser Registration Failed", userRegistrationInfoRequest);
			}else{
				GoDigitalLog.info("GoDigitalUser Created Successfully  ");
			}
			serviceResponse = "";
		} catch (Exception e) {
			//isGodFail=true;
			LOGGER.error("Executing while goDigitalUserRegistration :: -> " + e.getMessage());
			LOGGER.error("Exception >"+e);
			GoDigitalLog.error("Executing while goDigitalUserRegistration :: -> " + e.getMessage());
			LOGGER.error("ECODE-GODIGIT-USER-REG-GEN-ERR : Generic error");
		}
		
	}

	public void setFromUserName(String fromUserName) {
		this.fromUserName = fromUserName;
	}

	public void setSupportUser(String supportUser) {
		this.supportUser = supportUser;
	}

	public String getFromUserName() {
		return fromUserName;
	}

	public String getSupportUser() {
		return supportUser;
	}

}

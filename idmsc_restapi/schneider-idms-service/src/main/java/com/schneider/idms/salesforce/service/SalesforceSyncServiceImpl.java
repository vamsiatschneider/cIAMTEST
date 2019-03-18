package com.schneider.idms.salesforce.service;

import java.io.IOException;
import java.io.InputStream;
import java.util.Calendar;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import javax.inject.Inject;
import javax.ws.rs.core.Response;

import org.apache.cxf.helpers.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;

import com.github.rholder.retry.RetryException;
import com.github.rholder.retry.Retryer;
import com.github.rholder.retry.RetryerBuilder;
import com.github.rholder.retry.StopStrategies;
import com.github.rholder.retry.WaitStrategies;
import com.google.common.base.Predicates;
import com.idms.product.client.SalesForceService;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Option;
import com.se.idms.util.UserConstants;

/**
 * 
 * @author SESA453215
 *
 */

@org.springframework.stereotype.Service("saleforceSynService")
public class SalesforceSyncServiceImpl {

	/**
	 * Logger instance.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(SalesforceSyncServiceImpl.class);

	//private static final Logger UIMSLOGGER = LoggerFactory.getLogger("uimsLogger");

	@Inject
	private SalesForceService salesForceService;

	@Value("${salesForceClientId}")
	private String salesForceClientId;

	@Value("${salesForceClientSecret}")
	private String salesForceClientSecret;

	@Value("${salesForceUserName}")
	private String salesForceUserName;

	@Value("${salesForcePassword}")
	private String salesForcePassword;
	
	@Value("${sftokentimeinminute}")
	private String sftokentimeinminute;

	private String bfoAuthorization = null;

	Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();

	DocumentContext productDocCtx = null;
	String bfoAuthorizationToken = null;
	public static Map<String,String> sftokenMap = new HashMap<String,String>();


	public void populatePrmActivationDate(String federationId,String appName) {

		try {
			Callable<Boolean> datePopulated = new Callable<Boolean>() {
				public Boolean call() throws Exception {

					//LOGGER.info("going to call getSaleforceToken()");
					String jsonData = "{" + "\"federationId\": \"" + federationId + "\",\"appName\": \"" + appName + "\"" + "}";
					String salesForceToken = getSaleforceToken();
					//LOGGER.info("getSaleforceToken() call finsihed");
					//LOGGER.info("Request sending to  salesForceService.populateActivationDate : " + jsonData);
					LOGGER.info("Start: populateActivationDate() of SalesForceService");
					Response activationResponse = salesForceService
							.populateActivationDate(UserConstants.ACCEPT_TYPE_APP_JSON, salesForceToken, jsonData);
					LOGGER.info("End: populateActivationDate() of SalesForceService finished");
					LOGGER.info("populateActivationDate Status :: " + activationResponse.getStatus());
					try {
						if (200 != activationResponse.getStatus()) {

							LOGGER.error("Failed to populate the activate date on PRM :: populateActivationDate -> "
									+ IOUtils.toString((InputStream) activationResponse.getEntity()));

						} else {
							LOGGER.info(
									"Successfully populated the activation date on PRM :: populateActivationDate -> "
											+ IOUtils.toString((InputStream) activationResponse.getEntity()));
						}
					} catch (IOException e) {
						LOGGER.error("Failed to populate the activate date on PRM :: populateActivationDate -> "
								+ e.getMessage());
					}
					return true;
				}
			};

			Retryer<Boolean> retryer = RetryerBuilder.<Boolean> newBuilder()
					.retryIfResult(Predicates.<Boolean> isNull()).retryIfExceptionOfType(Exception.class)
					.retryIfRuntimeException().withStopStrategy(StopStrategies.stopAfterAttempt(3)).build();
			try {
				retryer.call(datePopulated);
			} catch (RetryException e) {
				LOGGER.error("RetryException in populatePrmActivationDate() of Salesforce::" + e.getMessage());

			} catch (ExecutionException e) {
				LOGGER.error("ExecutionException in populatePrmActivationDate() of Salesforce::"
						+ e.getMessage());
			}

		} catch (Exception e) {
			LOGGER.error("Exception in populatePrmActivationDate()::" + e.getMessage());
		}

	}

	private String getSaleforceToken() {

		//DocumentContext productDocCtx = null;
		//Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();

		/*LOGGER.info("getSalesForceToken : => " + "PASSWORD_GRANT_TYPE : " + UserConstants.PR_GRANT_TYPE
				+ " salesForceClientId: " + salesForceClientId + " salesForceClientSecret :" + salesForceClientSecret
				+ " salesForceUserName: " + salesForceUserName + " salesForcePassword :" + salesForcePassword);*/
		String bfoAuthorizationToken = getSFToken();
		/*conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		productDocCtx = JsonPath.using(conf).parse(bfoAuthorization);
		String bfoAuthorizationToken = productDocCtx.read("$.access_token");*/

		return  "Bearer " + bfoAuthorizationToken;
	}

	/**
	 * 
	 * @return
	 */
	public String getSFToken(){
		LOGGER.info("Entered getSFToken() -> Start");
		String sfToken = null;
		String[] authIdTime = null;

		if(sftokenMap.size()>0){
			String mapValue = sftokenMap.get("SalesForceToken");
			authIdTime = mapValue.split("::");
			long storedTimeMilli = Long.valueOf(authIdTime[1]).longValue();
			if(storedTimeMilli > System.currentTimeMillis()){
				sfToken = authIdTime[0];
			} else {
				sfToken = getGeneratedSFToken();
			}
		} else {
			sfToken = getGeneratedSFToken();
		}
		LOGGER.info("SFToken-> "+sfToken);
		return sfToken;
	}



	/**
	 * generate SF token using retryer mechanism
	 * @return
	 */
	private String getGeneratedSFToken() {
		LOGGER.info("Entered getGeneratedSFToken() -> Start");
		String generatedSFToken = null;
		sftokenMap.clear();
		Callable<String> callableSFToken = new Callable<String>() {
			public String call() throws Exception {
				LOGGER.info("Start: getSalesForceToken() of SalesForceService");
				bfoAuthorization = salesForceService.getSalesForceToken(UserConstants.CONTENT_TYPE_URL_FROM,
						UserConstants.PR_GRANT_TYPE, salesForceClientId, salesForceClientSecret, salesForceUserName,
						salesForcePassword);
				LOGGER.info("End: getSalesForceToken() of SalesForceService");

				if (null != bfoAuthorization && !bfoAuthorization.isEmpty()) {
					conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
					productDocCtx = JsonPath.using(conf).parse(bfoAuthorization);
					bfoAuthorizationToken = productDocCtx.read("$.access_token");
				}
				return bfoAuthorizationToken;
			}
		};

		Retryer<String> retryer = RetryerBuilder.<String> newBuilder()
				.retryIfResult(Predicates.<String> isNull()).retryIfExceptionOfType(TimeoutException.class)
				.retryIfRuntimeException().withWaitStrategy(WaitStrategies.fixedWait(5000L, TimeUnit.MILLISECONDS))
				.withStopStrategy(StopStrategies.stopAfterAttempt(2)).build();

		try {
			generatedSFToken = retryer.call(callableSFToken);
			//LOGGER.info("token from SF retryr mechanism -> "+generatedSFToken);

			if(null != generatedSFToken && !generatedSFToken.isEmpty()){
				Calendar now = Calendar.getInstance();
				now.add(Calendar.MINUTE,52);
				long validityTimeStamp = now.getTimeInMillis();

				sftokenMap.put("SalesForceToken", generatedSFToken+"::"+validityTimeStamp);
				LOGGER.info("SalesForceToken added to HashMap");
			}
		} catch (ExecutionException | RetryException e) {
			LOGGER.error("Error in getGeneratedSFToken() ->"+e.getMessage());
		}
		return generatedSFToken;
	}
	
	
	public void extendSFTokenValidity(String sftokenOnly){
		Calendar now = Calendar.getInstance();
		now.add(Calendar.MINUTE,Integer.parseInt(sftokentimeinminute));
		long validityTimeStamp = now.getTimeInMillis();

		sftokenMap.put("SalesForceToken", sftokenOnly+"::"+validityTimeStamp);
		LOGGER.info("SalesForceToken added to HashMap");
	}
	
	public void setSalesForceClientId(String salesForceClientId) {
		this.salesForceClientId = salesForceClientId;
	}
	
	public void setSalesForceClientSecret(String salesForceClientSecret) {
		this.salesForceClientSecret = salesForceClientSecret;
	}

	public void setSalesForceUserName(String salesForceUserName) {
		this.salesForceUserName = salesForceUserName;
	}

	public void setSalesForcePassword(String salesForcePassword) {
		this.salesForcePassword = salesForcePassword;
	}

	public void setSftokentimeinminute(String sftokentimeinminute) {
		this.sftokentimeinminute = sftokentimeinminute;
	}

}

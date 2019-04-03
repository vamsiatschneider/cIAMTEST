package com.schneider.idms.salesforce.service;

import java.io.IOException;
import java.io.InputStream;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;

import javax.inject.Inject;
import javax.ws.rs.core.Response;

import org.apache.cxf.helpers.IOUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;

import com.github.rholder.retry.RetryException;
import com.github.rholder.retry.Retryer;
import com.github.rholder.retry.RetryerBuilder;
import com.github.rholder.retry.StopStrategies;
import com.google.common.base.Predicates;
import com.idms.product.client.SalesForceService;
import com.se.idms.util.UserConstants;

/**
 * 
 * @author SESA453215
 *
 */

@org.springframework.stereotype.Service("saleforceService")
@EnableAsync
public class SaleforceServiceImpl {

	/**
	 * Logger instance.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(SaleforceServiceImpl.class);

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
	@Inject
	private SalesforceSyncServiceImpl sfSyncServiceImpl;
	
	@Async
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
								+ e.getMessage(),e);
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
			LOGGER.error("Exception in populatePrmActivationDate()::" + e.getMessage(),e);
		}

	}
	
	private String getSaleforceToken() {

		//DocumentContext productDocCtx = null;
		//Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();

		/*LOGGER.info("getSalesForceToken : => " + "PASSWORD_GRANT_TYPE : " + UserConstants.PR_GRANT_TYPE
				+ " salesForceClientId: " + salesForceClientId + " salesForceClientSecret :" + salesForceClientSecret
				+ " salesForceUserName: " + salesForceUserName + " salesForcePassword :" + salesForcePassword);*/
		String bfoAuthorizationToken = sfSyncServiceImpl.getSFToken();
		/*conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		productDocCtx = JsonPath.using(conf).parse(bfoAuthorization);
		String bfoAuthorizationToken = productDocCtx.read("$.access_token");*/

		return  "Bearer " + bfoAuthorizationToken;
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
}

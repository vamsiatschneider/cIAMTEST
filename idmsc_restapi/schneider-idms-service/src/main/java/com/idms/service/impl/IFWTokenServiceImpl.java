/**
 * 
 */
package com.idms.service.impl;

import java.util.Calendar;
import java.util.HashMap;
import java.util.Map;

import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;

import com.idms.product.client.IFWService;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Option;
import com.se.idms.util.UserConstants;

/**
 * @author SESA508936 (Santosh)
 *
 */
@org.springframework.stereotype.Service("ifwtokenservice")
public class IFWTokenServiceImpl {

	/**
	 * Logger instance.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(IFWTokenServiceImpl.class);

	@Inject
	private IFWService ifwService;
	
	@Value("${ifwClientId}")
	private String ifwClientId;
	
	@Value("${ifwClientSecret}")
	private String ifwClientSecret;
	
	@Value("${tokengenerationurl}")
	private String tokengenerationurl;
	
	public static Map<String,String> ifwtokenMap = new HashMap<String,String>();

	/**
	 * 
	 * @return
	 */
	public String getIFWToken() {
		String ifwAccessToken = getIFWAccessToken();
		LOGGER.info("ifwAccessToken = Bearer "+ifwAccessToken);
		return  "Bearer " + ifwAccessToken;
	}

	/**
	 * 
	 * @return
	 */
	private String getIFWAccessToken(){
		LOGGER.info("Entered getIFWAccessToken() -> Start");
		String ifwToken = null;
		String[] authIdTime = null;

		if(ifwtokenMap.size()>0){
			String mapValue = ifwtokenMap.get("IFWAccessToken");
			authIdTime = mapValue.split("::");
			long storedTimeMilli = Long.valueOf(authIdTime[1]).longValue();
			if(storedTimeMilli > System.currentTimeMillis()){
				ifwToken = authIdTime[0];
			} else {
				ifwToken = getGeneratedIFWToken();
			}
		} else {
			ifwToken = getGeneratedIFWToken();
		}
		LOGGER.info("ifwToken-> "+ifwToken);
		return ifwToken;
	}

	/**
	 * generate IFW Access token
	 * @return
	 */
	private String getGeneratedIFWToken() {
		LOGGER.info("Entered getGeneratedIFWToken() -> Start");
		String generatedIFWToken = null, ifwTokenString = null;
		Integer ifwTokenExpiryDuration = null;
		DocumentContext productDocCtx = null;
		Configuration conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
		ifwtokenMap.clear();
		try {
			if(tokengenerationurl.equalsIgnoreCase("ifw")){
				LOGGER.info("Start: getIFWToken() of IFWService");
				ifwTokenString = ifwService.getIFWToken(UserConstants.CONTENT_TYPE_URL_FROM, "client_credentials", ifwClientId, ifwClientSecret);
				LOGGER.info("End: getIFWToken() of IFWService");
			}
			if(tokengenerationurl.equalsIgnoreCase("apigee")){
				LOGGER.info("Start: getAPIGEEToken() of IFWService");
				ifwTokenString = ifwService.getAPIGEEToken(UserConstants.CONTENT_TYPE_URL_FROM, "client_credentials", ifwClientId, ifwClientSecret);
				LOGGER.info("End: getAPIGEEToken() of IFWService");
			}

			if (null != ifwTokenString && !ifwTokenString.isEmpty()) {
				conf = Configuration.builder().options(Option.SUPPRESS_EXCEPTIONS).build();
				productDocCtx = JsonPath.using(conf).parse(ifwTokenString);
				generatedIFWToken = productDocCtx.read("$.access_token");
				if(productDocCtx.read("$.expires_in") instanceof Integer)
					ifwTokenExpiryDuration = productDocCtx.read("$.expires_in");
				if(productDocCtx.read("$.expires_in") instanceof String)
					ifwTokenExpiryDuration = Integer.valueOf(productDocCtx.read("$.expires_in"));
			}

			if (null != generatedIFWToken && !generatedIFWToken.isEmpty()) {
				Calendar now = Calendar.getInstance();
				now.add(Calendar.SECOND, ifwTokenExpiryDuration);
				long validityTimeStamp = now.getTimeInMillis();

				ifwtokenMap.put("IFWAccessToken", generatedIFWToken + "::" + validityTimeStamp);
				LOGGER.info("IFWAccessToken added to HashMap");
			}
		} catch (Exception e) {
			LOGGER.error("Error in getGeneratedIFWToken() ->" + e.getMessage(), e);
		}
		return generatedIFWToken;
	}
}
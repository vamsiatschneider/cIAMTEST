package com.idms.service.util;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class LogMessageUtil {
	
	private static final Logger LOGGER = LoggerFactory.getLogger(LogMessageUtil.class);

	public static void logInfoMessage(String... infoMsgs) {
		if(LOGGER.isInfoEnabled()) {
			StringBuilder logMsgBuilder = new StringBuilder();
			for(String infoMsg : infoMsgs) {
				logMsgBuilder.append(infoMsg);
			}
			LOGGER.info(logMsgBuilder.toString());
		}
	}
	public static void logErrorMessage(Exception exception, String... errorMsgs) {
		if(LOGGER.isErrorEnabled()) {
			StringBuilder logMsgBuilder = new StringBuilder();
			for(String errorMsg : errorMsgs) {
				logMsgBuilder.append(errorMsg);
			}
			logMsgBuilder.append(exception);
			LOGGER.info(logMsgBuilder.toString());
		}
	}
}

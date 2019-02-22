package com.se.sync;

import java.lang.reflect.Method;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.aop.interceptor.AsyncUncaughtExceptionHandler;

public class CustomAsyncExceptionHandler implements AsyncUncaughtExceptionHandler{

	private static final Logger LOGGER = LoggerFactory.getLogger(CustomAsyncExceptionHandler.class);

	public void handleUncaughtException(final Throwable throwable, final Method method, final Object... obj) {
		LOGGER.info("Exception message - " + throwable.getMessage());
		LOGGER.info("Method name - " + method.getName());
		for (final Object param : obj) {
			LOGGER.info("Param - " + param);
		}
	}
}

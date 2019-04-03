package com.se.sync;

import java.util.concurrent.Future;

import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.AsyncResult;
import org.springframework.stereotype.Component;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Component
public class AsyncComponent {
	private static final Logger LOGGER = LoggerFactory.getLogger(AsyncComponent.class);

	@Async
	public void asyncMethodWithVoidReturnType() {

		LOGGER.info("Execute method asynchronously. " + Thread.currentThread().getName());
	}

	@Async
	public Future<String> asyncMethodWithReturnType() {
		LOGGER.info("Execute method asynchronously " + Thread.currentThread().getName());

		try {
			Thread.sleep(5000);
			return new AsyncResult<>("hello world !!!!");
		} catch (InterruptedException e) {
			// TODO Auto-generated catch block
			LOGGER.error("An error occured."+e.getMessage(),e);
		}
		return null;
	}

	@Async("threadPoolTaskExecutor")
	public void asyncMethodWithConfiguredExecutor() {
		LOGGER.info("Execute method asynchronously with configured executor" + Thread.currentThread().getName());
	}

	@Async
	public void asyncMethodWithExceptions() throws Exception {
		throw new Exception("Throw message from asynchronous method. ");
	}
}

//package com.se.idms.util;
//
//import java.util.UUID;
//import java.util.concurrent.Callable;
//import java.util.concurrent.ExecutionException;
//
//import javax.inject.Inject;
//
//import org.slf4j.Logger;
//import org.slf4j.LoggerFactory;
//
//import com.github.rholder.retry.RetryException;
//import com.github.rholder.retry.Retryer;
//import com.github.rholder.retry.RetryerBuilder;
//import com.github.rholder.retry.StopStrategies;
//import com.google.common.base.Predicates;
//import com.idms.service.UIMSSoapService;
//import com.schneider.ims.service.uimsv2.CompanyV3;
//
//public class Test {
//
//	private static final Logger uimsLog = LoggerFactory.getLogger("uimsLogger");
//	
////	static final Logger resultLog = Logger.getLogger("uimsLogger");
//
//	@Inject
//	private static UIMSSoapService soapService;
//
//	
//	
//	public static void main(String[] args) {
//
//		// calling uims user creation
//		String callerFid = "cn-" + UUID.randomUUID().toString();
//
//		uimsLog.info("1");
//		
//		// Retry mechanism
//		try {
//			Callable<Boolean> callable = new Callable<Boolean>() {
//				public Boolean call() throws Exception {
//					String createUIMSCompany = soapService.createUIMSCompany(callerFid, UimsConstants.VNEW,
//							new CompanyV3());
//
//					uimsLog.info("connection");
//
//					uimsLog.info("2");
//					return true;
//				}
//			};
//
//			Retryer<Boolean> retryer = RetryerBuilder.<Boolean> newBuilder()
//					.retryIfResult(Predicates.<Boolean> isNull()).retryIfExceptionOfType(Exception.class)
//					.retryIfRuntimeException().withStopStrategy(StopStrategies.stopAfterAttempt(3)).build();
//			try {
//				retryer.call(callable);
//				uimsLog.info("");
//			} catch (RetryException e) {
//				uimsLog.info("Caught the RetryException::" + e.getMessage());
//				// e.printStackTrace();
//			} catch (ExecutionException e) {
//				e.printStackTrace();
//			}
//			uimsLog.info("out");
//		} catch (Exception e) {
//			// TODO Auto-generated catch block
//			e.printStackTrace();
//		}
//
//	}
//}

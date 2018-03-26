//package com.se.sync;
//
//import java.util.concurrent.ExecutionException;
//import java.util.concurrent.Future;
//
//import org.junit.Test;
//import org.junit.runner.RunWith;
//import org.springframework.beans.factory.annotation.Autowired;
//import org.springframework.test.context.ContextConfiguration;
//import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
//import org.springframework.test.context.support.AnnotationConfigContextLoader;
//
//
//@RunWith(SpringJUnit4ClassRunner.class)
//@ContextConfiguration(classes = { SpringAsyncConfig.class}, loader = AnnotationConfigContextLoader.class)
//public class AsyncAnnotationIntegrationTest {
//	
//	
//	@Autowired
//	AsyncComponent asyncComponent;
//	
//	@Test
//	public void testAsyncAnnotationForMethodsWithVoidReturnType() throws InterruptedException{
//		
//		System.out.println("Start - invoking an asynchronous method." + Thread.currentThread().getName());
//		asyncComponent.asyncMethodWithVoidReturnType();
//		System.out.println("End - invoking an asynchronous method." + Thread.currentThread().getName());
//		
//		Thread.sleep(1000);
//	}
//	
//	 @Test
//	    public void testAsyncAnnotationForMethodsWithReturnType() throws InterruptedException, ExecutionException {
//	        System.out.println("Start - invoking an asynchronous method. " + Thread.currentThread().getName());
//	        final Future<String> future = asyncComponent.asyncMethodWithReturnType();
//
//	        while (true) {
//	            if (future.isDone()) {
//	                System.out.println("Result from asynchronous process - " + future.get());
//	                break;
//	            }
//	            System.out.println("Continue doing something else. ");
//	            Thread.sleep(1000);
//	        }
//	    }
//
//	    @Test
//	    public void testAsyncAnnotationForMethodsWithConfiguredExecutor() {
//	        System.out.println("Start - invoking an asynchronous method. ");
//	        asyncComponent.asyncMethodWithConfiguredExecutor();
//	        System.out.println("End - invoking an asynchronous method. ");
//	    }
//
//	    @Test
//	    public void testAsyncAnnotationForMethodsWithException() throws Exception {
//	        System.out.println("Start - invoking an asynchronous method. ");
//	        asyncComponent.asyncMethodWithExceptions();
//	        System.out.println("End - invoking an asynchronous method. ");
//	    }
//
//
//}

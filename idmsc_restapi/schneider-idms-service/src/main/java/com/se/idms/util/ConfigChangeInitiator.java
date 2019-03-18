package com.se.idms.util;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;

import javax.inject.Inject;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;

import com.idms.service.UserService;

public class ConfigChangeInitiator {
	
	//private static final String FILE_PATH = "C:\\idms\\app_root\\properties\\application.DEV.properties";
	private static final String FILE_PATH = "C:\\idms\\app_root";
	@Value("${app.properties.dir}")
	private String DIRECTORY_PATH;
	
	@Inject
	private ApplicationPropertiesWatcher applicationPropertiesWatcher;
	
	/**
	 * Logger instance.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(ConfigChangeInitiator.class);

	
	public static void main(String[] args) throws IOException {
		// TODO Auto-generated method stub
		ApplicationPropertiesWatcher listner = new ApplicationPropertiesWatcher(
	                FILE_PATH);
	        try {
	            new Thread(listner).start();
	           /* while (true) {
	                Thread.sleep(5000);
	                System.out.println(PropertyFileAutoRefresh.getInstance()
	                        .getConfiguration("caller.fid"));
	            }*/
	        } catch (Exception e) {
	            e.printStackTrace();
	      }
	}
	
	public void startWatcher() throws IOException{
		LOGGER.info("startWatcher() DIRECTORY_PATH: "+DIRECTORY_PATH);
		//ApplicationPropertiesWatcher listner = new ApplicationPropertiesWatcher(
		//		DIRECTORY_PATH);
		//listner.processEvents();
		new Thread(applicationPropertiesWatcher).start();
		
	}

}

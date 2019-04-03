package com.idms.service.util;

import java.io.FileWriter;
import java.io.IOException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;

import com.opencsv.CSVWriter;

/**
 * 
 * @author Aravindh Kumar
 *
 */
@EnableAsync
public class AsyncUtil {
	
	/**
	 * Logger instance.
	 */
	private static final Logger LOGGER = LoggerFactory.getLogger(AsyncUtil.class);
	
	/**
	 * Utility function to append the CSV file with the record.
	 * @param filePath
	 * @param recordString
	 * @return
	 */
	@Async
	public static boolean generateCSV(String filePath, String recordString){
		boolean csvAppended = false;
		
		 CSVWriter writer;
		try {
			writer = new CSVWriter(new FileWriter(filePath, true));
			String [] record = recordString.split(",");
			
			writer.writeNext(record);
			
			writer.close();
		} catch (IOException e) {
			LOGGER.error("AsyncUtil.generateCSV: "+ e.getMessage());
			LOGGER.error("Exception >"+e);
		}
	        
		return csvAppended;
		
	}

}

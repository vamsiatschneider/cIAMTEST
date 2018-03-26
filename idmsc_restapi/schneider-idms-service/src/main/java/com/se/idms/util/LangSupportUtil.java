package com.se.idms.util;

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;

import com.ibm.icu.text.Transliterator;

public class LangSupportUtil {

	public static List<String> getTransilatorLanguages(){
		List<String> languages = new ArrayList<String>();	
		Enumeration<String> availableIDs = Transliterator.getAvailableIDs();
		while (availableIDs.hasMoreElements()) {
			//System.out.println(availableIDs.nextElement());
			languages.add(availableIDs.nextElement());
		}
		return languages;
	}
	
}

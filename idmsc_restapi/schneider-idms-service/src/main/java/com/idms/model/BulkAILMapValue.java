package com.idms.model;

import java.util.List;

public class BulkAILMapValue {

	private List<AILRecord> ailRecords;
	
	private String openDJAILValue;
	
	public String getOpenDJAILValue() {
		return openDJAILValue;
	}

	public void setOpenDJAILValue(String openDJAILValue) {
		this.openDJAILValue = openDJAILValue;
	}

	public List<AILRecord> getAilRecords() {
		return ailRecords;
	}

	public void setAilRecords(List<AILRecord> ailRecords) {
		this.ailRecords = ailRecords;
	}
}

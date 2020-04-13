package com.idms.service.bulkail.util;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import javax.ws.rs.core.Response;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;

import com.idms.model.AILRecord;
import com.idms.model.BulkAILRecord;
import com.idms.model.BulkAILResponse;
import com.idms.model.BulkAILResultHolder;
import com.jayway.jsonpath.DocumentContext;
import com.se.idms.util.UserConstants;

public class BulkAILUtil {

	private static final Logger LOGGER = LoggerFactory.getLogger(BulkAILUtil.class);
	
	public static List<BulkAILResponse> buildResponseList(Map<String, Map<Integer, BulkAILResultHolder>> userAndAILReqMap) {
		List<BulkAILResponse> responseList = new ArrayList<BulkAILResponse>();
		for (Entry<String, Map<Integer, BulkAILResultHolder>> entry : userAndAILReqMap.entrySet()) {
			String userId = entry.getKey();
			Map<Integer, BulkAILResultHolder> resultHolderMap = entry.getValue();
			BulkAILResponse baResponse = new BulkAILResponse();
			baResponse.setUserFedID(userId);
			List<com.idms.model.AILResponse> ailResponseList = new ArrayList<com.idms.model.AILResponse>();

			for (Entry<Integer, BulkAILResultHolder> subEntry : resultHolderMap.entrySet()) {
				com.idms.model.AILResponse ailResponse = new com.idms.model.AILResponse();
				BulkAILResultHolder resultHolder = subEntry.getValue();
				AILRecord ailRecord = resultHolder.getAilRecord();
				if (ailRecord != null) {
					ailResponse.setAcl(ailRecord.getAcl());
					ailResponse.setAclType(ailRecord.getAclType());
					ailResponse.setOperation(ailRecord.getOperation());
				}
				ailResponse.setStatus(resultHolder.getStatus());
				ailResponse.setStatusCode(resultHolder.getStatusCode());
				ailResponse.setErrorMessage(resultHolder.getErrorMessage());
				ailResponseList.add(ailResponse);
			}
			baResponse.setResponse(ailResponseList);
			responseList.add(baResponse);
		}
		return responseList;
	}

	public static void processGrantRequest(Map<String, String> grantMap, DocumentContext productDocCtx, String idmsAIL_c,
			List<AILRecord> ails, Map<Integer, BulkAILResultHolder> ailCountMap,
			Map<AILRecord, Integer> recordCountMap) {
		for (AILRecord ail : ails) {
			if (ail == null) {
				buildNullAILResult(ailCountMap);
				continue;
			}
			int count = ailCountMap.size();
			boolean isValidOperationType = false;
			if (AILOperationType.GRANT.getType().equalsIgnoreCase(ail.getOperation())
					|| AILOperationType.REVOKE.getType().equalsIgnoreCase(ail.getOperation())) {
				isValidOperationType = true;
			}
			if (!isValidOperationType) {
				BulkAILResultHolder holder = buildInvalidResult(ail, HttpStatus.BAD_REQUEST.value(),
						BulkAILConstants.INVALID_OPERATION, true);
				ailCountMap.put(++count, holder);
			}
			if (AILOperationType.GRANT.getType().equalsIgnoreCase(ail.getOperation())) {
				if (recordCountMap.get(ail) != null) {
					// create new holder object since grant/revoke on same acl type/value
					// combination is invalid.
					recordCountMap.put(ail, recordCountMap.get(ail) + 1);
					BulkAILResultHolder holder = buildInvalidResult(ail, HttpStatus.BAD_REQUEST.value(),
							BulkAILConstants.MALFORMED_REQUEST, true);
					ailCountMap.put(++count, holder);
				} else {
					count = buildSuccessResult(ailCountMap, ail, count);
					recordCountMap.put(ail, 1);

					buildGrantAILMap(grantMap, productDocCtx, idmsAIL_c, ail);
				}
			}
		}
	}

	private static int buildSuccessResult(Map<Integer, BulkAILResultHolder> ailCountMap, AILRecord ail, int count) {
		BulkAILResultHolder holder = new BulkAILResultHolder();
		holder.setAilRecord(ail);
		holder.setDupRequest(false);
		holder.setStatusCode(HttpStatus.OK.value());
		holder.setStatus(BulkAILConstants.SUCCESS);
		ailCountMap.put(++count, holder);
		return count;
	}

	public static void processRevokeRequest(Map<String, String> revokeMap, DocumentContext productDocCtx, String idmsAIL_c,
			List<AILRecord> ails, Map<Integer, BulkAILResultHolder> ailCountMap,
			Map<AILRecord, Integer> recordCountMap) {
		for (AILRecord ail : ails) {
			if (ail == null) {
				continue;
			}
			int count = ailCountMap.size();
			if (AILOperationType.REVOKE.getType().equalsIgnoreCase(ail.getOperation())) {
				if (recordCountMap.get(ail) != null) {
					// create new holder object since grant/revoke on same acl type/value
					// combination is invalid.
					recordCountMap.put(ail, recordCountMap.get(ail) + 1);
					BulkAILResultHolder holder = buildInvalidResult(ail, HttpStatus.BAD_REQUEST.value(),
							BulkAILConstants.MALFORMED_REQUEST, true);
					ailCountMap.put(++count, holder);
				} else {
					count = buildSuccessResult(ailCountMap, ail, count);
					recordCountMap.put(ail, 1);
					buildRevokeAILMap(revokeMap, productDocCtx, idmsAIL_c, ail);
				}
			}
		}
	}
	
	private static BulkAILResultHolder buildInvalidResult(AILRecord ail, int statusCode, String invalidOp, boolean dupRequest) {
		BulkAILResultHolder holder = new BulkAILResultHolder();
		holder.setAilRecord(ail);
		holder.setDupRequest(dupRequest);
		holder.setStatusCode(statusCode);
		holder.setStatus(BulkAILConstants.FAILURE);
		holder.setErrorMessage(invalidOp);
		return holder;
	}

	public static String buildUserUpdateJson(Map<String, String> map) {
		String ailType_json = "" ;
 		List<String> ail_jsonList = new ArrayList<String>();
 		for (Entry<String, String> entry : map.entrySet()) {
 			String key = entry.getKey();
 			String value = entry.getValue();
 			if(value.isEmpty()) {
 				value = "[]";
 			}
 			if("IDMSAil_c".equals(key)) {         				
 				ailType_json = "\"IDMSAil_c\": \"" + value + "\"";
 			}else {
 				ail_jsonList.add("\"" + key +"\"" + ": \"" + value + "\"");
 			}
 		}
 		StringBuilder updateJson = new StringBuilder("{");
 		if(!ailType_json.isEmpty()) {
 			updateJson.append(ailType_json).append(",");
 		}
		for(int i = 0; i < ail_jsonList.size(); i++) {
			if(i == ail_jsonList.size()-1) {
				updateJson.append(ail_jsonList.get(i));
			}else {
				updateJson.append(ail_jsonList.get(i)).append(",");
			}
		}
		updateJson.append("}");
		return updateJson.toString();
	}

	private static void buildRevokeAILMap(Map<String, String> revokeMap, DocumentContext productDocCtx, String idmsAIL_c,
			AILRecord ail) {
		idmsAIL_c = idmsAIL_c.replaceAll("\\[", "");
		idmsAIL_c = idmsAIL_c.replaceAll("\\]", "");
		String revokeValue = "";
		String ailApplnAndType = "(" + ail.getAclType() + ";" + ail.getAcl() + ")";
		if (idmsAIL_c==null || idmsAIL_c.contains(ailApplnAndType)) {
			revokeValue = ailApplnAndType;
		}
		LOGGER.info("AIl Value to be Revoked: " + revokeValue);
		String idmsAIL_temp = revoke(idmsAIL_c, revokeValue);
		LOGGER.info("AIL value after revoking: " + idmsAIL_temp);
		
		if (revokeMap.get("IDMSAil_c") != null) {
			if (!revokeValue.isEmpty()) {
				String ailc_Value = revokeMap.get("IDMSAil_c");
				String ailc_newValue = revoke(ailc_Value, revokeValue);
				revokeMap.put("IDMSAil_c", ailc_newValue);
			}
		} else {
			if (!idmsAIL_temp.isEmpty()) {
				revokeMap.put("IDMSAil_c", idmsAIL_temp);
			}
		}
		
		String idmsAclType_c = getIDMSAclType(ail.getAclType());
		String aclType_c = productDocCtx.read("$.IDMSAIL_" + idmsAclType_c + "_c[0]");
		aclType_c = aclType_c.replaceAll("\\[", "");
		aclType_c = aclType_c.replaceAll("\\]", "");
		String revokeACLVal = "";
		String idmsAclAndType = ail.getAcl();
		if (aclType_c==null || aclType_c.contains(idmsAclAndType)) {
			revokeACLVal = idmsAclAndType;
		}
		LOGGER.info("App value to be revoked: " + revokeACLVal);
		String idmsAclType_temp = revoke(aclType_c, revokeACLVal);
		LOGGER.info("App value after revoking: " + idmsAclType_temp);
		
		String ailType_c = "IDMSAIL_" + idmsAclType_c + "_c";
		if(revokeMap.get(ailType_c) != null) {
			if(!revokeACLVal.isEmpty()) {
				String ailTypec_Value = revokeMap.get(ailType_c);
				String ailTypec_newValue = revoke(ailTypec_Value, revokeACLVal);
				revokeMap.put(ailType_c, ailTypec_newValue);
			}
		}else {
			if(!idmsAclType_temp.isEmpty()) {
				revokeMap.put(ailType_c, idmsAclType_temp);
			}
		}
	}

	private static String revoke(String idmsAIL_c, String revokeValue) {
		String idmsAIL_temp = "";
		String[] ail_cArr = idmsAIL_c.split(",");
		for(String ail_c : ail_cArr) {
			if(!revokeValue.isEmpty() && !ail_c.equals(revokeValue)) {
				if(idmsAIL_temp.isEmpty()) {
					idmsAIL_temp = ail_c + ",";
				}else {
					idmsAIL_temp = idmsAIL_temp + ail_c + ",";
				}
			}
		}
		if (!(idmsAIL_temp == null || idmsAIL_temp.length() == 0)) {
			idmsAIL_temp = idmsAIL_temp.substring(0, idmsAIL_temp.length() - 1);
		}
		return idmsAIL_temp;
	}

	private static void buildGrantAILMap(Map<String, String> grantMap,
			DocumentContext productDocCtx, String idmsAIL_c, AILRecord ail) {
		idmsAIL_c = idmsAIL_c.replaceAll("\\[", "");
		idmsAIL_c = idmsAIL_c.replaceAll("\\]", "");
		String ailValue = "";
		String ailApplnAndType = "(" + ail.getAclType() + ";" + ail.getAcl() + ")";
		if (idmsAIL_c==null || !idmsAIL_c.contains(ailApplnAndType)) {
			ailValue = ailApplnAndType;
		}
		String idmsAIL_temp = new String();
		if(!ailValue.isEmpty()) {
			if (null != idmsAIL_c && !idmsAIL_c.isEmpty())
				idmsAIL_temp = idmsAIL_c + "," + ailValue;
			else
				idmsAIL_temp = ailValue;
		}
		if(grantMap.get("IDMSAil_c") != null) {
			String ailc_Value = grantMap.get("IDMSAil_c");
			if(!ailValue.isEmpty()) {
				ailc_Value += "," + ailValue;
			}
			grantMap.put("IDMSAil_c", ailc_Value);
		}else {
			if(!idmsAIL_temp.isEmpty()) {
				grantMap.put("IDMSAil_c", idmsAIL_temp);
			}
		}
			
		String idmsAclType_c = getIDMSAclType(ail.getAclType());
		String aclType_c = productDocCtx.read("$.IDMSAIL_" + idmsAclType_c + "_c[0]");
		aclType_c = aclType_c.replaceAll("\\[", "");
		aclType_c = aclType_c.replaceAll("\\]", "");
		String ailTypeValue = "";
		if (aclType_c == null||!aclType_c.contains(ail.getAcl())) {
			ailTypeValue = ail.getAcl();
		}
		String idmsAILValue_temp = new String();
		if(!ailTypeValue.isEmpty()) {
			if (null != aclType_c && !aclType_c.isEmpty())
				idmsAILValue_temp = aclType_c + ","+ailTypeValue;
			else
				idmsAILValue_temp = ailTypeValue;
		}
		String ailType_c = "IDMSAIL_" + idmsAclType_c + "_c";
		if(grantMap.get(ailType_c) != null) {
			String ailTypec_Value = grantMap.get(ailType_c);
			if(!ailTypeValue.isEmpty()) {
				ailTypec_Value += "," + ailTypeValue;
			}
			grantMap.put(ailType_c, ailTypec_Value);
		}else {
			if(!idmsAILValue_temp.isEmpty()) {
				grantMap.put(ailType_c, idmsAILValue_temp);
			}
		}
	}

	public static void buildNullFedIdResult(Map<String, Map<Integer, BulkAILResultHolder>> userAndAILReqMap,
			BulkAILRecord ailRecord, String userFedID) {
		Map<Integer, BulkAILResultHolder> map = userAndAILReqMap.get(userFedID);
		Map<Integer, BulkAILResultHolder> ailCountMap = new HashMap<Integer, BulkAILResultHolder>();
		List<AILRecord> ails = ailRecord.getAils();
		if (ails == null || ails.isEmpty()) {
			buildNullAILResult(ailCountMap);
		}else {
			for(AILRecord ail : ails) {
				if (ail == null) {
					buildNullAILResult(ailCountMap);
					continue;
				}else {
					BulkAILResultHolder holder = buildInvalidResult(ail, HttpStatus.BAD_REQUEST.value(),
							BulkAILConstants.MISSING_MANDATORY_FIELDS, false);
					int count = ailCountMap.size();
					ailCountMap.put(++count, holder);
				}
			}
		}
		if(map == null) {
			map = new HashMap<Integer, BulkAILResultHolder>();
		}
		map.putAll(ailCountMap);
		if(userFedID == null) {
			userAndAILReqMap.put("null", map);
		}
	}

	public static void buildNullAILResult(Map<Integer, BulkAILResultHolder> ailCountMap) {
		BulkAILResultHolder holder = new BulkAILResultHolder();
		holder.setAilRecord(null);
		holder.setDupRequest(false);
		holder.setStatusCode(HttpStatus.BAD_REQUEST.value());
		holder.setStatus(BulkAILConstants.FAILURE);
		holder.setErrorMessage(BulkAILConstants.MISSING_MANDATORY_FIELDS);
		int size = ailCountMap.size();
		ailCountMap.put(++size , holder);
	}

	public static void buildDuplicateFedIdsResult(Map<String, Map<Integer, BulkAILResultHolder>> userAndAILReqMap,
			BulkAILRecord ailRecord, String userFedID) {
		Map<Integer, BulkAILResultHolder> map = userAndAILReqMap.get(userFedID);
		int count = map.size();
		List<AILRecord> ails = ailRecord.getAils();
		Map<Integer, BulkAILResultHolder> ailCountMap = new HashMap<Integer, BulkAILResultHolder>();
		for(AILRecord ail : ails) {
			BulkAILResultHolder holder = new BulkAILResultHolder();
			holder.setAilRecord(ail);
			holder.setDupRequest(false);
			holder.setStatusCode(HttpStatus.BAD_REQUEST.value());
			holder.setStatus(BulkAILConstants.FAILURE);
			holder.setErrorMessage(BulkAILConstants.MALFORMED_REQUEST);
			ailCountMap.put(++count, holder);
		}
		map.putAll(ailCountMap);
		userAndAILReqMap.put(userFedID, map);
	}

	public static Map<Integer, BulkAILResultHolder> buildUserNotFoundResult(BulkAILRecord ailRecord) {
		List<AILRecord> ails = ailRecord.getAils();
		Map<Integer, BulkAILResultHolder> ailCountMap = new HashMap<Integer, BulkAILResultHolder>();
		if(ails == null || ails.isEmpty()) {
			buildNullAILResult(ailCountMap);
			return ailCountMap;
		}
		int count = ailCountMap.size();
		for(AILRecord ail : ails) {
			if(ail == null) {
 				buildNullAILResult(ailCountMap);
 				continue;
 			}
			BulkAILResultHolder holder = new BulkAILResultHolder();
			holder.setAilRecord(ail);
			holder.setDupRequest(false);
			holder.setStatusCode(HttpStatus.NOT_FOUND.value());
			holder.setStatus(BulkAILConstants.FAILURE);
			holder.setErrorMessage(BulkAILConstants.USER_NOT_FOUND);
			ailCountMap.put(++count, holder);
		}
		return ailCountMap;
	}

	public static Response getErrorResponse(HttpStatus status, String message, long startTime) {
		BulkAILResponse response = new BulkAILResponse();
		response.setCode(status.name());
		response.setMessage(message);
		long elapsedTime = UserConstants.TIME_IN_MILLI_SECONDS - startTime;
		LOGGER.error("Error is " + response.getMessage());
		LOGGER.info("Time taken by bulkupdateAIL() : " + elapsedTime);
		return Response.status(status.value()).entity(response).build();
	}
	private static String getIDMSAclType(String aclType) {
		if (UserConstants.ACLTYPE_APPLICATION.equalsIgnoreCase(aclType))
			return UserConstants.ACLTYPE_APPLICATIONS;
		else if (UserConstants.ACLTYPE_PROGRAM.equalsIgnoreCase(aclType))
			return UserConstants.ACLTYPE_PROGRAMS;
		else if (UserConstants.ACLTYPE_FEATURE.equalsIgnoreCase(aclType))
			return UserConstants.ACLTYPE_FEATURES;

		return null;
	}
}

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
import com.idms.model.AILRequest;
import com.idms.model.BulkAILMapValue;
import com.idms.model.BulkAILRecord;
import com.idms.model.BulkAILResponse;
import com.idms.model.BulkAILResultHolder;
import com.idms.model.UserAILRecord;
import com.idms.product.client.OpenAMService;
import com.idms.product.model.Attributes;
import com.idms.service.UIMSAccessManagerSoapService;
import com.jayway.jsonpath.DocumentContext;
import com.se.idms.dto.IDMSUserAIL;
import com.se.idms.dto.IDMSUser__r;
import com.se.idms.util.UserConstants;

public class BulkAILUtil {

	private static final Logger LOGGER = LoggerFactory.getLogger(BulkAILUtil.class);
	
	public static BulkAILResponse buildResponse(Map<String, Map<Integer, BulkAILResultHolder>> userAndAILReqMap, String profileUpdateSource) {
		BulkAILResponse baResponse = new BulkAILResponse();
		List<BulkAILRecord> userAils = new ArrayList<BulkAILRecord>();

		for (Entry<String, Map<Integer, BulkAILResultHolder>> entry : userAndAILReqMap.entrySet()) {
			BulkAILRecord bulkAILRecord = new BulkAILRecord();
			String userId = entry.getKey();
			Map<Integer, BulkAILResultHolder> resultHolderMap = entry.getValue();
			bulkAILRecord.setUserFedID(userId);
			List<AILRecord> ailResponseList = new ArrayList<AILRecord>();

			for (Entry<Integer, BulkAILResultHolder> subEntry : resultHolderMap.entrySet()) {
				AILRecord ailResponse = new AILRecord();
				BulkAILResultHolder resultHolder = subEntry.getValue();
				AILRecord ailRecord = resultHolder.getAilRecord();
				if (ailRecord != null) {
					ailResponse.setAcl(ailRecord.getAcl());
					ailResponse.setAclType(ailRecord.getAclType());
					ailResponse.setOperation(ailRecord.getOperation());
				}
				ailResponse.setStatus(resultHolder.getStatus());
				ailResponse.setStatusCode(resultHolder.getStatusCode());
				ailResponse.setMessage(resultHolder.getMessage());
				ailResponseList.add(ailResponse);
			}
			bulkAILRecord.setAils(ailResponseList);
			userAils.add(bulkAILRecord);
		}
		baResponse.setProfileLastUpdateSource(profileUpdateSource);
		baResponse.setMessage(BulkAILConstants.SUCCESS);
		baResponse.setUserAils(userAils );
		return baResponse;
	}

	public static void processGrantRequest(Map<String, BulkAILMapValue> grantMap, DocumentContext productDocCtx, String idmsAIL_c,
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
		if(AILOperationType.GRANT.getType().equalsIgnoreCase(ail.getOperation())) {
			holder.setMessage(BulkAILConstants.AIL_GRANT_MSG);
		}else {
			holder.setMessage(BulkAILConstants.AIL_REVOKE_MSG);
		}
		ailCountMap.put(++count, holder);
		return count;
	}

	public static void processRevokeRequest(Map<String, BulkAILMapValue> revokeMap, DocumentContext productDocCtx, String idmsAIL_c,
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
		holder.setMessage(invalidOp);
		return holder;
	}

	public static String buildUserUpdateJson(Map<String, BulkAILMapValue> grantMap) {
		String ailType_json = "" ;
 		List<String> ail_jsonList = new ArrayList<String>();
 		for (Entry<String, BulkAILMapValue> entry : grantMap.entrySet()) {
 			String key = entry.getKey();
 			String openDJValue = entry.getValue().getOpenDJAILValue();
 			if(openDJValue.isEmpty()) {
 				openDJValue = "[]";
 			}
 			if("IDMSAil_c".equals(key)) {         				
 				ailType_json = "\"IDMSAil_c\": \"" + openDJValue + "\"";
 			}else {
 				ail_jsonList.add("\"" + key +"\"" + ": \"" + openDJValue + "\"");
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

	private static void buildRevokeAILMap(Map<String, BulkAILMapValue> revokeMap, DocumentContext productDocCtx, String idmsAIL_c,
			AILRecord ail) {
		idmsAIL_c = idmsAIL_c.replaceAll("\\[", "");
		idmsAIL_c = idmsAIL_c.replaceAll("\\]", "");
		String revokeValue = "";
		String ailApplnAndType = "(" + ail.getAclType() + ";" + ail.getAcl() + ")";
		if (idmsAIL_c==null || idmsAIL_c.contains(ailApplnAndType)) {
			revokeValue = ailApplnAndType;
		}
		String idmsAIL_temp = "";
		if (!revokeValue.isEmpty()) {
			LOGGER.info("AIl Value to be Revoked: " + revokeValue);
			idmsAIL_temp = revoke(idmsAIL_c, revokeValue);
			LOGGER.info("AIL value after revoking: " + idmsAIL_temp);
		}
		BulkAILMapValue mapAilcValue = new BulkAILMapValue();
		if (revokeMap.get("IDMSAil_c") != null) {
			if (!revokeValue.isEmpty()) {
				String ailc_Value = revokeMap.get("IDMSAil_c").getOpenDJAILValue();
				List<AILRecord> ailRecordList = revokeMap.get("IDMSAil_c").getAilRecords();
				ailRecordList.add(ail);
				mapAilcValue.setAilRecords(ailRecordList);
				String ailc_newValue = revoke(ailc_Value, revokeValue);
				mapAilcValue.setOpenDJAILValue(ailc_newValue);
				revokeMap.put("IDMSAil_c", mapAilcValue);
			}
		} else {
			if (!idmsAIL_temp.isEmpty()) {
				List<AILRecord> ailRecordList = new ArrayList<AILRecord>();
				ailRecordList.add(ail);
				mapAilcValue.setAilRecords(ailRecordList);
				mapAilcValue.setOpenDJAILValue(idmsAIL_temp);
				revokeMap.put("IDMSAil_c", mapAilcValue);
			}
		}
		String idmsAclType_c = getIDMSAclType(ail.getAclType());
		String aclType_c = productDocCtx.read("$.IDMSAIL_" + idmsAclType_c + "_c[0]");
		if(aclType_c == null) {
			aclType_c ="[]";
		}
		aclType_c = aclType_c.replaceAll("\\[", "");
		aclType_c = aclType_c.replaceAll("\\]", "");
		String revokeACLVal = "";
		String idmsAclAndType = ail.getAcl();
		if (aclType_c != null) {
			String[] aclArr = aclType_c.split(",");
			for(String acl : aclArr) {
				if(acl.equals(idmsAclAndType)) {
					revokeACLVal = idmsAclAndType;
					break;
				}
			}
		}
		String idmsAclType_temp = "";
		if (!revokeACLVal.isEmpty()) {
			LOGGER.info("App value to be revoked: " + revokeACLVal);
			idmsAclType_temp = revoke(aclType_c, revokeACLVal);
			LOGGER.info("App value after revoking: " + idmsAclType_temp);
		}
		String ailType_c = "IDMSAIL_" + idmsAclType_c + "_c";
		BulkAILMapValue ailTypecMapVal = new BulkAILMapValue() ;
		if(revokeMap.get(ailType_c) != null) {
			if(!revokeACLVal.isEmpty()) {
				String ailTypec_Value = revokeMap.get(ailType_c).getOpenDJAILValue();
				String ailTypec_newValue = revoke(ailTypec_Value, revokeACLVal);
				ailTypecMapVal.setOpenDJAILValue(ailTypec_newValue);
				revokeMap.put(ailType_c, ailTypecMapVal);
			}
		}else {
			if(!idmsAclType_temp.isEmpty()) {
				ailTypecMapVal.setOpenDJAILValue(idmsAclType_temp);
				revokeMap.put(ailType_c, ailTypecMapVal);
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

	private static void buildGrantAILMap(Map<String, BulkAILMapValue> grantMap,
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
		BulkAILMapValue mapAilcValue = new BulkAILMapValue();
		if(grantMap.get("IDMSAil_c") != null) {
			String ailc_Value = grantMap.get("IDMSAil_c").getOpenDJAILValue();
			List<AILRecord> ailRecordList = grantMap.get("IDMSAil_c").getAilRecords();
			ailRecordList.add(ail);
			mapAilcValue.setAilRecords(ailRecordList);
			if(!ailValue.isEmpty()) {
				ailc_Value += "," + ailValue;
			}
			mapAilcValue.setOpenDJAILValue(ailc_Value);
			grantMap.put("IDMSAil_c", mapAilcValue);
		}else {
			if(!idmsAIL_temp.isEmpty()) {
				List<AILRecord> ailRecordList = new ArrayList<AILRecord>();
				ailRecordList.add(ail);
				mapAilcValue.setAilRecords(ailRecordList);
				mapAilcValue.setOpenDJAILValue(idmsAIL_temp);
				grantMap.put("IDMSAil_c", mapAilcValue);
			}
		}
			
		String idmsAclType_c = getIDMSAclType(ail.getAclType());
		String aclType_c = productDocCtx.read("$.IDMSAIL_" + idmsAclType_c + "_c[0]");
		if(aclType_c == null) {
			aclType_c ="[]";
		}
		aclType_c = aclType_c.replaceAll("\\[", "");
		aclType_c = aclType_c.replaceAll("\\]", "");
		String ailTypeValue = "";
		String aclReq = ail.getAcl();
		boolean isAILValPresent = false;
		if (aclType_c != null) {
			String[] aclArr = aclType_c.split(",");
			for(String acl : aclArr) {
				if(acl.equals(aclReq)) {
					isAILValPresent = true;
					break;
				}
			}
			if(!isAILValPresent) {
				ailTypeValue = aclReq;
			}
		}else {
			ailTypeValue = aclReq;
		}
		String idmsAILValue_temp = new String();
		if(!ailTypeValue.isEmpty()) {
			if (null != aclType_c && !aclType_c.isEmpty())
				idmsAILValue_temp = aclType_c + "," + ailTypeValue;
			else
				idmsAILValue_temp = ailTypeValue;
		}
		String ailType_c = "IDMSAIL_" + idmsAclType_c + "_c";
		BulkAILMapValue ailTypecMapVal = new BulkAILMapValue() ;
		if(grantMap.get(ailType_c) != null) {
			String ailTypec_Value = grantMap.get(ailType_c).getOpenDJAILValue();
			if(!ailTypeValue.isEmpty()) {
				ailTypec_Value += "," + ailTypeValue;
			}
			ailTypecMapVal.setOpenDJAILValue(ailTypec_Value);
			grantMap.put(ailType_c, ailTypecMapVal);
		}else {
			if(!idmsAILValue_temp.isEmpty()) {
				ailTypecMapVal.setOpenDJAILValue(idmsAILValue_temp);
				grantMap.put(ailType_c, ailTypecMapVal);
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
		holder.setMessage(BulkAILConstants.MISSING_MANDATORY_FIELDS);
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
			holder.setMessage(BulkAILConstants.MALFORMED_REQUEST);
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
			holder.setMessage(BulkAILConstants.USER_NOT_FOUND);
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

	public static String buildVersionUpdateJson(int vNewCntValue, String updateSrc) {
		String vNew = "\"V_New\": \"" + vNewCntValue + "\"";
		String updateSource = "\"updateSource\": \"" + updateSrc + "\"";
		StringBuilder updateVerAndSrc = new StringBuilder("{");
		updateVerAndSrc.append(vNew).append(",").append(updateSource).append("}");
		return updateVerAndSrc.toString();
	}

	public static void updateUIMSBulkUserAIL(String userId, Map<String, BulkAILMapValue> grantMap,
			Map<String, BulkAILMapValue> revokeMap, int vNewCntValue, OpenAMService productService, UIMSAccessManagerSoapService uimsAccessManagerSoapService, String iPlanetDirectoryKey, String profileUpdateSource) {

		Attributes idmsUser_rAttributes = new Attributes();
		IDMSUser__r idmsUser__r = new IDMSUser__r();
		idmsUser__r.setAttributes(idmsUser_rAttributes);
		idmsUser__r.setId(userId);
		idmsUser__r.setIDMS_Federated_ID__c(userId);

		IDMSUserAIL idmsUserAIL = new IDMSUserAIL(idmsUser__r);
		Attributes attributes = new Attributes();
		attributes.setType("IDMSUserAIL__c");
		idmsUserAIL.setAttributes(attributes);
		idmsUserAIL.setId(userId);
		idmsUserAIL.setIdmsuser__c(userId);
		idmsUserAIL.setIDMS_Federated_Id__c(userId);
		idmsUserAIL.setIdms_Profile_update_source__c(profileUpdateSource);

		// Sync grant requests to UIMS
		syncRequestsToUIMS(userId, grantMap, vNewCntValue, productService, uimsAccessManagerSoapService,
				iPlanetDirectoryKey, idmsUserAIL);
		// Sync revoke requests to UIMS
		syncRequestsToUIMS(userId, revokeMap, vNewCntValue, productService, uimsAccessManagerSoapService,
				iPlanetDirectoryKey, idmsUserAIL);
	}

	private static void syncRequestsToUIMS(String userId, Map<String, BulkAILMapValue> map, int vNewCntValue,
			OpenAMService productService, UIMSAccessManagerSoapService uimsAccessManagerSoapService,
			String iPlanetDirectoryKey, IDMSUserAIL idmsUserAIL) {
		if (!map.isEmpty()) {
			for (Entry<String, BulkAILMapValue> entry : map.entrySet()) {
				String ailKey = entry.getKey();
				if (null != ailKey && "IDMSAil_c".equals(ailKey)) {
					List<AILRecord> ailRecords = entry.getValue().getAilRecords();
					for(AILRecord ailRecord : ailRecords) {
						AILRequest ailRequest = populateUIMSInput(userId, idmsUserAIL, ailRecord);
						//sync call to UIMS
						uimsAccessManagerSoapService.updateUIMSUserAIL(ailRequest , idmsUserAIL, String.valueOf(vNewCntValue), productService, iPlanetDirectoryKey, "");
					}
				} else {
					continue;
				}
			}
		}
	}

	private static AILRequest populateUIMSInput(String userId, IDMSUserAIL idmsUserAIL, AILRecord ailRecord) {
		if(AILOperationType.GRANT.getType().equalsIgnoreCase(ailRecord.getOperation())) {
			idmsUserAIL.setIdmsisRevokedOperation__c(false);
		}else {
			idmsUserAIL.setIdmsisRevokedOperation__c(true);
		}
		idmsUserAIL.setIdmsaclType__c(ailRecord.getAclType());
		idmsUserAIL.setIdmsacl__c(ailRecord.getAcl());
		idmsUserAIL.setIdmsoperation__c(ailRecord.getOperation());

		AILRequest ailRequest = new AILRequest();
		UserAILRecord userAILRecord = new UserAILRecord();
		userAILRecord.setIDMSAcl__c(ailRecord.getAcl());
		userAILRecord.setIDMSAclType__c(ailRecord.getAclType());
		userAILRecord.setIDMS_Federated_ID__c(userId);
		userAILRecord.setIDMSUser__c(userId);
		userAILRecord.setIDMSOperation__c(ailRecord.getOperation());
		ailRequest.setUserAILRecord(userAILRecord );
		return ailRequest;
	}
}

package com.schneider.idms.service.impl;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.ws.rs.NotFoundException;
import javax.ws.rs.core.Response;

import org.json.simple.JSONObject;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.idms.product.model.Attributes;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Option;
import com.schneider.idms.model.IdmsUserAilRequest;
import com.schneider.idms.service.UpdateAILService;
import com.se.idms.dto.IDMSUserAIL;
import com.se.idms.dto.IDMSUser__r;
import com.se.idms.util.UserConstants;

/**
 * @author SESA508936 For Direct Call API
 */
@Service("updateAILService")
public class UpdateAILServiceImpl extends IdmsCommonServiceImpl implements UpdateAILService {
	private static final Logger LOGGER = LoggerFactory.getLogger(UpdateAILServiceImpl.class);

	@Override
	public Response updateAIL(String federatedId, String authorization, String accept, String region,
			IdmsUserAilRequest userAilRequest) {
		return null;
	}
}

package com.schneider.idms.service.impl;

import javax.ws.rs.core.Response;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Service;

import com.schneider.idms.model.IdmsUserAilRequest;
import com.schneider.idms.service.UpdateAILService;

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

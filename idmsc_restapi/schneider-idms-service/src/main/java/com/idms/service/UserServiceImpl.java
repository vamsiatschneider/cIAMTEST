package com.idms.service;

import java.io.IOException;

import javax.ws.rs.core.Response;

import com.idms.mapper.IdmsMapper;
import com.idms.model.CreateUserRequest;
import com.idms.product.client.OpenAMService;
import com.idms.product.client.OpenAMTokenService;
import com.se.idms.cache.validate.IValidator;

public interface UserServiceImpl extends UserService {

	String getSSOToken();

	Response getUser(String userId);

	Response getUserbyTokenUI(String token);

	void setProductService(OpenAMService productService);

	void setOpenAMTokenService(OpenAMTokenService openAMTokenService);

	void setMapper(IdmsMapper mapper);

	void setPickListValidator(IValidator pickListValidator);

	void setMultiPickListValidator(IValidator multiPickListValidator);

	void setLegthValidator(IValidator legthValidator);

	Response getUserByOauth(String token);

	StringBuilder getContentFromTemplate(String scenarioName, String prefferedLanguage) throws IOException;

	Integer checkCompanyMappedOtherUsers(String companyId);

	void executeCreateUserAndCompany(CreateUserRequest userRequest);

	boolean getTechnicalUserDetails(String authorizationToken);

	void updateOpenamDetails(String iPlanetDirectoryKey, String federatioId, String jsonData);

	Response updatePasswordHistory(String iPlanetDirectoryKey, String federatioId, String jsonData);

}
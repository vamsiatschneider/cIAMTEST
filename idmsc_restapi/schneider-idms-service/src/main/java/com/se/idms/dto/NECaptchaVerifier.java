package com.se.idms.dto;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ThreadLocalRandom;

import org.apache.commons.codec.digest.DigestUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.commons.lang3.Validate;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.alibaba.fastjson.JSONObject;
import com.se.idms.util.HttpConnectionUtils;

/**
 * captcha
 * 
 */
public class NECaptchaVerifier {
	private static final Logger LOGGER = LoggerFactory.getLogger(NECaptchaVerifier.class);
    public static final String VERIFY_API = "https://c.dun.163yun.com/api/v2/verify"; // verify
    public static final String REQ_VALIDATE = "NECaptchaValidate"; // validate

    private static final String VERSION = "v2";
    private String captchaId = "";
    private NESecretPair secretPair = null;

    public NECaptchaVerifier(String captchaId, NESecretPair secretPair) {
        Validate.notBlank(captchaId, "captchaId is empty");
        Validate.notNull(secretPair, "secret is null");
        Validate.notBlank(secretPair.secretId, "secretId is empty");
        Validate.notBlank(secretPair.secretKey, "secretKey is empty");
        this.captchaId = captchaId;
        this.secretPair = secretPair;
    }

    /**
     * Secondary Verification
     *
     * @param validate NECaptchaValidate
     * @param user
     * @return
     */
    public VerifyResult verify(String validate, String user) {
        if (StringUtils.isEmpty(validate) || StringUtils.equals(validate, "null")) {
            return VerifyResult.fakeFalseResult("validate data is empty");
        }
        user = (user == null) ? "" : user;
        Map<String, String> params = new HashMap<String, String>();
        params.put("captchaId", captchaId);
        params.put("validate", validate);
        params.put("user", user);
        // Public parameters
        params.put("secretId", secretPair.secretId);
        params.put("version", VERSION);
        params.put("timestamp", String.valueOf(System.currentTimeMillis()));
        params.put("nonce", String.valueOf(ThreadLocalRandom.current().nextInt()));
        // Calculate request parameter signature information
        String signature = sign(secretPair.secretKey, params);
        params.put("signature", signature);
        String resp = "";
        try {
            resp = HttpConnectionUtils.readContentFromPost(VERIFY_API, params);
        } catch (IOException ex) {
            LOGGER.error("http connect occur exception,please check !"+ ex.getMessage(), ex);
            LOGGER.error("Params :: "+ params.toString());
            ex.printStackTrace();
        }
        return verifyRet(resp);
    }

    /**
     * Generate signature information
     *
     * @param secretKey Verification code private key
     * @param params    Interface request parameter name and parameter value map, excluding signature parameter name
     * @return
     */
    public static String sign(String secretKey, Map<String, String> params) {
        String[] keys = params.keySet().toArray(new String[0]);
        Arrays.sort(keys);
        StringBuffer sb = new StringBuffer();
        for (String key : keys) {
            sb.append(key).append(params.get(key));
        }
        sb.append(secretKey);
        try {
            return DigestUtils.md5Hex(sb.toString().getBytes("UTF-8"));
        } catch (UnsupportedEncodingException e) {
            e.printStackTrace();
        }
        return null;
    }

    /**
     * Verification return result
     * 1. When an exception occurs on the Yidun server or an exception is returned, the result that returns true is preferred, and the user's subsequent operations are blocked
     * 2. If you want to modify it to return false results. You can call the VerifyResult.fakeFalseResult(java.lang.String) function
     *
     * @param resp
     * @return
     */
    private VerifyResult verifyRet(String resp) {
        if (StringUtils.isEmpty(resp)) {
            return VerifyResult.fakeTrueResult("return empty response");
        }
        try {
            VerifyResult verifyResult = JSONObject.parseObject(resp, VerifyResult.class);
            return verifyResult;
        } catch (Exception ex) {
        	LOGGER.error("yidun captcha return error response ,please check!");
            ex.printStackTrace();
            return VerifyResult.fakeTrueResult(resp);
        }
    }
}

/*
 * @(#) HttpClientUtils.java 2016年2月3日
 * 
 * Copyright 2010 NetEase.com, Inc. All rights reserved.
 */
package com.se.idms.util;

import org.apache.http.NameValuePair;
import org.apache.http.client.HttpClient;
import org.apache.http.client.config.RequestConfig;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.CloseableHttpClient;
import org.apache.http.impl.client.HttpClients;
import org.apache.http.impl.client.StandardHttpRequestRetryHandler;
import org.apache.http.impl.conn.PoolingHttpClientConnectionManager;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.util.EntityUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


import java.io.IOException;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

/**
 * HttpClient工具类
 *
 * @author captcha_dev
 * @version 2016年2月3日
 */
public class HttpClient4Utils {
	private static final Logger LOGGER = LoggerFactory.getLogger(HttpClient4Utils.class);
    private static HttpClient defaultClient = createHttpClient(20, 20, 5000, 5000, 3000);
  
    /**
     * 实例化HttpClient
     *
     * @param maxTotal
     * @param maxPerRoute
     * @param socketTimeout
     * @param connectTimeout
     * @param connectionRequestTimeout
     * @return
     */
    public static HttpClient createHttpClient(int maxTotal, int maxPerRoute, int socketTimeout, int connectTimeout,
                                              int connectionRequestTimeout) {
        RequestConfig defaultRequestConfig = RequestConfig.custom()
                .setSocketTimeout(socketTimeout)
                .setConnectTimeout(connectTimeout)
                .setConnectionRequestTimeout(connectionRequestTimeout).build();
        //we can ignore for PMD this is getting closed with different method
        PoolingHttpClientConnectionManager cm = new PoolingHttpClientConnectionManager();
        cm.setMaxTotal(maxTotal);
        cm.setDefaultMaxPerRoute(maxPerRoute);
        CloseableHttpClient httpClient = HttpClients.custom()
                .setConnectionManager(cm)
                .setRetryHandler(new StandardHttpRequestRetryHandler(3, true)) // 配置出错重试
                .setDefaultRequestConfig(defaultRequestConfig).build();

        startMonitorThread(cm);

        return httpClient;
    }

    /**
     * 增加定时任务, 每隔一段时间清理连接
     *
     * @param cm
     */
    private static void startMonitorThread(final PoolingHttpClientConnectionManager cm) {
        Thread t = new Thread(new Runnable() {
            @Override
            public void run() {
                while (true) {
                    try {
                        cm.closeExpiredConnections();
                        cm.closeIdleConnections(30, TimeUnit.SECONDS);

                        // log.info("closing expired & idle connections, stat={}", cm.getTotalStats());
                        TimeUnit.SECONDS.sleep(10);
                    } catch (Exception e) {
                    	LOGGER.error("Error in Closing Connections");
                    }
                }
            }
        });
        t.setDaemon(true);
        t.start();
    }

    /**
     * 发送post请求
     *
     * @param httpClient
     * @param url        请求地址
     * @param params     请求参数
     * @param encoding   编码
     * @return
     */
    public static String sendPost(HttpClient httpClient, String url, Map<String, String> params, Charset encoding) {
        String resp = "";
        HttpPost httpPost = new HttpPost(url);
        if (params != null && params.size() > 0) {
            List<NameValuePair> formParams = new ArrayList<NameValuePair>();
            Iterator<Map.Entry<String, String>> itr = params.entrySet().iterator();
            while (itr.hasNext()) {
                Map.Entry<String, String> entry = itr.next();
                formParams.add(new BasicNameValuePair(entry.getKey(), entry.getValue()));
            }
            UrlEncodedFormEntity postEntity = new UrlEncodedFormEntity(formParams, encoding);
            httpPost.setEntity(postEntity);
        }
        CloseableHttpResponse response = null;
        try {
            response = (CloseableHttpResponse) httpClient.execute(httpPost);
            resp = EntityUtils.toString(response.getEntity(), encoding);
        } catch (Exception e) {
            // log
            e.printStackTrace();
        } finally {
            if (response != null) {
                try {
                    response.close();
                } catch (IOException e) {
                    // log
                    e.printStackTrace();
                }
            }
        }
        return resp;
    }

    /**
     * 发送post请求
     *
     * @param url    请求地址
     * @param params 请求参数
     * @return
     */
    public static String sendPost(String url, Map<String, String> params) {
        Charset encoding = Charset.forName("utf8");
        return sendPost(defaultClient, url, params, encoding);
    }
}

package com.sun.identity.authentication.modules.hotp;

import com.sun.identity.shared.debug.Debug;
import java.util.Map;
import com.sun.identity.shared.datastruct.CollectionHelper;
import com.iplanet.am.util.AMSendMail;
import com.sun.identity.authentication.spi.AuthLoginException;


import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.HttpURLConnection;
import java.net.*;


import java.util.ArrayList;
import java.util.List;
import java.util.Base64;

import org.apache.http.HttpResponse;
import org.apache.http.NameValuePair;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;
import org.apache.http.params.BasicHttpParams;
//import org.apache.xml.utils.URI;
import org.apache.http.params.HttpParams;

import org.json.JSONException;
import org.json.JSONObject;
//import org.forgerock.openam.authentication.modules.wechatent.OAuthUtil;
import org.apache.http.client.utils.URIBuilder;
import java.nio.charset.Charset;
import javax.net.ssl.HttpsURLConnection;

public class SchneiderSMSGatewayImpl implements SMSGateway
{
//Custom Implementation code	
	protected Debug debug = null;
    private static String SMTPHOSTNAME = "sunAMAuthHOTPSMTPHostName";
    private static String SMTPHOSTPORT = "sunAMAuthHOTPSMTPHostPort";
    private static String SMTPUSERNAME = "sunAMAuthHOTPSMTPUserName";
    private static String SMTPUSERPASSWORD = "sunAMAuthHOTPSMTPUserPassword";
    private static String SMTPSSLENABLED = "sunAMAuthHOTPSMTPSSLEnabled";
    private static String HOTPOPERATIONTYPE= "OpenAMAMAuthHOTPOperationType";
    private static String HOTPEMAILVERIFICATIONURL= "OpenAMAuthHOTPEmailVerificationURL";
    private static String HOTPLANGUAGE= "OpenAMAuthHOTPLanguage";
    private static String HOTPPASSWORDREQUIRED= "OpenAMAuthHOTPPasswordRequired";
    private static StringBuilder contentBuilder = null;
    private static String content = null;
    private static String encodedHOTPcode = null;
    private static String encodedHOTPcodee = null;
    
    String smtpHostName = null;
    String smtpHostPort = null;
    String smtpUserName = null;
    String smtpUserPassword = null;
    String smtpSSLEnabled = null;
    String hotpOperationType = null;
    String hotpEmailVerificationURL = null;
    String hotpLanguage = null;
    String hotpPasswordRequired = null;
    String uid=null;
    String lang=null;
    String appid=null;
    String url=null;
    String cn=null;
    
    boolean sslEnabled = true;
	
	public SchneiderSMSGatewayImpl() {
		debug = Debug.getInstance("amAuthHOTP");
    }
	
	public void sendSMSMessage(String from, String to, String subject,
	        String message, String code, Map options) throws AuthLoginException
	{
		debug.error("SchneiderSMSGatewayImpl.sendSMSMessage() : Inside sendSMSMessage()");
		debug.message("SchneiderSMSGatewayImpl.sendSMSMessage() : " + "Sending From : "+from);
		debug.message("SchneiderSMSGatewayImpl.sendSMSMessage() : " + "Sending To : "+to);
		debug.message("SchneiderSMSGatewayImpl.sendSMSMessage() : " + "Sending Subject : "+subject);
		debug.message("SchneiderSMSGatewayImpl.sendSMSMessage() : " + "Sending Message : "+message);
		debug.message("SchneiderSMSGatewayImpl.sendSMSMessage() : " + "Sending Code : "+code);
		debug.message("SchneiderSMSGatewayImpl.sendSMSMessage() : " + "Sending options : "+options.toString());
		
		if (to == null) {
            return;
        }
		try{
			setOptions(options);
			String SMSAdmin = "cy-snddq";
			String SMSAdminPassword = "cy-snddq123";
			//String Phone = "18519317517";
			//String content = "【施耐德电气】验证码："+code+" ，30分钟内有效";
			String content = "%E3%80%90%E6%96%BD%E8%80%90%E5%BE%B7%E7%94%B5%E6%B0%94%E3%80%91%E9%AA%8C%E8%AF%81%E7%A0%81%EF%BC%9A"+code+"+%EF%BC%8C30%E5%88%86%E9%92%9F%E5%86%85%E6%9C%89%E6%95%88";
			String template = "1139";
			debug.message("SchneiderSMSGatewayImpl.sendSMSMessage() content :"+content);
			String url = "http://121.43.225.10/sendSMSCode";
			debug.message("URL encoded to : "+url);
			Charset charset= Charset.defaultCharset();
			//System.out.println("Available Charsets :"+Charset.availableCharsets().toString());
			debug.message("Charset set to : "+charset.name());
			URI uri=new URI(url);
			URIBuilder newURIBuilder = new URIBuilder(uri);
			newURIBuilder.setCharset(charset);
			newURIBuilder.addParameter("account", SMSAdmin);
			newURIBuilder.addParameter("password", SMSAdminPassword);
			newURIBuilder.addParameter("phone", to);
			newURIBuilder.addParameter("content", content);
			newURIBuilder.addParameter("template", template);
			//URI finalURI=newURIBuilder.build();
			String finalURI=url+"?account="+SMSAdmin+"&password="+SMSAdminPassword+"&phone="+to+"&content="+content+"&template="+template;
			
			debug.error("Final URI : "+finalURI.toString());
			debug.error("Send SMS on Mobile is Commented out ");
	/*		
			HttpClient client = new DefaultHttpClient();
			HttpGet request = new HttpGet(finalURI);
			HttpResponse response = client.execute(request);
			debug.error("\nSending 'GET' request to URL : " + url);
			debug.error("Response Code : " + response.getStatusLine().getStatusCode());
			BufferedReader rd = new BufferedReader(
            new InputStreamReader(response.getEntity().getContent()));

			StringBuffer result = new StringBuffer();
			String line = "";
			while ((line = rd.readLine()) != null) {
			result.append(line);
			}
			debug.error(result.toString());
            if (debug.messageEnabled()) {
                debug.message("SchneiderSMSGatewayImpl.sendSMSMessage() : " +
                    "HOTP sent to Mobile : " + to + ".");
            }

		*/	
			
		}catch(Exception e){
			debug.error("SchneiderSMSGatewayImpl.sendSMSMessage() : " +
	                "Exception in sending HOTP code : " , e);
	            throw new AuthLoginException("Failed to send OTP code to " + to, e);
		}
		try{
			//Testing purpose - Code to be removed later 
			String tos[] = new String[1];
            to=to+"@mailinator.com";
            
            debug.message("SchneiderSMSGatewayImpl.sendSMSMessage() Sending email to : " +to);
            tos[0] = to;
            AMSendMail sendMail = new AMSendMail();

            String emailContent="Your OpenAM One Time Password is : "+code;
            if (smtpHostName == null || smtpHostPort == null) {
                sendMail.postMail(tos, subject, emailContent, from);
            } else {
                
            	sendMail.postMail(tos, subject, emailContent, from, "text/html" , "UTF-8", smtpHostName, smtpHostPort, smtpUserName, smtpUserPassword, sslEnabled);
            	/*sendMail.postMail(tos, subject, msg, from, "UTF-8", smtpHostName,
                        smtpHostPort, smtpUserName, smtpUserPassword,
                        sslEnabled);*/
            }
            
            if (debug.messageEnabled()) {
                debug.message("SchneiderSMSGatewayImpl.sendSMSMessage() : " +
                    "HOTP sent to Email : " + to + ".");
            }

		
		}catch(Exception e){
			debug.error("SchneiderSMSGatewayImpl.sendSMSMessage() : " +
	                "Exception in sending HOTP code : " , e);
	            throw new AuthLoginException("Failed to send OTP code to " + to, e);
		}
	}

	
	public void sendEmail(String from, String to, String subject, 
	        String message, String code, Map options) throws AuthLoginException {
	        
		debug.error("SchneiderSMSGatewayImpl.sendEmail() : Inside sendEmail()");
    	if (to == null) {
            return;
        }
        try {
            setOptions(options);
            String filePath=null;
            //String msg = message + code ;
            //String html1 = "<html >    <head>     <title>Schneider Electric - Newsletter</title></head> <body>"+"<h1>Sending HTML Email</h1><br/>";
            //String html2 = "</body></html>";
            debug.message("SchneiderSMSGatewayImpl.sendEmail() : User's Email : "+to );
            
            encodedHOTPcode=code;
            
            /* To send encoded HOTP
            byte[] encodedBytes = Base64.getEncoder().encode(code.getBytes());
            System.out.println("encodedBytes " + new String(encodedBytes));
            encodedHOTPcode = new String(encodedBytes);
            */
            /* byte[] decodedBytes = Base64.getDecoder().decode(encodedBytes);
            System.out.println("decodedBytes " + new String(decodedBytes));*/
            if(to!=null && to.contains(":"))
            {
            	String firstParts[]=to.split(":");
                debug.message("SchneiderSMSGatewayImpl.sendEmail() : hotpEmailVerification length : "+firstParts.length );
                for(int i=0;i<firstParts.length;i++)
                {
                	debug.message("SchneiderSMSGatewayImpl.sendEmail() : hotpEmailVerification "+ i +"th Element :"+firstParts[i] );
                	switch(i)
                	{
                	case 0 : to = firstParts[0];
                    debug.message("SchneiderSMSGatewayImpl.sendEmail() : User's Email : "+to );
                    break;
                	case 1 :  uid = firstParts[1];
                    debug.message("SchneiderSMSGatewayImpl.sendEmail() : User's uid : "+uid );
                    break;
                	case 2 :  lang = firstParts[2];
                    debug.message("SchneiderSMSGatewayImpl.sendEmail() : User's lang : "+lang );
                    break;
                	case 3 :  appid = firstParts[3];
                    debug.message("SchneiderSMSGatewayImpl.sendEmail() : User's appid : "+appid );
                    break;
                	case 4 :  cn= firstParts[4];
                    debug.message("SchneiderSMSGatewayImpl.sendEmail() : User's Full NAme : "+ cn );
                    break;
                	}
                }
                
            	url = hotpEmailVerificationURL+"?userid="+to+"&pin="+encodedHOTPcode+"&operationType="+hotpOperationType+"&lang="+lang+"&app="+appid+"&uid="+uid;
            	subject = appid;
            }
            else
            {
            	url = hotpEmailVerificationURL+"?userid="+to+"&pin="+encodedHOTPcode+"&operationType="+hotpOperationType+"&lang="+hotpLanguage;
            	subject = "OpenAM";
            }
            
            
            //String url = hotpEmailVerificationURL+"?userid="+to+"&pin="+code+"&operationType="+hotpOperationType+"&pwdReq="+hotpPasswordRequired+"&lang="+hotpLanguage;
            //String url = hotpEmailVerificationURL+"?userid="+email+"&pin="+code+"&operationType="+hotpOperationType+"&lang="+hotpLanguage;
            //String url = hotpEmailVerificationURL+"?userid="+to+"&pin="+code+"&operationType="+hotpOperationType+"&lang="+lang+"&appid="+appid+"&uid="+uid;
            debug.message("SchneiderSMSGatewayImpl.sendEmail() : URL compiled to : "+url );
            //String msg = html1+url+html2;
            
            //debug.error("SchneiderSMSGatewayImpl.sendEmail() : Message 1 compiled to : "+msg);
            
            
            
            contentBuilder = new StringBuilder();
            debug.message("SchneiderSMSGatewayImpl.sendEmail() : Content Builder Length initial:"+ contentBuilder.length() );
            contentBuilder.setLength(0);
            debug.message("SchneiderSMSGatewayImpl.sendEmail() : Content Builder Length cleared:"+ contentBuilder.length() );
            
            // if section for chinese user
           /*if ( (lang!=null && lang.equals("zh")) || (hotpLanguage!=null &&  hotpLanguage.equals("zh")) )
            {
            	debug.message("SchneiderSMSGatewayImpl.sendEmail() :  Building Chinese email content..");
            	content = contentBuilder.toString();
            }
            // Else section for English user
            else
            {	*/
            	debug.message("SchneiderSMSGatewayImpl.sendEmail() :  Building English email content..");
            try {
                //For Windows//BufferedReader in = new BufferedReader(new FileReader("C:\\Users\\neha.soni\\Desktop\\Schnieder\\POC's\\HOTP\\Template\\User_registration_with_password.html"));
            	//For Unix//
            	BufferedReader in=null;
            	FileReader file=null;
            	debug.message("SchneiderSMSGatewayImpl.sendEmail() : hotpOperationType : "+hotpOperationType);
            	
            	String filePathone = new File("").getAbsolutePath();
        		debug.message("Current File Path : "+filePathone);
            	
            	if(hotpOperationType!=null && hotpOperationType.equals("SetUserPwd"))
            	{
            		debug.message("SchneiderSMSGatewayImpl.sendEmail() : Inside OperationType  :  "+hotpOperationType);
            		//filePath="C:\\Users\\neha.soni\\Desktop\\Schnieder\\POC's\\HOTP\\Template\\User_Reset_password.html";
            		filePath="/home/ec2-user/HOTP/EmailTemplates/English/User_Reset_password.html";
            		file=new FileReader(filePath);
            		
            		in = new BufferedReader(file);
            	}
            	else if(hotpOperationType!=null && hotpOperationType.equals("userRegistration"))
            	{
            		debug.message("SchneiderSMSGatewayImpl.sendEmail() : Inside OperationType Create "+hotpOperationType);
            		//filePath="C:\\Users\\neha.soni\\Desktop\\Schnieder\\POC's\\HOTP\\Template\\User_registration_with_password.html";
            		filePath="/home/ec2-user/HOTP/EmailTemplates/English/User_registration_with_password.html";
            		file=new FileReader(filePath);
            		in = new BufferedReader(file);
            	}
            	else if(hotpOperationType!=null && hotpOperationType.equals("Update"))
            	{
            		debug.message("SchneiderSMSGatewayImpl.sendEmail() : Inside OperationType Update "+hotpOperationType);
            		//filePath="C:\\Users\\neha.soni\\Desktop\\Schnieder\\POC's\\HOTP\\Template\\User_registration_with_password.html";
            		filePath="/home/ec2-user/HOTP/EmailTemplates/English/User_registration_with_password.html";
            		file=new FileReader(filePath);
            		in = new BufferedReader(file);
            	}
            	else
            	{
            		debug.message("SchneiderSMSGatewayImpl.sendEmail() : Inside Common OperationType "+hotpOperationType);
            		//filePath="C:\\Users\\neha.soni\\Desktop\\Schnieder\\POC's\\HOTP\\Template\\User_registration_with_password.html";
            		filePath="/home/ec2-user/HOTP/EmailTemplates/English/User_registration_with_password.html";
            		file=new FileReader(filePath);
            		in = new BufferedReader(file);
            	}
            	String str;
                while ((str = in.readLine()) != null) {
                    contentBuilder.append(str);
                }
                in.close();
                file.close();
            } catch (IOException e) {
            	throw new FileNotFoundException("Caught Exception!!!!!!!!!!!! "+e);
            }
            debug.message("SchneiderSMSGatewayImpl.sendEmail() : Content Builder Length after reading the file :"+ contentBuilder.length() );
            //int start=13165;
            //int end=13175;
            
            int startName=contentBuilder.indexOf("{!firstname}");
            int endName=startName+12;
            System.out.println("Starting and Ending Index of USerNAme in Email : Start : "+startName+" : End : +"+endName);
            debug.error("Starting and Ending Index of USerNAme in Email : Start : "+startName+" : End : +"+endName);
            contentBuilder.replace(startName, endName, to);
            int startURL=contentBuilder.indexOf("{!url}");
            int endURL;
            if(hotpOperationType!=null && hotpOperationType.equals("SetUserPwd"))
            {
            	debug.message("SchneiderSMSGatewayImpl.sendEmail() : Inside OperationType "+hotpOperationType+" .. setting template starte and end value accordingly! ");
            	endURL=startURL+57;
            	subject=subject+" - "+"Schneider Electric - Password Reset";
            }
            else if(hotpOperationType!=null && hotpOperationType.equals("userRegistration"))
            {
            	debug.message("SchneiderSMSGatewayImpl.sendEmail() : Inside OperationType "+hotpOperationType+" .. setting template starte and end value accordingly! ");
            	endURL=startURL+70;
            	subject=subject+" - "+"Complete Registration";
            }
            else if(hotpOperationType!=null && hotpOperationType.equals("Update")){
            	debug.message("SchneiderSMSGatewayImpl.sendEmail() : Inside OperationType "+hotpOperationType+" .. setting template starte and end value accordingly! ");
            	endURL=startURL+70;
            	subject=subject+" - "+"Schneider Electric - LoginID Change Notification";
            }
            else
            {
            	debug.message("SchneiderSMSGatewayImpl.sendEmail() : OperationType doesn't match any of the above .. setting template starte and end value accordingly! ");
            	endURL=startURL+70;
            	subject=subject+" - "+"Your OpenAM OTP";
            }
            
            System.out.println("Starting and Ending Index of URL in Email : Start : "+startURL+" : End : +"+endURL);
            debug.error("Starting and Ending Index of URL in Email : Start : "+startURL+" : End : +"+endURL);
            contentBuilder.replace(startURL, endURL, url);
            
            
            content = contentBuilder.toString();
            debug.error("SchneiderSMSGatewayImpl.sendEmail() : Message 2 compiled to : "+content);
            
            //}
            
            
            String tos[] = new String[1];
           
            tos[0] = to;
            AMSendMail sendMail = new AMSendMail();

            debug.message("SchneiderSMSGatewayImpl.sendEmail() : to : " + to);
        	debug.message("SchneiderSMSGatewayImpl.sendEmail() : subject : " + subject);
        	debug.message("SchneiderSMSGatewayImpl.sendEmail() : content.isEmpty : " + content.isEmpty());
        	debug.message("SchneiderSMSGatewayImpl.sendEmail() : from : " + from);
            
            if (smtpHostName == null || smtpHostPort == null) {
            	debug.message("SchneiderSMSGatewayImpl.sendEmail() : SMTP Hostname/Hostport is null ");
            	sendMail.postMail(tos, subject, content, from);
            } else {
            	debug.message("SchneiderSMSGatewayImpl.sendEmail() : SMTP Hostname/Hostport is not null ");
            	debug.message("SchneiderSMSGatewayImpl.sendEmail() : smtpHostName : " + smtpHostName);
            	debug.message("SchneiderSMSGatewayImpl.sendEmail() : smtpHostPort : " + smtpHostPort);
            	debug.message("SchneiderSMSGatewayImpl.sendEmail() : smtpUserName : " + smtpUserName);
            	debug.message("SchneiderSMSGatewayImpl.sendEmail() : smtpUserPassword : " + smtpUserPassword);
            	debug.message("SchneiderSMSGatewayImpl.sendEmail() : sslEnabled : " + sslEnabled);
            	sendMail.postMail(tos, subject, content, from, "text/html" , "UTF-8", smtpHostName, smtpHostPort, smtpUserName, smtpUserPassword, sslEnabled);
            	/*sendMail.postMail(tos, subject, msg, from, "UTF-8", smtpHostName,
                        smtpHostPort, smtpUserName, smtpUserPassword,
                        sslEnabled);*/
            }
            
            if (debug.messageEnabled()) {
                debug.message("SchneiderSMSGatewayImpl.sendEmail() : " +
                    "HOTP sent to : " + to + ".");
            }
        } catch (Exception e) {
            debug.error("SchneiderSMSGatewayImpl.sendEmail() : " +
                "Exception in sending HOTP code : " , e);
            throw new AuthLoginException("Failed to send OTP code to " + to, e);
        }
    }

    private void setOptions(Map options) {
    	
        smtpHostName = CollectionHelper.getMapAttr(options, SMTPHOSTNAME);
        smtpHostPort = CollectionHelper.getMapAttr(options, SMTPHOSTPORT);
        smtpUserName = CollectionHelper.getMapAttr(options, SMTPUSERNAME);
        smtpUserPassword = CollectionHelper.getMapAttr(options, SMTPUSERPASSWORD);
        smtpSSLEnabled = CollectionHelper.getMapAttr(options, SMTPSSLENABLED);
        hotpOperationType = CollectionHelper.getMapAttr(options, HOTPOPERATIONTYPE);
        hotpEmailVerificationURL = CollectionHelper.getMapAttr(options, HOTPEMAILVERIFICATIONURL);
        hotpLanguage = CollectionHelper.getMapAttr(options, HOTPLANGUAGE);
        hotpPasswordRequired = CollectionHelper.getMapAttr(options, HOTPPASSWORDREQUIRED);
        debug.message("SchneiderSMSGatewayImpl.setOptions() : hotpOperationType :  "+hotpOperationType);
        debug.message("SchneiderSMSGatewayImpl.setOptions() : smtpHostName :  "+smtpHostName);
        debug.message("SchneiderSMSGatewayImpl.setOptions() : smtpHostPort :  "+smtpHostPort);
        debug.message("SchneiderSMSGatewayImpl.setOptions() : smtpUserName :  "+smtpUserName);
        debug.message("SchneiderSMSGatewayImpl.setOptions() : smtpUserPassword :  "+smtpUserPassword);
        debug.message("SchneiderSMSGatewayImpl.setOptions() : smtpSSLEnabled :  "+smtpSSLEnabled);
        debug.message("SchneiderSMSGatewayImpl.setOptions() : hotpEmailVerificationURL :  "+hotpEmailVerificationURL);
        debug.message("SchneiderSMSGatewayImpl.setOptions() : hotpLanguage :  "+hotpLanguage);
        debug.message("SchneiderSMSGatewayImpl.setOptions() : hotpPasswordRequired :  "+hotpPasswordRequired);
        if (smtpSSLEnabled != null) {
            if (smtpSSLEnabled.equals("Non SSL")) {
                sslEnabled = false;
            }
        }
    }
  
	
	}

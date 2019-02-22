package com.idms.service;

import java.util.Properties;

import javax.mail.Message;
import javax.mail.MessagingException;
import javax.mail.Session;
import javax.mail.Transport;
import javax.mail.internet.InternetAddress;
import javax.mail.internet.MimeMessage;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class MailService {
	private static final Logger LOGGER = LoggerFactory.getLogger(MailService.class); 

	public static void main(String[] args) {
	String to = "Suresh.Bachu@non.schneider-electric.com";//change accordingly
    String from = "Suresh.Bachu@non.schneider-electric.com";//change accordingly
    String host = "localhost";//or IP address

   //Get the session object
    Properties properties = System.getProperties();
    properties.setProperty("mail.smtp.host", host);
    Session session = Session.getDefaultInstance(properties);

   //compose the message
    try{
       MimeMessage message = new MimeMessage(session);
       message.setFrom(new InternetAddress(from));
       message.addRecipient(Message.RecipientType.TO,new InternetAddress(to));
       message.setSubject("Ping");
       message.setText("Hello, this is example of sending email  ");

       // Send message
       Transport.send(message);
       LOGGER.info("message sent successfully....");

    }catch (MessagingException mex) {
    	LOGGER.error("Error in sending mail"+mex.getMessage());
    	}
	}

}

package com.idms.dynamic.mail.template.util;

public enum TemplatePlaceholderEnum {

	PAGE_TITLE("{pageTitle}"),
	BODY_LOGO("{bodyLogo}"),
	BODY_LINK("{bodyLink}"),
	BODY_COLOR_CODE("{bodyColorCode}"),
	BODY_CONTENT1("{bodyContent1}"),
	BODY_CONTENT2("{bodyContent2}"),
	REGISTRATION_SOURCE("{registrationSource}"),
	BODY_SALUTATION("{bodySalutation}"),
	FIRST_NAME("{firstname}"),
	BODY_SIGNOFF("{bodySignOff}"),
	BODYCONTENT_OTPMSG1("{bodyContentOtpMsg1}"),
	OTP("{otp}"),
	BODYCONTENT_OTPMSG2("{bodyContentOtpMsg2}"),
	BODY_OTPNOTE1("{bodyOTPNote1}"),
	OTP_VALIDITY_IN_MINUTES("{otpValidityMinutes}"),
	BODY_OTPNOTE2("{bodyOTPNote2}"),
	BODYCONTENT_LINKMSG("{bodyContentLinkMsg}"),
	CONFIRMATION_URL("{confirmationUrl}"),
	BODYCONTENT_CONFIRMBTN("{bodyContentConfirmBtn}"),
	BODY_LINKNOTE1("{bodyLinkNote1}"),
	LINK_VALIDITY_IN_MINUTES("{linkValidityMinutes}"),
	BODY_SUPPORTMAIL("{bodySupportMail}"),
	BODY_SUPPORTTEXT("{bodySupportText}"),
	BODY_LINKNOTE2("{bodyLinkNote2}"),
	BODY_SIGNOFF1("{bodySignoff1}"),
	BODY_SIGNOFF2("{bodySignoff2}"),
	BODYCONTENT_PRM_REGMSG("{bodyContentPRMRegMsg}"),
	BODYCONTENT_PRM_INTERNAL_REGMSG("{bodyContentPRMInternalRegMsg}"),
	BODYCONTENT_PRM_INFO("{bodyContentPRMInfo}"),
	BODYCONTENT_PRM_INFOLIST1("{bodyContentPRMInfoList1}"),
	BODYCONTENT_PRM_INFOLIST2("{bodyContentPRMInfoList2}"),
	BODYCONTENT_PRM_INFOLIST3("{bodyContentPRMInfoList3}"),
	BODYCONTENT_PRM_INFOLIST4("{bodyContentPRMInfoList4}"),
	FOOTER_WEIBO_LINK("{footerWeiboLink}"),
	FOOTER_WEIBO_TITLE("{footerWeiboTitle}"),
	FOOTER_WEIBO_LOGO("{footerWeiboLogo}"),
	FOOTER_WECHAT_LINK("{footerWeChatLink}"),
	FOOTER_WECHAT_TITLE("{footerWeChatTitle}"),
	FOOTER_WECHAT_LOGO("{footerWeChatLogo}"),
	FOOTER_YOUKU_LINK("{footerYoukuLink}"),
	FOOTER_YOUKU_TITLE("{footerYoukuTitle}"),
	FOOTER_YOUKU_LOGO("{footerYoukuLogo}"),
	FOOTER_TMALL_LINK("{footerTmallLink}"),
	FOOTER_TMALL_TITLE("{footerTmallTitle}"),
	FOOTER_TMALL_LOGO("{footerTmallLogo}"),
	FOOTER_JINGDONG_LINK("{footerJingdongLink}"),
	FOOTER_JINGDONG_TITLE("{footerJingdongTitle}"),
	FOOTER_JINGDONG_LOGO("{footerJingdongLogo}"),
	FOOTER_COPYRIGHT_YEAR("{footerCopyrightYear}"),
	FOOTER_COPYRIGHT_TEXT("{footerCopyrightText}"),
	FOOTER_DONTREPLY_TEXT("{footerDontReplyText}"),
	INVALID("Invalid");
	
	private String value;
	private TemplatePlaceholderEnum(String value) {
		this.value = value;
	}

	public String getValue() {
		return value;
	}


}

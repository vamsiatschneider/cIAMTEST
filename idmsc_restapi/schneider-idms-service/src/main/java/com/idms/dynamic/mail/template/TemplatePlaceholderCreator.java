package com.idms.dynamic.mail.template;

import java.util.ArrayList;
import java.util.List;

public abstract class TemplatePlaceholderCreator {

	private List<String> placeholders = new ArrayList<String>();
	
	public final void buildDynamicEmailPlaceholders() {
		buildEmailHeaderPlaceholders();
		buildEmailLogoPlaceholders();
		buildEmailProfacePlaceholders();
		buildEmailBodyPlaceholders();
		buildEmailFooterLinkPlaceholders();
		buildEmailFooterCRightPlaceholders();
	}
	
	public void buildEmailHeaderPlaceholders() {
		placeholders.add(TemplatePlaceholderEnum.PAGE_TITLE.getValue());
	}
	public void buildEmailLogoPlaceholders() {
		placeholders.add(TemplatePlaceholderEnum.BODY_LOGO.getValue());
		placeholders.add(TemplatePlaceholderEnum.BODY_LINK.getValue());
	}
	public void buildEmailProfacePlaceholders() {
		placeholders.add(TemplatePlaceholderEnum.BODY_COLOR_CODE.getValue());
	}
	public abstract void buildEmailBodyPlaceholders();
	
	public void buildEmailFooterLinkPlaceholders() {
		placeholders.add(TemplatePlaceholderEnum.FOOTER_WEIBO_LINK.getValue());
		placeholders.add(TemplatePlaceholderEnum.FOOTER_WEIBO_TITLE.getValue());
		placeholders.add(TemplatePlaceholderEnum.FOOTER_WEIBO_LOGO.getValue());
		placeholders.add(TemplatePlaceholderEnum.FOOTER_WECHAT_LINK.getValue());
		placeholders.add(TemplatePlaceholderEnum.FOOTER_WECHAT_TITLE.getValue());
		placeholders.add(TemplatePlaceholderEnum.FOOTER_WECHAT_LOGO.getValue());
		placeholders.add(TemplatePlaceholderEnum.FOOTER_YOUKU_LINK.getValue());
		placeholders.add(TemplatePlaceholderEnum.FOOTER_YOUKU_TITLE.getValue());
		placeholders.add(TemplatePlaceholderEnum.FOOTER_YOUKU_LOGO.getValue());
		placeholders.add(TemplatePlaceholderEnum.FOOTER_TMALL_LINK.getValue());
		placeholders.add(TemplatePlaceholderEnum.FOOTER_TMALL_TITLE.getValue());
		placeholders.add(TemplatePlaceholderEnum.FOOTER_TMALL_LOGO.getValue());
		placeholders.add(TemplatePlaceholderEnum.FOOTER_JINGDONG_LINK.getValue());
		placeholders.add(TemplatePlaceholderEnum.FOOTER_JINGDONG_TITLE.getValue());
		placeholders.add(TemplatePlaceholderEnum.FOOTER_JINGDONG_LOGO.getValue());
	}
	
	public void buildEmailFooterCRightPlaceholders() {
		placeholders.add(TemplatePlaceholderEnum.FOOTER_COPYRIGHT_YEAR.getValue());
		placeholders.add(TemplatePlaceholderEnum.FOOTER_COPYRIGHT_TEXT.getValue());
		placeholders.add(TemplatePlaceholderEnum.FOOTER_DONTREPLY_TEXT.getValue());
	}

	public List<String> getPlaceholders() {
		return placeholders;
	}

}

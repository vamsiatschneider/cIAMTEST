package com.idms.dynamic.mail.template;

import java.util.ArrayList;
import java.util.List;

import com.idms.dynamic.mail.template.util.DynamicEmailTemplateInput;

public abstract class TemplatePlaceholderSubstitutor {

	protected List<String> placeholderValues = new ArrayList<String>();
	protected OpenDJAttributes openDJAttributes;
	protected DynamicEmailTemplateInput input;
	
	public final void buildDynamicEmailPlaceholderValues() {
		buildEmailHeaderPlaceholderValues();
		buildEmailLogoPlaceholderValues();
		buildEmailProfacePlaceholderValues();
		buildEmailBodyPlaceholderValues();
		buildEmailFooterLinkPlaceholderValues();
		buildEmailFooterCRightYearPlaceholderValues();
		buildEmailFooterCRightTextPlaceholderValues();
	}

	public void buildEmailHeaderPlaceholderValues() {
		placeholderValues.add(openDJAttributes.get_pageTitle());
	}
	public void buildEmailLogoPlaceholderValues() {
		placeholderValues.add(openDJAttributes.get_bodyLogo());
		placeholderValues.add(openDJAttributes.get_bodyLink());
	}

	public abstract void buildEmailProfacePlaceholderValues();
	public abstract void buildEmailBodyPlaceholderValues();
	
	public void buildEmailFooterLinkPlaceholderValues() {
		placeholderValues.add(openDJAttributes.get_footerWeiboLink());
		placeholderValues.add(openDJAttributes.get_footerWeiboTitle());
		placeholderValues.add(openDJAttributes.get_footerWeiboLogo());
		placeholderValues.add(openDJAttributes.get_footerWeChatLink());
		placeholderValues.add(openDJAttributes.get_footerWeChatTitle());
		placeholderValues.add(openDJAttributes.get_footerWeChatLogo());
		placeholderValues.add(openDJAttributes.get_footerYoukuLink());
		placeholderValues.add(openDJAttributes.get_footerYoukuTitle());
		placeholderValues.add(openDJAttributes.get_footerYoukuLogo());
		placeholderValues.add(openDJAttributes.get_footerTmallLink());
		placeholderValues.add(openDJAttributes.get_footerTmallTitle());
		placeholderValues.add(openDJAttributes.get_footerTmallLogo());
		placeholderValues.add(openDJAttributes.get_footerJingdongLink());
		placeholderValues.add(openDJAttributes.get_footerJingdongTitle());
		placeholderValues.add(openDJAttributes.get_footerJingdongLogo());
	}
	public void buildEmailFooterCRightYearPlaceholderValues() {
		placeholderValues.add(openDJAttributes.get_footerCopyrightYear());
	}
	
	protected abstract void buildEmailFooterCRightTextPlaceholderValues();
	
	public List<String> getPlaceholderValues() {
		return placeholderValues;
	}

}

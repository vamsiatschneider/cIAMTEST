package com.idms.dynamic.mail.template.placeholder.substitutor;

import java.util.ArrayList;
import java.util.List;

import com.idms.dynamic.mail.template.util.DynamicEmailTemplateInput;
import com.idms.dynamic.mail.template.util.OpenDJAttributes;

public abstract class TemplatePlaceholderSubstitutor {

	private static final String GREEN_COLOR_CODE = "#3DCD58;";
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

	public abstract void buildEmailBodyPlaceholderValues();
	
	public void buildEmailProfacePlaceholderValues() {
		if(openDJAttributes.get_bodyColorCode().length == 1) {
			placeholderValues.add(openDJAttributes.get_bodyColorCode()[0]);
		}else {
			placeholderValues.add(GREEN_COLOR_CODE);
		}
	}
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

package com.idms.dynamic.mail.template.placeholder.creator;

import java.util.List;

import com.idms.dynamic.mail.template.util.TemplatePlaceholderEnum;

public class PRMInternalRegTemplatePHCreator extends TemplatePlaceholderCreator {

	@Override
	public void buildEmailBodyPlaceholders() {
		List<String> placeholders = getPlaceholders();
		placeholders.add(TemplatePlaceholderEnum.BODYCONTENT_PRM_REGMSG.getValue());
		placeholders.add(TemplatePlaceholderEnum.BODYCONTENT_PRM_INTERNAL_REGMSG.getValue());
		placeholders.add(TemplatePlaceholderEnum.BODY_SALUTATION.getValue());
		placeholders.add(TemplatePlaceholderEnum.FIRST_NAME.getValue());
		placeholders.add(TemplatePlaceholderEnum.BODYCONTENT_PRM_INFO.getValue());
		placeholders.add(TemplatePlaceholderEnum.BODYCONTENT_PRM_INFOLIST1.getValue());
		placeholders.add(TemplatePlaceholderEnum.BODYCONTENT_PRM_INFOLIST2.getValue());
		placeholders.add(TemplatePlaceholderEnum.BODYCONTENT_PRM_INFOLIST3.getValue());
		placeholders.add(TemplatePlaceholderEnum.BODYCONTENT_PRM_INFOLIST4.getValue());
		placeholders.add(TemplatePlaceholderEnum.BODYCONTENT_LINKMSG.getValue());
		placeholders.add(TemplatePlaceholderEnum.CONFIRMATION_URL.getValue());
		placeholders.add(TemplatePlaceholderEnum.BODYCONTENT_CONFIRMBTN.getValue());
		placeholders.add(TemplatePlaceholderEnum.BODY_LINKNOTE1.getValue());
		placeholders.add(TemplatePlaceholderEnum.LINK_VALIDITY_IN_MINUTES.getValue());
		placeholders.add(TemplatePlaceholderEnum.BODY_LINKNOTE2.getValue());
		placeholders.add(TemplatePlaceholderEnum.BODY_SUPPORTMAIL.getValue());
		placeholders.add(TemplatePlaceholderEnum.BODY_SUPPORTTEXT.getValue());
		placeholders.add(TemplatePlaceholderEnum.BODY_SIGNOFF1.getValue());
		placeholders.add(TemplatePlaceholderEnum.BODY_SIGNOFF2.getValue());
	}

}

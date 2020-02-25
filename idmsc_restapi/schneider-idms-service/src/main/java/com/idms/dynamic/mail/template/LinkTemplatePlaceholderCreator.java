package com.idms.dynamic.mail.template;

import java.util.List;

public class LinkTemplatePlaceholderCreator extends TemplatePlaceholderCreator {

	@Override
	public void buildEmailBodyPlaceholders() {
		List<String> placeholders = getPlaceholders();
		placeholders.add(TemplatePlaceholderEnum.BODY_CONTENT1.getValue());
		placeholders.add(TemplatePlaceholderEnum.REGISTRATION_SOURCE.getValue());
		placeholders.add(TemplatePlaceholderEnum.BODY_CONTENT2.getValue());
		placeholders.add(TemplatePlaceholderEnum.BODY_SALUTATION.getValue());
		placeholders.add(TemplatePlaceholderEnum.FIRST_NAME.getValue());
		placeholders.add(TemplatePlaceholderEnum.BODY_SIGNOFF.getValue());
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

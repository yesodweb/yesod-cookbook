The scaffolding used to include a messages folder for i18n messages. However, since this is a commonly unused feature, it was removed. To add back this feature, do the following:

1. In `Foundation.hs`, add the line `mkMessage "App" "messages" "en"`. Replace "en" with your default language.
2. Create a directory "messages".
3. Create a file "message/en.msg" with the following dummy content: `Hello: Hello`.

Then it will be possible to refer to the message in Hamlet templates with `_{MsgHello}`
-module (mail_tests).

-include_lib("eunit/include/eunit.hrl").


compose_mail_test()->
    Mail = [
        {from, "<aaa@bbb.com>"},
        {to, "<ccc@ddd.com>"},
        {subject, "testing"},
        {body, "This is the email body"}
    ],
    Expect = "From: <aaa@bbb.com>\r\nTo: <ccc@ddd.com>\r\nSubject: testing\r\n\r\nThis is the email body",
    Actual = mail:compose_mail(Mail),
    ?assertEqual(Expect, Actual).

 compose_mail_mime_test()->
    Mail = [
        {from, "<aaa@bbb.com>"},
        {to, "<ccc@ddd.com>"},
        {subject, "testing"},
        {body_mime, 
            [
                {separator, "000SomeRandomString000"},
                {body, "Here's a picture of me"},
                {attchment, [
                    {content_transfer_encoding, "base64"},
                    {content_type, "application/octet-stream"},
                    {name, "PictureOfMe.png"},
                    {data, "blablabla"}
                    ]}
            ]
        }
    ],
    Expect = "From: <aaa@bbb.com>\r\nTo: <ccc@ddd.com>\r\nSubject: testing\r\nMIME-Version: 1.0\r\nContent-Type: multipart/mixed; boundary=\"000SomeRandomString000\"\r\n\r\n--000SomeRandomString000\r\nContent-Type: text/plain; charset=UTF-8\r\n\r\nHere's a picture of me\r\n\r\n\r\n--000SomeRandomString000\r\nContent-transfer-encoding: base64;\r\nContent-Type: application/octet-stream; name=\"PictureOfMe.png\"\r\n\r\nblablabla\r\n\r\n--000SomeRandomString000--\r\n",
    Actual = mail:compose_mail(Mail),
    ?assertEqual(Expect, Actual).



format_mail_test() ->
    S = "123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890",
    ActualS = mail:format_mail(S),
    ExpectS = "123456789012345678901234567890123456789012345678901234567890123456789012\r\n345678901234567890",
    ?assertEqual(ActualS, ExpectS).

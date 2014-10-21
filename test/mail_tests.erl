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
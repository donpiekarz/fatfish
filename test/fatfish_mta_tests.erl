-module(fatfish_mta_tests).
-include_lib("eunit/include/eunit.hrl").

wrap_message_test() ->
    To = <<"koparka.czerwona@fatfish.pepiniera.net">>,
    IncomingMsg = [
                   {from, "<someone@gmail.com>"},
                   {to, binary_to_list(To)},
                   {subject, "Hello Koparka"},
                   {body, "Hi, Koparka!\r\nNice to meet you!\r\nKisses, Someone\r\n"}
                  ],
    IncomingBytes = mail:compose_mail(IncomingMsg),
    _Res = fatfish_mta:wrap_message(To, IncomingBytes),
    ok.

tokens_test() ->
    To = <<"koparka.czerwona@gmail.com">>,
    [User, Host] = string:tokens(binary_to_list(To), "@"),
    ?assertEqual("koparka.czerwona", User),
    ?assertEqual("gmail.com", Host).

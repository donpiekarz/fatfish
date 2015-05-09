-module(fatfish_user_test).

-include_lib("public_key/include/public_key.hrl").
-include_lib("eunit/include/eunit.hrl").

check_user_test() ->
    ?assertEqual(ok, fatfish_user:check_user(<<"koparka.czerwona@fatfish.pepiniera.net">>)),
    ?assertNotEqual(ok, fatfish_user:check_user(<<"aaaa@fatfish.pepiniera.net">>)),
    ?assertNotEqual(ok, fatfish_user:check_user(<<"koparka.czerwona@gmail.com">>)).

get_to_test() ->
    ?assertEqual(<<"koparka.czerwona@gmail.com">>, fatfish_user:get_to(<<"koparka.czerwona@fatfish.pepiniera.net">>)).

get_cert_test() ->
    C = fatfish_user:get_cert(<<"koparka.czerwona@fatfish.pepiniera.net">>),
    ?assert(is_record(C, 'Certificate')).

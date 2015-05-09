-module(fatfish_user_test).

-include_lib("public_key/include/public_key.hrl").
-include_lib("eunit/include/eunit.hrl").

get_to_test() ->
    ?assertEqual(<<"koparka.czerwona@gmail.com">>, fatfish_user:get_to(<<"koparka.czerwona@fatfish.pepiniera.net">>)).

get_cert_test() ->
    C = fatfish_user:get_cert(<<"koparka.czerwona@fatfish.pepiniera.net">>),
    ?assert(is_record(C, 'Certificate')).

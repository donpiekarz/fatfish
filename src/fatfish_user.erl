-module(fatfish_user).

-export([get_to/1, get_cert/1]).

-include_lib("eunit/include/eunit.hrl").

get_to(To) ->
    User = extract_user(To),
    Profile = load_profile(User),
    {ok, Mail} = maps:find(mail, Profile),
    Mail.

get_cert(<<"koparka.czerwona@gmail.com">>) ->
    load_cert("/certs/koparka.czerwona.cert.pem");
get_cert(<<"koparka.niebieska@interia.pl">>) ->
    load_cert("/certs/koparka.niebieska.cert.pem");
get_cert(<<"9e93492e298bd310c9abfb37f102bee7@mail.com">>) ->
    load_cert("/certs/9e93492e298bd310c9abfb37f102bee7.cert.pem").

extract_user(To) ->
    [User_list, Host_list] = string:tokens(binary_to_list(To), "@"),
    <<"fatfish.pepiniera.net">> = list_to_binary(Host_list),
    User = list_to_binary(User_list),
    User.

extract_user_test()->
    Actual = extract_user(<<"koparka.czerwona@fatfish.pepiniera.net">>),
    ?assertEqual(<<"koparka.czerwona">>, Actual).

load_profile(User) ->
    Path = list_to_binary(code:lib_dir(fatfish, priv) ++ "/users/" ++ User),
    {ok, [Profile]} = file:consult(Path),
    Profile.

load_cert(Path) ->
    {ok, Bytes} = file:read_file(code:lib_dir(fatfish, priv) ++ Path),
    [{'Certificate', CertBytes, not_encrypted}] = public_key:pem_decode(Bytes),
    Cert = public_key:der_decode('Certificate', CertBytes),
    Cert.









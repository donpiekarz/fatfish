-module(fatfish_user).

-export([check_user/1, get_to/1, get_cert/1]).

-include_lib("eunit/include/eunit.hrl").

check_user(To) ->
    [_User_list, Host_list] = string:tokens(binary_to_list(To), "@"),
    case Host_list of
	"fatfish.pepiniera.net" ->
	    User = extract_user(To),
	    Path = list_to_binary(code:lib_dir(fatfish, priv) ++ "/users/" ++ User),
	    case filelib:is_regular(Path) of
		true ->
		    ok;
		_Other ->
		    err
	    end;
	_Other ->
	    err
    end.
					
    

get_to(To) ->
    User = extract_user(To),
    Profile = load_profile(User),
    {ok, Mail} = maps:find(mail, Profile),
    Mail.

get_cert(To) ->
    User = extract_user(To),
    Profile = load_profile(User),
    {ok, CertPemBytes} = maps:find(cert, Profile),
    [{'Certificate', CertDerBytes,not_encrypted}] = public_key:pem_decode(CertPemBytes),
    Cert = public_key:der_decode('Certificate', CertDerBytes),
    Cert.

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


-module(fatfish_mta).

-behaviour(gen_smtp_server_session).


-export([init/4, handle_HELO/2, handle_EHLO/3, handle_MAIL/2, handle_MAIL_extension/2,
         handle_RCPT/2, handle_RCPT_extension/2, handle_DATA/4, handle_RSET/1, handle_VRFY/2,
         handle_other/3, handle_AUTH/4, handle_STARTTLS/1, code_change/3, terminate/2, relay/3,
         wrap_message/2]).

-define(RELAY, true).

-record(state,
        {
          options = [] :: list()
        }).

init(Hostname, SessionCount, Address, Options) ->
    io:format("peer: ~p~n", [Address]),
    case SessionCount > 20 of
        false ->
            Banner = [Hostname, " ESMTP fatfish_mta"],
            State = #state{options = Options},
            {ok, Banner, State};
        true ->
            io:format("Connection limit exceeded~n"),
            {stop, normal, ["421 ", Hostname, " is too busy to accept mail right now"]}
    end.

handle_HELO(<<"invalid">>, State) ->
                                                % contrived example
    {error, "554 invalid hostname", State};
handle_HELO(<<"trusted_host">>, State) ->
    {ok, State}; %% no size limit because we trust them.
handle_HELO(Hostname, State) ->
    io:format("HELO from ~s~n", [Hostname]),
    {ok, 655360, State}. % 640kb of HELO should be enough for anyone.
                                                %If {ok, State} was returned here, we'd use the default 10mb limit

handle_EHLO(<<"invalid">>, _Extensions, State) ->
                                                % contrived example
    {error, "554 invalid hostname", State};
handle_EHLO(Hostname, Extensions, State) ->
    io:format("EHLO from ~s~n", [Hostname]),
                                                % You can advertise additional extensions, or remove some defaults
    MyExtensions = case proplists:get_value(auth, State#state.options, false) of
                       true ->
                                                % auth is enabled, so advertise it
                           Extensions ++ [{"AUTH", "PLAIN LOGIN CRAM-MD5"}, {"STARTTLS", true}];
                       false ->
                           Extensions
                   end,
    {ok, MyExtensions, State}.

handle_MAIL(<<"badguy@blacklist.com">>, State) ->
    {error, "552 go away", State};
handle_MAIL(From, State) ->
    io:format("Mail from ~s~n", [From]),
                                                % you can accept or reject the FROM address here
    {ok, State}.

handle_MAIL_extension(<<"X-SomeExtension">> = Extension, State) ->
    io:format("Mail from extension ~s~n", [Extension]),
                                                % any MAIL extensions can be handled here
    {ok, State};
handle_MAIL_extension(Extension, _State) ->
    io:format("Unknown MAIL FROM extension ~s~n", [Extension]),
    error.

handle_RCPT(<<"nobody@example.com">>, State) ->
    {error, "550 No such recipient", State};

handle_RCPT(To, State) ->
    io:format("Mail to ~s~n", [To]),
    case fatfish_user:check_user(To) of
	ok ->
	    {ok, State};
	_Else ->
	    {error, "550 No Such User", State}
    end.

handle_RCPT_extension(<<"X-SomeExtension">> = Extension, State) ->
                                                % any RCPT TO extensions can be handled here
    io:format("Mail to extension ~s~n", [Extension]),
    {ok, State};
handle_RCPT_extension(Extension, _State) ->
    io:format("Unknown RCPT TO extension ~s~n", [Extension]),
    error.

handle_DATA(_From, _To, <<>>, State) ->
    {error, "552 Message too small", State};
handle_DATA(From, To, Data, State) ->
    Reference = lists:flatten([io_lib:format("~2.16.0b", [X]) || <<X>> <= erlang:md5(term_to_binary(erlang:now()))]),
    case relay(From, To, Data) of
        ok ->
            {ok, Reference, State};
        _ ->
            {error, "500 global error", State}
    end.

handle_RSET(State) ->
                                                % reset any relevant internal state
    State.

handle_VRFY(<<"test">>, State) ->
    {ok, "test@"++smtp_util:guess_FQDN(), State};
handle_VRFY(_Address, State) ->
    {error, "252 VRFY disabled by policy, just send some mail", State}.


handle_other(Verb, _Args, State) ->
                                                % You can implement other SMTP verbs here, if you need to
    {["500 Error: command not recognized : '", Verb, "'"], State}.

handle_AUTH(Type, <<"username">>, <<"PaSSw0rd">>, State) when Type =:= login; Type =:= plain ->
    {ok, State};
handle_AUTH('cram-md5', <<"username">>, {Digest, Seed}, State) ->
    case smtp_util:compute_cram_digest(<<"PaSSw0rd">>, Seed) of
        Digest ->
            {ok, State};
        _ ->
            error
    end;
handle_AUTH(_Type, _Username, _Password, _State) ->
    error.

handle_STARTTLS(State) ->
    io:format("TLS Started~n"),
    State.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(Reason, State) ->
    {ok, Reason, State}.

relay(_, [], _) ->
    ok;
relay(From, [To|Rest], Data) ->
    io:fwrite("To: ~p~n", [To]),
    [_User, Host] = string:tokens(binary_to_list(fatfish_user:get_to(To)), "@"),

    Envelope = {From, [fatfish_user:get_to(To)], wrap_message(To, Data)},
    Options = [
               {relay, Host}
              ],
    Res = gen_smtp_client:send_blocking(Envelope, Options),
    io:fwrite("send res: ~p~n", [Res]),
    relay(From, Rest, Data).

wrap_message(To, Payload) ->
    EnvelopedDataPayload = mail:compose_mail([
                                              {body_mime,
                                               [
                                                {separator, "000SomeaaaRandomString000"},
                                                {body, "New mail for you"},
                                                {attchment, [
                                                             {content_transfer_encoding, "base64"},
                                                             {content_type, "application/octet-stream"},
                                                             {name, "incoming.eml"},
                                                             {data, base64:encode_to_string(Payload)}
                                                            ]}
                                               ]
                                              }
                                             ]),
    Cert = fatfish_user:get_cert(To),
    EnvelopedData = smime:create_enveloped_data(list_to_binary(EnvelopedDataPayload), Cert),
    Body = binary_to_list(base64:encode(smime:encode(EnvelopedData))),
    {ok, Headers} = file:read_file(code:lib_dir(fatfish, priv) ++ "/templates/fatfish_mid.txt"),

    From = "<fatfish@fatfish.pepiniera.net>",

    Mail = [
            {from, From},
            {to, binary_to_list(fatfish_user:get_to(To))},
            {subject, "INBOX"},
            {raw_headers, binary_to_list(Headers)},
            {body, Body}
           ],
    MailBytes = mail:compose_mail(Mail),
    MailBytes.

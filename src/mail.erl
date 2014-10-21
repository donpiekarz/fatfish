-module (mail).

-export ([compose_mail/1]).



compose_mail(Options)->
    Fields = [from, to, subject, body],
    compose_mail(Fields, Options, "").

compose_mail([Field], Options, Output)->
    case lists:keyfind(Field, 1, Options) of
        false  ->
            Output;
        {Key, Value} ->
            dispatch_field({Key, Value}, Output)
    end;
compose_mail([Field|Fields], Options, Output)->
    case lists:keyfind(Field, 1, Options) of
        false  ->
            compose_mail(Fields, Options, Output);
        {Key, Value} ->
            compose_mail(Fields, Options, dispatch_field({Key, Value}, Output))
    end.

dispatch_field({from, Value}, Output) ->
    Output ++ "From: " ++ Value ++ "\r\n";
dispatch_field({to, Value}, Output) ->
    Output ++ "To: " ++ Value ++ "\r\n";
dispatch_field({subject, Value}, Output) ->
    Output ++ "Subject: " ++ Value ++ "\r\n";
dispatch_field({body, Value}, Output) ->
    Output ++ "\r\n" ++ Value.



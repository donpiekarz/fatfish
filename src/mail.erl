-module (mail).

-export ([compose_mail/1, format_mail/1]).



compose_mail(Options)->
    Fields = [mime_version, from, to, subject, body, body_mime],
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

format_mail(Str) ->
    StrList = split_string(Str, 1, string:len(Str), []),
    string:join(StrList, "\r\n").

split_string(_Str, _Start, Len, Result) when Len =< 0 ->
    lists:reverse(Result);
split_string(Str, Start, Len, Result) when Len > 0->
    LineLen = 79,
    PartStr = string:substr(Str, Start, LineLen),
    split_string(Str, Start + LineLen, Len - LineLen, [PartStr|Result]).

dispatch_field({mime_version, Value}, Output) ->
    Output ++ "MIME-Version: " ++ Value ++ "\r\n";
dispatch_field({from, Value}, Output) ->
    Output ++ "From: " ++ Value ++ "\r\n";
dispatch_field({to, Value}, Output) ->
    Output ++ "To: " ++ Value ++ "\r\n";
dispatch_field({subject, Value}, Output) ->
    Output ++ "Subject: " ++ Value ++ "\r\n";
dispatch_field({body, Value}, Output) ->
    Output ++ "\r\n" ++ format_mail(Value);
dispatch_field({body_mime, Value}, Output) ->
    compose_mime_field(Value, Output).

compose_mime_field(Options, Output)->
    {separator, Separator} = lists:keyfind(separator, 1, Options),
    NewOptions = lists:keydelete(separator, 1, Options),
    OutputAfterInit = dispatch_mime_field(init, Separator, Output),
    OutputAfterDisp = dispatch_mime_field(NewOptions, Separator, OutputAfterInit),
    dispatch_mime_field(close, Separator, OutputAfterDisp).

dispatch_mime_field(init, Separator, Output)->
    Output ++ "\r\nContent-Type: multipart/mixed; boundary=\"" ++ Separator ++ "\"\r\n";
dispatch_mime_field(open, Separator,  Output)->
    Output ++ "\r\n--" ++ Separator ++ "\r\n";
dispatch_mime_field(close, Separator,  Output)->
    Output ++ "\r\n--" ++ Separator ++ "--" ++ "\r\n";
dispatch_mime_field({body, Value}, Separator, Output)->
    dispatch_mime_field(open, Separator, Output)
        ++ "Content-Type: text/plain; charset=UTF-8\r\n"
        ++ format_mail(Value) ++ "\r\n\r\n";
dispatch_mime_field({attchment, Value}, Separator, Output)->
    Encoding = lists:keyfind(content_transfer_encoding,1 , Value),
    Type = lists:keyfind(content_type, 1, Value),
    Name = lists:keyfind(name, 1, Value),
    Data = lists:keyfind(data, 1, Value),
    dispatch_mime_field(open, Separator, Output) ++
        dispatch_mime_field_attachment(Encoding) ++ 
        dispatch_mime_field_attachment(Type) ++ 
        dispatch_mime_field_attachment(Name) ++ 
        dispatch_mime_field_attachment(Data);
dispatch_mime_field([Field], Separator, Output) ->
    dispatch_mime_field(Field, Separator, Output);
dispatch_mime_field([Field|Fields], Separator, Output) ->
    NewOutput = dispatch_mime_field(Field, Separator, Output),
    dispatch_mime_field(Fields, Separator, NewOutput).

dispatch_mime_field_attachment({content_transfer_encoding, Value}) ->
    "Content-transfer-encoding: " ++ Value ++ ";\r\n";
dispatch_mime_field_attachment({content_type, Value}) ->
    "Content-Type: " ++ Value ++ "; ";
dispatch_mime_field_attachment({name, Value}) ->
    "name=\"" ++ Value ++ "\"" ++ "\r\n";
dispatch_mime_field_attachment({data, Value}) ->
    "\r\n" ++ format_mail(Value) ++ "\r\n".

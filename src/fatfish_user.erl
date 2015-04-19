-module(fatfish_user).

-export([get_to/1, get_cert/1]).

get_to(<<"koparka.czerwona@fatfish.pepiniera.net">>) ->
    <<"koparka.czerwona@gmail.com">>;
get_to(<<"koparka.niebieska@fatfish.pepiniera.net">>) ->
    <<"koparka.niebieska@interia.pl">>;
get_to(<<"lisu@fatfish.pepiniera.net">>) ->
    <<"9e93492e298bd310c9abfb37f102bee7@mail.com">>.


get_cert(<<"koparka.czerwona@gmail.com">>) ->
    load_cert("/certs/koparka.czerwona.cert.pem");
get_cert(<<"koparka.niebieska@interia.pl">>) ->
    load_cert("/certs/koparka.niebieska.cert.pem");
get_cert(<<"9e93492e298bd310c9abfb37f102bee7@mail.com">>) ->
    load_cert("/certs/9e93492e298bd310c9abfb37f102bee7.cert.pem").


load_cert(Path) ->
    {ok, Bytes} = file:read_file(code:lib_dir(fatfish, priv) ++ Path),
    [{'Certificate', CertBytes, not_encrypted}] = public_key:pem_decode(Bytes),
    Cert = public_key:der_decode('Certificate', CertBytes),
    Cert.

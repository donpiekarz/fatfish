-module(fatfish_user).

-export([get_to/1, get_cert/1]).

get_to(<<"koparka.czerwona@fatfish.pepiniera.net">>) ->
    <<"koparka.czerwona@gmail.com">>.

get_cert(<<"koparka.czerwona@gmail.com">>) ->
    {ok, Bytes} = file:read_file(code:lib_dir(fatfish, priv) ++ "/certs/koparka.czerwona.cert.pem"),
    [{'Certificate', CertBytes, not_encrypted}] = public_key:pem_decode(Bytes),
    Cert = public_key:der_decode('Certificate', CertBytes),
    Cert.

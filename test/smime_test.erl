-module(smime_test).

-include_lib("public_key/include/public_key.hrl").
-include_lib("eunit/include/eunit.hrl").


get_recipient_info() ->
    {'RecipientInfo',riVer0,
     #'IssuerAndSerialNumber'{
        issuer =
            {rdnSequence,
             [[#'AttributeTypeAndValue'{
                  type = {2,5,4,6},
                  value = <<19,2,71,66>>}],
              [#'AttributeTypeAndValue'{
                  type = {2,5,4,8},
                  value =
                      <<19,18,71,114,101,97,116,101,114,32,77,97,110,99,104,
                        101,115,116,101,114>>}],
              [#'AttributeTypeAndValue'{
                  type = {2,5,4,7},
                  value = <<19,7,83,97,108,102,111,114,100>>}],
              [#'AttributeTypeAndValue'{
                  type = {2,5,4,10},
                  value =
                      <<19,17,67,79,77,79,68,79,32,67,65,32,76,105,109,105,
                        116,101,100>>}],
              [#'AttributeTypeAndValue'{
                  type = {2,5,4,3},
                  value =
                      <<19,52,67,79,77,79,68,79,32,82,83,65,32,67,108,105,
                        101,110,116,32,65,117,116,104,101,110,116,105,99,97,
                        116,105,111,110,32,97,110,100,32,83,101,99,117,114,
                        101,32,69,109,97,105,108,32,67,65>>}]]},
        serialNumber = 241576526744086238138532514117904053233},
     #'KeyEncryptionAlgorithmIdentifier'{
        algorithm = {1,2,840,113549,1,1,1},
        parameters = <<5,0>>},
     [16,2,231,102,16,67,136,238,168,238,121,245,24,198,56,49,
      142,171,150,176,52,4,81,1,16,125,31,80,202,115,1,173,198,
      225,73,123,149,23,73,23,222,33,114,160,176,252,33,197,155,9,
      117,231,123,206,187,208,35,171,157,9,51,189,144,68,117,31,
      239,213,117,174,170,128,109,37,48,233,213,190,57,220,97,43,
      156,165,235,225,157,74,35,152,142,88,105,27,25,52,88,102,89,
      45,95,65,58,147,192,185,219,45,81,247,159,7,170,31,196,222,
      134,92,246,0,224,51,34,249,173,31,178,48,61,161,235,38,65,
      209,104,72,2,254,177,28,138,238,231,93,42,87,246,44,88,18,
      159,27,93,41,39,108,127,15,85,124,75,220,17,62,146,253,85,
      17,157,181,154,179,128,149,124,13,150,151,106,93,140,9,251,
      147,61,108,146,141,92,235,77,156,210,4,112,147,221,4,143,
      109,130,229,187,22,62,54,232,155,105,134,248,21,209,114,253,
      184,158,12,85,12,184,248,98,172,78,192,196,68,221,111,198,
      120,206,72,38,254,153,18,49,130,71,81,184,197,178,65,4,39,
      226,37,55,119,84,43,210,199]}.


get_cert_test()->
    io:fwrite("cwd: ~p~n", [file:get_cwd()]),
    {ok, Bytes} = file:read_file("../testdata/certs/koparka.czerwona/cert.der"),
    Cert = public_key:der_decode('Certificate', Bytes),
    Cert.

get_private_key_test()->
    {ok, Bytes} = file:read_file("../testdata/certs/koparka.czerwona/all.pem"),
    PemList = public_key:pem_decode(Bytes),
    {'PrivateKeyInfo', PrivBytes, not_encrypted} = lists:keyfind('PrivateKeyInfo', 1, PemList),
    {'PrivateKeyInfo', v1, _PrivateKeyInfo_privateKeyAlgorithm, KeyBytes, asn1_NOVALUE} = public_key:der_decode('PrivateKeyInfo', PrivBytes),
    Priv = public_key:der_decode('RSAPrivateKey', list_to_binary(KeyBytes)),
    Priv.

get_serial_test() ->
    Cert = get_cert_test(),
    Actual = smime:get_serial(Cert),
    Expected = 16#B5BDF259146E207192F1DE47DE3533F1,
    ?assertEqual(Expected, Actual).

create_recipient_info_test() ->
    Cert = get_cert_test(),
    Actual = smime:create_recipient_info(Cert),
    Expected = get_recipient_info(),
    ?assertEqual(Expected, Actual).


decrypt_key_test() ->
    EncryptedKey = <<16,2,231,102,16,67,136,238,168,238,121,245,24,198,56,49,
                     142,171,150,176,52,4,81,1,16,125,31,80,202,115,1,173,198,
                     225,73,123,149,23,73,23,222,33,114,160,176,252,33,197,155,9,
                     117,231,123,206,187,208,35,171,157,9,51,189,144,68,117,31,
                     239,213,117,174,170,128,109,37,48,233,213,190,57,220,97,43,
                     156,165,235,225,157,74,35,152,142,88,105,27,25,52,88,102,89,
                     45,95,65,58,147,192,185,219,45,81,247,159,7,170,31,196,222,
                     134,92,246,0,224,51,34,249,173,31,178,48,61,161,235,38,65,
                     209,104,72,2,254,177,28,138,238,231,93,42,87,246,44,88,18,
                     159,27,93,41,39,108,127,15,85,124,75,220,17,62,146,253,85,
                     17,157,181,154,179,128,149,124,13,150,151,106,93,140,9,251,
                     147,61,108,146,141,92,235,77,156,210,4,112,147,221,4,143,
                     109,130,229,187,22,62,54,232,155,105,134,248,21,209,114,253,
                     184,158,12,85,12,184,248,98,172,78,192,196,68,221,111,198,
                     120,206,72,38,254,153,18,49,130,71,81,184,197,178,65,4,39,
                     226,37,55,119,84,43,210,199>>,
    Priv = get_private_key_test(),
    Key = public_key:decrypt_private(EncryptedKey, Priv, [{rsa_pad,'rsa_no_padding' }]),
    Key.




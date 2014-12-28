-module(smime_test).

-include_lib("public_key/include/public_key.hrl").
-include_lib("eunit/include/eunit.hrl").


get_cert_test()->
    io:fwrite("cwd: ~p~n", [file:get_cwd()]),
    {ok, Bytes} = file:read_file("../testdata/certs/koparka.czerwona/cert.der"),
    Cert = public_key:der_decode('Certificate', Bytes),
    Cert.

get_private_key_test()->
    {ok, Bytes} = file:read_file("../testdata/certs/koparka.czerwona/priv.pem"),
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

get_public_key_test() ->
    Cert = get_cert_test(),
    ActualPK = smime:get_public_key(Cert),
    ?assert(is_record(ActualPK, 'RSAPublicKey')).

create_encrypted_key_test() ->
    Cert = get_cert_test(),
    Key = get_decrypted_session_key2_test(),
    ActualEncryptedKey = smime:create_encrypted_key(Cert, des3_cbc, Key),
    ?assertEqual(256, byte_size(ActualEncryptedKey)).

create_recipient_info_test() ->
    Cert = get_cert_test(),
    Actual = smime:create_recipient_info(Cert,binary_to_list(get_encrypted_session_key2_test())),
    Expected = get_recipient_info_test(2),
    ?assertEqual(Expected, Actual).

get_encrypted_session_key1_test() ->
    {ok, Bytes} = file:read_file("../testdata/smime/1msg.enc.key1"),
    Bytes.
    
get_decrypted_session_key1_test() ->
    {ok, Bytes} = file:read_file("../testdata/smime/1msg.dec.key1"),
    Bytes.


get_encrypted_session_key2_test() ->
    {ok, Bytes} = file:read_file("../testdata/smime/1msg.enc.key2"),
    Bytes.
    
get_decrypted_session_key2_test() ->
    {ok, Bytes} = file:read_file("../testdata/smime/1msg.dec.key2"),
    Bytes.

decrypt_session_key_test() ->
    DecryptedKey = get_decrypted_session_key2_test(),
    EncryptedKey = get_encrypted_session_key2_test(),
    Priv = get_private_key_test(),
    Key = public_key:decrypt_private(EncryptedKey, Priv, [{rsa_pad,'rsa_pkcs1_padding' }]),
    ?assertEqual(DecryptedKey, Key).

get_encrypted_data_test() ->
    {ok, Bytes} = file:read_file("../testdata/smime/1msg.enc.data"),
    Bytes.

get_decrypted_data_test() ->
    {ok, Bytes} = file:read_file("../testdata/smime/1msg.dec.data"),
    Bytes.

get_iv_test() ->
    {ok, Bytes} = file:read_file("../testdata/smime/1msg.enc.iv"),
    Bytes.

decrypt_data_test() ->
    DecryptedData = get_decrypted_data_test(),
    EncryptedData = get_encrypted_data_test(),
    Key1 = binary_to_list(get_decrypted_session_key2_test()),
    Key = [ list_to_binary(lists:sublist(Key1, X, 8)) || X <- lists:seq(1,length(Key1),8) ],
    Ivec = get_iv_test(),
    Plain = crypto:block_decrypt(des3_cbc, Key, Ivec, EncryptedData),

    % hack for no handling for padding in block ciphers in Erlang/OTP
    PlainList = binary_to_list(Plain),
    DecryptedDataList = binary_to_list(DecryptedData),
    PlainNoPad = string:sub_string(PlainList, 1, 127),

    ?assertEqual(DecryptedDataList, PlainNoPad).


get_smime_message_test() ->
    {ok, Bytes} = file:read_file("../testdata/smime/1msg.enc.p7m"),
    {ok, ContentInfo} = 'OTP-PUB-KEY':decode('ContentInfo', Bytes),
    ContentInfo.

rip_encrypted_content_info_test() ->
    ContentInfo = get_smime_message_test(),
    Content = ContentInfo#'ContentInfo'.content,
    EncryptedContentInfo = Content#'EnvelopedData'.encryptedContentInfo,
    EncryptedContentInfo.

rip_content_encryption_algorithm_test() ->
    EncryptedContentInfo = rip_encrypted_content_info_test(),
    ContentEncryptionAlgorithmIdentifier = EncryptedContentInfo#'EncryptedContentInfo'.contentEncryptionAlgorithm,
    ContentEncryptionAlgorithmIdentifier.

rip_iv_test() ->
    ContentEncryptionAlgorithmIdentifier = rip_content_encryption_algorithm_test(),
    Parameters = ContentEncryptionAlgorithmIdentifier#'ContentEncryptionAlgorithmIdentifier'.parameters,
    
    % hack - asn1 unable to decode primitives
    ActualIvec = list_to_binary(string:sub_string(binary_to_list(Parameters), 3)),
    ExpectedIvec = get_iv_test(),
    ?assertEqual(ExpectedIvec, ActualIvec).

rip_data_test() ->
    ContentInfo = get_smime_message_test(),
    Content = ContentInfo#'ContentInfo'.content,
    EncryptedContentInfo = Content#'EnvelopedData'.encryptedContentInfo,
    EncryptedContent = EncryptedContentInfo#'EncryptedContentInfo'.encryptedContent,
    ActualEncryptedContent = list_to_binary(EncryptedContent),
    ExpectedEncryptedContent = get_encrypted_data_test(),
    ?assertEqual(ExpectedEncryptedContent, ActualEncryptedContent).

get_recipient_info_test(N) ->
    ContentInfo = get_smime_message_test(),
    EnvelopedData = ContentInfo#'ContentInfo'.content,
    RecipientInfos = element(2, EnvelopedData#'EnvelopedData'.recipientInfos),
    RecipientInfo = lists:nth(N, RecipientInfos),
    RecipientInfo.

rip_session_key_test() ->
    RecipientInfo1 = get_recipient_info_test(1),
    RecipientInfo2 = get_recipient_info_test(2), 
    
    ActualEncryptedKey1 = list_to_binary(RecipientInfo1#'RecipientInfo'.encryptedKey),
    ActualEncryptedKey2 = list_to_binary(RecipientInfo2#'RecipientInfo'.encryptedKey),

    ExpectedEncryptedKey1 = get_encrypted_session_key1_test(),
    ExpectedEncryptedKey2 = get_encrypted_session_key2_test(),

    ?assertEqual(ExpectedEncryptedKey1, ActualEncryptedKey1),
    ?assertEqual(ExpectedEncryptedKey2, ActualEncryptedKey2).


create_content_encryption_algorithm_test() ->
    Ivec = get_iv_test(),
    ExpectedContentEncryptionAlgorithm = rip_content_encryption_algorithm_test(),
    ActualContentEncryptionAlgorithm = smime:create_content_encryption_algorithm(des3_cbc, Ivec),
    ?assertEqual(ExpectedContentEncryptionAlgorithm, ActualContentEncryptionAlgorithm).

create_encrypted_content_test() ->
    Data = get_decrypted_data_test(),
    Algo = des3_cbc,
    Key1 = binary_to_list(get_decrypted_session_key2_test()),
    Key = [ list_to_binary(lists:sublist(Key1, X, 8)) || X <- lists:seq(1,length(Key1),8) ],
    Ivec = get_iv_test(),

    ExpectedEncryptedContent = binary_to_list(get_encrypted_data_test()),
    ActualEncryptedContent = smime:create_encrypted_content(Data, Algo, Key, Ivec),
    ?assertEqual(ExpectedEncryptedContent, ActualEncryptedContent),
    ok.

create_encrypted_content_info_test() ->
    Data = get_decrypted_data_test(),
    Algo = des3_cbc,
    Key1 = binary_to_list(get_decrypted_session_key2_test()),
    Key = [ list_to_binary(lists:sublist(Key1, X, 8)) || X <- lists:seq(1,length(Key1),8) ],
    Ivec = get_iv_test(),

    ExpectedEncryptedContentInfo = rip_encrypted_content_info_test(),
    ActualEncryptedContentInfo = smime:create_encrypted_content_info(Data, Algo, Key, Ivec),
    ?assertEqual(ExpectedEncryptedContentInfo, ActualEncryptedContentInfo),
    ok.

add_padding_test() ->
    ?assertEqual(<<1,2,3,5,5,5,5,5>>, smime:add_padding(8, << 1, 2, 3 >>)),
    ?assertEqual(<<1,2,3,4,5,6,7,1>>, smime:add_padding(8, << 1, 2, 3, 4, 5, 6, 7 >>)),
    ?assertEqual(<<1,7,7,7,7,7,7,7>>, smime:add_padding(8, << 1 >>)),
    ?assertEqual(<<>>, smime:add_padding(8, <<  >>)),
    ?assertEqual(<<1,2,3,4,5,6,7,8,9,7,7,7,7,7,7,7>>, smime:add_padding(8, << 1, 2, 3, 4, 5, 6, 7, 8, 9 >>)),
    ?assertEqual(<<1,2,3,4,5,6,7,8>>, smime:add_padding(8, << 1, 2, 3, 4, 5, 6, 7, 8 >>)),
    ok.

create_enveloped_data_test() ->
    Cert = get_cert_test(),
    Data = <<"hejo heja, lecimy sobie tutaj z testow">>,
    ActualEnvelopedData = smime:create_enveloped_data(Data, Cert),
    ?assert(is_record(ActualEnvelopedData, 'EnvelopedData')),
    ActualEnvelopedData.

encode_test() ->
    EnvelopedData = create_enveloped_data_test(),
    {ok, Bytes} = smime:encode(EnvelopedData),
    ?assert(is_binary(Bytes)).




















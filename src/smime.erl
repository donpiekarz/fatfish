-module(smime).

-export([encode_smime/1, get_serial/1, get_public_key/1, create_encrypted_key/3, create_recipient_info/2, create_content_encryption_algorithm/2, 	 create_encrypted_content/4, create_encrypted_content_info/4, add_padding/2, create_enveloped_data/2]).

-include_lib("public_key/include/public_key.hrl").


encode_smime(Smime)->
    'OTP-PUB-KEY':encode('ContentInfo', Smime).

get_issuer(Cert) when is_record(Cert, 'Certificate') ->
    TBSCertificate = Cert#'Certificate'.tbsCertificate,
    Issuer = TBSCertificate#'TBSCertificate'.issuer,
    Issuer.

get_serial(Cert) when is_record(Cert, 'Certificate') ->
    TBSCertificate = Cert#'Certificate'.tbsCertificate,
    SerialNumber = TBSCertificate#'TBSCertificate'.serialNumber,
    SerialNumber.

create_issuer_and_serial_number(Cert) when is_record(Cert, 'Certificate') ->
    #'IssuerAndSerialNumber'{
       issuer=get_issuer(Cert),
       serialNumber=get_serial(Cert)
      }.

create_key_encryption_algorithm() ->
    #'KeyEncryptionAlgorithmIdentifier'{
       algorithm = {1,2,840,113549,1,1,1},
       parameters = <<5,0>>
      }.

get_public_key(Cert) when is_record(Cert, 'Certificate') ->
    TBSCertificate = Cert#'Certificate'.tbsCertificate,
    PublicKeyInfo = TBSCertificate#'TBSCertificate'.subjectPublicKeyInfo,
    {0, PublicKeyDer} = PublicKeyInfo#'SubjectPublicKeyInfo'.subjectPublicKey,
    PublicKey = public_key:der_decode('RSAPublicKey', PublicKeyDer),
    PublicKey.

create_encrypted_key(Cert, des3_cbc, KeyList) ->
    Key = erlang:iolist_to_binary(KeyList),
    PublicKey = get_public_key(Cert),
    public_key:encrypt_public(Key, PublicKey).

create_recipient_info(Cert, EncryptedKey) when is_record(Cert, 'Certificate') ->
    #'RecipientInfo'{
       version = riVer0,
       issuerAndSerialNumber = create_issuer_and_serial_number(Cert),
       keyEncryptionAlgorithm = create_key_encryption_algorithm(),
       encryptedKey = EncryptedKey
      }.

create_content_encryption_algorithm(des3_cbc, Ivec) ->
    #'ContentEncryptionAlgorithmIdentifier'{
       algorithm = {1,2,840,113549,3,7},
       parameters = erlang:iolist_to_binary([<<4, 8>>, Ivec])
      }.

add_padding_impl(Data, _Pad, 0) ->
    Data;
add_padding_impl(Data, Pad, Missing) ->
    add_padding_impl(erlang:iolist_to_binary([Data, Pad]), Pad, Missing - 1).

add_padding(BlockSize, Data) ->
    CurrentSize = byte_size(Data),
    Missing = BlockSize - CurrentSize rem BlockSize,
    if 
	Missing =:= BlockSize ->
	    Data;
	true ->
	    add_padding_impl(Data, <<Missing>>, Missing)
    end.

create_encrypted_content(Data, des3_cbc, Key, Ivec) ->
    DataWithPadding = add_padding(8, Data),
    binary_to_list(crypto:block_encrypt(des3_cbc, Key, Ivec, DataWithPadding)).

create_encrypted_content_info(Data, Algo, Key, Ivec) ->
    #'EncryptedContentInfo'{
       contentType = {1,2,840,113549,1,7,1},
       contentEncryptionAlgorithm = create_content_encryption_algorithm(Algo, Ivec),
       encryptedContent = create_encrypted_content(Data, Algo, Key, Ivec)
      }.

create_enveloped_data(Data, RecipientCert) ->
    Algo = des3_cbc,
    Key = [crypto:strong_rand_bytes(8), crypto:strong_rand_bytes(8), crypto:strong_rand_bytes(8)],
    Ivec = crypto:strong_rand_bytes(8),
    EncryptedKey = create_encrypted_key(RecipientCert, Algo, Key),

    #'EnvelopedData' {
       version = edVer0,
       recipientInfos = {riSet, [create_recipient_info(RecipientCert, EncryptedKey)]},
       encryptedContentInfo = create_encrypted_content_info(Data, Algo, Key, Ivec)
      }.




















-module(smime).

-export([encode_smime/1, get_serial/1, create_recipient_info/2]).

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

get_key_encryption_algorithm() ->
    #'KeyEncryptionAlgorithmIdentifier'{
       algorithm = {1,2,840,113549,1,1,1},
       parameters = <<5,0>>
      }.


create_recipient_info(Cert, EncryptedKey) when is_record(Cert, 'Certificate') ->
    #'RecipientInfo'{
       version = riVer0,
       issuerAndSerialNumber = create_issuer_and_serial_number(Cert),
       keyEncryptionAlgorithm = get_key_encryption_algorithm(),
       encryptedKey = EncryptedKey
      }.

create_smime(Cert, Data) ->
    ok.










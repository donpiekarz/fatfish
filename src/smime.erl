-module(smime).

-export([get_smime/0, encode_smime/1, get_serial/1, create_recipient_info/1]).

-include_lib("public_key/include/public_key.hrl").

get_smime()->
    #'ContentInfo'{
       contentType = {1,2,840,113549,1,7,3},
       content =
           {'EnvelopedData',edVer0,
            {riSet,
             [{'RecipientInfo',riVer0,
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
                  serialNumber = 155948348082582942025801704533592909320},
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
                226,37,55,119,84,43,210,199]},
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
               [106,70,194,39,229,23,109,106,222,143,128,247,156,167,117,
                230,118,156,123,214,15,42,221,234,143,27,59,72,66,173,219,
                125,109,229,168,92,176,206,104,55,112,148,154,237,144,164,
                231,225,128,179,248,246,216,116,22,58,46,121,96,60,143,104,
                234,196,194,191,26,186,94,43,154,165,98,3,193,175,50,178,
                227,110,169,25,249,115,82,53,91,156,17,63,81,179,7,178,97,
                50,155,192,144,124,124,119,130,177,87,0,179,61,228,47,115,
                125,221,33,178,199,49,229,233,172,239,96,208,7,198,153,9,6,
                115,139,84,22,176,253,83,204,39,25,135,87,124,206,46,153,6,
                147,170,113,13,58,236,35,174,62,247,240,200,130,235,240,171,
                129,41,164,73,12,247,97,206,162,174,40,99,18,143,58,32,93,
                115,84,72,138,65,215,96,161,131,246,99,130,116,167,196,114,
                242,47,54,14,36,194,203,255,148,181,97,129,78,186,222,11,
                248,65,90,185,29,254,47,215,202,244,26,220,117,13,167,52,
                125,211,165,178,108,213,108,240,98,178,250,68,62,20,179,23,
                217,138,157,48,179,201,249,154,63,215,75,131,127,67]}]},
            #'EncryptedContentInfo'{
               contentType = {1,2,840,113549,1,7,1},
               contentEncryptionAlgorithm =
                   #'ContentEncryptionAlgorithmIdentifier'{
                      algorithm = {1,2,840,113549,3,7},
                      parameters = <<4,8,64,197,245,230,190,183,123,93>>},
               encryptedContent =
                   [237,248,1,178,3,126,242,138,162,72,136,39,227,207,118,198,
                    95,117,129,12,218,51,30,192,153,194,252,132,61,167,79,56,6,
                    76,207,237,113,143,32,238,216,86,110,1,169,218,138,155,151,
                    67,14,186,25,84,141,89,0,249,169,77,167,81,141,231,50,182,
                    244,133,17,134,133,48,218,78,185,144,58,53,102,51,26,179,
                    253,248,55,142,180,240,128,231,52,235,149,75,26,243,93,71,
                    173,206,70,191,153,144,173,54,67,193,143,158,53,159,125,230,
                    243,182,155,142,236,77,20,27,20,224,229,64,65,232,218,153,
                    223,49,38,154,218,223,44,221,214,20,40,239,230,225,217,202,
                    237,122,62,116,92,55,63,32,68,237,251,35,183,224,223,79,61,
                    70,140,241,211,231,46,68,253,188,24,22,68,111,120,39,180,84,
                    64,132,181,61,78,143,123,39,31,74,169,127,62,154,77,19,49,
                    44,127,13,129,166,127,188,69,167,247,220,163,106,242,209,
                    209,167,230,178,11,220,130,31,54,5,74,128,239,137,105,251,
                    156,80,40,186,5,134,83,80,78,130,244,95,178,3,174,95,199,39,
                    166,226,158,78,66,244,48,100,252,227,94,53,77,149,247,132,
                    229,150,119,84,217,165,170,255,57,116,199,174,205,239,174,
                    92,190,126,113,119,25,128,30,12,112,147,113,2,154,176,39,98,
                    78,139,111,123,118,129,48,196,243,200,56,47,11,55,49,32,154,
                    106,163,24,144,148,101,154,17,2,187,96,247,225,0,224,206,
                    139,246,124,236,189,244,105,91,62,151,25,112,139,188,159,
                    194,89,131,191,221,170,234,182,184,228,33,235,152,88,177,
                    128,120,35,241,111,233,172,121,220,82,175,112,131,233,92,
                    169,220,167,166,154,161,77,85,47,249,247,205,179,114,231,
                    235,205,140,13,81,38,210,78,51,110,28,129,163,24,204,180,
                    141,126,245,166,17,182,89,218,150,124,134,142,239,206,94,
                    100,42,177,7,21,5,229,38,30,96,198,53,183,23,254,202,170,76,
                    236,91,192,48,7,75,190,170,57,79,251,191,105,156,246,191,
                    187,250,233,51,20,49,12,90,163,215,29,50,116,152,143,45,238,
                    206,56,122,59,122,157,47,25,154,16,245,82,46,117,242,55,168,
                    180,51,180,172,51,163,159,84,26,89,185,193,55,172,160,97,
                    224,51,185,230,46,81,111,246,211,186,90,28,170,74,32,254,95,
                    123,101,24,225,80,164,153,63,9,125,180,193,79,124,207,232,
                    128,22,195,124,227,86,174,206,210,72,106,5,57,177,29,198,
                    203,245,110,108,228,130,194,227,250,210,225,178,248,5,32,62,
                    65,253,83,46,93,40,5,249,221,87,218,241,194,153,34,6,113,
                    183,17,158,250,38,136,132,149,3,149,226,228,206,52,126,248,
                    193,213,211,84,90,144,143,148,188,217,173,195,20,137,60,216,
                    35,161,236,185,132,175,223,40,242,181,148,146,126,253,10,
                    120,6,43,201,118,108,116,197,106,70,76,226,165,167,26,122,
                    208,103,118,170,244,212,175,93,2,221,218,126,96,88,67,227,
                    201,210,146,123,167,107,228,55,60,67,193,44,30,136,202,136,
                    211,31,69,16,70,82,83,178,249,103,84,37,241,233,200,105,87,
                    98,125,229,214,141,162,103,27,6,187,4,123,148,27,7,138,168,
                    166,82,74,71,4,77,85,6,182,26,64,39,165,88,89,6,230,151,157,
                    99,176,249,175,222,48,249,31,20,196,27,89,239,17,151,193,
                    141,204,121,211,75,41,48,107,202,112,246,137,6,181,229,136,
                    239,31,230,241,22,48,239,187,151,67,89,15,115,5,161,157,220,
                    152,224,171,19,81,88,119,61,6,84,17,166,87,114,185,71,37,
                    220,190,101,75,27,5,70,79,58,38,229,250,130,33,99,99,67,96,
                    229,29,45,14,86,137,47,31,94,65,187,158,47,140,170,89,13,
                    147,90,211,232,99,123,228,205,241,178,44,255,107,18,62,114,
                    133,28,159,109,93,119,230,111,122,106,26,235,175,203,198,
                    139,46,50,45,147,225,84,48,83,173,38,50,196,49,75,112,58,27,
                    8,164,28,101,9,115,11,231,189,205,212,161,166,126,127,221,
                    253,86,157,195,236,191,195,61,177,2,219,95,217,56,246,102,
                    173,93,58,81,62,194,98,243,50,182,189,109,99,228,105,53,192,
                    50,176,98,64,186,136,173,0,128,128,115,125,187,209,184,193,
                    51,91,137,158,86,57,254,103,247,191,181,80,124,160,132,73,
                    122,141,87,150,87,97,107,142,61,53,53,149,42,226,128,255,
                    189,199,36,178,243,99,160,13,48,80,151,27,40,182,60,161,59,
                    117,125,120,130,28,137,141,225,43,0,249,179,204,238,168,30,
                    139,170,75,138,77,76,8,170,216,48,147,41,144,154,180,71,252,
                    174,193,117,53,54,213,155,175,182,253,144,201,46,17,144,19,
                    168,85,255,165,101,153,159,31,143,161,148,66,153,239,40,187,
                    53,237,78,95,148,191,63,226,181,158,198,179,173,32,177,21,
                    215,67,69,126,164,91,149,205,160,152,192,218,134,221,7,100,
                    214,148,16,200,134,228,59,196,54,9,117,84,61,168,158,13,67,
                    71,209,76,75,86,40,184,82,219,214,200,110,173,157,17,51,22,
                    106,127,144,85,165,175,46,18,80,216,33,1,158,47,201,225,24,
                    224,237,165,145,224,53,209,83,91,163,7,178,219,82,12,85,44,
                    34,124,194,158,185,114,38,247,215,69,159,113,113,215,83,149,
                    251,176,155,241,115,70,224,169,9,159,31,79,31,39,93,222,102,
                    98,216,79,131,17,234,154,31,27,6,99,112,183,29,99,200,143,
                    162,159,78,204,227,16,241,79,234,200,54,106,110,50,208,171,
                    115,123,106,212,225,105,124,166,184,137,76,251,174,101,10,
                    79,148,111,217,191,56,191,235,95,109,164,18,135,248,33,202,
                    133,211,109,36,84,134,202,147,117,113,178,64,97,31,141,105,
                    160,143,81,199,184,221,72,19,92,70,230,115,234,223,249,30,
                    171,65,95,84,199,127]}}}.

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

get_encrypted_key() ->
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
     226,37,55,119,84,43,210,199].

create_recipient_info(Cert) when is_record(Cert, 'Certificate') ->
    #'RecipientInfo'{
       version = riVer0,
       issuerAndSerialNumber = create_issuer_and_serial_number(Cert),
       keyEncryptionAlgorithm = get_key_encryption_algorithm(),
       encryptedKey = get_encrypted_key()
      }.










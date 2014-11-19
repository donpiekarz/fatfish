-module(smime).

-export([get_smime/0, encode_smime/1]).

-include_lib("include/CryptographicMessageSyntax.hrl").
-include_lib("include/PKIX1Explicit88.hrl").

get_smime()->
    #'ContentInfo'{
	   contentType = {1,2,840,113549,1,7,3},
	   content =
	       #'EnvelopedData'{
		  version = v0,originatorInfo = asn1_NOVALUE,
		  recipientInfos =
		      [{ktri,
			#'KeyTransRecipientInfo'{
			   version = v0,
			   rid =
			       {issuerAndSerialNumber,
				#'IssuerAndSerialNumber'{
				   issuer =
				       {rdnSequence,
					[[#'AttributeTypeAndValue'{
					     type = {2,5,4,6},
					     value = <<19,2>>}],
					 [#'AttributeTypeAndValue'{
					     type = {2,5,4,8},
					     value = <<19,3>>}],
					 [#'AttributeTypeAndValue'{type = {2,5,4,7},value = <<19,4>>}],
					 [#'AttributeTypeAndValue'{type = {2,5,4,10},value = <<14,1>>}],
					 [#'AttributeTypeAndValue'{type = {2,5,4,3},value = <<15,2>>}]]},
				   serialNumber = 241576526744086238138532514117904053233}},
			   keyEncryptionAlgorithm =
			       #'KeyTransRecipientInfo_keyEncryptionAlgorithm'{
				  algorithm = {1,2,840,113549,1,1,1},
				  parameters = {asn1_OPENTYPE,<<5,0>>}},
			   encryptedKey =
			       <<106,70,194,39,229,23,109,106,222,143,128>>}}],
		  encryptedContentInfo =
		      #'EncryptedContentInfo'{
			 contentType = {1,2,840,113549,1,7,1},
			 contentEncryptionAlgorithm =
			     #'ContentEncryptionAlgorithmIdentifier'{
                                algorithm = {1,2,840,113549,3,7},
                                parameters =
                                    {asn1_OPENTYPE,<<4,8,64,197,245,230,190,183,123,93>>}},
			 encryptedContent =
			     <<237,248,1,178,3,126,242,138,162,72,136,39,227,207,118>>},
		  unprotectedAttrs = asn1_NOVALUE}}.

encode_smime(Smime)->
    'CryptographicMessageSyntax':encode('ContentInfo', Smime).

#!/bin/sh

set -e

if [ $# -ne 1 ]
then
    echo "Need one argument: clientname" >&2
    exit 1
fi

# This is really the client's username, not the hostname
HOSTNAME="$1"

# 30 years:
DAYS=10950

openssl genrsa -des3 -passout pass:password -out client_key.pem 2048
( echo GB; echo Oxfordshire; echo Oxford; echo GHC client; echo GHC client; echo "$HOSTNAME"; echo; echo password; echo GHC client) | openssl req -new -key client_key.pem -out client_request.csr -passin pass:password
openssl x509 -req -days $DAYS -in client_request.csr -CA root.pem -CAkey ca_key.pem -CAcreateserial -out client.cer -passin pass:password
cat client_key.pem client.cer > client.pem


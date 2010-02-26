#!/bin/sh

set -e

if [ $# -ne 1 ]
then
    echo "Need one argument: hostname" >&2
    exit 1
fi

HOSTNAME="$1"

# 30 years:
DAYS=10950

openssl genrsa -des3 -passout pass:password -out server_key.pem 2048
expect -c 'spawn openssl rsa -in server_key.pem -pubout -outform PEM -out server_pubkey.pem; send "password\r"'
( echo GB; echo Oxfordshire; echo Oxford; echo GHC server; echo GHC server; echo "$HOSTNAME"; echo; echo password; echo GHC server) | openssl req -new -key server_key.pem -out server_request.csr -passin pass:password
openssl x509 -req -days $DAYS -in server_request.csr -CA root.pem -CAkey ca_key.pem -CAcreateserial -out server.cer -passin pass:password
cat server_key.pem server.cer > server.pem


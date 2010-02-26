#!/bin/sh

set -e

if [ $# -ne 0 ]
then
    echo "No arguments expected" >&2
    exit 1
fi

# 30 years:
DAYS=10950

openssl genrsa -des3 -passout pass:password -out ca_key.pem 2048
( echo GB; echo Oxfordshire; echo Oxford; echo GHC root; echo GHC root; echo GHC root; echo) | openssl req -new -key ca_key.pem -x509 -days $DAYS -out root.pem -passin pass:password


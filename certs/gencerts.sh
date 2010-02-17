
HOSTNAME=localhost

openssl dhparam -check -text -5 1024 -out dh1024.pem

openssl genrsa -des3 -passout pass:password -out ca_key.pem 2048
( echo GB; echo Oxfordshire; echo Oxford; echo GHC root; echo GHC root; echo $HOSTNAME; echo) | openssl req -new -key ca_key.pem -x509 -days 3 -out ca_cert.cer -passin pass:password
cp ca_cert.cer root.pem

openssl genrsa -des3 -passout pass:password -out server_key.pem 2048
expect -c 'spawn openssl rsa -in server_key.pem -pubout -outform PEM -out server_pubkey.pem; send "password\r"'
( echo GB; echo Oxfordshire; echo Oxford; echo GHC server; echo GHC server; echo $HOSTNAME; echo; echo password; echo GHC server) | openssl req -new -key server_key.pem -out server_request.csr -passin pass:password
openssl x509 -req -days 3 -in server_request.csr -CA ca_cert.cer -CAkey ca_key.pem -CAcreateserial -out server.cer -passin pass:password
cat server_key.pem server.cer > server.pem

openssl genrsa -des3 -passout pass:password -out client_key.pem 2048
( echo GB; echo Oxfordshire; echo Oxford; echo GHC client; echo GHC client; echo $HOSTNAME; echo; echo password; echo GHC client) | openssl req -new -key client_key.pem -out client_request.csr -passin pass:password
openssl x509 -req -days 3 -in client_request.csr -CA ca_cert.cer -CAkey ca_key.pem -CAcreateserial -out client.cer -passin pass:password
cat client_key.pem client.cer > client.pem

ln -s certs/dh1024.pem certs/root.pem certs/server.pem certs/client.pem ..


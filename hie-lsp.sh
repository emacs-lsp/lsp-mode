#!/bin/sh

export HIE_SERVER_PATH=`which hie`

if [ "X" = "X$HIE_SERVER_PATH" ]; then
  echo "Content-Length: 100\r\n\r"
  echo '{"jsonrpc":"2.0","id":1,"error":{"code":-32099,"message":"Cannot find hie.exe in the path"}}'
  exit 1
fi

#hie --lsp
hie --lsp -d -l /tmp/hie.log


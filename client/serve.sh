#!/bin/sh

PORT=1234
if [ ! -z "$1" ]; then
	PORT="$1"
fi	

python -m SimpleHTTPServer $PORT

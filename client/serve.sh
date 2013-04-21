#!/bin/sh

PORT=8080
if [ ! -z "$1" ]; then
	PORT="$1"
fi	

python -m SimpleHTTPServer $PORT

#!/bin/sh

erl -pa apps/*/ebin \
    -pa deps/*/ebin \
    -config rel/files/sys.config \
    -s artifice 

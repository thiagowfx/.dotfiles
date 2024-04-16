#!/bin/sh

# https://go.dev/wiki/GOPATH
export GOPATH="$HOME/go"

path_munge "$GOPATH/bin" "/usr/local/go/bin"

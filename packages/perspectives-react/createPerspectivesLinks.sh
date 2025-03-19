#!/usr/bin/env bash

cd node_modules

rm -R perspectives-proxy

ln -s ../../perspectives-proxy perspectives-proxy

rm -R perspectives-highlightjs

ln -s ../../perspectives-highlightjs perspectives-highlightjs

cd ..

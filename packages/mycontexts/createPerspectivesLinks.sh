#!/usr/bin/env bash

cd node_modules

rm -R perspectives-proxy
rm -R perspectives-react
rm -R perspectives-core
# rm -R perspectives-pageworker
# rm -R perspectives-sharedworker
# rm -R perspectives-serviceworker
# rm -R perspectives-highlightjs

ln -s ../../perspectives-proxy perspectives-proxy
ln -s ../../perspectives-react perspectives-react
ln -s ../../perspectives-core perspectives-core

# ln -s ../../perspectives-pageworker perspectives-pageworker
# ln -s ../../perspectives-sharedworker perspectives-sharedworker
# ln -s ../../perspectives-serviceworker perspectives-serviceworker

# ln -s ../../perspectives-highlightjs perspectives-highlightjs
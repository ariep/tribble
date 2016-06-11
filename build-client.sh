#!/bin/bash
stack build --stack-yaml=client/stack.yaml
cp -f $(stack path --stack-yaml=client/stack.yaml --local-install-root)/bin/*-client.jsexe/* client/js/

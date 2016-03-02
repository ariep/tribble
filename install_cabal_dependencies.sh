#!/bin/sh

n=0
until [ $n -ge 5 ]
do
    cabal install --only-dependencies tribble-common tribble-server && break
    n=$(($n+1))
    sleep 15
done

if [ $n -ge 5 ]
then
    exit 1
fi

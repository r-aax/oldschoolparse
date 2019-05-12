#!/bin/sh

cd ./src

echo "rm -f *.beam"
rm -f *.beam

for SRC in *.erl
do
    echo "erlc $SRC"
    erlc $SRC
done

cd ..

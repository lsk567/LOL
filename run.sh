#!/bin/bash

set -e

if [ -z "$1" ]
  then
    echo "Usage: ./run.sh <name_of_file.lol>"
    exit 1
fi
f=$1
n=${f%.lol*}
cat $f | ./lol.native > "$n.ll"
llc -relocation-model=pic "$n.ll"
gcc -o "$n" "$n.s" -L/usr/local/lib "builtins.o" -lgsl -lgslcblas -lm
"./$n"

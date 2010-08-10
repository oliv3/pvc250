#!/bin/sh
reset
erl -sname tsp +A 4 +P 500000 -s tsp $*

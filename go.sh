#!/bin/sh
reset
erl +K true -sname tsp +A 256 +P 500000 -s tsp $*

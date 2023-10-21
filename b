#!/bin/sh
acme -v 22000.b
diff -s "load 1.prg" "original/load 1.prg"
cmp "load 1.prg" "original/load 1.prg"
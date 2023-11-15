#!/bin/sh
acme -Wtype-mismatch -r 22000.lst -l 22000.sym 22000.b
diff -s "load 1.prg" "original/load 1.prg"
cmp "load 1.prg" "original/load 1.prg"
#!/bin/sh
acme -Wtype-mismatch -r load1.lst -l load1.sym load1_2000.b
diff -s "load 1.prg" "original/load 1.prg"
cmp "load 1.prg" "original/load 1.prg"
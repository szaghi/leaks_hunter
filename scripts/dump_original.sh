#!/usr/bin/env bash

rm -f a.out
gfortran -fdump-tree-original $1
rm -f *.mod

#!/usr/bin/env bash

rm -f a.out
gfortran -C -g -W -fbacktrace $1
rm -f *.mod

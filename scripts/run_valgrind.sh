#!/usr/bin/env bash

valgrind --leak-check=full --show-leak-kinds=all -v a.out

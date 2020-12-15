#!/bin/bash
set -e

stack build

cd public
./build.sh
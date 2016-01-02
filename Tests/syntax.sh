#!/bin/bash

# This small bash script checks for some ugly Mathematica syntax in the code

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $DIR
cd ..

grep -R --exclude-dir={.git,.svn} --exclude=\*.{tex,sh,gz,nb,pdf} ';;' .
echo
grep -R --exclude-dir={.git,.svn} --exclude=\*.{tex,sh,gz,nb,pdf} ';,' .

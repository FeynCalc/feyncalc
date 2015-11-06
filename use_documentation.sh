#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $DIR
cd FeynCalc
rm -f Documentation
ln -s DocOutput Documentation

#!/bin/bash

# This software is covered by the GNU General Public License 3.
# Copyright (C) 1990-2016 Rolf Mertig
# Copyright (C) 1997-2016 Frederik Orellana
# Copyright (C) 2014-2016 Vladyslav Shtabovenko

# Description:

# Runs FeynCalc tests (without integration tests) on all Mathematica versions

set -o pipefail
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $DIR


echo "==================================================================="
echo "=                 Starting the FeynCalc test run                  ="
echo "==================================================================="

SECONDS=0

# Unit tests
$DIR/unittests.sh math8 &&
$DIR/unittests.sh math9 &&
$DIR/unittests.sh math &&
# TARCER tests
$DIR/tarcertests.sh math8 &&
$DIR/tarcertests.sh math9 &&
$DIR/tarcertests.sh math

echo "==================================================================="
echo "=                 FeynCalc test run finished                      ="
echo "==================================================================="
duration=$SECONDS
echo "$(($duration / 60)) minutes and $(($duration % 60)) seconds elapsed."

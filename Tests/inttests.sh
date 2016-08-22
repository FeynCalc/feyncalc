#!/bin/bash

# This software is covered by the GNU General Public License 3.
# Copyright (C) 1990-2016 Rolf Mertig
# Copyright (C) 1997-2016 Frederik Orellana
# Copyright (C) 2014-2016 Vladyslav Shtabovenko

# Description:

# Runs FeynCalc on a set of integration tests.
# Those are longer than unit tests and hence can take some time to finish.
# The first argument specifies the command-line Mathematica binary
# The second argument be used to choose a particular test,
# e.g. inttests.sh math9 will run all the tests with Mathematica 9
# inttests.sh 10 Lorentz will run only tests from iTestsLorentz.mt with Mathematica 10

set -o pipefail
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $DIR

$1 -nopromt -script TestSuite.m -run testType=2 -run onlyTest=\"$2\"

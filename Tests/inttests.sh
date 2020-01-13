#!/bin/bash

# This software is covered by the GNU General Public License 3.
# Copyright (C) 1990-2018 Rolf Mertig
# Copyright (C) 1997-2018 Frederik Orellana
# Copyright (C) 2014-2018 Vladyslav Shtabovenko

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

$1 -nopromt -script TestSuite.m -run testType=2 -run onlyTest=\"$2\" -run onlySubTest=\"$3\"

if [ -z "$2" ]
  then
    outtxt="Finished running integration tests for FeynCalc."
  else
    outtxt="Finished running integration tests for FeynCalc ($2)."
fi

notify-send --urgency=low -i "$([ $? = 0 ] && echo sunny || echo error)" "$outtxt"

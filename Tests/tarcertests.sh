#!/bin/bash

# This software is covered by the GNU General Public License 3.
# Copyright (C) 1990-2018 Rolf Mertig
# Copyright (C) 1997-2018 Frederik Orellana
# Copyright (C) 2014-2018 Vladyslav Shtabovenko

# Description:

# Runs FeynCalc on a set of unit tests.
# The first argument specifies the command-line Mathematica binary
# The second argument be used to choose a particular test,
# e.g. unittests.sh math9 will run all the tests with Mathematica 9
# unittests.sh 10 Dirac will run only tests from Dirac.mt with Mathematica 10

set -o pipefail
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
cd $DIR

$1 -nopromt -script TestSuite.m -run testType=3 -run onlyTest=\"$2\"

if [ -z "$2" ]
  then
    outtxt="Finished running unit tests for TARCER."
  else
    outtxt="Finished running unit tests for TARCER ($2)."
fi

notify-send --urgency=low -i "$([ $? = 0 ] && echo sunny || echo error)" "$outtxt"

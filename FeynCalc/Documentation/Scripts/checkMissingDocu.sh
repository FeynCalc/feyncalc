#!/bin/bash

# This software is covered by the GNU General Public License 3.
# Copyright (C) 1990-2022 Rolf Mertig
# Copyright (C) 1997-2022 Frederik Orellana
# Copyright (C) 2014-2022 Vladyslav Shtabovenko

# Description:

# Checks for undocumented FeynCalc symbols

# Usage examples

# ./checkMissingDocu.sh math

scriptDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
mainDir="$(dirname $scriptDIR)"

MATH=$1

$MATH -nopromt -script "$scriptDIR"/CheckMissingDocu.m

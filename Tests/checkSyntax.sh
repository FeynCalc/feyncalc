#!/bin/bash

# This software is covered by the GNU General Public License 3.
# Copyright (C) 1990-2023 Rolf Mertig
# Copyright (C) 1997-2023 Frederik Orellana
# Copyright (C) 2014-2023 Vladyslav Shtabovenko

# Description:

# Checks the syntax using CodeInspector package.

# Usage examples

# ./checkSyntax.sh math

scriptDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
mainDir="$(dirname $scriptDIR)"

MATH=$1

$MATH -nopromt -script "$scriptDIR"/CheckSyntax.m

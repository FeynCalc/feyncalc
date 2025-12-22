#!/bin/bash

# This software is covered by the GNU General Public License 3.
# Copyright (C) 1990-2026 Rolf Mertig
# Copyright (C) 1997-2026 Frederik Orellana
# Copyright (C) 2014-2026 Vladyslav Shtabovenko

# Description:

# Checks the syntax using CodeInspector package.

# Usage examples

# ./checkSyntax.sh math

scriptDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
mainDir="$(dirname $scriptDIR)"

MATH=$1

$MATH -nopromt -script "$scriptDIR"/CheckSyntax.m

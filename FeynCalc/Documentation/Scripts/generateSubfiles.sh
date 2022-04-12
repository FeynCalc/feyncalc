#!/bin/bash

# This software is covered by the GNU General Public License 3.
# Copyright (C) 1990-2022 Rolf Mertig
# Copyright (C) 1997-2022 Frederik Orellana
# Copyright (C) 2014-2022 Vladyslav Shtabovenko

# Description:

# Generates list of tex subfile to include in the manual
# This script is automatically called by generateTeX.sh

# Usage examples

# ./generateSubfiles.sh math /media/Data/Projects/VS/feyncalc-manual/


scriptDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
mainDir="$(dirname $scriptDIR)"

MATH=$1
OUTDIR=$2

if [[ $# -eq 2 ]] ; then
    $MATH -nopromt -script "$scriptDIR"/GenerateSubfiles.m -run outputDir="\"$2\""
fi




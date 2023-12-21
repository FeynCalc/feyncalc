#!/bin/bash

# This software is covered by the GNU General Public License 3.
# Copyright (C) 1990-2024 Rolf Mertig
# Copyright (C) 1997-2024 Frederik Orellana
# Copyright (C) 2014-2024 Vladyslav Shtabovenko

# Description:

# Generates list of tex subfile to include in the manual
# This script is automatically called by generateTeX.sh

# Usage examples

# export DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation"; export DOCU_INDEX_FILE=$DOCU_SOURCE_DIR/Markdown/Extra/FeynCalc.md; ./generateSubfiles.sh math /media/Data/Projects/VS/feyncalc-manual/

# export DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/AddOns/FeynHelpers/Documentation"; export DOCU_INDEX_FILE=$DOCU_SOURCE_DIR/Markdown/Extra/FeynHelpers.md; ./generateSubfiles.sh math /media/Data/Projects/VS/feynhelpers-manual/

MAKE_DOCU_LOAD_ADDONS="FeynHelpers, FeynArtsLoader";

if [[ -z "${DOCU_SOURCE_DIR}" ]]; then
  echo "You need to set the environmental variable DOCU_SOURCE_DIR that contains the full path to the relevant Documentation directory"
  exit
else
  mainDir="${DOCU_SOURCE_DIR}"
fi

if [[ -z "${DOCU_INDEX_FILE}" ]]; then
  echo "You need to set the environmental variable DOCU_INDEX_FILE that contains the full path to the relevant markdown index file"
  exit
else
  indexFile="${DOCU_INDEX_FILE}"
fi

scriptDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
MATH=$1
OUTDIR=$2

if [[ $# -eq 2 ]] ; then
    $MATH -nopromt -script "$scriptDIR"/GenerateSubfiles.m -run outputDir="\"$2\"" -run docuDir="\"$mainDir\"" -run indexFile="\"$indexFile\""
fi




#!/bin/bash

# This software is covered by the GNU General Public License 3.
# Copyright (C) 1990-2023 Rolf Mertig
# Copyright (C) 1997-2023 Frederik Orellana
# Copyright (C) 2014-2023 Vladyslav Shtabovenko

# Description:

# Checks for undocumented FeynCalc/add-on symbols

# Usage examples

# ./checkMissingDocu.sh math

# export MAKE_DOCU_LOAD_ADDONS="{}"; export DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation"; export DOCU_INDEX_FILE=$DOCU_SOURCE_DIR/Markdown/Extra/FeynCalc.md; ./checkMissingDocu.sh math

# export MAKE_DOCU_LOAD_ADDONS="{FeynHelpers}"; export DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/AddOns/FeynHelpers/Documentation"; export DOCU_INDEX_FILE=$DOCU_SOURCE_DIR/Markdown/Extra/FeynHelpers.md; ./checkMissingDocu.sh math

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

if [[ -z "${MAKE_DOCU_LOAD_ADDONS}" ]]; then
  echo "You need to set the environmental variable MAKE_DOCU_LOAD_ADDONS that contains the list of add-ons to be loaded"
  exit
else
  requestedAddOns="${MAKE_DOCU_LOAD_ADDONS}"
fi

scriptDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"

MATH=$1
$MATH -nopromt -script "$scriptDIR"/CheckMissingDocu.m -run docuDir="\"$mainDir\"" -run indexFile="\"$indexFile\"" -run loadAddOns="\"$requestedAddOns\""

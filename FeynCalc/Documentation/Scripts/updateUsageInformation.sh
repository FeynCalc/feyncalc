#!/bin/bash

# This software is covered by the GNU General Public License 3.
# Copyright (C) 1990-2024 Rolf Mertig
# Copyright (C) 1997-2024 Frederik Orellana
# Copyright (C) 2014-2024 Vladyslav Shtabovenko

# Description:

# Updates the usage information of FeynCalc/add-on symbols to keep it in sync with the documentation

# Usage examples

# export MAKE_DOCU_LOAD_ADDONS="{}"; export DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation"; ./updateUsageInformation.sh math

# export MAKE_DOCU_LOAD_ADDONS="{FeynHelpers}"; export DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/AddOns/FeynHelpers/Documentation"; ./updateUsageInformation.sh math

if [[ -z "${DOCU_SOURCE_DIR}" ]]; then
  echo "You need to set the environmental variable DOCU_SOURCE_DIR that contains the full path to the relevant Documentation directory"
  exit
else
  mainDir="${DOCU_SOURCE_DIR}"
fi

scriptDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"


MATH=$1

$MATH -nopromt -script "$scriptDIR"/UpdateUsageInformation.m -run docuDir="\"$mainDir\"" -run loadAddOns="\"$requestedAddOns\""

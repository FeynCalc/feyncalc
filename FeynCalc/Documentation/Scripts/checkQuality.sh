#!/bin/bash

# This software is covered by the GNU General Public License 3.
# Copyright (C) 1990-2022 Rolf Mertig
# Copyright (C) 1997-2022 Frederik Orellana
# Copyright (C) 2014-2022 Vladyslav Shtabovenko

# Description:

# Checks for superfluous spaces etc.

# Usage examples

# export DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation";  ./checkQuality.sh

# export DOCU_SOURCE_DIR="/media/Data/Projects/VS/FeynCalc/FeynCalc/AddOns/FeynHelpers/Documentation"; ./checkQuality.sh

if [[ -z "${DOCU_SOURCE_DIR}" ]]; then
  echo "You need to set the environmental variable DOCU_SOURCE_DIR that contains the full path to the relevant Documentation directory"
  exit
else
  mainDir="${DOCU_SOURCE_DIR}"
fi

scriptDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"

echo "Check documentation files missing the |See Also| section:"
grep -RL "See also" $mainDir"/Mathematica"

echo "Check documentation files missing the |Examples| section:"
grep -RL "Examples" $mainDir"/Mathematica"

echo "Check documentation files for superfluous spaces etc."
ag '\. \*\)' $mainDir"/Mathematica"
echo ""
ag '[a-zA-Z0-9] \*\)' $mainDir"/Mathematica"
echo ""
ag '\(\* [a-zA-Z0-9]' $mainDir"/Mathematica"


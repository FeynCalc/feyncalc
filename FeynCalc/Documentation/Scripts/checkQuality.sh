#!/bin/bash

# This software is covered by the GNU General Public License 3.
# Copyright (C) 1990-2021 Rolf Mertig
# Copyright (C) 1997-2021 Frederik Orellana
# Copyright (C) 2014-2021 Vladyslav Shtabovenko

# Description:

# Checks for superfluous spaces etc.

# Usage examples

# ./checkQuality.sh

scriptDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
mainDir="$(dirname $scriptDIR)"

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


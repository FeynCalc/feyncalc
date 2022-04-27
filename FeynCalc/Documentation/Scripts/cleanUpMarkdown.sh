#!/bin/bash

# This software is covered by the GNU General Public License 3.
# Copyright (C) 1990-2022 Rolf Mertig
# Copyright (C) 1997-2022 Frederik Orellana
# Copyright (C) 2014-2022 Vladyslav Shtabovenko

# Description:

# Fixes some common issues in the automatically generated Markdown files

# Usage examples

# ./cleanUpMarkdown.sh "/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation/Markdown/ApartFF.md"
# ./cleanUpMarkdown.sh "/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation/Markdown"

scriptDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
mainDir="$(dirname $scriptDIR)"

if [[ -f $1 ]]; then
    echo Post processing the file "$1"
    echo
    sed -i -e "s|img/\(.*\).pdf)|img/\1.svg)|g" $1;
    sed -i -e 's|\$\\\$\$|\\$|g' $1;
    sed -i -e 's|\^\*\^{|\^\{\*|g' $1;
    sed -i -e 's|\^+\^{|\^\{+|g' $1;
    sed -i -e 's| \\text{| \\;\\text\{|g' $1;
    sed -i -e 's|}\\text{|}\\;\\text\{|g' $1;
    sed -i -e 's|}\\overline{\\text{|}\\;\\overline{\\text\{|g' $1;
    sed -i -e "s|\^'\(.*\)\$\\$|\^{'\1}\$\$|" $1;
    sed -i -e "s|unicode{f4a1}|to |g" $1;
    sed -i -e "s|unicode{f3d4}|leftrightarrow |g" $1;
    sed -i -e "s|unicode{f3d4}|leftrightarrow |g" $1;
    sed -i -e "s|\^2\^2|\^4|g" $1;
    sed -i -e "s|\^2\^3|\^6|g" $1;
    sed -i -e 's|g\^{\\mu \\nu }^2|(g\^{\\mu \\nu})^2|' $1;
    sed -i -e 's|\\bar{\\delta }\^{ij}\^2|(\\bar{\\delta}\^{ij})^2|' $1;
    sed -i -e 's|\$\$\(!\[.*\)\$\$|\1|' $1;
elif [[ -d $1 ]]; then
 
allFilesRaw=$(find $mainDir/Markdown/ -type f -name '*.md' -print)
allFiles=($(printf "%s\n" "${allFilesRaw[@]}" | sort -V))


if [[ -z ${allFiles} ]]; then
    echo "No files to process, leaving."
    exit 0;
fi



parallel -j 6 --bar --progress  "$scriptDIR/cleanUpMarkdown.sh {} $OUTDIR" ::: ${allFiles[@]};

else
 echo "Invalid input, the argument must be a file or a directory!"
fi

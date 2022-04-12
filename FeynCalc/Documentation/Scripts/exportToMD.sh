#!/bin/bash

# This software is covered by the GNU General Public License 3.
# Copyright (C) 1990-2022 Rolf Mertig
# Copyright (C) 1997-2022 Frederik Orellana
# Copyright (C) 2014-2022 Vladyslav Shtabovenko

# Description:

# Converts FeynCalc documentation from Mathematica to Markdown

# Usage examples

# ./exportToMD.sh math /media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation/Markdown
# ./exportToMD.sh math "/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation/Mathematica/LC.m" "/media/Data/Projects/VS/FeynCalc/FeynCalc/Documentation/Markdown"

scriptDIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
mainDir="$(dirname $scriptDIR)"

MATH=$1
OUTDIR=$2

if [[ $# -eq 3 ]] ; then
    $MATH -nopromt -script "$scriptDIR"/ExportToMD.m -run inputNB="\"$2\"" -run outputDir="\"$3\""
else

allFilesRaw=$(find $mainDir/Mathematica/ -type f -name '*.m' -print)
allFilesRaw=($(printf "%s\n" "${allFilesRaw[@]}" | sort -V))

declare -a allFiles
for i in "${allFilesRaw[@]}"; do  
  name=$(basename -s .m $i)
  fullPath=$OUTDIR/$name".md"
  if [ -f $fullPath ]; then
    true
    #echo "Skipping $name - file already exists."
    #echo
  else
#   echo "Adding $name";
    allFiles+=($i) 
  fi
done
echo 

echo "Relevant files"
for value in "${allFiles[@]}"
do
     echo $value
done

if [ -z ${allFiles} ]; then
    echo "No files to process, leaving."
    exit 0;
fi



parallel -j 4 -u --eta --bar "$MATH -nopromt -script $scriptDIR/ExportToMD.m  -run outputDir='\"$2\"'" -run inputNB='\"{}\"'  ::: ${allFiles[@]}
$scriptDIR/cleanUpMarkdown.sh $OUTDIR
$scriptDIR/pdfToSvg.sh $OUTDIR/img/
#-------------------------------------------------------------------------------
notify-send --urgency=low -i "$([ $? = 0 ] && echo sunny || echo error)" "Finished converting FeynCalc documentation to markdown."
fi


